{-# LANGUAGE OverloadedStrings #-}
module WebApi where

import Web.Scotty
import Data.Aeson
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status (status400)

import Data.IORef
import Control.Monad (when)
import LogicaQuiz
import Tipos
import BuscarApi (buscarCategorias)


type GameSession = IORef (Maybe (ModoJogo, String, Maybe String, EstadoJogo, Questao))

iniciarServidor :: IO ()
iniciarServidor = do
    putStrLn "Servidor iniciado em http://0.0.0.0:3000"
    putStrLn "Acesso local: http://localhost:3000" 
    putStrLn "Para acessar de outros dispositivos, use o IP da sua máquina na porta 3000"
    -- Criar sessão global simplificada
    gameSession <- newIORef Nothing
    
    scotty 3000 $ do
        middleware simpleCors
        
        -- Arquivos estáticos
        get "/" $ file "static/index.html"
        get "/style.css" $ file "static/estetica.css"
        get "/script.js" $ file "static/script.js"
        
        -- GET /api/categorias - Lista as categorias disponíveis
        get "/api/categorias" $ do
            categorias <- liftIO buscarCategorias
            json categorias
        
        -- POST /api/jogo/iniciar - Inicia um novo jogo
        post "/api/jogo/iniciar" $ do
            config <- jsonData :: ActionM ConfiguracaoJogo
            let modoJogo = modo config
            let dificuldadeJogo = dificuldade config
            let categoriaJogo = categoria_filtro config
            
            liftIO $ putStrLn $ "Iniciando jogo - Modo: " ++ show modoJogo ++ ", Dificuldade: " ++ dificuldadeJogo ++ ", Categoria: " ++ show categoriaJogo
            
            -- Iniciação do jogo
            estadoInicial <- liftIO $ iniciarJogo modoJogo
            liftIO $ putStrLn $ "Estado inicial criado: " ++ show estadoInicial
            
            primeiraQuestao <- liftIO $ obterProximaQuestao dificuldadeJogo categoriaJogo
            case primeiraQuestao of
                Just questao -> do
                    liftIO $ putStrLn $ "Primeira questão obtida: " ++ show (questao_id questao)
                    liftIO $ writeIORef gameSession (Just (modoJogo, dificuldadeJogo, categoriaJogo, estadoInicial, questao))
                    json $ object [
                        "estado" .= estadoInicial,
                        "questao" .= questao,
                        "modo" .= modoJogo,
                        "jogo_ativo" .= True
                        ]
                Nothing -> do
                    liftIO $ putStrLn "AVISO: API externa indisponível ao iniciar"
                    liftIO $ writeIORef gameSession (Just (modoJogo, dificuldadeJogo, categoriaJogo, estadoInicial, Questao 0 "Sem questão" [] 0 "" ""))
                    json $ object [
                        "estado" .= estadoInicial,
                        "questao" .= Null,
                        "modo" .= modoJogo,
                        "jogo_ativo" .= True,
                        "erro_api" .= True
                        ]
        
        -- POST /api/jogo/responder - Processa uma resposta
        post "/api/jogo/responder" $ do
            resposta <- jsonData :: ActionM Resposta
            liftIO $ putStrLn $ "Resposta recebida: " ++ show resposta
            
            -- Recuperar estado da sessão
            sessaoAtual <- liftIO $ readIORef gameSession
            case sessaoAtual of
                Nothing -> do
                    status status400
                    json $ object ["erro" .= ("Nenhum jogo ativo." :: String)]
                Just (modoJogo, _dificuldade, _categoria, estadoJogo, questaoAtual) -> do
                    -- Validar se id bate (quando >0)
                    let idEsperado = questao_id questaoAtual
                    let idRecebido = questao_respondida resposta
                    when (idEsperado /= 0 && idRecebido /= idEsperado) $ liftIO $ putStrLn "AVISO: ID da questão recebido difere do esperado"
                    (novoEstado, resultado, deveEncerrar) <- liftIO $ processarJogada modoJogo estadoJogo questaoAtual resposta
                    if deveEncerrar
                        then do
                            let resultadoFinal = calcularResultadoFinal modoJogo novoEstado
                            liftIO $ writeIORef gameSession Nothing
                            json $ object [
                                "resultado" .= resultado,
                                "jogo_encerrado" .= True,
                                "resultado_final" .= resultadoFinal
                                ]
                        else do
                            proxima <- liftIO $ obterProximaQuestao _dificuldade _categoria
                            case proxima of
                                Just qn -> do
                                    liftIO $ writeIORef gameSession (Just (modoJogo, _dificuldade, _categoria, novoEstado, qn))
                                    json $ object [
                                        "resultado" .= resultado,
                                        "estado" .= novoEstado,
                                        "questao" .= qn,
                                        "jogo_encerrado" .= False
                                        ]
                                Nothing -> do
                                    -- Mantém mesma questão se API falhou
                                    liftIO $ writeIORef gameSession (Just (modoJogo, _dificuldade, _categoria, novoEstado, questaoAtual))
                                    json $ object [
                                        "resultado" .= resultado,
                                        "estado" .= novoEstado,
                                        "questao" .= Null,
                                        "erro_api" .= True,
                                        "jogo_encerrado" .= False
                                        ]
        
        -- GET /api/jogo/status - Verifica status do jogo atual
        get "/api/jogo/status" $ do
            sessaoAtual <- liftIO $ readIORef gameSession
            case sessaoAtual of
                Nothing -> json $ object ["jogo_ativo" .= False]
                Just (modoAtual, _dificuldadeAtual, _categoriaAtual, estadoAtual, questaoAtual) -> json $ object [
                    "jogo_ativo" .= True,
                    "modo" .= modoAtual,
                    "estado" .= estadoAtual,
                    "questao_id" .= questao_id questaoAtual
                    ]
        
        -- POST /api/jogo/encerrar - Força o encerramento do jogo
        post "/api/jogo/encerrar" $ do
            sessaoAtual <- liftIO $ readIORef gameSession
            case sessaoAtual of
                Nothing -> json $ object ["jogo_encerrado" .= False]
                Just (modoFinal, _dificuldadeFinal, _categoriaFinal, estadoFinal, _) -> do
                    let resultadoFinal = calcularResultadoFinal modoFinal estadoFinal
                    liftIO $ writeIORef gameSession Nothing
                    json $ object ["jogo_encerrado" .= True, "resultado_final" .= resultadoFinal]
        
        -- GET /api/tentar-questao - Tenta obter questão sem processar jogada (para recovery de API)
        get "/api/tentar-questao" $ do
            sessaoAtual <- liftIO $ readIORef gameSession
            case sessaoAtual of
                Nothing -> do status status400 >> json (object ["erro" .= ("Nenhum jogo" :: String)])
                Just (m, _dific, _categ, estadoJogo, _) -> do
                    nova <- liftIO $ obterProximaQuestao _dific _categ
                    case nova of
                        Just qn -> do
                            liftIO $ writeIORef gameSession (Just (m, _dific, _categ, estadoJogo, qn))
                            json $ object ["questao" .= qn, "estado" .= estadoJogo, "sucesso" .= True]
                        Nothing -> json $ object ["erro_api" .= True, "sucesso" .= False]
        
        -- POST /api/answer - Processa resposta e retorna feedback imediato
        post "/api/answer" $ do
            resposta <- jsonData :: ActionM Resposta
            sessaoAtual <- liftIO $ readIORef gameSession
            case sessaoAtual of
                Nothing -> status status400 >> json (object ["erro" .= ("Nenhum jogo" :: String)])
                Just (modoJogo, _dif, _categ, estadoJogo, questaoAtual) -> do
                    (novoEstado, resultado, deveEncerrar) <- liftIO $ processarJogada modoJogo estadoJogo questaoAtual resposta
                    liftIO $ writeIORef gameSession (Just (modoJogo, _dif, _categ, novoEstado, questaoAtual))
                    json $ object [
                        "resultado" .= resultado,
                        "estado" .= novoEstado,
                        "deve_encerrar" .= deveEncerrar
                        ]