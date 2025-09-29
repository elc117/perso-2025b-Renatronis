module LogicaQuiz where

import Tipos
import Data.Time (getCurrentTime, diffUTCTime)
import BuscarApi

-- Verifica se a resposta está correta
verificarResposta :: Questao -> Resposta -> Resultado
verificarResposta questao resposta =
    let
        respostaCerta = resposta_certa questao
        respostaEscolhida = alternativa_escolhida resposta
        acertou = (respostaCerta == respostaEscolhida)
    in 
        Resultado {
            saber_resposta_correta = acertou,
            guardar_resposta_certa = respostaCerta,
            alt_escolhida = respostaEscolhida
        }

-- Função que encerra o modo de jogo "Ate Errar"
encerrarAteErrar :: Resultado -> Bool
encerrarAteErrar resultado_jogo = 
    if saber_resposta_correta resultado_jogo
        then False  
        else True    

-- Função que encerra o modo de jogo "Clássico"
encerrarClassico :: EstadoJogo -> Bool  
encerrarClassico jogo_estado = 
    erros jogo_estado + acertos jogo_estado >= 10

-- Função que encerra o modo de jogo "Contra o tempo"
encerrarCronometrado :: EstadoJogo -> IO Bool
encerrarCronometrado estado = do
    tempoAtual <- getCurrentTime
    let tempoDecorrido = diffUTCTime tempoAtual (tempo_iniciado estado)
    return (tempoDecorrido >= 60)  

-- Verifica se deve encerrar baseado no modo. Agrupa todos os encerramentos
verificarEncerramentoGeral :: ModoJogo -> EstadoJogo -> Resultado -> IO Bool
verificarEncerramentoGeral AteErrar _ resultado = return (encerrarAteErrar resultado)
verificarEncerramentoGeral Classico estado _ = return (encerrarClassico estado)
verificarEncerramentoGeral Cronometrado estado _ = encerrarCronometrado estado 

-- Cria o estado inicial baseado no modo escolhido
iniciarJogo :: ModoJogo -> IO EstadoJogo
iniciarJogo modo = do
    tempoAtual <- getCurrentTime
    return EstadoJogo {
        questoes_respondidas = 0,
        acertos = 0,
        erros = 0,
        tempo_iniciado = tempoAtual,
        jogo_ativo = True,
        proximo_id = 1,              
        questoes_ja_usadas = []      
    }
-- Atualiza o jogo após cada interação
atualizarEstado :: EstadoJogo -> Resultado -> EstadoJogo
atualizarEstado estado resultado = 
   estado {
    questoes_respondidas = questoes_respondidas estado + 1,
    acertos = if saber_resposta_correta resultado
        then acertos estado + 1
        else acertos estado,
    erros = if saber_resposta_correta resultado
        then erros estado
        else erros estado + 1
   }

-- Wrapper que usa dados do EstadoJogo para chamar obterProximaQuestao
obterProximaQuestaoDoEstado :: EstadoJogo -> String -> Maybe String -> IO (Maybe Questao)
obterProximaQuestaoDoEstado estado dificuldade mbCategoria = 
    obterProximaQuestao 
        (proximo_id estado) 
        dificuldade 
        mbCategoria 
        (questoes_ja_usadas estado)

-- Busca questão da API, filtra duplicatas e atribui ID único
obterProximaQuestao :: Int -> String -> Maybe String -> [String] -> IO (Maybe Questao)
obterProximaQuestao idAtual dificuldade mbCategoria textosUsados = do
    questoesApi <- buscarQuestoes 5 dificuldade mbCategoria  -- Busca 5 para ter opções
    let questoesNovas = filter (\q -> texto q `notElem` textosUsados) questoesApi
    
    case questoesNovas of
        (questao:_) -> return $ Just questao { questao_id = idAtual }
        [] -> do
            putStrLn "Nenhuma questão nova encontrada, tentando novamente..."
            return Nothing

--  Função que processa uma jogada completa: verifica resposta, atualiza estado e checa encerramento
processarJogada :: ModoJogo -> EstadoJogo -> Questao -> Resposta -> IO (EstadoJogo, Resultado, Bool)
processarJogada modo estado questao resposta = do
    let resultado = verificarResposta questao resposta
    let novoEstado = atualizarEstado estado resultado
    let estadoComQuestaoUsada = novoEstado { 
        questoes_ja_usadas = texto questao : questoes_ja_usadas novoEstado
    }
    
    deveEncerrar <- verificarEncerramentoGeral modo estadoComQuestaoUsada resultado
    return (estadoComQuestaoUsada, resultado, deveEncerrar)

-- Função que calcula o resultado final da partida
calcularResultadoFinal :: ModoJogo -> EstadoJogo -> ResultadoFinal
calcularResultadoFinal modo estado = 
    let
        totalQuestoes = questoes_respondidas estado
        totalAcertos = acertos estado
        pontuacao = totalAcertos * 10
        porcentagem = if totalQuestoes > 0 
                      then round ((fromIntegral totalAcertos / fromIntegral totalQuestoes) * 100 :: Double)
                      else 0
    in
        ResultadoFinal {
            modo_jogado = modo,
            total_questoes = totalQuestoes,
            total_acertos = totalAcertos,
            pontuacao = pontuacao,
            porcentagem_acerto = porcentagem
        }