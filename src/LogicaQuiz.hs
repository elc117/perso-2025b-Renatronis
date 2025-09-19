module LogicaQuiz where

import Tipos
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
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
        jogo_ativo = True
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

-- Função que obtém a proxima questão usando "buscarQuestoes"
obterProximaQuestao :: IO (Maybe Questao)
obterProximaQuestao = do
    questoesApi <- buscarQuestoes 1
    case questoesApi of
        (questao:_) -> return (Just questao)
        [] -> return Nothing

-- Função que armazena os textos da questão pro frontend usar
obterDadosQuestao :: Questao -> (Int, String, [String], String, String)
obterDadosQuestao questao = (
    questao_id questao,
    texto questao,
    alternativas questao,
    categoria questao,
    dificuldade_questao questao
    )

--  Função que processa uma jogada completa: verifica resposta, atualiza estado e checa encerramento
processarJogada :: ModoJogo -> EstadoJogo -> Questao -> Resposta -> IO (EstadoJogo, Resultado, Bool)
processarJogada modo estado questao resposta = do
    let resultado = verificarResposta questao resposta
    let novoEstado = atualizarEstado estado resultado
    
    deveEncerrar <- verificarEncerramentoGeral modo novoEstado resultado
    return (novoEstado, resultado, deveEncerrar)

-- Loop principal do jogo
loopJogo :: ModoJogo -> EstadoJogo -> IO ResultadoFinal
loopJogo modo estado = do
    proximaQuestao <- obterProximaQuestao
    case proximaQuestao of
        Nothing -> do
            return (calcularResultadoFinal modo estado)
        Just questao -> do
            let respostaSimulada = Resposta (questao_id questao) 0
            
            (novoEstado, resultado, deveEncerrar) <- processarJogada modo estado questao respostaSimulada
            
            if deveEncerrar
                then return (calcularResultadoFinal modo novoEstado)
                else loopJogo modo novoEstado

-- Função principal que executa um jogo completo
executarJogo :: ModoJogo -> IO ResultadoFinal
executarJogo modo = do
    estadoInicial <- iniciarJogo modo
    loopJogo modo estadoInicial

-- Função que calcula o resultado final da "partida"
calcularResultadoFinal :: ModoJogo -> EstadoJogo -> ResultadoFinal
calcularResultadoFinal modo estado = 
    let
        totalQuestoes = questoes_respondidas estado
        totalAcertos = acertos estado
        pontuacao = totalAcertos * 10  
    in
        ResultadoFinal {
            modo_jogado = modo,
            total_questoes = totalQuestoes,
            total_acertos = totalAcertos,
            tempo_total = 0,  
            pontuacao = pontuacao
        }

