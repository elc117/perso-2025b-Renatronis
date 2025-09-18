module LogicaQuiz where

import Tipos
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

--Questão para testes locais (Se precisar)
questoes :: [Questao]
questoes = [questao1, questao2, questao3, questao4, questao5]

respostas :: [Resposta]
respostas = [resposta1, resposta2, resposta3, resposta4, resposta5]

questao1 = Questao {
    questao_id = 1,        -- ← FALTAVA
    texto = "Quanto é 2+2?",
    alternativas = ["2", "3", "4", "5"],
    resposta_certa = 2,
    categoria = "Matemática",          
    dificuldade_questao = "easy",     
    questao_tipo = "multiple"           
}

-- ...existing code...

questao2 = Questao {
    questao_id = 2,
    texto = "Quanto é 5 × 3?",
    alternativas = ["12", "15", "18", "20"],
    resposta_certa = 1,
    categoria = "Matemática",
    dificuldade_questao = "easy",
    questao_tipo = "multiple"
}

questao3 = Questao {
    questao_id = 3,
    texto = "Capital da França?",
    alternativas = ["Londres", "Paris", "Roma", "Madrid"],
    resposta_certa = 1,
    categoria = "Geografia",
    dificuldade_questao = "easy",
    questao_tipo = "multiple"
}

questao4 = Questao {
    questao_id = 4,
    texto = "Quantos dias tem um ano bissexto?",
    alternativas = ["365", "366", "364", "367"],
    resposta_certa = 1,
    categoria = "Conhecimentos Gerais",
    dificuldade_questao = "medium",
    questao_tipo = "multiple"
}

questao5 = Questao {
    questao_id = 5,
    texto = "Maior planeta do sistema solar?",
    alternativas = ["Terra", "Marte", "Júpiter", "Saturno"],
    resposta_certa = 2,
    categoria = "Astronomia",
    dificuldade_questao = "easy",
    questao_tipo = "multiple"
}

resposta1 = Resposta {
    questao_respondida = 2,
    alternativa_escolhida = 2  -- correta (15)
}

resposta2 = Resposta {
    questao_respondida = 2,
    alternativa_escolhida = 1  -- correta (15)
}

resposta3 = Resposta {
    questao_respondida = 3,
    alternativa_escolhida = 0  -- errada (Londres em vez de Paris)
}

resposta4 = Resposta {
    questao_respondida = 4,
    alternativa_escolhida = 1  -- correta (366)
}

resposta5 = Resposta {
    questao_respondida = 5,
    alternativa_escolhida = 3  -- errada (Saturno em vez de Júpiter)
}

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

-- Função principal que executa um jogo completo
-- executarJogo :: ModoJogo -> IO ResultadoFinal
-- executarJogo modo = do
--     estado_inicio <- iniciarJogo modo
--     atualiza_jogo <- atualizarEstado modo 

-- Loop principal do jogo
-- loopJogo :: ModoJogo -> EstadoJogo -> IO ResultadoFinal


-- Exibe uma mensagem de erro ou acerto da alternativa escolhida
exibirMensagem :: Resultado -> String
exibirMensagem (Resultado True respostaCerta escolhida) =
    "Resposta correta! Você escolheu " ++ show escolhida

exibirMensagem (Resultado False respostaCerta escolhida) = 
    "Você escolhiu a resposta errada " ++ show escolhida ++
    ", mas a resposta correta era " ++ show respostaCerta
