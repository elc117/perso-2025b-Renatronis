module LogicaQuiz where

import Tipos
import BuscarApi

-- Questão para testes locais (Se precisar)
questoes :: [Questao]
questoes = [questao1, questao2, questao3, questao4, questao5]

respostas :: [Resposta]
respostas = [resposta1, resposta2, resposta3, resposta4, resposta5]

questao1 = Questao {        
    texto = "Quanto é 2+2?",
    alternativas = ["2", "3", "4", "5"],
    resposta_certa = 2
}

resposta1 = Resposta {
    questao_respondida = 1,
    alternativa_escolhida = 3
}
questao2 = Questao {
    texto = "Quanto é 5 × 3?",
    alternativas = ["12", "15", "18", "20"],
    resposta_certa = 1
}

questao3 = Questao {
    texto = "Capital da França?",
    alternativas = ["Londres", "Paris", "Roma", "Madrid"],
    resposta_certa = 1
}

questao4 = Questao {
    texto = "Quantos dias tem um ano bissexto?",
    alternativas = ["365", "366", "364", "367"],
    resposta_certa = 1
}

questao5 = Questao {
    texto = "Maior planeta do sistema solar?",
    alternativas = ["Terra", "Marte", "Júpiter", "Saturno"],
    resposta_certa = 2
}

-- Respostas para testar (algumas certas, algumas erradas)
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

-- Exibe uma mensagem de erro ou acerto da alternativa escolhida
exibirMensagem :: Resultado -> String
exibirMensagem (Resultado True respostaCerta escolhida) =
    "Resposta correta! Você escolheu " ++ show escolhida

exibirMensagem (Resultado False respostaCerta escolhida) = 
    "Você escolheu a resposta errada " ++ show escolhida ++
    ", mas a resposta correta era " ++ show respostaCerta


