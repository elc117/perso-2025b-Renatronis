module LogicaQuiz where

import Tipos

questao1 = Questao {        -- QUESTÃO PARA TESTE
    id_questao = 1,
    texto = "Quanto é 2+2?",
    alternativas = ["2", "3", "4", "5"],
    resposta_certa = 2
}

resposta1 = Resposta {
    questao_respondida = 1,
    alternativa_escolhida = 3
}

resultado1 = Resultado {
    saber_resposta_correta = False,
    guardar_resposta_certa = 2,
    alt_escolhida = 3
}

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