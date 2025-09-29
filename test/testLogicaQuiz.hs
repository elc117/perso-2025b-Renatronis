module TestLogicaQuiz (spec) where

import Test.Hspec
import LogicaQuiz
import Tipos

spec = do
  describe "verificarResposta" $ do
    it "retorna Resultado com acertou True quando resposta correta" $ do
      let questao   = Questao 1 "Pergunta?" ["A","B","C","D"] 2 "cat" "easy"
          resposta  = Resposta 1 2
          resultado = verificarResposta questao resposta
      saber_resposta_correta resultado `shouldBe` True
      guardar_resposta_certa resultado `shouldBe` 2
      alt_escolhida resultado `shouldBe` 2

    it "retorna Resultado com acertou False quando resposta errada" $ do
      let questao   = Questao 1 "Pergunta?" ["A","B","C","D"] 2 "cat" "easy"
          resposta  = Resposta 1 1
          resultado = verificarResposta questao resposta
      saber_resposta_correta resultado `shouldBe` False
      guardar_resposta_certa resultado `shouldBe` 2
      alt_escolhida resultado `shouldBe` 1

  describe "encerrarAteErrar" $ do
    it "encerra se errou (acertou == False)" $ do
      let resultado = Resultado False 2 1
      encerrarAteErrar resultado `shouldBe` True
    it "não encerra se acertou (acertou == True)" $ do
      let resultado = Resultado True 2 2
      encerrarAteErrar resultado `shouldBe` False

  describe "encerrarClassico" $ do
    it "encerra quando acertos + erros >= 10" $ do
      let estado = EstadoJogo 10 5 5 undefined True 1 []
      encerrarClassico estado `shouldBe` True
    it "não encerra quando acertos + erros < 10" $ do
      let estado = EstadoJogo 5 2 2 undefined True 1 []
      encerrarClassico estado `shouldBe` False

  describe "encerrarCronometrado" $ do
    it "retorna False logo após iniciar o estado" $ do
      estado <- iniciarJogo Cronometrado
      r <- encerrarCronometrado estado
      r `shouldBe` False

  describe "verificarEncerramentoGeral" $ do
    it "retorna True para encerrarAteErrar quando errar" $ do
      let resultado = Resultado False 2 1
          estado = EstadoJogo 1 0 1 undefined True 2 ["questao1"]
      r <- verificarEncerramentoGeral AteErrar estado resultado
      r `shouldBe` True
    it "retorna False para encerrarClassico quando não atingiu 10" $ do
      let resultado = Resultado True 2 2
          estado = EstadoJogo 5 2 2 undefined True 6 ["q1", "q2", "q3", "q4", "q5"]
      r <- verificarEncerramentoGeral Classico estado resultado
      r `shouldBe` False

  describe "iniciarJogo" $ do
    it "cria EstadoJogo com jogo_ativo True e questoes_respondidas 0" $ do
      estado <- iniciarJogo Classico
      jogo_ativo estado `shouldBe` True
      questoes_respondidas estado `shouldBe` 0
      proximo_id estado `shouldBe` 1
      questoes_ja_usadas estado `shouldBe` []

  describe "atualizarEstado" $ do
    it "incrementa acertos e questoes_respondidas quando acertar" $ do
      let estado = EstadoJogo 1 1 0 undefined True 2 ["questao_anterior"]
          resultado = Resultado True 2 2
          novo = atualizarEstado estado resultado
      acertos novo `shouldBe` 2
      questoes_respondidas novo `shouldBe` 2
      erros novo `shouldBe` 0
    it "incrementa erros e questoes_respondidas quando errar" $ do
      let estado = EstadoJogo 1 1 0 undefined True 2 ["questao_anterior"]
          resultado = Resultado False 2 1
          novo = atualizarEstado estado resultado
      acertos novo `shouldBe` 1
      questoes_respondidas novo `shouldBe` 2
      erros novo `shouldBe` 1

  describe "obterProximaQuestao (dependente de API externa)" $ do
    it "retorna Just Questao válida ao buscar uma questão com ID correto OU Nothing se API indisponível" $ do
      mq <- obterProximaQuestao 1 "easy" Nothing []
      case mq of
        Just q -> do
          questao_id q `shouldBe` 1
          texto q `shouldSatisfy` (not . null)
          length (alternativas q) `shouldBe` 4
        Nothing -> do
          -- Se a API estiver indisponível, isso é esperado
          putStrLn "API externa indisponível durante teste - comportamento esperado"
          True `shouldBe` True
    
    it "retorna questão com ID sequencial quando especificado (se API disponível)" $ do
      mq <- obterProximaQuestao 5 "medium" Nothing []
      case mq of
        Just q -> questao_id q `shouldBe` 5
        Nothing -> do
          putStrLn "API externa indisponível - teste pulado"
          True `shouldBe` True
    
    it "evita questões com textos já usados (se API disponível)" $ do
      -- Teste condicional baseado na disponibilidade da API
      mq1 <- obterProximaQuestao 1 "easy" Nothing []
      case mq1 of
        Nothing -> do
          putStrLn "API externa indisponível - teste de duplicatas pulado"
          True `shouldBe` True
        Just q1 -> do
          let textosUsados = [texto q1]
          mq2 <- obterProximaQuestao 2 "easy" Nothing textosUsados
          case mq2 of
            Nothing -> do
              putStrLn "API não retornou segunda questão - pode estar indisponível ou sem questões novas"
              True `shouldBe` True
            Just q2 -> do
              questao_id q2 `shouldBe` 2
              texto q2 `shouldNotBe` texto q1

  describe "obterProximaQuestaoDoEstado (dependente de API externa)" $ do
    it "usa corretamente os dados do EstadoJogo (se API disponível)" $ do
      let estado = EstadoJogo 2 1 1 undefined True 3 ["questao_usada"]
      mq <- obterProximaQuestaoDoEstado estado "easy" Nothing
      case mq of
        Just q -> do
          questao_id q `shouldBe` 3
          texto q `shouldNotBe` "questao_usada"
        Nothing -> do
          putStrLn "API externa indisponível durante teste do wrapper - comportamento esperado"
          True `shouldBe` True

  describe "processarJogada" $ do
    it "processa jogada e atualiza estado corretamente" $ do
      let modo = Classico
          estado = EstadoJogo 0 0 0 undefined True 1 []
          questao = Questao 1 "Pergunta?" ["A","B","C","D"] 2 "cat" "easy"
          resposta = Resposta 1 2
      (novoEstado, resultado, encerrar) <- processarJogada modo estado questao resposta
      -- Verifica se acertou
      acertos novoEstado `shouldBe` 1
      saber_resposta_correta resultado `shouldBe` True
      encerrar `shouldBe` False
      -- Verifica se atualizou controles de questão
      proximo_id novoEstado `shouldBe` 2
      questoes_ja_usadas novoEstado `shouldBe` ["Pergunta?"]

    it "atualiza lista de questões usadas após resposta errada" $ do
      let modo = AteErrar
          estado = EstadoJogo 0 0 0 undefined True 1 []
          questao = Questao 1 "Pergunta difícil?" ["A","B","C","D"] 2 "cat" "hard"
          resposta = Resposta 1 1  -- Resposta errada
      (novoEstado, resultado, encerrar) <- processarJogada modo estado questao resposta
      -- Verifica se errou e encerrou
      erros novoEstado `shouldBe` 1
      saber_resposta_correta resultado `shouldBe` False
      encerrar `shouldBe` True
      -- Verifica se ainda atualizou os controles
      proximo_id novoEstado `shouldBe` 2
      questoes_ja_usadas novoEstado `shouldBe` ["Pergunta difícil?"]

  describe "calcularResultadoFinal" $ do
    it "calcula porcentagem corretamente" $ do
      let estado = EstadoJogo 10 7 3 undefined True 11 ["q1","q2","q3","q4","q5","q6","q7","q8","q9","q10"]
          resultado = calcularResultadoFinal Classico estado
      porcentagem_acerto resultado `shouldBe` 70
      total_questoes resultado `shouldBe` 10
      total_acertos resultado `shouldBe` 7

    it "calcula pontuação corretamente (acertos * 10)" $ do
      let estado = EstadoJogo 5 3 2 undefined True 6 ["q1","q2","q3","q4","q5"]
          resultado = calcularResultadoFinal AteErrar estado
      pontuacao resultado `shouldBe` 30  -- 3 acertos * 10
      modo_jogado resultado `shouldBe` AteErrar