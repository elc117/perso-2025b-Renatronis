module TestBuscarApi (spec) where

import Test.Hspec
import BuscarApi
import Tipos

spec :: Spec
spec = do
  describe "atribuirId" $ do
    it "atribui ids sequenciais começando em 1" $ do
      let x = [ ApiQuestion "horse" "multiple" "easy" "q1" "A" ["B","C","D"]
              , ApiQuestion "horse" "multiple" "hard" "q2" "B" ["A","C","D"]
              ]
          resultado = atribuirId x
      map snd resultado `shouldBe` [1,2]

  describe "converterApiQuestion" $ do
    it "converte ApiQuestion para Questao corretamente" $ do
      let apiQ = ApiQuestion "horse" "multiple" "easy" "q1" "A" ["B","C","D"]
          idQ = 1
      questao <- converterApiQuestion (apiQ, idQ)
      texto questao `shouldBe` "q1"
      questao_id questao `shouldBe` 1
      categoria questao `shouldBe` "horse"
      dificuldade_questao questao `shouldBe` "easy"
      length (alternativas questao) `shouldBe` 4

  describe "buscarQuestoes (mock)" $ do
    let mockBuscarQuestoes :: Int -> String -> Maybe String -> IO [Questao]
        mockBuscarQuestoes _ _ _ = return [ Questao 1 "Pergunta mock" ["A","B","C","D"] 2 "cat" "easy" ]
    it "retorna questões simuladas" $ do
      qs <- mockBuscarQuestoes 1 "easy" Nothing
      length qs `shouldBe` 1
      texto (head qs) `shouldBe` "Pergunta mock"

  describe "buscarCategorias" $ do
    it "busca categorias da API e retorna uma lista" $ do
      categorias <- buscarCategorias
      length categorias `shouldSatisfy` (> 0)