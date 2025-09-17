{-# LANGUAGE OverloadedStrings #-}
module Main where

import LogicaQuiz
import BuscarApi
import Tipos

main :: IO ()
main = do
    putStrLn "=== TESTE ==="
    
    putStrLn "\n1. TESTE DE LÓGICAQUIZ:"
    let resultado1 = verificarResposta questao1 resposta1
    putStrLn (exibirMensagem resultado1)
    
    putStrLn "\n2. TESTE DE PARSE DA API:"
    testarParse
    
    putStrLn "\n3. TESTE DE UMA QUESTÃO:"
    testeUmaQuestao
      
    putStrLn "\n4. TESTE DE MÚLTIPLAS QUESTÕES:"
    testeMultiplasQuestoes
    
-- Teste completo com uma questão
testeUmaQuestao :: IO ()
testeUmaQuestao = do
    questoes <- BuscarApi.buscarQuestoes 1
    case questoes of
        [] -> putStrLn "ERRO: Nenhuma questão recebida"
        (questao:_) -> do
            putStrLn "Questão convertida:"
            putStrLn $ "Texto: " ++ texto questao
            putStrLn $ "Alternativas: " ++ show (alternativas questao)
            putStrLn $ "Resposta correta (índice): " ++ show (resposta_certa questao)
            putStrLn $ "Categoria: " ++ categoria questao
            putStrLn $ "Dificuldade: " ++ dificuldade questao

-- Teste de mais de uma questão (Ex: 3)
testeMultiplasQuestoes :: IO ()
testeMultiplasQuestoes = do
    questoes <- buscarQuestoes 3
    putStrLn $ "Recebidas " ++ show (length questoes) ++ " questões"
    mapM_ (\(i, q) -> putStrLn $ show i ++ ". " ++ texto q) (zip [1..] questoes)



