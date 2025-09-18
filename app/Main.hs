{-# LANGUAGE OverloadedStrings #-}
module Main where

import LogicaQuiz
import BuscarApi
import Tipos

main :: IO ()
main = do
    putStrLn "=== TESTES DO JOGO ==="
    
    putStrLn "\n1. TESTE INICIAR JOGO:"
    testeIniciarJogo
    
    putStrLn "\n2. TESTE ATUALIZAR ESTADO:"
    testeAtualizarEstado
    
-- Teste da função iniciarJogo
testeIniciarJogo :: IO ()
testeIniciarJogo = do
    putStrLn "Iniciando modo Clássico..."
    estadoClassico <- iniciarJogo Classico
    putStrLn $ "Estado inicial: " ++ show estadoClassico
    
    putStrLn "\nIniciando modo AteErrar..."
    estadoAteErrar <- iniciarJogo AteErrar
    putStrLn $ "Estado inicial: " ++ show estadoAteErrar

-- Teste da função atualizarEstado
testeAtualizarEstado :: IO ()
testeAtualizarEstado = do
    estadoInicial <- iniciarJogo Classico
    putStrLn $ "Estado antes: " ++ show estadoInicial
    
    -- Simular uma resposta correta
    let resultado1 = verificarResposta questao1 resposta2  -- resposta2 está correta
    let novoEstado1 = atualizarEstado estadoInicial resultado1
    putStrLn $ "Após acerto: " ++ show novoEstado1
    
    -- Simular uma resposta errada
    let resultado2 = verificarResposta questao3 resposta3  -- resposta3 está errada
    let novoEstado2 = atualizarEstado novoEstado1 resultado2
    putStrLn $ "Após erro: " ++ show novoEstado2



