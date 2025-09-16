module Main where

import Tipos
import LogicaQuiz

main :: IO ()
main = do
    let resultado = verificarResposta questao1 resposta1
    print resultado
    putStrLn "Teste concluído!"