{-# LANGUAGE OverloadedStrings #-}
module Main where

import LogicaQuiz
import BuscarApi
import Tipos
import WebApi
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["web"] -> do
            putStrLn "Iniciando interface web..."
            iniciarServidor