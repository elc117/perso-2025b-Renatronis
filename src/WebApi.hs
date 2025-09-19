{-# LANGUAGE OverloadedStrings #-}
module WebApi where

import Web.Scotty
import Data.Aeson
import Network.Wai.Middleware.Cors
import LogicaQuiz
import Tipos
import qualified Data.Text.Lazy as TL

-- Inicia o servidor web
iniciarServidor :: IO ()
iniciarServidor = do
    putStrLn "Servidor iniciado em http://localhost:3000"
    scotty 3000 $ do
        middleware simpleCors
        
        get "/" $ do
            html $ TL.concat [
                "<html><head><title>Quiz Game</title></head><body>",
                "<h1>Quiz Game</h1>",
                "<p>Servidor funcionando!</p>",
                "</body></html>"
                ]