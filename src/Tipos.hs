module Tipos where

data Questao = Questao {
    id :: Int,
    texto :: String,
    alternativas :: [String],
    resposta_certa :: Int
} deriving (Show, Eq)

data Resposta = Resposta {
    questao_respondida :: Int,
    alternativa_escolhida :: Int
} deriving (Show, Eq)

data Resultado = Resultado {
    saber_resposta_correta :: Bool,
    guardar_resposta_certa :: Int,
    alt_escolhida :: Int
} deriving (Show, Eq)
     