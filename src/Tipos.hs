module Tipos where

import Data.Time (UTCTime)

data ModoJogo =
    AteErrar        -- Joga até errar
    |Classico       -- Número fixo de 10 questões
    |Cronometrado   -- 1 minuto para responder a maior quantidade de questões
    deriving (Show, Eq)

data ConfiguracaoJogo = ConfiguracaoJogo {
    modo :: ModoJogo,
    dificuldade :: String
} deriving (Show, Eq)

data EstadoJogo = EstadoJogo {
    questoes_respondidas :: Int,
    acertos :: Int,
    erros :: Int,
    tempo_iniciado :: UTCTime,  
    jogo_ativo :: Bool
} deriving (Show, Eq)

data ResultadoFinal = ResultadoFinal {
    modo_jogado :: ModoJogo,
    total_questoes :: Int,
    total_acertos :: Int,
    tempo_total :: Int, 
    pontuacao :: Int
} deriving (Show, Eq)

data Questao = Questao {
    questao_id :: Int,
    texto :: String,
    alternativas :: [String],
    resposta_certa :: Int,
    categoria :: String,
    dificuldade_questao :: String,
    questao_tipo :: String
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

