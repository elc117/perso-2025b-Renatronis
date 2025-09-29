{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Tipos where

import Data.Time (UTCTime)
import Data.Aeson
import Control.Applicative ((<|>))

data ModoJogo =
    AteErrar        -- Joga até errar
    |Classico       -- Número fixo de 10 questões
    |Cronometrado   -- 1 minuto para responder a maior quantidade de questões
    deriving (Show, Eq)

data ConfiguracaoJogo = ConfiguracaoJogo {
    modo :: ModoJogo,
    dificuldade :: String,
    categoria_filtro :: Maybe String
} deriving (Show, Eq)

data EstadoJogo = EstadoJogo {
    questoes_respondidas :: Int,
    acertos :: Int,
    erros :: Int,
    tempo_iniciado :: UTCTime,  
    jogo_ativo :: Bool,
    proximo_id :: Int,                   
    questoes_ja_usadas :: [String]     
} deriving (Show, Eq)

data ResultadoFinal = ResultadoFinal {
    modo_jogado :: ModoJogo,
    total_questoes :: Int,
    total_acertos :: Int,
    pontuacao :: Int,
    porcentagem_acerto :: Int
} deriving (Show, Eq)

data Questao = Questao {
    questao_id :: Int,
    texto :: String,
    alternativas :: [String],
    resposta_certa :: Int,
    categoria :: String,
    dificuldade_questao :: String
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

data ApiResponse = ApiResponse {
    response_code :: Int,
    results :: [ApiQuestion]
} deriving (Show)

data ApiQuestion = ApiQuestion {
    category :: String,
    question_type :: String,  
    difficulty :: String,
    question :: String,
    correct_answer :: String,
    incorrect_answers :: [String]
} deriving (Show)

instance ToJSON ModoJogo where
    toJSON AteErrar = "ateErrar"
    toJSON Classico = "classico" 
    toJSON Cronometrado = "cronometrado"

instance FromJSON ModoJogo where
    parseJSON = withText "ModoJogo" $ \t -> case t of
        "ateErrar" -> return AteErrar
        "classico" -> return Classico
        "cronometrado" -> return Cronometrado
        _ -> fail "Modo de jogo inválido"

instance ToJSON EstadoJogo where
    toJSON estado = object [
        "questoes_respondidas" .= questoes_respondidas estado,
        "acertos" .= acertos estado,
        "erros" .= erros estado,
        "jogo_ativo" .= jogo_ativo estado
        ]

instance ToJSON Questao where
    toJSON questao = object [
        "id" .= questao_id questao,
        "texto" .= texto questao,
        "alternativas" .= alternativas questao,
        "categoria" .= categoria questao,
        "dificuldade" .= dificuldade_questao questao
        ]

instance ToJSON ResultadoFinal where
    toJSON :: ResultadoFinal -> Value
    toJSON resultado = object [
        "modo_jogado" .= modo_jogado resultado,
        "total_questoes" .= total_questoes resultado,
        "total_acertos" .= total_acertos resultado,
        "pontuacao" .= pontuacao resultado,
        "porcentagem_acerto" .= porcentagem_acerto resultado
        ]

-- Instâncias para integração com frontend
instance ToJSON Resultado where
    toJSON resultado = object [
        "acertou" .= saber_resposta_correta resultado,
        "respostaCorreta" .= guardar_resposta_certa resultado,
        "alternativaEscolhida" .= alt_escolhida resultado
        ]

instance FromJSON Resposta where
    parseJSON = withObject "Resposta" $ \o -> do
        qid <- (o .: "questao_id") <|> (o .: "id") <|> (o .: "questao_respondida")
        alt <- o .: "alternativa_escolhida"
        return (Resposta qid alt)

-- Instâncias JSON para API
instance FromJSON ApiResponse where
    parseJSON = withObject "ApiResponse" $ \o -> do
        code <- o .: "response_code"
        resultsArray <- (o .: "results") <|> (o .: "result")
        return $ ApiResponse code resultsArray

instance FromJSON ApiQuestion where
    parseJSON = withObject "ApiQuestion" $ \o -> ApiQuestion
        <$> o .: "category"
        <*> o .: "type"
        <*> o .: "difficulty"
        <*> o .: "question"
        <*> o .: "correct_answer"
        <*> o .: "incorrect_answers"

-- Instâncias JSON para ConfiguracaoJogo
instance FromJSON ConfiguracaoJogo where
    parseJSON = withObject "ConfiguracaoJogo" $ \o -> ConfiguracaoJogo
        <$> o .: "modo"
        <*> o .: "dificuldade"
        <*> (o .:? "categoria")

instance ToJSON ConfiguracaoJogo where
    toJSON config = object [
        "modo" .= modo config,
        "dificuldade" .= dificuldade config,
        "categoria" .= categoria_filtro config
        ]