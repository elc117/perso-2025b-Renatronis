module BuscarApi where

import Tipos
import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Key as Key
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.Random.Shuffle (shuffleM)
import Control.Applicative ((<|>))

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

-- Traduz o arquivo Json vindo da Api
instance FromJSON ApiResponse where
    parseJSON = withObject "ApiResponse" $ \o -> do
        code <- o .: Key.fromString "response_code"
        resultsArray <- (o .: Key.fromString "results") <|> (o .: Key.fromString "result")
        return $ ApiResponse code resultsArray

-- Traduz a questão do Json paro o tipo(data) de ApiQuestion
instance FromJSON ApiQuestion where
    parseJSON = withObject "ApiQuestion" $ \o -> ApiQuestion
        <$> o .: Key.fromString "category"
        <*> o .: Key.fromString "type"
        <*> o .: Key.fromString "difficulty"
        <*> o .: Key.fromString "question"
        <*> o .: Key.fromString "correct_answer"
        <*> o .: Key.fromString "incorrect_answers"                             

-- Testa se os dados estão sendo recebidos coretamente
testarParse :: IO ()
testarParse = do
    request <- parseRequest "https://opentdb.com/api.php?amount=1&type=multiple"
    response <- httpLBS request
    let jsonBody = getResponseBody response
    case eitherDecode jsonBody :: Either String ApiResponse of
        Left err -> Prelude.putStrLn ("Erro no parse: " ++ err)
        Right apiResp -> print apiResp


-- Converssão de dados da Api para os tipos de dado de Questao 
converterApiQuestion :: ApiQuestion -> IO Questao
converterApiQuestion apiQ = do
    let 
        respostaCerta = correct_answer apiQ
        alternativasIncorretas = incorrect_answers apiQ
        todasAlternativas = alternativasIncorretas ++ [respostaCerta]
        
    listaEmbaralhada <- shuffleM todasAlternativas
    
    let 
        indexRespostaCerta = fromMaybe 0 (elemIndex respostaCerta listaEmbaralhada)
        
    return Questao {
        texto = question apiQ,
        alternativas = listaEmbaralhada,
        resposta_certa = indexRespostaCerta,
        categoria = category apiQ,
        dificuldade = difficulty apiQ,
        questao_tipo = question_type apiQ
    }

-- Função para buscar questões na API
buscarQuestoes :: Int -> IO [Questao]
buscarQuestoes quantidade = do
    request <- parseRequest $ "https://opentdb.com/api.php?amount=" ++ show quantidade ++ "&type=multiple"
    response <- httpLBS request
    let jsonBody = getResponseBody response
    case eitherDecode jsonBody :: Either String ApiResponse of
        Left err -> do
            putStrLn "Erro ao processar dados da API"
            return []
        Right apiResp -> do
            case response_code apiResp of
                0 -> mapM converterApiQuestion (results apiResp)
                _ -> do
                    putStrLn "Erro ao buscar questões"
                    return []