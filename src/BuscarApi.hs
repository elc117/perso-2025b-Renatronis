module BuscarApi where

import Tipos
import Network.HTTP.Simple
import Data.Aeson
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.Random.Shuffle (shuffleM)

-- Função que atribui um id para cada questão
atribuirId :: [ApiQuestion] -> [(ApiQuestion, Int)]
atribuirId apiQuestions = zip apiQuestions [1..]

-- Converssão de dados da Api para os tipos de dado de Questao 
converterApiQuestion :: (ApiQuestion, Int) -> IO Questao
converterApiQuestion (apiQ, id) = do
    let 
        respostaCerta = correct_answer apiQ
        alternativasIncorretas = incorrect_answers apiQ
        todasAlternativas = alternativasIncorretas ++ [respostaCerta]
        
    listaEmbaralhada <- shuffleM todasAlternativas
    
    let 
        indexRespostaCerta = fromMaybe 0 (elemIndex respostaCerta listaEmbaralhada)
        
    return Questao {
        questao_id = id,
        texto = question apiQ,
        alternativas = listaEmbaralhada,
        resposta_certa = indexRespostaCerta,
        categoria = category apiQ,
        dificuldade_questao = difficulty apiQ,
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
                0 -> mapM converterApiQuestion (atribuirId (results apiResp))
                _ -> do
                    putStrLn "Erro ao buscar questões"
                    return []