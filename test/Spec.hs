module Main where

import Test.Hspec
import qualified TestLogicaQuiz
import qualified TestBuscarApi

main :: IO ()
main = hspec $ do
  TestLogicaQuiz.spec
  TestBuscarApi.spec
