{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Projeto_Quizz (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Projeto_Quizz"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Jogo Quizz"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
