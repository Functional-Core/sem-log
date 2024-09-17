module Core
    ( module Core
    , module Incipit
    , module GHC.Records
    ) where

import Data.Char qualified as Char
import GHC.Records (HasField (..))
import Incipit

strToTitle :: String -> String
strToTitle "" = ""
strToTitle (c : cs) = Char.toUpper c : map Char.toLower cs
