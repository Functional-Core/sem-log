module Logger.Utils where

import Core

import GHC.Exception (SrcLoc)
import GHC.Stack (CallStack, getCallStack)
import Logger.Types

colourText :: AnsiColour -> Text -> Text
colourText c t = "\ESC[" <> show c <> "m" <> t <> "\ESC[0m"

boldText :: Text -> Text
boldText t = "\ESC[1m" <> t <> "\ESC[0m"

boxText :: Text -> Text
boxText t = "[" <> t <> "]"

mLocFromCS :: CallStack -> Maybe SrcLoc
mLocFromCS cs = case reverse $ getCallStack cs of
    ((_, loc) : _) -> Just loc
    _ -> Nothing
