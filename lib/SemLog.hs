module SemLog where

import Core

import Data.Aeson ((.=))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Logger.Effects
import Logger.Formatter (formatLog)
import Logger.Settings
import Logger.Types
import Polysemy.Log qualified as Log
import Polysemy.Time qualified as Time

testData :: Text
testData = "potato"

type AppSettings = LogSettings

app :: AppSettings -> IO ()
app settings = runM
    . Time.interpretTimeGhc
    . Log.interpretDataLogStderrWith (formatLog settings)
    . interpretJsonLogAsDataLog
    . setLogLevel settings.level
    $ do
        log Warn "Beware the untyped" (JSON.fromList [])
        log Info "Hello world" (JSON.fromList [])
        withContext (JSON.fromList ["ping" .= JSON.String "pong"]) $ do
            log Info "With Context" (JSON.fromList ["a" .= JSON.String "b"])
        log Info "No Context" (JSON.fromList ["potato" .= JSON.String "tomato"])
        logInfo "easy logging"
        logWarn' "easy logging with data" ["myData" .= testData]

main :: IO ()
main = app def
