module Main where

import Hedgehog (property, test, withTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
-- import SemLog.Test.NameTest (test_name)

tests :: TestTree
tests =
  testGroup "all" [
    -- testProperty "name" (withTests 1 (property (test test_name)))
  ]

main :: IO ()
main = defaultMain tests

-- testData :: Text
-- testData = "potato"
--
-- type AppSettings = LogSettings
--
-- app :: AppSettings -> IO ()
-- app settings = runM
--     . Time.interpretTimeGhc
--     . Log.interpretDataLogStderrWith (formatLog settings)
--     . interpretJsonLogAsDataLog
--     . setLogLevel settings.level
--     $ do
--         log Warn "Beware the untyped" (JSON.fromList [])
--         log Info "Hello world" (JSON.fromList [])
--         withContext (JSON.fromList ["ping" .= JSON.String "pong"]) $ do
--             log Info "With Context" (JSON.fromList ["a" .= JSON.String "b"])
--         log Info "No Context" (JSON.fromList ["potato" .= JSON.String "tomato"])
--         logInfo "easy logging"
--         logWarn' "easy logging with data" ["myData" .= testData]
--
-- main :: IO ()
-- main = app def
