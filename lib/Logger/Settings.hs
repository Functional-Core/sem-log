module Logger.Settings where

import Core

import Logger.Types
import System.Envy (FromEnv (..), Parser, envMaybe, (.!=))

data LogSettings = LogSettings
    { level :: Severity
    , colours :: LogColours
    , style :: LogStyle
    }
    deriving stock (Show, Generic)

instance FromEnv LogSettings where
    fromEnv :: Maybe LogSettings -> Parser LogSettings
    fromEnv mdef =
        LogSettings
            <$> envMaybe "LOG_LEVEL" .!= maybe Info level mdef
            <*> fromEnv (Just $ maybe def colours mdef)
            <*> envMaybe "LOG_STYLE" .!= maybe Summary style mdef

instance Default LogSettings where
    def = LogSettings Info def Both

data LogColours = LogColours
    { primary :: AnsiColour
    , secondary :: AnsiColour
    , trace :: AnsiColour
    , debug :: AnsiColour
    , info :: AnsiColour
    , warn :: AnsiColour
    , error :: AnsiColour
    , fatal :: AnsiColour
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromEnv)

instance Default LogColours where
    def =
        LogColours
            { primary = 97
            , secondary = 90
            , trace = 35
            , debug = 32
            , info = 34
            , warn = 33
            , error = 31
            , fatal = 91
            }

severityColour :: LogColours -> Severity -> AnsiColour
severityColour lc Trace = lc.trace
severityColour lc Debug = lc.debug
severityColour lc Info = lc.info
severityColour lc Warn = lc.warn
severityColour lc Error = lc.error
severityColour lc Fatal = lc.fatal
