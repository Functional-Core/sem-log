module Logger.Types where

import Core

import Data.Aeson qualified as JSON
import Data.Time (UTCTime)
import System.Envy (Var (..))

data Severity
    = Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
    deriving stock (Show, Read, Eq, Ord, Generic)

instance Var Severity where
    toVar = show
    fromVar = readMaybe . strToTitle

instance ToText Severity where
    toText Trace = "trace"
    toText Debug = "debug"
    toText Info = "info"
    toText Warn = "warn"
    toText Error = "error"
    toText Fatal = "fatal"

data LogPayload = LogPayload
    { msg :: !Text
    , severity :: !Severity
    , time :: !UTCTime
    , source :: !(Maybe JSON.Object)
    , metadata :: !JSON.Object
    }
    deriving (Show)

data LogStyle = Json | Summary | Both
    deriving (Show, Read, Eq, Generic)

instance Var LogStyle where
    toVar = show
    fromVar = readMaybe . strToTitle

type AnsiColour = Int
