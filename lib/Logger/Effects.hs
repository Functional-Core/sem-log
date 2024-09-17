module Logger.Effects where

import Core

import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import GHC.Stack qualified as GHC
import Logger.Formatter
import Logger.Types
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Log qualified as Log
import Polysemy.Time (GhcTime, now)

data JsonLog :: Effect where
    Log :: (HasCallStack) => Severity -> Text -> JSON.Object -> JsonLog m ()
    WithContext :: (HasCallStack) => JSON.Object -> m a -> JsonLog m a

makeSem ''JsonLog

mkPayload
    :: (HasCallStack)
    => (Member GhcTime r)
    => Text
    -> Severity
    -> JSON.Object
    -> Sem r LogPayload
mkPayload msg severity ctx = do
    time <- now
    let source = mkLogSource GHC.callStack
    pure $ LogPayload msg severity time source ctx

interpretJsonLogAsDataLog
    :: (Members [DataLog LogPayload, GhcTime] r)
    => InterpreterFor JsonLog r
interpretJsonLogAsDataLog = interpretH \case
    Log severity msg ctx -> do
        payload <- mkPayload msg severity ctx
        liftT $ Log.dataLog payload
    WithContext ctx act0 -> do
        act1 <- runT act0
        let act2 = interpretJsonLogAsDataLog act1
        raise $ Log.local (insertContext ctx) act2
{-# INLINE interpretJsonLogAsDataLog #-}

setLogLevel :: (Member JsonLog r) => Severity -> Sem r a -> Sem r a
setLogLevel level = interceptH @JsonLog \case
    Log severity msg ctx -> do
        when (severity >= level) do
            log severity msg ctx
        pureT ()
    WithContext ctx act ->
        withContext ctx (runTSimple act)

logSimple :: (Member JsonLog r) => Severity -> Text -> Sem r ()
logSimple sev msg = log sev msg (JSON.fromList [])

logTrace
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logFatal
        :: (Member JsonLog r) => Text -> Sem r ()
logTrace = logSimple Trace
logDebug = logSimple Debug
logInfo = logSimple Info
logWarn = logSimple Warn
logError = logSimple Error
logFatal = logSimple Fatal

logTrace'
    , logDebug'
    , logInfo'
    , logWarn'
    , logError'
    , logFatal'
        :: (Member JsonLog r) => Text -> [(JSON.Key, JSON.Value)] -> Sem r ()
logTrace' msg = log Trace msg . JSON.fromList
logDebug' msg = log Debug msg . JSON.fromList
logInfo' msg = log Info msg . JSON.fromList
logWarn' msg = log Warn msg . JSON.fromList
logError' msg = log Error msg . JSON.fromList
logFatal' msg = log Fatal msg . JSON.fromList
