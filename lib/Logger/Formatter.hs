module Logger.Formatter where

import Core

import Data.Aeson ((.=))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.Aeson.Text (encodeToLazyText)
import Data.Text qualified as T
import Data.Time (Day (ModifiedJulianDay), UTCTime (..), formatTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Stack.Types (SrcLoc (..))
import Logger.Settings
import Logger.Types
import Logger.Utils

type Formatter = LogPayload -> Text

jsonToText :: JSON.Object -> Text
jsonToText = toText . encodeToLazyText

payloadToJson :: LogPayload -> JSON.Object
payloadToJson lp =
    JSON.fromList
        [ "message" .= lp.msg
        , "severity" .= toText lp.severity
        , "time" .= toText (iso8601Show lp.time)
        , "source" .= maybe JSON.Null JSON.Object lp.source
        , "metadata" .= lp.metadata
        ]

{- | The length of a timestamp in the format "YYYY-MM-DD hh:mm:ss.μμμμμμ".
This definition is top-level in order to avoid multiple reevaluation at runtime.

Taken from RIO.Prelude.Logger
-}
timestampLength :: Int
timestampLength =
    length (formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0))

insertContext :: JSON.Object -> LogPayload -> LogPayload
insertContext ctx lp = lp{metadata = ctx <> lp.metadata}

mkLogContext :: JSON.Value -> JSON.Object
mkLogContext (JSON.Object obj) = obj
mkLogContext v = JSON.singleton "metadata" v

mkLogSource :: CallStack -> Maybe JSON.Object
mkLogSource = fmap formatLoc . mLocFromCS

formatLoc :: SrcLoc -> JSON.Object
formatLoc loc =
    JSON.fromList
        [ "package" .= srcLocPackage loc
        , "module" .= srcLocModule loc
        , "file" .= srcLocFile loc
        , "line" .= srcLocStartLine loc
        , "col" .= srcLocStartCol loc
        ]

formatJson :: Formatter
formatJson = jsonToText . payloadToJson

formatSummary :: LogColours -> Formatter
formatSummary lc lp =
    unwords
        [ fmtTime lp.time
        , fmtSeverity lp.severity
        , fmtMsg lp.msg
        ]
    where
        fmtSeverity =
            colourText (severityColour lc lp.severity)
                . boldText
                . boxText
                . toText
        fmtTime =
            colourText lc.secondary
                . T.take timestampLength
                . toText
                . formatTime defaultTimeLocale "%F %T.%q"
        fmtMsg = colourText lc.primary

formatJsonWithSummary :: LogColours -> Formatter
formatJsonWithSummary lc lp =
    summary <> " " <> json
    where
        summary = formatSummary lc lp
        json = colourText lc.secondary (formatJson lp)

formatLog
    :: (HasField "colours" settings LogColours)
    => (HasField "style" settings LogStyle)
    => settings
    -> Formatter
formatLog settings = case settings.style of
    Json -> formatJson
    Summary -> formatSummary settings.colours
    Both -> formatJsonWithSummary settings.colours
