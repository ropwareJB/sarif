--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Result` type which represents results of a
-- static analysis tool.
module Data.SARIF.Result (
    Level(..),
    Result(..),
    BaselineState(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional hiding (Result, Error)
import Data.Text

import Data.SARIF.Level
import Data.SARIF.Location
import Data.SARIF.MultiformatMessageString

--------------------------------------------------------------------------------

-- | Represents the results of a run of a static analysis tool.
data Result = MkResult {
    -- | The unique ID of the rule of which this result is an occurrence of.
    resultRuleId :: Text,
    -- | A result-specific message which may refer to specific variable names
    -- etc. which caused the rule to trigger.
    resultMessage :: MultiformatMessageString,
    -- | A list of locations which caused the rule to trigger.
    resultLocations :: [Location],
    -- | An optional override for the default `Level` of the rule.
    resultLevel :: Maybe Level,
    -- | An optional difference indicator between scans
    resultBaselineState :: Maybe BaselineState,
    -- | An optional array of `codeFlow` objects
    resultCodeFlows :: Maybe [CodeFlow]
} deriving (Eq, Show)

instance ToJSON Result where
    toJSON MkResult{..} = object
        [ "ruleId" .= resultRuleId
        , "message" .= resultMessage
        , "locations" .= resultLocations
        , "level" .=? resultLevel
        , "baselineState" .=? resultBaselineState
        ]

instance FromJSON Result where
    parseJSON = withObject "Result" $ \obj ->
        MkResult <$> obj .: "ruleId"
                 <*> obj .: "message"
                 <*> obj .: "locations"
                 <*> obj .:? "level"
                 <*> obj .:? "baselineState"

data BaselineState
    = BaselineStateNew
    | BaselineStateUnchanged
    | BaselineStateUpdated
    | BaselineStateAbsent
    deriving (Eq, Show)

instance ToJSON BaselineState where
    toJSON BaselineStateNew = String "new"
    toJSON BaselineStateUnchanged = String "unchanged"
    toJSON BaselineStateUpdated = String "updated"
    toJSON BaselineStateAbsent = String "absent"

instance FromJSON BaselineState where
    parseJSON (String "new") = pure BaselineStateNew
    parseJSON (String "unchanged") = pure BaselineStateUnchanged
    parseJSON (String "updated") = pure BaselineStateUpdated
    parseJSON (String "absent") = pure BaselineStateAbsent
    parseJSON _ = fail "Invalid BaselineState"

-- | §3.36 codeFlow object
data CodeFlow = MkCodeFlow
    { codeFlowMessage :: Maybe Message
    , codeFlowThreadFlows :: [ThreadFlow]
    } deriving (Eq, Show)
instance ToJSON CodeFlow where
    toJSON MkCodeFlow{..} = object
        [ "message" .=? messageText
        , "threadFlows" .=? messageId
        ]
instance FromJSON CodeFlow where
    parseJSON = withObject "CodeFlow" $ \obj ->
        MkCodeFlow <$> obj .:? "message"
                 <*> obj .:? "threadFlows"

-- | §3.11 message object
data Message = MkMessage
    { messageText :: Maybe Text
    , messageId :: Maybe Text
    } deriving (Eq, Show)
instance ToJSON Message where
    toJSON MkMessage{..} = object
        [ "text" .=? messageText
        , "id" .=? messageId
        ]
instance FromJSON Message where
    parseJSON = withObject "Message" $ \obj ->
        MkMessage <$> obj .:? "text"
                 <*> obj .:? "id"

-- | §3.37 threadFlow object
data ThreadFlow = MkThreadFlow
    { threadFlowId :: Maybe Text
    , threadFlowMessage :: Maybe Message
    -- , threadFlowInitialState :: Maybe (Dict Text MultiformatMessageString) -- 3.37.4
    -- , threadFlowImmutableState :: Maybe (Dict Text MultiformatMessageString) -- 3.37.5
    , threadFlowLocations :: Maybe [ThreadFlowLocation]
    } deriving (Eq, Show)
instance ToJSON ThreadFlow where
    toJSON MkThreadFlow{..} = object
        [ "text" .=? messageText
        , "id" .=? messageId
        ]
instance FromJSON ThreadFlow where
    parseJSON = withObject "ThreadFlow" $ \obj ->
        MkThreadFlow
                <$> obj .:? "text"
                <*> obj .:? "id"

-- | §3.38 threadFlowLocation object
data ThreadFlowLocation = MkThreadFlowLocation
    { threadFlowLocationIndex :: Maybe Int
    , threadFlowLocationLocation :: Maybe Location
    , threadFlowLocationModule :: Maybe Text
    -- , threadFlowLocationStack :: Maybe Stack -- 3.38.5
    -- , threadFlowLocationWebRequest :: Maybe WebRequest -- 3.38.6
    -- , threadFlowLocationWebResponse :: Maybe WebResponse -- 3.38.7
    , threadFlowLocationKinds :: Maybe [Text]
    -- , threadFlowLocationState :: Maybe (Dict Text MuliformatMessageString) -- 3.38.9
    , threadFlowLocationNestingLevel :: Maybe Int
    , threadFlowLocationExecutionOrder :: Maybe Int
    , threadFlowLocationExecutionTimeUtc :: Maybe Text
    , threadFlowLocationImportance :: Maybe Text
    -- , threadFlowLocationTaxa :: Maybe [ReportingDescriptorReference] -- 3.38.14
    } deriving (Eq, Show)
instance ToJSON ThreadFlowLocation where
    toJSON MkThreadFlowLocation{..} = object
        [ "index" .=? threadFlowLocationIndex
        , "location" .=? threadFlowLocationLocation
        , "module" .=? threadFlowLocationModule
        -- , "stack" .=? threadFlowLocationStack
        -- , "webRequest" .=? threadFlowLocationWebRequest
        -- , "webResponse" .=? threadFlowLocationWebResponse
        , "kinds" .=? threadFlowLocationKinds
        -- , "state" .=? threadFlowState
        , "nestingLevel" .=? threadFlowNestingLevel
        , "executionOrder" .=? threadFlowExecutionOrder
        , "executionTimeUtc" .=? threadFlowExecutionTimeUtc
        , "importance" .=? threadFlowImportance
        -- , "taxa" .=? threadFlowLocationTaxa
        ]
instance FromJSON ThreadFlowLocation where
    parseJSON = withObject "ThreadFlowLocation" $ \obj ->
        MkThreadFlowLocation
             <$> obj .:? "index"
             <*> obj .:? "location"
             <*> obj .:? "module"
             -- <*> obj .:? "stack"
             -- <*> obj .:? "webRequest"
             -- <*> obj .:? "webResponse"
             <*> obj .:? "kinds"
             -- <*> obj .:? "state"
             <*> obj .:? "nestingLevel"
             <*> obj .:? "executionOrder"
             <*> obj .:? "executionTimeUtc"
             <*> obj .:? "importance"
             -- <*> obj .:? "taxa"
