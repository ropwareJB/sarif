--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `VersionControlDetails` type to represent SARIF artifacts.
module Data.SARIF.VersionControlDetails (
    VersionControlDetails(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Text

--------------------------------------------------------------------------------

-- | An artifact represents e.g. a source file.
data VersionControlDetails = MkVersionControlDetails {
    -- | The location of the repository containing the scanned files.
    repositoryUri :: Text,
    -- | String that uniquely and permanently identifies the appropriate revision of the scanned files.
    revisionId :: Maybe Text,
    -- | String containing the name of a branch containing the correct revision of the scanne files
    branch :: Maybe Text,
    -- | String containing a tag that has been applied to the revision in the VCS.
    revisionTag :: Maybe Text
} deriving (Eq, Show)

instance ToJSON VersionControlDetails where
    toJSON MkVersionControlDetails{..} = object
        [ "repositoryUri" .= repositoryUri
        , "revisionId" .=? revisionId
        , "branch" .=? branch
        , "revisionTag" .=? revisionTag
        ]

instance FromJSON VersionControlDetails where
    parseJSON = withObject "VersionControlDetails" $ \obj ->
        MkVersionControlDetails <$> obj .: "repositoryUri"
                                <*> obj .:? "revisionId"
                                <*> obj .:? "branch"
                                <*> obj .:? "revisionTag"

--------------------------------------------------------------------------------
