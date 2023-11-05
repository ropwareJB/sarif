--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides types used to refer to different sorts of locations, such as
-- files, as well as parts of files.
module Data.SARIF.Location (
    Location(..),
    ArtifactLocation(..),
    ArtifactContent(..),
    Region(..),
    PhysicalLocation(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Data.SARIF.MultiformatMessageString

--------------------------------------------------------------------------------

-- | Represents locations.
newtype Location = MkLocation {
    -- | The physical location (i.e. part of a file) to which this location
    -- refers to.
    locationPhysicalLocation :: Maybe PhysicalLocation
} deriving (Eq, Show)

instance ToJSON Location where
    toJSON MkLocation{..} = object
        [ "physicalLocation" .= locationPhysicalLocation
        ]

instance FromJSON Location where
    parseJSON = withObject "Location" $ \obj ->
        MkLocation <$> obj .:? "physicalLocation"

-- | Represents artifact locations such as file paths.
newtype ArtifactLocation = MkArtifactLocation {
    artifactLocationUri :: Text
} deriving (Eq, Show)

instance ToJSON ArtifactLocation where
    toJSON MkArtifactLocation{..} = object
        [ "uri" .= artifactLocationUri
        ]

instance FromJSON ArtifactLocation where
    parseJSON = withObject "ArtifactLocation" $ \obj ->
        MkArtifactLocation <$> obj .: "uri"

-- | Represents regions of code with a start and an end.
data Region = MkRegion {
    -- | The line on which this region starts.
    regionStartLine :: Int,
    -- | The column within the starting line where this region starts.
    regionStartColumn :: Maybe Int,
    -- | The line on which this region ends.
    regionEndLine :: Maybe Int,
    -- | The column within the ending line where this region ends.
    regionEndColumn :: Maybe Int,
    -- | The code that this region corresponds to
    regionSnippet :: Maybe ArtifactContent
} deriving (Eq, Show)

instance ToJSON Region where
    toJSON MkRegion{..} = object
        [ "startLine" .= regionStartLine
        , "startColumn" .= regionStartColumn
        , "endLine" .= regionEndLine
        , "endColumn" .= regionEndColumn
        , "snippet" .= regionSnippet
        ]

instance FromJSON Region where
    parseJSON = withObject "Region" $ \obj ->
        MkRegion <$> obj .: "startLine"
                 <*> obj .:? "startColumn"
                 <*> obj .:? "endLine"
                 <*> obj .:? "endColumn"
                 <*> obj .:? "snippet"

-- | Represents parts of artifacts, e.g. a `Region` within a file.
data PhysicalLocation = MkPhysicalLocation {
    physicalLocationArtifactLocation :: ArtifactLocation,
    physicalLocationRegion :: Region,
    physicalLocationContextRegion :: Maybe Region
} deriving (Eq, Show)

instance ToJSON PhysicalLocation where
    toJSON MkPhysicalLocation{..} = object
        [ "artifactLocation" .= physicalLocationArtifactLocation
        , "region" .= physicalLocationRegion
        , "contextRegion" .= physicalLocationContextRegion
        ]

instance FromJSON PhysicalLocation where
    parseJSON = withObject "PhysicalLocation" $ \obj ->
        MkPhysicalLocation <$> obj .: "artifactLocation"
                           <*> obj .: "region"
                           <*> obj .: "contextRegion"

data ArtifactContent = MkArtifactContent {
    artifactContentText :: Maybe Text,
    artifactContentBinaryBase64 :: Maybe Text,
    artifactContentRendered :: Maybe MultiformatMessageString
} deriving (Eq, Show)

instance ToJSON ArtifactContent where
    toJSON MkArtifactContent{..} = object
        [ "text" .= artifactContentText
        , "binary" .= artifactContentBinaryBase64
        , "rendered" .= artifactContentRendered
        ]

instance FromJSON ArtifactContent where
    parseJSON = withObject "ArtifactContent" $ \obj ->
        MkArtifactContent <$> obj .:? "text"
                           <*> obj .:? "binary"
                           <*> obj .:? "rendered"

--------------------------------------------------------------------------------
