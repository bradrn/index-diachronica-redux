{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ID.Schemata where

import Prelude hiding (lookup)

import Brassica.SoundChange.Types (Rule)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), object, (.=))
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Types (parseFail)
import Data.Csv (FromNamedRecord(..), lookup, ToNamedRecord(..), namedField, namedRecord)
import Data.Text (Text)

data Convention = Convention
    { c_sym :: Text
    , c_ipa :: [Text]
    , c_explicit :: Bool
    , c_notes :: [Text]
    } deriving (Show)

data ReferenceData = ReferenceData
    { r_source :: Text
    , r_transcriber :: Text
    , r_reviewer :: Maybe Text
    , r_ordering :: Int
    , r_exhaustivity :: Int
    , r_detail :: Int
    , r_consensus :: Int
    , r_transcription :: Int
    , r_conventions :: [Convention]
    } deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier=drop 2} ''Convention)
$(deriveJSON defaultOptions {fieldLabelModifier=drop 2} ''ReferenceData)

data Suprasegmental = Binary Text [Text] | Multi [Text] [Text]
    deriving (Show)

instance FromJSON Suprasegmental where
    parseJSON = withObject "Suprasegmental" $ \v ->
        v .:: "type" >>= \case
            "binary" -> Binary <$> v .: "value" <*> v .: "notes"
            "multi" -> Multi <$> v .: "value" <*> v .: "notes"
            x -> parseFail $ "Unexpected type: " ++ x
      where
        (.::) = (.:) @String

instance ToJSON Suprasegmental where
    toJSON (Binary value notes) = object ["type" .= ("binary"::Text), "value" .= value, "notes" .= notes]
    toJSON (Multi  value notes) = object ["type" .= ("multi" ::Text), "value" .= value, "notes" .= notes]

data Inventory = Inventory
    { i_source :: Text
    , i_consonants :: [[Text]]
    , i_cnotes :: [Text]
    , i_vowels :: [[Text]]
    , i_vnotes :: [Text]
    , i_supras :: [Suprasegmental]
    } deriving (Show)

data LangInfo = LangInfo
    { l_root :: Text
    , l_inventory :: [Inventory]
    } deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier=drop 2} ''Inventory)
$(deriveJSON defaultOptions {fieldLabelModifier=drop 2} ''LangInfo)

data Languoid = Languoid
    { la_id :: Text
    , la_name :: Text
    } deriving (Show)

instance FromNamedRecord Languoid where
    parseNamedRecord m = Languoid <$> lookup m "ID" <*> lookup m "Name"
instance ToNamedRecord Languoid where
    toNamedRecord l = namedRecord [namedField "ID" (la_id l), namedField "name" (la_name l)]

data Overrides = NoOverride | WholeRule Text | WholeEnvironment Text | ExtraConditions [Text]
    deriving (Show)

data Change = Change
    { ch_rule :: Rule
    , ch_overrides :: Overrides
    , ch_notes :: [Text]
    } deriving (Show)

data Edge = Edge
    { e_from :: Text
    , e_to :: Text
    , e_root :: Bool
    , e_changes :: [(Text, [Change])]
    } deriving (Show)

data SoundChanges = SoundChanges
    { sc_title :: Text
    , sc_contents :: [Edge]
    } deriving (Show)
