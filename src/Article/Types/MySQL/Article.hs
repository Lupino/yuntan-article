{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.MySQL.Article
  ( Article (..)
  , pickExtra
  , setJsonContent
  , pickContent
  ) where

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     Value (..), decodeStrict,
                                                     object, withObject, (.:),
                                                     (.=))
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (encodeUtf8)
import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)
import           Yuntan.Utils.JSON                  (pickValue)

import           Article.Types.Class
import           Article.Types.Internal
import           Article.Types.MySQL.File

data Article = Article
  { artID          :: ID
  , artTitle       :: Title
  , artSummary     :: Summary
  , artContent     :: Content
  , artJsonContent :: Bool
  , artContentJson :: Value
  , artFromURL     :: FromURL
  , artFromURLHash :: FromURLHASH
  , artCover       :: Maybe File
  , artTags        :: [TagName]
  , artTimelines   :: [String]
  , artExtra       :: Value
  , artCreatedAt   :: CreatedAt
  }
  deriving (Show)

instance QueryResults Article where
    convertResults [fa, fb, fc, fd, fe, ff, fg, _, fi]
                   [va, vb, vc, vd, ve, vf, vg, vh, vi] = Article{..}
      where !artID          = convert fa va
            !artTitle       = convert fb vb
            !artSummary     = convert fc vc
            !artContent     = convert fd vd
            !artFromURL     = convert fe ve
            !artFromURLHash = convert ff vf
            !artCover       = convert fg vg
            !artExtra       = fromMaybe Null . decodeStrict $ fromMaybe "{}" vh
            !artCreatedAt   = convert fi vi
            artTags         = []
            artTimelines    = []
            artJsonContent  = False
            artContentJson  = Null
    convertResults fs vs  = convertError fs vs 2

instance ToJSON Article where
  toJSON Article{..} = object
    [ "id"            .= artID
    , "title"         .= artTitle
    , "summary"       .= artSummary
    , "content"       .= content
    , "from_url"      .= artFromURL
    , "from_url_hash" .= artFromURLHash
    , "tags"          .= artTags
    , "timelines"     .= artTimelines
    , "cover"         .= artCover
    , "extra"         .= artExtra
    , "created_at"    .= artCreatedAt
    ]
    where content = if artJsonContent then artContentJson else toJSON artContent

instance FromJSON Article where
  parseJSON = withObject "GroupMeta" $ \o -> do
    artID          <- o .: "id"
    artTitle       <- o .: "title"
    artSummary     <- o .: "summary"
    artContent     <- o .: "content"
    artFromURL     <- o .: "from_url"
    artFromURLHash <- o .: "from_url_hash"
    artTags        <- o .: "tags"
    artTimelines   <- o .: "timelines"
    artCover       <- o .: "cover"
    artExtra       <- o .: "extra"
    artCreatedAt   <- o .: "created_at"
    return Article
      { artJsonContent = False
      , artContentJson = Null
      , ..
      }

instance Created Article where
  createdAt = artCreatedAt

pickExtra :: [Text] -> Article -> Article
pickExtra [] art   = art
pickExtra keys art = art {artExtra = pickValue keys $ artExtra art}

setJsonContent :: Article -> Article
setJsonContent art = art
  { artJsonContent = True
  , artContentJson = fromMaybe Null . decodeStrict . encodeUtf8 $ artContent art
  }

pickContent :: [Text] -> Article -> Article
pickContent keys art = a {artContentJson = pickValue keys $ artContentJson a}
  where a = if artJsonContent art then art else setJsonContent art
