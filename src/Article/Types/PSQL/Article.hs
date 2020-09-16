{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.PSQL.Article
  ( Article (..)
  , pickExtra
  , setJsonContent
  , pickContent
  ) where

import           Article.Types.Class
import           Article.Types.Internal
import           Article.Types.PSQL.File
import           Data.Aeson              (FromJSON (..), Result (..),
                                          ToJSON (..), Value (..), decodeStrict,
                                          fromJSON, object, withObject, (.:),
                                          (.=))
import           Data.Aeson.Helper       (pick)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Database.PSQL.Types     (FromRow (..), field)

data Article = Article
  { artID          :: ID
  , artTitle       :: Title
  , artSummary     :: Summary
  , artContent     :: Content
  , artJsonContent :: Bool
  , artContentJson :: Value
  , artCover       :: Maybe File
  , artTags        :: [TagName]
  , artAliases     :: [Text]
  , artTimelines   :: [String]
  , artExtra       :: Value
  , artCreatedAt   :: CreatedAt
  }
  deriving (Show)

toMaybe :: Result a -> Maybe a
toMaybe (Error _)   = Nothing
toMaybe (Success a) = Just a

instance FromRow Article where
  fromRow = do
    artID          <- field
    artTitle       <- field
    artSummary     <- field
    artContent     <- field
    artCover       <- toMaybe . fromJSON . fromMaybe Null <$> field
    artExtra       <- fromMaybe Null <$> field
    artCreatedAt   <- field
    return Article
      { artTags         = []
      , artAliases      = []
      , artTimelines    = []
      , artJsonContent  = False
      , artContentJson  = Null
      , ..
      }


instance ToJSON Article where
  toJSON Article{..} = object
    [ "id"            .= artID
    , "title"         .= artTitle
    , "summary"       .= artSummary
    , "content"       .= content
    , "tags"          .= artTags
    , "aliases"       .= artAliases
    , "timelines"     .= artTimelines
    , "cover"         .= artCover
    , "extra"         .= artExtra
    , "created_at"    .= artCreatedAt
    ]
    where content = if artJsonContent then artContentJson else toJSON artContent

instance FromJSON Article where
  parseJSON = withObject "Article" $ \o -> do
    artID          <- o .: "id"
    artTitle       <- o .: "title"
    artSummary     <- o .: "summary"
    artContent     <- o .: "content"
    artTags        <- o .: "tags"
    artAliases     <- o .: "aliases"
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
pickExtra keys art = art {artExtra = pick keys $ artExtra art}

setJsonContent :: Article -> Article
setJsonContent art = art
  { artJsonContent = True
  , artContentJson = fromMaybe Null . decodeStrict . encodeUtf8 $ artContent art
  }

pickContent :: [Text] -> Article -> Article
pickContent keys art = a {artContentJson = pick keys $ artContentJson a}
  where a = if artJsonContent art then art else setJsonContent art
