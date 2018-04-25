{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.MySQL.File
  (
    File (..)
  , fileEmpty
  ) where

import           Article.Types.Class
import           Article.Types.Internal

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     Value (..), decodeStrict,
                                                     object, parseJSON,
                                                     withObject, (.:), (.=))
import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (Result, convert)

import           Data.Hashable                      (Hashable (..))
import           Data.Maybe                         (fromMaybe)
import           GHC.Generics                       (Generic)

data File = File { fileID        :: ID
                 , fileKey       :: FileKey
                 , fileBucket    :: FileBucket
                 , fileExtra     :: FileExtra
                 , fileCreatedAt :: CreatedAt
                 }
  deriving (Generic, Show)

instance Eq File where
  f1 == f2 = fileID f1 == fileID f2

instance Hashable File where
  hashWithSalt s f = hashWithSalt s $ fileID f

instance QueryResults File where
    convertResults [fa, fb, fc, _, fe] [va, vb, vc, vd, ve] = File{..}
      where !fileID        = convert fa va
            !fileKey       = convert fb vb
            !fileBucket    = convert fc vc
            !fileExtra     = fromMaybe Null . decodeStrict $ fromMaybe "{}" vd
            !fileCreatedAt = convert fe ve
    convertResults fs vs  = convertError fs vs 2

fileEmpty :: File
fileEmpty = File { fileID        = 0
                 , fileKey       = ""
                 , fileBucket    = ""
                 , fileExtra     = Null
                 , fileCreatedAt = 0
                 }

instance Result File where
  convert _ (Just bs) = fromMaybe fileEmpty (decodeStrict bs)
  convert _ Nothing   = fileEmpty

instance ToJSON File where
  toJSON File{..} = object [ "id"         .= fileID
                           , "key"        .= fileKey
                           , "bucket"     .= fileBucket
                           , "extra"      .= fileExtra
                           , "created_at" .= fileCreatedAt
                           ]

instance FromJSON File where
  parseJSON = withObject "File" $ \o -> do
    fileID        <- o .: "id"
    fileKey       <- o .: "key"
    fileBucket    <- o .: "bucket"
    fileExtra     <- o .: "extra"
    fileCreatedAt <- o .: "created_at"
    return File{..}

instance Created File where
  createdAt = fileCreatedAt
