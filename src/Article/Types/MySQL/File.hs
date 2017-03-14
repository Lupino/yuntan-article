{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.MySQL.File
  (
    FileExtra (..)
  , fileExtraEmpty
  , File (..)
  , fileEmpty
  ) where

import           Article.Types.Class
import           Article.Types.Internal

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     decodeStrict, object,
                                                     parseJSON, withObject,
                                                     (.:), (.=))
import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (Result, convert)

import           Data.Hashable                      (Hashable (..))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           GHC.Generics                       (Generic)

data FileExtra = FileExtra { fileExt    :: String
                           , fileType   :: String
                           , fileSize   :: Int64
                           , fileName   :: String
                           , fileWidth  :: Maybe Int
                           , fileHeight :: Maybe Int
                           }
  deriving (Generic, Eq, Show)

instance Hashable FileExtra

fileExtraEmpty :: FileExtra
fileExtraEmpty = FileExtra { fileExt    = ""
                           , fileType   = ""
                           , fileSize   = 0
                           , fileName   = ""
                           , fileWidth  = Nothing
                           , fileHeight = Nothing
                           }

instance ToJSON FileExtra where
  toJSON FileExtra{..} = object [ "ext"    .= fileExt
                                , "type"   .= fileType
                                , "size"   .= fileSize
                                , "name"   .= fileName
                                , "width"  .= fileWidth
                                , "height" .= fileHeight
                                ]

instance FromJSON FileExtra where
  parseJSON = withObject "FileExtra" $ \o -> do
    fileExt    <- o .: "ext"
    fileType   <- o .: "type"
    fileSize   <- o .: "size"
    fileName   <- o .: "name"
    fileWidth  <- o .: "width"
    fileHeight <- o .: "height"
    return FileExtra{..}

instance Result FileExtra where
  convert _ (Just bs) = fromMaybe fileExtraEmpty (decodeStrict bs)
  convert _ Nothing   = fileExtraEmpty

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
    convertResults [fa, fb, fc, fd, fe] [va, vb, vc, vd, ve] = File{..}
      where !fileID        = convert fa va
            !fileKey       = convert fb vb
            !fileBucket    = convert fc vc
            !fileExtra     = convert fd vd
            !fileCreatedAt = convert fe ve
    convertResults fs vs  = convertError fs vs 2

fileEmpty :: File
fileEmpty = File { fileID        = 0
                 , fileKey       = ""
                 , fileBucket    = ""
                 , fileExtra     = fileExtraEmpty
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
