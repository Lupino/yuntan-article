{-# LANGUAGE OverloadedStrings #-}
module Article.DataSource.File
  (
    uploadFile
  , uploadFileWithExtra
  , getFile
  , getFiles
  , getFileWithKey
  ) where

import           Control.Monad         (void)
import           Database.MySQL.Simple (Connection, In (..), Only (..), execute,
                                        query)

import           Control.Applicative   ((<$>))
import           Crypto.Hash.SHA1      (hashlazy)
import           Data.Aeson            (encode)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as LB (ByteString, writeFile)
import           Data.Hex              (hex)
import           Data.Maybe            (listToMaybe)
import           Data.UnixTime         (getUnixTime, toEpochTime)

import           Data.String           (fromString)
import           System.FilePath       ((</>))

import           Article.Types

getFile :: ID -> TablePrefix -> Connection -> IO (Maybe File)
getFile fid prefix conn = listToMaybe <$> query conn sql (Only fid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_files` WHERE `id`=?" ]

getFileWithKey :: FileKey -> TablePrefix -> Connection -> IO (Maybe File)
getFileWithKey key prefix conn = listToMaybe <$> query conn sql (Only key)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_files` WHERE `key`=?" ]

getFiles :: [ID] -> TablePrefix -> Connection -> IO [File]
getFiles ids prefix conn = query conn sql $ Only (In ids)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_files` WHERE `id` in ?" ]

uploadFile :: FileBucket -> LB.ByteString -> TablePrefix -> Connection -> IO (Maybe File)
uploadFile bucket fc prefix conn = do
  file <- getFileWithKey key prefix conn
  case file of
    (Just f) -> return (Just f)
    Nothing -> do
      LB.writeFile fn fc
      t <- getUnixTime
      void $ execute conn sql (key, bucket, show $ toEpochTime t)
      getFileWithKey key prefix conn
  where key = unpack $ hex $ hashlazy fc
        fn = bucket </> key
        sql = fromString $ concat [ "INSERT INTO `", prefix, "_files` (`key`, `bucket`, `created_at`) VALUES ( ?, ?, ? )" ]

uploadFileWithExtra :: FileBucket -> LB.ByteString -> FileExtra -> TablePrefix -> Connection -> IO (Maybe File)
uploadFileWithExtra bucket fc extra prefix conn = do
  file <- getFileWithKey key prefix conn
  case file of
    (Just f) -> return (Just f)
    Nothing -> do
      LB.writeFile fn fc
      t <- getUnixTime
      void $ execute conn sql (key, bucket, encode extra, show $ toEpochTime t)
      getFileWithKey key prefix conn
  where key = unpack $ hex $ hashlazy fc
        ext = fileExt extra
        fn = bucket </> key ++ "."  ++ ext
        sql = fromString $ concat [ "INSERT INTO `", prefix, "_files` (`key`, `bucket`, `extra`, `created_at`) VALUES ( ?, ?, ?, ? )" ]
