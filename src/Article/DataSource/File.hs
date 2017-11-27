{-# LANGUAGE OverloadedStrings #-}
module Article.DataSource.File
  (
    saveFile
  , saveFileWithExtra
  , getFile
  , getFiles
  , getFileWithKey
  ) where

import           Control.Monad         (void)
import           Database.MySQL.Simple (In (..), Only (..), execute, query)
import           Yuntan.Types.HasMySQL (MySQL)

import           Control.Applicative   ((<$>))
import           Data.Aeson            (encode)
import           Data.Maybe            (listToMaybe)
import           Data.UnixTime         (getUnixTime, toEpochTime)

import           Data.String           (fromString)

import           Article.Types

getFile :: ID -> MySQL (Maybe File)
getFile fid prefix conn = listToMaybe <$> query conn sql (Only fid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_files` WHERE `id`=?" ]

getFileWithKey :: FileKey -> MySQL (Maybe File)
getFileWithKey key prefix conn = listToMaybe <$> query conn sql (Only key)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_files` WHERE `key`=?" ]

getFiles :: [ID] -> MySQL [File]
getFiles ids prefix conn = query conn sql $ Only (In ids)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_files` WHERE `id` in ?" ]

saveFile :: FileBucket -> FileKey -> MySQL (Maybe File)
saveFile bucket key prefix conn = do
  file <- getFileWithKey key prefix conn
  case file of
    (Just f) -> return (Just f)
    Nothing -> do
      t <- getUnixTime
      void $ execute conn sql (key, bucket, show $ toEpochTime t)
      getFileWithKey key prefix conn
  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_files` (`key`, `bucket`, `created_at`) VALUES ( ?, ?, ? )" ]

saveFileWithExtra :: FileBucket -> FileKey -> FileExtra -> MySQL (Maybe File)
saveFileWithExtra bucket key extra prefix conn = do
  file <- getFileWithKey key prefix conn
  case file of
    (Just f) -> return (Just f)
    Nothing -> do
      t <- getUnixTime
      void $ execute conn sql (key, bucket, encode extra, show $ toEpochTime t)
      getFileWithKey key prefix conn
  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_files` (`key`, `bucket`, `extra`, `created_at`) VALUES ( ?, ?, ?, ? )" ]
