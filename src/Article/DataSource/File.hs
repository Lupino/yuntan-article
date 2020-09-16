{-# LANGUAGE OverloadedStrings #-}
module Article.DataSource.File
  ( saveFile
  , saveFileWithExtra
  , getFile
  , getFileWithKey
  , removeFile
  ) where

import           Article.DataSource.Table (files)
import           Article.Types
import           Control.Monad            (void)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (encode)
import           Data.Int                 (Int64)
import           Data.UnixTime            (getUnixTime, toEpochTime)
import           Database.PSQL.Types      (Only (..), PSQL, delete,
                                           insertOrUpdate, selectOne)

getFile :: ID -> PSQL (Maybe File)
getFile fid =
  selectOne files ["*"] "id = ?" (Only fid)

getFileWithKey :: FileKey -> PSQL (Maybe File)
getFileWithKey key =
  selectOne files ["*"] "key = ?" (Only key)

saveFile :: FileBucket -> FileKey -> PSQL (Maybe File)
saveFile bucket key = do
  t <- liftIO getUnixTime
  void $ insertOrUpdate files ["key"] [] ["bucket", "created_at"]
    (key, bucket, show $ toEpochTime t)
  getFileWithKey key

saveFileWithExtra :: FileBucket -> FileKey -> FileExtra -> PSQL (Maybe File)
saveFileWithExtra bucket key extra = do
  t <- liftIO getUnixTime
  void $ insertOrUpdate files ["key"] [] ["bucket", "extra", "created_at"]
    (key, bucket, encode extra, show $ toEpochTime t)
  getFileWithKey key

removeFile :: ID -> PSQL Int64
removeFile fid = delete files "id = ?" (Only fid)
