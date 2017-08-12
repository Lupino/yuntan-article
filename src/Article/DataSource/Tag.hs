{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Tag
  (
    addTag
  , getTagById
  , getTagByName
  , getTags
  , updateTag
  , addArticleTag
  , removeArticleTag
  , removeAllArticleTag
  , getAllArticleTagName
  ) where

import           Article.Types
import           Control.Monad           (void)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime
import           Database.MySQL.Simple   (Connection, Only (..), execute,
                                          insertID, query)
import           Prelude                 hiding (id)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

addTag :: TagName -> TablePrefix -> Connection -> IO ID
addTag name prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (name, show $ toEpochTime t)
  fromIntegral <$> insertID conn
  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_tags` (`name`, `created_at`) values ( ?, ? )" ]

getTagById :: ID -> TablePrefix -> Connection -> IO (Maybe Tag)
getTagById id prefix conn = listToMaybe <$> query conn sql (Only id)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_tags` WHERE `id` = ?" ]

getTagByName :: TagName -> TablePrefix -> Connection -> IO (Maybe Tag)
getTagByName name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_tags` WHERE `name` = ?" ]

updateTag :: ID -> TagName -> TablePrefix -> Connection -> IO Int64
updateTag id name prefix conn = execute conn sql (name, id)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_tags` SET `name`=? WHERE `id`=?" ]

getTags :: From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [Tag]
getTags from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_tags` ", show o, " LIMIT ?,?" ]

getAllArticleTagName :: ID -> TablePrefix -> Connection -> IO [TagName]
getAllArticleTagName aid prefix conn = map fromOnly <$> query conn sql (Only aid)
  where sql = fromString $ concat [ "SELECT t.`name` FROM `", prefix, "_tags` AS t, `", prefix, "_article_tag` AS at WHERE at.`art_id`=? AND at.`tag_id` = t.`id`" ]

addArticleTag :: ID -> ID -> TablePrefix -> Connection -> IO ID
addArticleTag aid tid prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (aid, tid, show $ toEpochTime t)
  fromIntegral <$> insertID conn
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_article_tag` (`art_id`, `tag_id`, `created_at`) values ( ?, ?, ? )" ]

removeArticleTag :: ID -> ID -> TablePrefix -> Connection -> IO Int64
removeArticleTag aid tid prefix conn = execute conn sql (aid, tid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_article_tag` WHERE `art_id` = ? AND `tag_id`=?" ]

removeAllArticleTag :: ID -> TablePrefix -> Connection -> IO Int64
removeAllArticleTag aid prefix conn = execute conn sql (Only aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_article_tag` WHERE `art_id` = ?" ]
