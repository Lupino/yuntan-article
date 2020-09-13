{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Tag
  ( addTag
  , getTagById
  , getTagByName
  , getTags
  , updateTag
  , addArticleTag
  , removeArticleTag
  , removeAllArticleTag
  , getAllArticleTagName
  ) where

import           Article.DataSource.Table (articleTag, tags)
import           Article.Types
import           Control.Monad.IO.Class   (liftIO)
import           Data.Int                 (Int64)
import           Data.UnixTime
import           Database.PSQL.Types      (From, Only (..), OrderBy, PSQL, Size,
                                           as, delete, insertOrUpdate,
                                           insertRet, join, none, selectOne,
                                           selectOnly, select_, update)
import           Prelude                  hiding (id)

addTag :: TagName -> PSQL ID
addTag name = do
  t <- liftIO getUnixTime
  insertRet tags ["name", "created_at"] "id" (name, show $ toEpochTime t) 0

getTagById :: ID -> PSQL (Maybe Tag)
getTagById id = selectOne tags ["*"] "id=?" (Only id)


getTagByName :: TagName -> PSQL (Maybe Tag)
getTagByName name = selectOne tags ["*"] "name=?" (Only name)

updateTag :: ID -> TagName -> PSQL Int64
updateTag id name = update tags ["name"] "id=?" (name, id)

getTags :: From -> Size -> OrderBy -> PSQL [Tag]
getTags = select_ tags ["*"]

getAllArticleTagName :: ID -> PSQL [TagName]
getAllArticleTagName aid =
  selectOnly table "t.name" partSql (Only aid) 0 100 none
  where table = tags `as` "t" `join` articleTag `as` "at"
        partSql = "at.art_id = ? AND at.tag_id = t.id"

addArticleTag :: ID -> ID -> PSQL Int64
addArticleTag aid tid = do
  t <- liftIO getUnixTime
  insertOrUpdate articleTag ["art_id", "tag_id"] [] ["created_at"] (aid, tid, show $ toEpochTime t)

removeArticleTag :: ID -> ID -> PSQL Int64
removeArticleTag aid tid =
  delete articleTag "art_id = ? AND tag_id = ?" (aid, tid)

removeAllArticleTag :: ID -> PSQL Int64
removeAllArticleTag aid =
  delete articleTag "art_id = ? " (Only aid)
