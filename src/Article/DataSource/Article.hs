{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Article
  (
    createArticle
  , getArticle
  , removeArticle
  , getAllArticle
  , countAllArticle
  , updateArticle
  , updateArticleCover
  , updateArticleExtra
  , updateArticleTitle
  , updateArticleSummary
  , updateArticleContent
  , existsArticle
  ) where

import           Control.Monad             (void)
import           Database.MySQL.Simple     (Connection, Only (..), execute,
                                            insertID, query, query_)

import           Control.Applicative       ((<$>))
import           Crypto.Hash.SHA1          (hash)
import           Data.ByteString.Char8     (pack)
import           Data.Hex                  (hex)
import           Data.String               (fromString)

import           Article.Types
import           Article.Utils             (onlyToMaybe)
import           Data.Aeson                (Value (..), encode)
import           Data.Int                  (Int64)
import           Data.Maybe                (listToMaybe)
import           Dispatch.Types.ListResult (From, Size)
import           Dispatch.Types.OrderBy    (OrderBy)
import           Prelude                   hiding (id)

createArticle :: Title -> Summary -> Content -> FromURL -> CreatedAt -> TablePrefix -> Connection -> IO Int64
createArticle title summary content fromURL ct prefix conn = do
  oldID <- existsArticle fromURL prefix conn
  case oldID of
    Just id -> return id
    Nothing -> do
      void $ execute conn sql (title, summary, content, fromURL, fromURLHash, ct)
      fromIntegral <$> insertID conn
  where fromURLHash = hex $ hash (pack fromURL)
        sql = fromString $ concat [ "INSERT INTO `", prefix, "_articles` "
                                  , "(`title`, `summary`, `content`, `from_url`,"
                                  , " `from_url_hash`, `created_at`)"
                                  , " VALUES "
                                  , "( ?, ?, ?, ?, ?, ?)"
                                  ]

existsArticle :: FromURL -> TablePrefix -> Connection -> IO (Maybe Int64)
existsArticle fromURL prefix conn = onlyToMaybe . listToMaybe <$> query conn sql (Only fromURLHash)
  where fromURLHash = hex $ hash (pack fromURL)
        sql = fromString $ concat ["SELECT `id` FROM `", prefix, "_articles` WHERE `from_url_hash` = ?"]


getArticle :: ID -> TablePrefix -> Connection -> IO (Maybe Article)
getArticle aid prefix conn = listToMaybe <$> query conn sql (Only aid)
  where sql = fromString $ concat ["SELECT * FROM `", prefix, "_articles` WHERE `id`=?"]

removeArticle :: ID -> TablePrefix -> Connection -> IO Int64
removeArticle aid prefix conn = execute conn sql (Only aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_articles` WHERE `id`=?"]

updateArticle :: ID -> Title -> Summary -> Content -> TablePrefix -> Connection -> IO Int64
updateArticle aid title summary content prefix conn =
  execute conn sql (title, summary, content, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `title` = ?, `summary` = ?, `content` = ? WHERE `id` = ?"]

updateArticleCover :: ID -> Maybe File -> TablePrefix -> Connection -> IO Int64
updateArticleCover artId (Just cover) prefix conn =
  execute conn sql (encode cover, artId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `cover` = ? WHERE `id` = ?" ]
updateArticleCover artId Nothing prefix conn =
  execute conn sql (Only artId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `cover` = NULL WHERE `id` = ?" ]

updateArticleExtra :: ID -> Value -> TablePrefix -> Connection -> IO Int64
updateArticleExtra artId extra prefix conn =
  execute conn sql (encode extra, artId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `extra` = ? WHERE `id` = ?" ]

updateArticleTitle :: ID -> Title -> TablePrefix -> Connection -> IO Int64
updateArticleTitle aid title prefix conn =
  execute conn sql (title, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `title` = ? WHERE `id` = ?" ]

updateArticleSummary :: ID -> Summary -> TablePrefix -> Connection -> IO Int64
updateArticleSummary aid summary prefix conn =
  execute conn sql (summary, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `summary` = ? WHERE `id` = ?" ]

updateArticleContent :: ID -> Content -> TablePrefix -> Connection -> IO Int64
updateArticleContent aid content prefix conn =
  execute conn sql (content, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `content` = ? WHERE `id` = ?" ]

getAllArticle :: From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [Article]
getAllArticle from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_articles` ", show o, " LIMIT ?,?" ]

countAllArticle :: TablePrefix -> Connection -> IO Int64
countAllArticle prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_articles`" ]
