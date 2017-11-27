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

import           Control.Monad           (void)
import           Database.MySQL.Simple   (Only (..), execute, insertID, query,
                                          query_)
import           Yuntan.Types.HasMySQL   (MySQL)

import           Control.Applicative     ((<$>))
import           Crypto.Hash.SHA1        (hash)
import           Data.ByteString.Char8   (pack)
import           Data.Hex                (hex)
import           Data.String             (fromString)

import           Article.Types
import           Article.Utils           (onlyToMaybe)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (Value (..), encode)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.UnixTime
import           Prelude                 hiding (id)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createArticle :: Title -> Summary -> Content -> FromURL -> CreatedAt -> MySQL Int64
createArticle title summary content fromURL ct prefix conn = do
  oldID <- existsArticle fromURL prefix conn
  ct' <- if ct > 0 then return ct else liftIO $ read . show . toEpochTime <$> getUnixTime
  case oldID of
    Just id -> return id
    Nothing -> do
      void $ execute conn sql (title, summary, content, fromURL, fromURLHash, ct')
      fromIntegral <$> insertID conn
  where fromURLHash = hex $ hash (pack fromURL)
        sql = fromString $ concat [ "INSERT INTO `", prefix, "_articles` "
                                  , "(`title`, `summary`, `content`, `from_url`,"
                                  , " `from_url_hash`, `created_at`)"
                                  , " VALUES "
                                  , "( ?, ?, ?, ?, ?, ?)"
                                  ]

existsArticle :: FromURL -> MySQL (Maybe Int64)
existsArticle fromURL prefix conn = onlyToMaybe . listToMaybe <$> query conn sql (Only fromURLHash)
  where fromURLHash = hex $ hash (pack fromURL)
        sql = fromString $ concat ["SELECT `id` FROM `", prefix, "_articles` WHERE `from_url_hash` = ?"]


getArticle :: ID -> MySQL (Maybe Article)
getArticle aid prefix conn = listToMaybe <$> query conn sql (Only aid)
  where sql = fromString $ concat ["SELECT * FROM `", prefix, "_articles` WHERE `id`=?"]

removeArticle :: ID -> MySQL Int64
removeArticle aid prefix conn = execute conn sql (Only aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_articles` WHERE `id`=?"]

updateArticle :: ID -> Title -> Summary -> Content -> MySQL Int64
updateArticle aid title summary content prefix conn =
  execute conn sql (title, summary, content, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `title` = ?, `summary` = ?, `content` = ? WHERE `id` = ?"]

updateArticleCover :: ID -> Maybe File -> MySQL Int64
updateArticleCover artId (Just cover) prefix conn =
  execute conn sql (encode cover, artId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `cover` = ? WHERE `id` = ?" ]
updateArticleCover artId Nothing prefix conn =
  execute conn sql (Only artId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `cover` = NULL WHERE `id` = ?" ]

updateArticleExtra :: ID -> Value -> MySQL Int64
updateArticleExtra artId extra prefix conn =
  execute conn sql (encode extra, artId)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `extra` = ? WHERE `id` = ?" ]

updateArticleTitle :: ID -> Title -> MySQL Int64
updateArticleTitle aid title prefix conn =
  execute conn sql (title, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `title` = ? WHERE `id` = ?" ]

updateArticleSummary :: ID -> Summary -> MySQL Int64
updateArticleSummary aid summary prefix conn =
  execute conn sql (summary, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `summary` = ? WHERE `id` = ?" ]

updateArticleContent :: ID -> Content -> MySQL Int64
updateArticleContent aid content prefix conn =
  execute conn sql (content, aid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_articles` SET `content` = ? WHERE `id` = ?" ]

getAllArticle :: From -> Size -> OrderBy -> MySQL [Article]
getAllArticle from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_articles` ", show o, " LIMIT ?,?" ]

countAllArticle :: MySQL Int64
countAllArticle prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_articles`" ]
