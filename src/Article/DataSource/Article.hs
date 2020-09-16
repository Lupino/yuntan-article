{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Article
  ( createArticle
  , getArticle
  , removeArticle
  , getArticleIdList
  , countArticle
  , updateArticle
  , updateArticleCover
  , updateArticleExtra
  , updateArticleTitle
  , updateArticleSummary
  , updateArticleContent

  , saveAlias
  , getAlias
  , removeAlias
  , getAllAlias
  , removeAllAlias
  ) where

import           Article.DataSource.Table (articleAlias, articles)
import           Article.Types
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value (..), encode)
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import           Data.UnixTime
import           Database.PSQL.Types      (From, Only (..), OrderBy, PSQL, Size,
                                           count_, delete, insertOrUpdate,
                                           insertRet, none, selectOne,
                                           selectOneOnly, selectOnly,
                                           selectOnly_, update)

createArticle :: Title -> Summary -> Content -> CreatedAt -> PSQL Int64
createArticle title summary content ct = do
  ct' <- if ct > 0 then return ct else liftIO $ read . show . toEpochTime <$> getUnixTime
  insertRet articles
    ["title", "summary", "content", "created_at"] "id"
    (title, summary, content, ct') 0

getArticle :: ID -> PSQL (Maybe Article)
getArticle aid = selectOne articles ["*"] "id = ?" (Only aid)

removeArticle :: ID -> PSQL Int64
removeArticle aid = delete articles "id = ?" (Only aid)

updateArticle :: ID -> Title -> Summary -> Content -> PSQL Int64
updateArticle aid title summary content =
  update articles ["title", "summary", "content"] "id = ?" (title, summary, content, aid)

updateArticleCover :: ID -> Maybe File -> PSQL Int64
updateArticleCover aid (Just cover) =
  update articles ["cover"] "id = ?" (encode cover, aid)
updateArticleCover aid Nothing =
  update articles ["cover"] "id = ?" ("{}" :: String, aid)

updateArticleExtra :: ID -> Value -> PSQL Int64
updateArticleExtra aid extra =
  update articles ["extra"] "id = ?" (encode extra, aid)

updateArticleTitle :: ID -> Title -> PSQL Int64
updateArticleTitle aid title =
  update articles ["title"] "id = ?" (title, aid)

updateArticleSummary :: ID -> Summary -> PSQL Int64
updateArticleSummary aid summary =
  update articles ["summary"] "id = ?" (summary, aid)

updateArticleContent :: ID -> Content -> PSQL Int64
updateArticleContent aid content =
  update articles ["content"] "id = ?" (content, aid)

getArticleIdList :: From -> Size -> OrderBy -> PSQL [ID]
getArticleIdList = selectOnly_ articles "id"

countArticle :: PSQL Int64
countArticle = count_ articles

saveAlias :: Text -> ID -> PSQL Int64
saveAlias alias aid = insertOrUpdate articleAlias ["alias"] ["id"] [] (alias, aid)

removeAlias :: Text -> PSQL Int64
removeAlias alias = delete articleAlias "alias = ?" (Only alias)

removeAllAlias :: ID -> PSQL Int64
removeAllAlias aid = delete articleAlias "id = ?" (Only aid)

getAllAlias :: ID -> PSQL [Text]
getAllAlias aid = selectOnly articleAlias "alias" "id = ?" (Only aid) 0 100 none

getAlias :: Text -> PSQL (Maybe ID)
getAlias alias = selectOneOnly articleAlias "id" "alias = ?" (Only alias)
