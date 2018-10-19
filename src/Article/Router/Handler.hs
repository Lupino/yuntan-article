{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Handler
  (
    saveFileHandler
  , getFileHandler
  , createArticleHandler
  , updateArticleHandler
  , updateArticleCoverHandler
  , removeArticleCoverHandler
  , updateArticleExtraHandler
  , removeArticleExtraHandler
  , clearArticleExtraHandler
  , removeArticleHandler
  , getArticleHandler
  , getArticleExtraHandler
  , getAllArticleHandler
  , existsArticleHandler

  , createTagHandler
  , getTagHandler
  , addArticleTagHandler
  , removeArticleTagHandler
  , updateTagHandler

  , createTimelineHandler
  , removeTimelineHandler
  , getAllTimelineHandler
  , saveTimelineMetaHandler
  , removeTimelineMetaHandler
  , getTimelineMetaHandler

  , graphqlHandler
  , graphqlByArticleHandler
  ) where

import           Control.Monad           (unless, void, when)
import           Control.Monad.Reader    (lift)

import           Data.Aeson              (Value (..), decode, object, (.=))
import           Data.Maybe              (fromMaybe, isJust)
import           Web.Scotty.Trans        (json, param, rescue)

import qualified Data.Text               as T (Text, length)

import           Article
import           Article.Config          (Cache)
import           Article.GraphQL         (schema, schemaByArticle)
import           Article.Router.Helper
import           Data.GraphQL            (graphql)
import           Yuntan.Types.ListResult (ListResult (getResult), merge)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.JSON       (differenceValue, pickValue,
                                          unionValue)
import           Yuntan.Utils.Scotty     (errBadRequest, errNotFound,
                                          maybeNotFound, ok, okListResult,
                                          safeParam)

import           Yuntan.Types.HasMySQL   (HasMySQL, HasOtherEnv)

saveFileHandler :: HasMySQL u => ActionH u ()
saveFileHandler = do
  bucket <- param "bucket" `rescue` (\_ -> return "upload")
  key <- param "key"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))

  fileObj <- lift $ saveFileWithExtra bucket key (fromMaybe Null extra)
  json fileObj

getFileHandler :: HasMySQL u => ActionH u ()
getFileHandler = do
  key <- param "key"
  file <- lift $ getFileWithKey key
  maybeNotFound "File" file

createArticleHandler :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u ()
createArticleHandler = do
  title     <- param "title"
  summary   <- safeParam "summary" ""
  content   <- safeParam "content" (""::T.Text)
  from_url  <- param "from_url"
  ct <- safeParam "created_at" 0

  result <- lift $ createArticleAndFetch title summary content from_url ct

  ok "article" result

updateArticleHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
updateArticleHandler Article{artID = aid} = do
  title   <- safeParam "title" ""
  summary <- safeParam "summary" ""
  content <- safeParam "content" (""::T.Text)

  if (not . null) title && (not . null) summary && T.length content > 0 then
    lift . void $ updateArticle aid title summary content
  else do
    unless (null title) $ lift . void $ updateArticleTitle aid title
    unless (null summary) $ lift . void $ updateArticleSummary aid summary
    when (T.length content > 0) $ lift . void $ updateArticleContent aid content

  resultOK

updateArticleCoverHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
updateArticleCoverHandler Article{artID = aid} = do
  fileId <- param "file_id"
  file <- lift $ getFileById fileId
  case file of
    Just _ -> do
      void . lift $ updateArticleCover aid file
      resultOK

    Nothing -> errNotFound $ concat [ "File (", show fileId, ") not found." ]

removeArticleCoverHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
removeArticleCoverHandler Article{artID = aid} = do
  void . lift $ updateArticleCover aid Nothing
  resultOK

updateArticleExtraHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
updateArticleExtraHandler Article{artID = aid, artExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra aid $ unionValue ev oev
      resultOK


removeArticleExtraHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
removeArticleExtraHandler Article{artID = aid, artExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra aid $ differenceValue oev ev
      resultOK

clearArticleExtraHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
clearArticleExtraHandler Article{artID = aid} = do
  void . lift $ updateArticleExtra aid Null
  resultOK

removeArticleHandler :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u ()
removeArticleHandler = do
  artId   <- param "art_id"

  lift . void $ removeArticle artId
  lift . void $ removeAllArticleTag artId
  lift . void $ removeTimelineListById artId

  resultOK


getArticleHandler :: Article -> ActionH u ()
getArticleHandler art = do
  exkeys <- extraKeys
  ok "article" $ pickExtra exkeys art

getArticleExtraHandler :: Article -> ActionH u ()
getArticleExtraHandler art = do
  exkeys <- extraKeys
  ok "extra" $ pickValue exkeys $ artExtra art

existsArticleHandler :: HasMySQL u => ActionH u ()
existsArticleHandler = do
  fromURL <- param "from_url"

  art <- lift $ existsArticle fromURL

  ok "id" $ fromMaybe 0 art


getAllArticleHandler :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u ()
getAllArticleHandler = do
  result <- articles
  resultArticle result

createTagHandler :: HasMySQL u => ActionH u ()
createTagHandler = do
  name <- param "tag"
  tag <- lift $ do
    t <- getTagByName name
    if isJust t then return t
    else do
      tid <- addTag name
      getTagById tid

  ok "tag" tag

getTagHandler :: HasMySQL u => Tag -> ActionH u ()
getTagHandler = ok "tag"

addArticleTagHandler :: (HasMySQL u, HasOtherEnv Cache u) => Tag -> Article -> ActionH u ()
addArticleTagHandler Tag{tagID = tid} Article{artID = aid} = do
  void . lift $ addArticleTag aid tid

  resultOK

removeArticleTagHandler :: (HasMySQL u, HasOtherEnv Cache u) => Tag -> Article -> ActionH u ()
removeArticleTagHandler Tag{tagID = tid} Article{artID = aid} = do
  void . lift $ removeArticleTag aid tid
  resultOK

updateTagHandler :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u ()
updateTagHandler = do
  tid <- param "tag_id"
  name <- param "tag"

  oldtag <- lift $ getTagById tid
  if isJust oldtag then do
    tag2 <- lift $ getTagByName name
    if isJust tag2 then errBadRequest "Bad Request."
    else do
      void . lift $ updateTag tid name
      resultOK

  else errNotFound "Not Found."

createTimelineHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
createTimelineHandler Article{artID = aid} = do
  name <- param "timeline"
  void . lift $ addTimeline name aid

  resultOK

removeTimelineHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
removeTimelineHandler Article{artID = aid} = do
  name <- param "timeline"
  void . lift $ removeTimeline name aid

  resultOK

getAllTimelineHandler :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u ()
getAllTimelineHandler = do
  name <- param "timeline"
  result <- timeline name

  resultArticle result

saveTimelineMetaHandler :: HasMySQL u => ActionH u ()
saveTimelineMetaHandler = do
  name <- param "timeline"
  title <- safeParam "title" ""
  summary <- safeParam "summary" ""
  void . lift $ saveTimelineMeta name title summary

  resultOK

removeTimelineMetaHandler :: HasMySQL u => ActionH u ()
removeTimelineMetaHandler = do
  name <- param "timeline"
  void . lift $ removeTimelineMeta name

  resultOK

getTimelineMetaHandler :: HasMySQL u => ActionH u ()
getTimelineMetaHandler = do
  name <- param "timeline"
  meta <- lift $ getTimelineMeta name
  case meta of
    Nothing     -> json $ object ["title" .= Null, "summary" .= Null]
    Just (t, s) -> json $ object ["title" .= t, "summary" .= s]

resultArticle :: HasMySQL u => ListResult Article -> ActionH u ()
resultArticle result_ = do
  isCard <- safeParam "card" ("" :: String)
  exkeys <- extraKeys

  let result = result_ {getResult = map (pickExtra exkeys) $ getResult result_}

  if null isCard then okListResult "articles" result
                 else do
                      cards <- lift $ mapM toCardItem $ getResult result
                      okListResult "cards" $ merge cards result

graphqlHandler :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u ()
graphqlHandler = do
  query <- param "query"
  ret <- lift $ graphql schema query
  json ret

graphqlByArticleHandler :: (HasMySQL u, HasOtherEnv Cache u) => Article -> ActionH u ()
graphqlByArticleHandler art = do
  query <- param "query"
  ret <- lift $ graphql (schemaByArticle art) query
  json ret
