{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Handler
  ( saveFileHandler
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

import           Article
import           Article.Config        (Cache)
import           Article.GraphQL       (schema, schemaByArticle)
import           Article.Router.Helper
import           Control.Monad         (unless, void, when)
import           Control.Monad.Reader  (lift)
import           Data.Aeson            (Value (..), decode, object, toJSON,
                                        (.=))
import           Data.Aeson.Helper     (difference, pick, union)
import           Data.Aeson.Result     (List (getResult), fromList, merge)
import           Data.GraphQL          (graphql)
import           Data.Maybe            (fromMaybe, isJust)
import qualified Data.Text             as T (Text, length)
import           Database.PSQL.Types   (HasOtherEnv, HasPSQL)
import           Haxl.Core             (GenHaxl, memo)
import           Web.Scotty.Haxl       (ActionH)
import           Web.Scotty.Trans      (json, param, rescue)
import           Web.Scotty.Utils      (errBadRequest, errNotFound,
                                        maybeNotFound, ok, safeParam)

saveFileHandler :: HasPSQL u => ActionH u w ()
saveFileHandler = do
  bucket <- param "bucket" `rescue` (\_ -> return "upload")
  key <- param "key"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))

  fileObj <- lift $ saveFileWithExtra bucket key (fromMaybe Null extra)
  json fileObj

getFileHandler :: HasPSQL u => ActionH u w ()
getFileHandler = do
  key <- param "key"
  file <- lift $ getFileWithKey key
  maybeNotFound "File" file

createArticleHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
createArticleHandler = do
  title     <- param "title"
  summary   <- safeParam "summary" ""
  content   <- safeParam "content" (""::T.Text)
  ct <- safeParam "created_at" 0

  result <- lift $ createArticleAndFetch title summary content ct

  ok "article" result

updateArticleHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
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

updateArticleCoverHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
updateArticleCoverHandler Article{artID = aid} = do
  fileId <- param "file_id"
  file <- lift $ getFileById fileId
  case file of
    Just _ -> do
      void . lift $ updateArticleCover aid file
      resultOK

    Nothing -> errNotFound $ concat [ "File (", show fileId, ") not found." ]

removeArticleCoverHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
removeArticleCoverHandler Article{artID = aid} = do
  void . lift $ updateArticleCover aid Nothing
  resultOK

updateArticleExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
updateArticleExtraHandler Article{artID = aid, artExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra aid $ union ev oev
      resultOK


removeArticleExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
removeArticleExtraHandler Article{artID = aid, artExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra aid $ difference oev ev
      resultOK

clearArticleExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
clearArticleExtraHandler Article{artID = aid} = do
  void . lift $ updateArticleExtra aid Null
  resultOK

removeArticleHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
removeArticleHandler = do
  artId   <- param "art_id"

  lift . void $ removeArticle artId
  lift . void $ removeAllArticleTag artId
  lift . void $ removeTimelineListById artId

  resultOK


getArticleHandler :: Article -> ActionH u w ()
getArticleHandler art = ok "article" =<< checkSed . toJSON =<< preprocessArticle art

getArticleExtraHandler :: Article -> ActionH u w ()
getArticleExtraHandler art = do
  exkeys <- extraKeys
  ok "extra" $ pick exkeys $ artExtra art

getAllArticleHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getAllArticleHandler = do
  result <- articles
  resultArticle result

createTagHandler :: HasPSQL u => ActionH u w ()
createTagHandler = do
  name <- param "tag"
  tag <- lift $ do
    t <- getTagByName name
    if isJust t then return t
    else do
      tid <- addTag name
      getTagById tid

  ok "tag" tag

getTagHandler :: HasPSQL u => Tag -> ActionH u w ()
getTagHandler = ok "tag"

addArticleTagHandler :: (HasPSQL u, HasOtherEnv Cache u) => Tag -> Article -> ActionH u w ()
addArticleTagHandler Tag{tagID = tid} Article{artID = aid} = do
  void . lift $ addArticleTag aid tid

  resultOK

removeArticleTagHandler :: (HasPSQL u, HasOtherEnv Cache u) => Tag -> Article -> ActionH u w ()
removeArticleTagHandler Tag{tagID = tid} Article{artID = aid} = do
  void . lift $ removeArticleTag aid tid
  resultOK

updateTagHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
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

createTimelineHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
createTimelineHandler Article{artID = aid} = do
  name <- param "timeline"
  void . lift $ addTimeline name aid

  resultOK

removeTimelineHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
removeTimelineHandler Article{artID = aid} = do
  name <- param "timeline"
  void . lift $ removeTimeline name aid

  resultOK

getAllTimelineHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getAllTimelineHandler = do
  name <- param "timeline"
  result <- timeline name

  resultArticle result

saveTimelineMetaHandler :: HasPSQL u => ActionH u w ()
saveTimelineMetaHandler = do
  name <- param "timeline"
  title <- safeParam "title" ""
  summary <- safeParam "summary" ""
  void . lift $ saveTimelineMeta name title summary

  resultOK

removeTimelineMetaHandler :: HasPSQL u => ActionH u w ()
removeTimelineMetaHandler = do
  name <- param "timeline"
  void . lift $ removeTimelineMeta name

  resultOK

getTimelineMetaHandler :: HasPSQL u => ActionH u w ()
getTimelineMetaHandler = do
  name <- param "timeline"
  meta <- lift $ getTimelineMeta name
  case meta of
    Nothing     -> json $ object ["title" .= Null, "summary" .= Null]
    Just (t, s) -> json $ object ["title" .= t, "summary" .= s]

resultArticle :: HasPSQL u => List Article -> ActionH u w ()
resultArticle result_ = do
  isCard <- safeParam "card" ("" :: String)

  arts <- preprocessArticleList $ getResult result_

  let result = result_ {getResult = arts}

  if null isCard then json =<< checkSed (fromList "articles" result)
                 else do
                      cards <- lift $ mapM toCardItem $ getResult result
                      json =<< checkSed (fromList "cards" $ merge cards result)

cachedValue :: T.Text -> Maybe Value -> GenHaxl u w ()
cachedValue k = void . memo k . pure . fromMaybe Null

graphqlHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
graphqlHandler = do
  query <- param "query"
  fev <- decode <$> safeParam "file_extra" "{}"
  lift $ cachedValue "file_extra" fev
  aev <- decode <$> safeParam "article_extra" "{}"
  lift $ cachedValue "article_extra" aev
  ret <- lift $ graphql schema query
  json ret

graphqlByArticleHandler :: (HasPSQL u, HasOtherEnv Cache u) => Article -> ActionH u w ()
graphqlByArticleHandler art = do
  query <- param "query"
  fev <- fromMaybe Null . decode <$> safeParam "file_extra" "{}"
  aev <- fromMaybe Null . decode <$> safeParam "article_extra" "{}"
  ret <- lift $ graphql (schemaByArticle aev fev art) query
  json ret
