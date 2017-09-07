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
  ) where

import           Control.Monad           (void)
import           Control.Monad.Reader    (lift)

import           Data.Aeson              (Value (..), decode, object, (.=))
import           Data.Maybe              (fromMaybe, isJust)
import           Web.Scotty.Trans        (json, param, rescue)

import qualified Data.Text               as T (Text, length)


import           Article
import           Article.Router.Helper
import           Article.UserEnv         (ActionM)
import           Yuntan.Types.ListResult (ListResult (getResult), merge)
import           Yuntan.Utils.JSON       (differenceValue, unionValue)
import           Yuntan.Utils.Scotty     (errBadRequest, errNotFound,
                                          maybeNotFound, ok, okListResult,
                                          safeParam)

import           Yuntan.Utils.Haxl       (runWithEnv)

saveFileHandler :: ActionM ()
saveFileHandler = do
  bucket <- param "bucket" `rescue` (\_ -> return "upload")
  key <- param "key"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))

  fileObj <- lift $ saveFileWithExtra bucket key (fromMaybe Null extra)
  json fileObj

getFileHandler :: ActionM ()
getFileHandler = do
  key <- param "key"
  file <- lift $ getFileWithKey key
  maybeNotFound "File" file

createArticleHandler :: ActionM ()
createArticleHandler = do
  title     <- param "title"
  summary   <- safeParam "summary" ""
  content   <- safeParam "content" (""::T.Text)
  from_url  <- param "from_url"
  ct <- safeParam "created_at" 0

  result <- lift $ createArticleAndFetch title summary content from_url ct

  ok "article" result

updateArticleHandler :: Article -> ActionM ()
updateArticleHandler art@Article{artID = aid} = do
  title   <- safeParam "title" ""
  summary <- safeParam "summary" ""
  content <- safeParam "content" (""::T.Text)

  art2 <- lift $ do
    changed <- if (not . null) title && (not . null) summary && T.length content > 0 then
      updateArticle aid title summary content
    else do
      changed1 <- if (not . null) title then updateArticleTitle aid title else return 0
      changed2 <- if (not . null) summary then updateArticleSummary aid summary else return 0
      changed3 <- if T.length content > 0 then updateArticleContent aid content else return 0
      return (changed1 + changed2 + changed3)
    if changed > 0 then runWithEnv $ getArticleById aid
                   else return (Just art)

  ok "article" art2

updateArticleCoverHandler :: Article -> ActionM ()
updateArticleCoverHandler Article{artID = aid} = do
  fileId <- param "file_id"
  file <- lift $ getFileById fileId
  case file of
    Just _ -> do
      void . lift $ updateArticleCover aid file
      resultOK

    Nothing -> errNotFound $ concat [ "File (", show fileId, ") not found." ]

removeArticleCoverHandler :: Article -> ActionM ()
removeArticleCoverHandler Article{artID = aid} = do
  void . lift $ updateArticleCover aid Nothing
  resultOK

updateArticleExtraHandler :: Article -> ActionM ()
updateArticleExtraHandler Article{artID = aid, artExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra aid $ unionValue ev oev
      resultOK


removeArticleExtraHandler :: Article -> ActionM ()
removeArticleExtraHandler Article{artID = aid, artExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra aid $ differenceValue oev ev
      resultOK

clearArticleExtraHandler :: Article -> ActionM ()
clearArticleExtraHandler Article{artID = aid} = do
  void . lift $ updateArticleExtra aid Null
  resultOK

removeArticleHandler :: ActionM ()
removeArticleHandler = do
  artId   <- param "art_id"

  lift . void $ removeArticle artId
  lift . void $ removeAllArticleTag artId
  lift . void $ removeAllTimelineByArtId artId

  resultOK


getArticleHandler :: Article -> ActionM ()
getArticleHandler = ok "article"

existsArticleHandler :: ActionM ()
existsArticleHandler = do
  fromURL <- param "from_url"

  art <- lift $ existsArticle fromURL

  ok "id" $ fromMaybe 0 art


getAllArticleHandler :: ActionM ()
getAllArticleHandler = do
  result <- articles
  resultArticle result

createTagHandler :: ActionM ()
createTagHandler = do
  name <- param "tag"
  tag <- lift $ do
    t <- getTagByName name
    if isJust t then return t
    else do
      tid <- addTag name
      getTagById tid

  ok "tag" tag

getTagHandler :: Tag -> ActionM ()
getTagHandler = ok "tag"

addArticleTagHandler :: Tag -> Article -> ActionM ()
addArticleTagHandler Tag{tagID = tid} Article{artID = aid} = do
  void . lift $ addArticleTag aid tid

  resultOK

removeArticleTagHandler :: Tag -> Article -> ActionM ()
removeArticleTagHandler Tag{tagID = tid} Article{artID = aid} = do
  void . lift $ removeArticleTag aid tid
  resultOK

updateTagHandler :: ActionM ()
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

createTimelineHandler :: Article -> ActionM ()
createTimelineHandler Article{artID = aid} = do
  name <- param "timeline"
  void . lift $ addTimeline name aid

  resultOK

removeTimelineHandler :: Article -> ActionM ()
removeTimelineHandler Article{artID = aid} = do
  name <- param "timeline"
  void . lift $ removeTimeline name aid

  resultOK

getAllTimelineHandler :: ActionM ()
getAllTimelineHandler = do
  name <- param "timeline"
  result <- timeline name

  resultArticle result

saveTimelineMetaHandler :: ActionM ()
saveTimelineMetaHandler = do
  name <- param "timeline"
  title <- safeParam "title" ""
  summary <- safeParam "summary" ""
  void . lift $ saveTimelineMeta name title summary

  resultOK

removeTimelineMetaHandler :: ActionM ()
removeTimelineMetaHandler = do
  name <- param "timeline"
  void . lift $ removeTimelineMeta name

  resultOK

getTimelineMetaHandler :: ActionM ()
getTimelineMetaHandler = do
  name <- param "timeline"
  meta <- lift $ getTimelineMeta name
  case meta of
    Nothing     -> json $ object ["title" .= Null, "summary" .= Null]
    Just (t, s) -> json $ object ["title" .= t, "summary" .= s]

resultArticle :: ListResult Article -> ActionM ()
resultArticle result = do
  isCard <- safeParam "card" ("" :: String)

  if null isCard then okListResult "articles" result
                 else do
                      cards <- lift $ mapM toCardItem $ getResult result
                      okListResult "cards" $ merge cards result
