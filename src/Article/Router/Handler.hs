{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Handler
  (
    uploadHandler
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
  ) where

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (lift)
import           Haxl.Core                 (Env (..), env)

import           Data.Aeson                (Value (..), decode)
import           Data.Map                  as Map (lookup)
import           Data.Maybe                (fromJust, fromMaybe, isJust)
import           Network.Mime              (defaultMimeMap, fileNameExtensions)
import           Web.Scotty.Trans          (body, json, param)

import           Control.Exception         (SomeException, try)
import qualified Data.ByteString.Char8     as BS (unpack)
import qualified Data.ByteString.Lazy      as LB (ByteString, length, toStrict)
import qualified Data.Text                 as T (Text, length, pack, unpack)


import           Article
import           Article.Router.Helper
import           Article.UserEnv           (ActionM, UserEnv (..))
import           Article.Utils             (getImageShape)
import           Dispatch.Types.ListResult (ListResult (getResult), merge)
import           Dispatch.Utils.JSON       (differenceValue, unionValue)
import           Dispatch.Utils.Scotty     (errBadRequest, errNotFound, ok,
                                            okListResult)

getMineType :: T.Text -> (String, String)
getMineType =
  go . fileNameExtensions
  where go :: [T.Text] -> (String, String)
        go [] = ("", "")
        go (e:es) =
          case Map.lookup e defaultMimeMap of
            Nothing -> if null es then (T.unpack e, "") else go es
            Just mt -> (T.unpack e, BS.unpack mt)

getFileExtra :: String -> LB.ByteString -> Maybe (Int, Int)-> FileExtra
getFileExtra fn fc shape = fileExtraEmpty { fileSize   = size
                                          , fileType   = mt
                                          , fileExt    = ext
                                          , fileName   = fn
                                          , fileWidth  = w
                                          , fileHeight = h
                                          }
  where size = LB.length fc
        fn' = T.pack fn
        (ext, mt) = getMineType fn'
        w | isJust shape = Just $ (fst . fromJust) shape
          | otherwise    = Nothing

        h | isJust shape = Just $ (snd . fromJust) shape
          | otherwise    = Nothing

uploadHandler :: ActionM ()
uploadHandler = do
  fn <- safeParam "fileName" "xxxx.xx"
  wb <- body

  path <- lift $ uploadPath <$> env userEnv

  e <- liftIO (try . return . getImageShape $ LB.toStrict wb :: IO (Either SomeException (Maybe (Int, Int))))

  let shape = case e of
                Left _       -> Nothing
                Right shape' -> shape'

  fileObj <- lift $ uploadFileWithExtra path wb (getFileExtra fn wb shape)
  json fileObj

createArticleHandler :: ActionM ()
createArticleHandler = do
  title     <- param "title"
  summary   <- safeParam "summary" ""
  content   <- safeParam "content" (""::T.Text)
  from_url  <- param "from_url"
  ct <- safeParam "created_at" 0

  result <- lift $ createArticleAndFetch title summary content from_url ct

  ok "article" result

updateArticleHandler :: ActionM ()
updateArticleHandler = hasArticle $ \_ -> do
  artId   <- param "art_id"
  title   <- safeParam "title" ""
  summary <- safeParam "summary" ""
  content <- safeParam "content" (""::T.Text)

  art2 <- lift $ do
    void $ if (not . null) title && (not . null) summary && T.length content > 0 then
      updateArticle artId title summary content
    else do
      changed1 <- if (not . null) title then updateArticleTitle artId title else return 0
      changed2 <- if (not . null) summary then updateArticleSummary artId summary else return 0
      changed3 <- if T.length content > 0 then updateArticleContent artId content else return 0
      return (changed1 + changed2 + changed3)
    runWithEnv $ getArticleById artId

  ok "article" art2

updateArticleCoverHandler :: ActionM ()
updateArticleCoverHandler = hasArticle $ \art -> do
  fileId <- param "file_id"
  file <- lift $ getFileById fileId
  case file of
    Just _ -> do
      void . lift $ updateArticleCover (artID art) file
      resultOK

    Nothing -> errNotFound $ concat [ "File (", show fileId, ") not found." ]

removeArticleCoverHandler :: ActionM ()
removeArticleCoverHandler = hasArticle $ \art -> do
  void . lift $ updateArticleCover (artID art) Nothing
  resultOK

updateArticleExtraHandler :: ActionM ()
updateArticleExtraHandler = hasArticle $ \art -> do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra (artID art) $ unionValue ev (artExtra art)
      resultOK


removeArticleExtraHandler :: ActionM ()
removeArticleExtraHandler = hasArticle $ \art -> do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Nothing -> errBadRequest "extra field is required."
    Just ev -> do
      void . lift $ updateArticleExtra (artID art) $ differenceValue (artExtra art) ev
      resultOK

clearArticleExtraHandler :: ActionM ()
clearArticleExtraHandler = hasArticle $ \art -> do
  void . lift $ updateArticleExtra (artID art) Null
  resultOK

removeArticleHandler :: ActionM ()
removeArticleHandler = do
  artId   <- param "art_id"

  lift $ withTransaction $ do
    void $ removeArticle artId
    void $ removeAllArticleTag artId
    void $ removeAllTimelineByArtId artId

  resultOK


getArticleHandler :: ActionM ()
getArticleHandler = hasArticle $ ok "article"

hasArticle :: (Article -> ActionM ()) -> ActionM ()
hasArticle action = do
  artId <- param "art_id"

  art <- lift $ getArticleById artId
  maybe (notFound artId) action art

  where notFound artId = errNotFound $ concat [ "Article (", show artId, ") not found" ]

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

getTagHandler :: ActionM ()
getTagHandler = hasTag $ ok "tag"

hasTag :: (Tag -> ActionM ()) -> ActionM ()
hasTag action = do
  tid <- safeParam "tag_id" (0::ID)
  name <- safeParam "tag" (""::String)

  tag <- lift $
    if tid > 0 then getTagById tid
    else getTagByName name

  maybe notFound action tag
  where notFound = errNotFound "Not Found."

addArticleTagHandler :: ActionM ()
addArticleTagHandler = hasTag $ \tag ->
  hasArticle $ \art -> do
    lift $ do
      void $ addArticleTag (artID art) (tagID tag)

    resultOK

removeArticleTagHandler :: ActionM ()
removeArticleTagHandler = hasTag $ \tag ->
  hasArticle $ \art -> do
    lift $ do
      void $ removeArticleTag (artID art) (tagID tag)

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

createTimelineHandler :: ActionM ()
createTimelineHandler =  hasArticle $ \art -> do
  name <- param "timeline"

  lift $ do
    void $ addTimeline name $ artID art

  resultOK

removeTimelineHandler :: ActionM ()
removeTimelineHandler =  hasArticle $ \art -> do
  name <- param "timeline"

  lift $ do
    void $ removeTimeline name $ artID art

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

resultArticle :: ListResult Article -> ActionM ()
resultArticle result = do
  isCard <- safeParam "card" ("" :: String)

  if null isCard then okListResult "articles" result
                 else do
                      cards <- lift $ mapM toCardItem $ getResult result
                      okListResult "cards" $ merge cards result
