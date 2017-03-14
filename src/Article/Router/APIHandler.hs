{-# LANGUAGE OverloadedStrings #-}
module Article.Router.APIHandler
  (
    uploadAPIHandler
  , createArticleAPIHandler
  , updateArticleAPIHandler
  , updateArticleCoverAPIHandler
  , removeArticleCoverAPIHandler
  , updateArticleExtraAPIHandler
  , removeArticleExtraAPIHandler
  , clearArticleExtraAPIHandler
  , removeArticleAPIHandler
  , getArticleAPIHandler
  , getAllArticleAPIHandler
  , existsArticleAPIHandler

  , createTagAPIHandler
  , getTagAPIHandler
  , addArticleTagAPIHandler
  , removeArticleTagAPIHandler
  , updateTagAPIHandler

  , createTimelineAPIHandler
  , removeTimelineAPIHandler
  , getAllTimelineAPIHandler
  , saveTimelineMetaAPIHandler
  , removeTimelineMetaAPIHandler
  ) where

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (lift)
import           Haxl.Core                 (Env (..), env)

import           Data.Aeson                (Value (..), decode, object, (.=))
import           Data.HashMap.Strict       (difference, union)
import           Data.Map                  as Map (lookup)
import           Data.Maybe                (fromJust, fromMaybe, isJust)
import           Network.HTTP.Types        (status400, status404)
import           Network.Mime              (defaultMimeMap, fileNameExtensions)
import           Web.Scotty.Trans          (body, json, param, status)

import           Control.Exception         (SomeException, try)
import qualified Data.ByteString.Char8     as BS (unpack)
import qualified Data.ByteString.Lazy      as LB (ByteString, length, toStrict)
import qualified Data.Text                 as T (Text, length, pack, unpack)


import           Article
import           Article.Router.Helper
import           Article.UserEnv           (ActionM, UserEnv (..))
import           Article.Utils             (getImageShape)
import           Dispatch.Types.ListResult (ListResult (..), fromListResult)

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

uploadAPIHandler :: ActionM ()
uploadAPIHandler = do
  fn <- safeParam "fileName" "xxxx.xx"
  wb <- body

  path <- lift $ uploadPath <$> env userEnv

  e <- liftIO (try . return . getImageShape $ LB.toStrict wb :: IO (Either SomeException (Maybe (Int, Int))))

  let shape = case e of
                Left _       -> Nothing
                Right shape' -> shape'

  fileObj <- runArticle $ uploadFileWithExtra path wb (getFileExtra fn wb shape)
  json fileObj

createArticleAPIHandler :: ActionM ()
createArticleAPIHandler = do
  title     <- param "title"
  summary   <- safeParam "summary" ""
  content   <- safeParam "content" (""::T.Text)
  from_url  <- param "from_url"
  ct <- param "created_at"

  result <- runArticle $ createArticleAndFetch title summary content from_url ct

  json $ object ["article" .= result]


updateArticleAPIHandler :: ActionM ()
updateArticleAPIHandler = hasArticle $ \_ -> do
  artId   <- param "art_id"
  title   <- safeParam "title" ""
  summary <- safeParam "summary" ""
  content <- safeParam "content" (""::T.Text)

  art2 <- runArticle $ do
    void $ if (not . null) title && (not . null) summary && T.length content > 0 then
      updateArticle artId title summary content
    else do
      changed1 <- if (not . null) title then updateArticleTitle artId title else return 0
      changed2 <- if (not . null) summary then updateArticleSummary artId summary else return 0
      changed3 <- if T.length content > 0 then updateArticleContent artId content else return 0
      return (changed1 + changed2 + changed3)
    runWithEnv $ getArticleById artId

  json $ object [ "article" .= art2 ]

updateArticleCoverAPIHandler :: ActionM ()
updateArticleCoverAPIHandler = hasArticle $ \art -> do
  fileId <- param "file_id"
  file <- runArticle $ getFileById fileId
  case file of
    Just _ -> do
      void . runArticle $ updateArticleCover (artID art) file
      resultOK
    Nothing -> do
      status status404
      json $ object [ "err" .= mconcat ["File (", T.pack $ show fileId, ") not found." ] ]

removeArticleCoverAPIHandler :: ActionM ()
removeArticleCoverAPIHandler = hasArticle $ \art -> do
  void . runArticle $ updateArticleCover (artID art) Nothing
  resultOK

updateArticleExtraAPIHandler :: ActionM ()
updateArticleExtraAPIHandler = hasArticle $ \art -> do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> do
      void . runArticle $ updateArticleExtra (artID art) $ unionExtraValue ev (artExtra art)
      resultOK
    Nothing -> do
      status status400
      json $ object [ "err" .= T.pack "extra field is required." ]

removeArticleExtraAPIHandler :: ActionM ()
removeArticleExtraAPIHandler = hasArticle $ \art -> do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> do
      void . runArticle $ updateArticleExtra (artID art) $ differenceExtraValue (artExtra art) ev
      resultOK
    Nothing -> do
      status status400
      json $ object [ "err" .= T.pack "extra field is required." ]

clearArticleExtraAPIHandler :: ActionM ()
clearArticleExtraAPIHandler = hasArticle $ \art -> do
  void . runArticle $ updateArticleExtra (artID art) Null
  resultOK

unionExtraValue :: Value -> Value -> Value
unionExtraValue (Object a) (Object b) = Object $ union a b
unionExtraValue (Object a) _          = Object a
unionExtraValue _ (Object b)          = Object b
unionExtraValue _ _                   = Null

differenceExtraValue :: Value -> Value -> Value
differenceExtraValue (Object a) (Object b) = Object $ difference a b
differenceExtraValue (Object a) _          = Object a
differenceExtraValue _ _                   = Null

removeArticleAPIHandler :: ActionM ()
removeArticleAPIHandler = do
  artId   <- param "art_id"

  runArticle $ withTransaction $ do
    void $ removeArticle artId
    void $ removeAllArticleTag artId
    void $ removeAllTimelineByArtId artId

  resultOK


getArticleAPIHandler :: ActionM ()
getArticleAPIHandler = hasArticle $ \art -> do
  json $ object ["article" .= art]

hasArticle :: (Article -> ActionM ()) -> ActionM ()
hasArticle action = do
  artId <- param "art_id"

  art <- runArticle $ getArticleById artId
  maybe (notFound artId) action art
  where notFound artId = do
          status status404
          json $ object ["err" .= mconcat [ "Article (", T.pack $ show artId, ") not found" ] ]

existsArticleAPIHandler :: ActionM ()
existsArticleAPIHandler = do
  fromURL <- param "from_url"

  art <- runArticle $ existsArticle fromURL

  json $ object ["id" .= fromMaybe 0 art]


getAllArticleAPIHandler :: ActionM ()
getAllArticleAPIHandler = do
  result <- articles
  resultArticle result

createTagAPIHandler :: ActionM ()
createTagAPIHandler = do
  name <- param "tag"
  tag <- runArticle $ do
    t <- getTagByName name
    if isJust t then return t
    else do
      tid <- addTag name
      getTagById tid

  json $ object ["tag" .= tag]

getTagAPIHandler :: ActionM ()
getTagAPIHandler = hasTag $ \tag -> json $ object ["tag" .= tag]

hasTag :: (Tag -> ActionM ()) -> ActionM ()
hasTag action = do
  tid <- safeParam "tag_id" (0::ID)
  name <- safeParam "tag" (""::String)

  tag <- runArticle $
    if tid > 0 then getTagById tid
    else getTagByName name

  maybe notFound action tag
  where notFound = do
          status status404
          json $ object ["err" .= T.pack "not found" ]

addArticleTagAPIHandler :: ActionM ()
addArticleTagAPIHandler = hasTag $ \tag ->
  hasArticle $ \art -> do
    runArticle $ do
      void $ addArticleTag (artID art) (tagID tag)

    resultOK

removeArticleTagAPIHandler :: ActionM ()
removeArticleTagAPIHandler = hasTag $ \tag ->
  hasArticle $ \art -> do
    runArticle $ do
      void $ removeArticleTag (artID art) (tagID tag)

    resultOK

updateTagAPIHandler :: ActionM ()
updateTagAPIHandler = do
  tid <- param "tag_id"
  name <- param "tag"

  oldtag <- runArticle $ getTagById tid
  if isJust oldtag then do
    tag2 <- runArticle $ getTagByName name
    if isJust tag2 then do
      status status400
      json $ object ["err" .= T.pack "bad request" ]
    else do
      void . runArticle $ updateTag tid name
      resultOK
  else do
    status status404
    json $ object ["err" .= T.pack "not found" ]

createTimelineAPIHandler :: ActionM ()
createTimelineAPIHandler =  hasArticle $ \art -> do
  name <- param "timeline"

  runArticle $ do
    void $ addTimeline name $ artID art

  resultOK

removeTimelineAPIHandler :: ActionM ()
removeTimelineAPIHandler =  hasArticle $ \art -> do
  name <- param "timeline"

  runArticle $ do
    void $ removeTimeline name $ artID art

  resultOK

getAllTimelineAPIHandler :: ActionM ()
getAllTimelineAPIHandler = do
  name <- param "timeline"
  result <- timeline name

  resultArticle result

saveTimelineMetaAPIHandler :: ActionM ()
saveTimelineMetaAPIHandler = do
  name <- param "timeline"
  title <- safeParam "title" ""
  summary <- safeParam "summary" ""
  void . runArticle $ saveTimelineMeta name title summary

  resultOK

removeTimelineMetaAPIHandler :: ActionM ()
removeTimelineMetaAPIHandler = do
  name <- param "timeline"
  void . runArticle $ removeTimelineMeta name

  resultOK

resultArticle :: ListResult Article -> ActionM ()
resultArticle result = do
  isCard <- safeParam "card" ("" :: String)

  if null isCard then
                 json . fromListResult "articles" $ result
  else do
    cards <- runArticle $ mapM toCardItem $ getResult result
    json . fromListResult "cards" $ ListResult { getFrom   = getFrom result
                                               , getSize   = getSize result
                                               , getTotal  = getTotal result
                                               , getResult = cards
                                               }
