{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Helper
  ( pageParam
  , timeline
  , timelineTitle
  , timelineMeta
  , articles
  , article
  , requireArticle
  , requireTag
  , requireTagAndArticle
  , resultOK
  , extraKeys
  , preprocessArticle
  , preprocessArticleList
  , checkSed
  ) where

import           Article
import           Article.Config         (Cache)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (Value)
import           Data.Aeson.Result      (List (..), emptyList)
import           Data.Int               (Int64)
import qualified Data.Map.Strict        as M (toList)
import           Data.String.Utils      (split)
import           Data.Text              (Text, pack)
import           Database.PSQL.Types    (From (..), HasOtherEnv, HasPSQL,
                                         Size (..), desc)
import           Haxl.Core              (GenHaxl)
import           JL.Functions           (scope)
import           JL.Interpreter         (desugar, eval, subst)
import           JL.Parser              (parseText)
import           JL.Serializer          (coreToValue, valueToExpression)
import           JL.Types               (Expression (..))
import           Text.Read              (readMaybe)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (param)
import           Web.Scotty.Utils       (errNotFound, ok, safeParam)

pageParam :: ActionH u w (From, Size)
pageParam = do
  page <- safeParam "page" (1 :: Int64)
  from <- safeParam "from" (0 :: Int64)
  size <- safeParam "size" 10
  return (From $ max ((page - 1) * size) from, Size size)

timelineMeta :: HasPSQL u => String -> ActionH u w (Maybe (Title, Summary))
timelineMeta = lift . getTimelineMeta

timelineTitle :: HasPSQL u => String -> ActionH u w Title
timelineTitle name = maybe name fst <$> timelineMeta name

timeline :: (HasPSQL u, HasOtherEnv Cache u) => String -> ActionH u w (List Article)
timeline name = articles' s t
  where s = flip' (getArticleListByTimeline name) $ desc "art_id"
        t = countTimeline name

articles :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (List Article)
articles = articles' s t
  where s = flip' getArticleList $ desc "id"
        t = countArticle

flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f = g
  where g c a b = f a b c


articles' :: HasPSQL u => (From -> Size -> GenHaxl u w [Article]) -> GenHaxl u w Int64 -> ActionH u w (List Article)
articles' s t = do
  (from, size) <- pageParam
  lift $ do
    arts <- s from size
    total <- t

    return emptyList
      { getResult = arts
      , getFrom   = unFrom from
      , getSize   = unSize size
      , getTotal  = total
      }

article :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (Maybe Article)
article = do
  aid <- param "art_id"
  lift $ getArticleById aid

requireArticle :: (HasPSQL u, HasOtherEnv Cache u) => (Article -> ActionH u w ()) -> ActionH u w ()
requireArticle action = do
  taid <- param "art_id"

  case readMaybe taid of
    Just aid -> do
      art <- lift $ getArticleById aid
      maybe (notFound taid) action art
    Nothing -> do
      maid <- lift $ getAlias (pack taid)
      case maid of
        Nothing -> notFound taid
        Just aid -> do
          art <- lift $ getArticleById aid
          maybe (notFound taid) action art

  where notFound artId = errNotFound $ concat [ "Article (", artId, ") not found" ]

requireTag :: HasPSQL u => (Tag -> ActionH u w ()) -> ActionH u w ()
requireTag action = do
  tid <- safeParam "tag_id" (0::ID)
  name <- safeParam "tag" (""::String)

  tag <- lift $
    if tid > 0 then getTagById tid
               else getTagByName name

  maybe notFound action tag
  where notFound = errNotFound "Not Found."

merge :: Monad m => ((a -> m ()) -> m ()) -> ((b -> m ()) -> m ()) -> (a -> b -> m ()) -> m ()
merge f g t = f $ \a -> g $ \b -> t a b

requireTagAndArticle :: (HasPSQL u, HasOtherEnv Cache u) => (Tag -> Article -> ActionH u w ()) -> ActionH u w ()
requireTagAndArticle = merge requireTag requireArticle

resultOK :: ActionH u w ()
resultOK = ok "result" ("OK" :: Text)

extraKeys :: ActionH u w [Text]
extraKeys = do
  ks <- safeParam "extra_keys" (""::String)

  if null ks then return []
             else return . map pack $ split "," ks

contentKeys :: ActionH u w [Text]
contentKeys = do
  ks <- safeParam "content_keys" (""::String)

  if null ks then return []
             else return . map pack $ split "," ks

isJsonContent :: ActionH u w Bool
isJsonContent = not . null <$> safeParam "content_json" ("" :: String)

preprocessArticle :: Article -> ActionH u w Article
preprocessArticle = checkExtraKeys $ checkJsonContent $ checkContentKeys return
  where checkExtraKeys :: (Article -> ActionH u w Article) -> Article -> ActionH u w Article
        checkExtraKeys next a = next =<< (flip pickExtra a <$> extraKeys)

        checkJsonContent :: (Article -> ActionH u w Article) -> Article -> ActionH u w Article
        checkJsonContent next a = do
          isJson <- isJsonContent
          if isJson then next (setJsonContent a) else next a

        checkContentKeys :: (Article -> ActionH u w Article) -> Article -> ActionH u w Article
        checkContentKeys next a = do
          keys <- contentKeys
          if null keys then next a else next (pickContent keys a)

preprocessArticleList :: [Article] -> ActionH u w [Article]
preprocessArticleList = mapM preprocessArticle

checkSed :: Value -> ActionH u w Value
checkSed j = do
  inp <- safeParam "jl" (""::String)
  if null inp then return j
  else
    case parseText "" (pack inp) of
      Left err -> do
        liftIO $ print err
        return j
      Right expr0 -> do
        let expr = ApplicationExpression expr0 (valueToExpression j)
        let core = eval (foldl (\e (v, f) -> subst v f e) (desugar expr) (M.toList scope))
        return $ coreToValue core
