{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Helper
  (
    pageParam
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
import           Article.Config          (Cache)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (lift)
import           Data.Aeson              (Value)
import           Data.Int                (Int64)
import qualified Data.Map.Strict         as M (toList)
import           Data.String.Utils       (split)
import           Data.Text               (Text, pack)
import           Haxl.Core               (GenHaxl)
import           JL.Functions            (scope)
import           JL.Interpreter          (desugar, eval, subst)
import           JL.Parser               (parseText)
import           JL.Serializer           (coreToValue, valueToExpression)
import           JL.Types                (Expression (..))
import           Web.Scotty.Trans        (param)
import           Yuntan.Types.HasMySQL   (HasMySQL, HasOtherEnv)
import           Yuntan.Types.ListResult (From, ListResult (..), Size,
                                          emptyListResult)
import           Yuntan.Types.OrderBy    (desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.Scotty     (errNotFound, ok, safeParam)

pageParam :: ActionH u w (Int64, Int64)
pageParam = do
  page <- safeParam "page" (1 :: Int64)
  from <- safeParam "from" (0 :: Int64)
  size <- safeParam "size" 10
  return (max ((page - 1) * size) from, size)

timelineMeta :: HasMySQL u => String -> ActionH u w (Maybe (Title, Summary))
timelineMeta = lift . getTimelineMeta

timelineTitle :: HasMySQL u => String -> ActionH u w Title
timelineTitle name = maybe name fst <$> timelineMeta name

timeline :: (HasMySQL u, HasOtherEnv Cache u) => String -> ActionH u w (ListResult Article)
timeline name = articles' s t
  where s = flip' (getArticleListByTimeline name) $ desc "art_id"
        t = countTimeline name

articles :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u w (ListResult Article)
articles = articles' s t
  where s = flip' getArticleList $ desc "id"
        t = countArticle

flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f = g
  where g c a b = f a b c


articles' :: HasMySQL u => (From -> Size -> GenHaxl u w [Article]) -> GenHaxl u w Int64 -> ActionH u w (ListResult Article)
articles' s t = do
  (from, size) <- pageParam
  lift $ do
    arts <- s from size
    total <- t

    return emptyListResult { getResult = arts
                           , getFrom   = from
                           , getSize   = size
                           , getTotal  = total
                           }

article :: (HasMySQL u, HasOtherEnv Cache u) => ActionH u w (Maybe Article)
article = do
  aid <- param "art_id"
  lift $ getArticleById aid

requireArticle :: (HasMySQL u, HasOtherEnv Cache u) => (Article -> ActionH u w ()) -> ActionH u w ()
requireArticle action = do
  artId <- param "art_id"

  art <- lift $ getArticleById artId
  maybe (notFound artId) action art

  where notFound artId = errNotFound $ concat [ "Article (", show artId, ") not found" ]

requireTag :: HasMySQL u => (Tag -> ActionH u w ()) -> ActionH u w ()
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

requireTagAndArticle :: (HasMySQL u, HasOtherEnv Cache u) => (Tag -> Article -> ActionH u w ()) -> ActionH u w ()
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
