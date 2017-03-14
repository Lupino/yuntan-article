{-# LANGUAGE OverloadedStrings #-}
module Article.Application
  (
    application
  ) where

import           Web.Scotty.Trans (delete, get, middleware, post, put)

import           Network.Wai      (Middleware)

import           Article.Router
import           Article.UserEnv  (ScottyM)

application :: [Middleware] -> ScottyM ()
application mids = do
  mapM_ middleware mids

  put    "/api/upload/" uploadAPIHandler

  post   "/api/articles/" createArticleAPIHandler
  post   "/api/articles/:art_id/" updateArticleAPIHandler
  post   "/api/articles/:art_id/cover" updateArticleCoverAPIHandler
  delete "/api/articles/:art_id/cover" removeArticleCoverAPIHandler
  post   "/api/articles/:art_id/extra" updateArticleExtraAPIHandler
  delete "/api/articles/:art_id/extra" removeArticleExtraAPIHandler
  post   "/api/articles/:art_id/extra/clear" clearArticleExtraAPIHandler
  delete "/api/articles/:art_id/" removeArticleAPIHandler
  get    "/api/articles/:art_id/" getArticleAPIHandler
  get    "/api/articles/" getAllArticleAPIHandler

  get    "/api/tags/:tag_id/" getTagAPIHandler
  post   "/api/tags/" createTagAPIHandler
  get    "/api/tags/" getTagAPIHandler
  post   "/api/tags/:tag_id/" updateTagAPIHandler

  get    "/api/timeline/:timeline/" getAllTimelineAPIHandler
  post   "/api/timeline/:timeline/" createTimelineAPIHandler
  post   "/api/timeline/:timeline/meta" saveTimelineMetaAPIHandler
  delete "/api/timeline/:timeline/meta" removeTimelineMetaAPIHandler
  delete "/api/timeline/:timeline/:art_id/" removeTimelineAPIHandler

  post   "/api/articles/:art_id/tags/" addArticleTagAPIHandler
  delete "/api/articles/:art_id/tags/" removeArticleAPIHandler


  get    "/api/check/" existsArticleAPIHandler
