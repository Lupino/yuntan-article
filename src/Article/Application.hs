{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.Application
  (
    application
  ) where

import           Article.Config      (Cache)
import           Article.Router
import           Database.PSQL.Types (HasOtherEnv, HasPSQL)
import           Network.Wai         (Middleware)
import           Web.Scotty.Haxl     (ScottyH)
import           Web.Scotty.Trans    (delete, get, middleware, post)

application :: (HasPSQL u, HasOtherEnv Cache u) => [Middleware] -> ScottyH u w ()
application mids = do
  mapM_ middleware mids

  post   "/api/articles/"                    createArticleHandler
  get    "/api/articles/"                    getAllArticleHandler
  post   "/api/articles/:art_id/"            $ requireArticle updateArticleHandler
  post   "/api/articles/:art_id/cover"       $ requireArticle updateArticleCoverHandler
  delete "/api/articles/:art_id/cover"       $ requireArticle removeArticleCoverHandler
  post   "/api/articles/:art_id/extra"       $ requireArticle updateArticleExtraHandler
  get    "/api/articles/:art_id/extra"       $ requireArticle getArticleExtraHandler
  delete "/api/articles/:art_id/extra"       $ requireArticle removeArticleExtraHandler
  post   "/api/articles/:art_id/extra/clear" $ requireArticle clearArticleExtraHandler
  delete "/api/articles/:art_id/"            removeArticleHandler
  get    "/api/articles/:art_id/"            $ requireArticle getArticleHandler
  get    "/api/articles/:art_id/graphql/"    $ requireArticle graphqlByArticleHandler

  post   "/api/articles/:art_id/alias"       $ requireArticle saveAliasHandler
  delete "/api/alias/:alias"                 removeAliasHandler
  get    "/api/alias/:alias"                 checkAliasHandler

  get    "/api/tags/:tag_id/"                $ requireTag getTagHandler
  post   "/api/tags/"                        createTagHandler
  get    "/api/tags/"                        $ requireTag getTagHandler
  post   "/api/tags/:tag_id/"                updateTagHandler

  get    "/api/timeline/:timeline/"          getAllTimelineHandler
  post   "/api/timeline/:timeline/"          $ requireArticle createTimelineHandler
  get    "/api/timeline/:timeline/meta"      getTimelineMetaHandler
  post   "/api/timeline/:timeline/meta"      saveTimelineMetaHandler
  delete "/api/timeline/:timeline/meta"      removeTimelineMetaHandler
  delete "/api/timeline/:timeline/:art_id/"  $ requireArticle removeTimelineHandler
  post   "/api/timeline/:timeline/:art_id/"  $ requireArticle createTimelineHandler

  post   "/api/articles/:art_id/tags/"       $ requireTagAndArticle addArticleTagHandler
  delete "/api/articles/:art_id/tags/"       $ requireTagAndArticle removeArticleTagHandler

  get    "/api/file/:key"                    getFileHandler
  post   "/api/file/:key"                    saveFileHandler
  delete "/api/file/:key"                    removeFileHandler

  post "/api/graphql/"                       graphqlHandler
