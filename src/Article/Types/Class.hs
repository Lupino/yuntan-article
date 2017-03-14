module Article.Types.Class
  (
    Created(..)
  ) where

import           Article.Types.Internal (CreatedAt)

class Created a where
  createdAt :: a -> CreatedAt
