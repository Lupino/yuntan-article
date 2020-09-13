module Article.Types.Internal
  ( ID
  , Title
  , Summary
  , Content
  , CreatedAt
  , FileKey
  , FileBucket
  , FileExtra
  , TagName
  ) where

import           Data.Aeson (Value)
import           Data.Int   (Int64)
import           Data.Text  (Text)

type ID          = Int64
type Title       = String
type Summary     = String
type Content     = Text
type CreatedAt   = Int64

type FileKey     = String
type FileBucket  = String
type FileExtra   = Value

type TagName     = String
