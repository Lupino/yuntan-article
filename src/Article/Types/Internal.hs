module Article.Types.Internal
  (
    ID
  , Title
  , Summary
  , FromURL
  , FromURLHASH
  , Content
  , CreatedAt
  , FileKey
  , FileBucket
  , FileExtra
  , TagName
  , TablePrefix
  ) where

import           Data.Aeson (Value)
import           Data.Int   (Int64)
import           Data.Text  (Text)

type ID          = Int64
type Title       = String
type Summary     = String
type FromURL     = String
type FromURLHASH = String
type Content     = Text
type CreatedAt   = Int64

type FileKey     = String
type FileBucket  = String
type FileExtra   = Value

type TagName     = String
type TablePrefix = String
