{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Article.Utils
  (
    firstImage
  , cleanHtml

  , onlyToMaybe

  , getImageShape
  , imageShape
  ) where


import           Text.HTML.TagSoup     as Soup

import           Data.ByteString       (ByteString)

import           Codec.Picture         (DynamicImage (..), Image (..),
                                        decodeImage)
import           Database.MySQL.Simple (Only (..))

firstImage :: String -> String
firstImage str = go $ parseTags str
  where go :: [Soup.Tag String] -> String
        go (x:xs) = if isTagOpenName "img" x then fromAttrib "src" x else go xs
        go [] = ""

cleanHtml :: String -> String
cleanHtml str = innerText $ parseTags str

getImageShape :: ByteString -> Maybe (Int, Int)
getImageShape bs =
  case decodeImage bs of
    Left _   -> Nothing
    Right di -> Just $ imageShape di

imageShape :: DynamicImage -> (Int, Int)
imageShape (ImageY8 i)     = (imageWidth i, imageHeight i)
imageShape (ImageY16 i)    = (imageWidth i, imageHeight i)
imageShape (ImageYF i)     = (imageWidth i, imageHeight i)
imageShape (ImageYA8 i)    = (imageWidth i, imageHeight i)
imageShape (ImageYA16 i)   = (imageWidth i, imageHeight i)
imageShape (ImageRGB8 i)   = (imageWidth i, imageHeight i)
imageShape (ImageRGB16 i)  = (imageWidth i, imageHeight i)
imageShape (ImageRGBF i)   = (imageWidth i, imageHeight i)
imageShape (ImageRGBA8 i)  = (imageWidth i, imageHeight i)
imageShape (ImageRGBA16 i) = (imageWidth i, imageHeight i)
imageShape (ImageYCbCr8 i) = (imageWidth i, imageHeight i)
imageShape (ImageCMYK8 i)  = (imageWidth i, imageHeight i)
imageShape (ImageCMYK16 i) = (imageWidth i, imageHeight i)

onlyToMaybe :: Maybe (Only a) -> Maybe a
onlyToMaybe (Just x) = Just $ fromOnly x
onlyToMaybe Nothing  = Nothing

