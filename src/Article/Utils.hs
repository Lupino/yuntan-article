{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Article.Utils
  (
    firstImage
  , cleanHtml

  , onlyToMaybe
  ) where


import           Database.MySQL.Simple (Only (..))
import           Text.HTML.TagSoup     as Soup

firstImage :: String -> String
firstImage str = go $ parseTags str
  where go :: [Soup.Tag String] -> String
        go (x:xs) = if isTagOpenName "img" x then fromAttrib "src" x else go xs
        go [] = ""

cleanHtml :: String -> String
cleanHtml str = innerText $ parseTags str

onlyToMaybe :: Maybe (Only a) -> Maybe a
onlyToMaybe (Just x) = Just $ fromOnly x
onlyToMaybe Nothing  = Nothing
