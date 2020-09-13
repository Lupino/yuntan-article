{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Article.Utils
  ( firstImage
  , cleanHtml
  ) where


import           Text.HTML.TagSoup as Soup

firstImage :: String -> String
firstImage str = go $ parseTags str
  where go :: [Soup.Tag String] -> String
        go (x:xs) = if isTagOpenName "img" x then fromAttrib "src" x else go xs
        go [] = ""

cleanHtml :: String -> String
cleanHtml str = innerText $ parseTags str
