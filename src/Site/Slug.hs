{-# LANGUAGE OverloadedStrings #-}
module Site.Slug
  ( slugify
  )
where

import           Data.Char                      ( isAlphaNum )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


slugify :: Text -> Text
slugify = T.intercalate (T.singleton '-') . T.words . T.toLower . clean

clean :: Text -> Text
clean = T.map keepAlphaNum . T.replace "'" "" . T.replace "&" "and"

keepAlphaNum :: Char -> Char
keepAlphaNum c | isAlphaNum c = c
               | otherwise    = ' '

