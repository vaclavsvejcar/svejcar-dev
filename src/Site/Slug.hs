{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Site.Slug
Description : URL slug generator
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Logic for generating /slugs/ for given URL.
-}

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

