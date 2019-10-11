{-|
Module      : Site.Contexts
Description : Collection of common Hakyll contexts.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing functions for constructing Hakyll contexts.
-}
module Site.Contexts
  ( postCtx
  , siteCtx
  )
where

import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                )
import           Hakyll
import           Site.Config
import           Site.Tags                      ( tagLinks )

-- | Creates a 'Context' for blog posts.
postCtx :: SiteConfig -> Tags -> Context String
postCtx config tags =
  dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> tagLinks getTags "tags" tags
    <> siteCtx config tags

-- | Creates a 'Context' for main template.
siteCtx :: SiteConfig -> Tags -> Context String
siteCtx config tags =
  tagCloudField "cloud" 60 150 tags
    <> constField "buildTime" formattedTime
    <> maybeField "gaId" (gaId config)
    <> missingField
    <> defaultContext
 where
  formattedTime = formatTime defaultTimeLocale "%F %T UTC" (builtAt config)

-- | Creates a 'field' for given key from optional value.
maybeField :: String -> Maybe String -> Context a
maybeField key = maybe missingField (constField key)
