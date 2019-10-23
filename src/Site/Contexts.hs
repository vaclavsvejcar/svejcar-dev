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

import           Hakyll                  hiding ( tagCloudField )
import           Site.Config
import           Site.Tags                      ( tagCloudField
                                                , tagLinks
                                                )

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
    <> maybeField "gaId" (scGaId config)
    <> defaultContext

-- | Creates a 'field' for given key from optional value.
maybeField :: String -> Maybe String -> Context a
maybeField key = maybe mempty (constField key)
