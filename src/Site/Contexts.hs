{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Site.Contexts
Description : Collection of common Hakyll contexts.
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing functions for constructing Hakyll contexts.
-}

module Site.Contexts
  ( postCtx
  , siteCtx
  )
where

import qualified Data.Text                     as T
import           Hakyll                  hiding ( tagCloudField )
import           Site.Config
import           Site.Slug                      ( slugify )
import           Site.Tags                      ( tagCloudField
                                                , tagLinks
                                                )
import           Site.Types                     ( RenderMode(..) )


-- | Creates a 'Context' for blog posts.
postCtx :: SiteConfig -> Tags -> Context String
postCtx config@SiteConfig {..} tags = mconcat
  [ dateField "date"     "%e %B %Y"
  , dateField "datetime" "%Y-%m-%d"
  , boolField "isProd" (const $ scMode == Prod)
  , tagLinks getTags "tags" tags
  , siteCtx config tags
  ]


-- | Creates a 'Context' for main template.
siteCtx :: SiteConfig -> Tags -> Context String
siteCtx SiteConfig {..} tags = mconcat
  [ tagCloudField "cloud" 60 150 tags
  , maybeField "gaId" scGaId
  , pageIdField "pageId"
  , defaultContext
  ]


maybeField :: String -> Maybe String -> Context a
maybeField key = maybe mempty (constField key)


pageIdField :: String -> Context a
pageIdField = mapContext (T.unpack . slugify . T.pack) . titleField
