{-|
Module      : Site.Config
Description : Logic related to site configuration.
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing functions and data structures for handling site configuration.
-}

module Site.Config
  ( SiteConfig(..)
  , def
  )
where

import           Data.Default.Class


-- | Site configuration.
data SiteConfig = SiteConfig
  { scGaId     :: Maybe String -- ^ /Google Analytics/ unique ID
  , scSiteRoot :: String       -- ^ Root URL of the site
  }


instance Default SiteConfig where
  def = SiteConfig { scGaId = Nothing, scSiteRoot = "https://example.com" }
