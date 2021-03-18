{-# LANGUAGE StrictData #-}

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
  )
where

import           Site.Types                     ( RenderMode(..) )



-- | Site configuration.
data SiteConfig = SiteConfig
  { scGaId     :: Maybe String -- ^ /Google Analytics/ unique ID
  , scMode     :: RenderMode   -- ^ render mode
  , scSiteRoot :: String       -- ^ Root URL of the site
  }

