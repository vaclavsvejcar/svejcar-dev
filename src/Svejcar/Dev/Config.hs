{-|
Module      : Svejcar.Dev.Config
Description : Logic related to site configuration.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing functions and data structures for handling site configuration.
-}
module Svejcar.Dev.Config where

-- | Site configuration.
data SiteConfig = SiteConfig
    { gaId :: String     -- ^ /Google Analytics/ unique ID
    , siteRoot :: String -- ^ Root URL of the site
    }
