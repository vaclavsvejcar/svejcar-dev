{-|
Module      : Site.Config
Description : Logic related to site configuration.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing functions and data structures for handling site configuration.
-}
module Site.Config where

import Data.Time (UTCTime(..))

-- | Site configuration.
data SiteConfig =
  SiteConfig
    { builtAt :: UTCTime
    , gaId :: String     -- ^ /Google Analytics/ unique ID
    , siteRoot :: String -- ^ Root URL of the site
    }
    