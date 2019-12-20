{-|
Module      : Site.Meta
Description : Provides additional info about build.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing functions for obtaining additional meta info about the build
itself.
-}
module Site.Meta
  ( buildVersion
  )
where

import           Data.Version                   ( showVersion )
import           Paths_svejcar_dev              ( version )

-- | Returns build version (specified in /package.yaml/).
buildVersion :: String
buildVersion = showVersion version
