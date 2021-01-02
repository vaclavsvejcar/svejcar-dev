{-|
Module      : Site.Meta
Description : Provides additional info about build.
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

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
