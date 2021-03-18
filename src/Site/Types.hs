{-|
Module      : Site.Types
Description : Provides additional info about build.
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing common data types for the site.
-}

module Site.Types
  ( RenderMode(..)
  )
where


-- | Represents the mode in which the site will be rendered.
data RenderMode = Draft | Prod deriving (Eq, Show)
