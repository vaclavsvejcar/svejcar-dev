{-|
Module      : Site.Types
Description : Provides additional info about build.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing common data types for the site.
-}
module Site.Types
  ( RenderMode(..)
  )
where

-- | Represents the mode in which the site will be rendered.
data RenderMode = Draft | Prod
