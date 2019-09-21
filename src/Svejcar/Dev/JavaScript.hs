{-|
Module      : Svejcar.Dev.JavaScript
Description : Functions related to JavaScript handling.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing functions for handling and manipulating JavaScript data.
-}
module Svejcar.Dev.JavaScript
  ( compressJsCompiler
  )
where

import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Hakyll
import qualified Text.Jasmine                  as J

-- | Minifies JavaScript content using /hjsmin/ library.
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap minify <$> getResourceString
 where
  minify :: String -> String
  minify src = LB.unpack $ J.minify $ LB.fromChunks [E.encodeUtf8 $ T.pack src]
