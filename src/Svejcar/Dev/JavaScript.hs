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

import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.ByteString.Lazy.Char8    as LB
import           Language.JavaScript.Parser
import           Language.JavaScript.Process.Minify
import           Hakyll

-- | Minifies JavaScript content.
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap minify <$> getResourceString
 where
  minify :: String -> String
  minify = LB.unpack . toLazyByteString . renderJS . minifyJS . readJs
