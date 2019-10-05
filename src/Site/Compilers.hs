{-|
Module      : Site.Compilers
Description : Extra compilers for Hakyll
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing extra compilers for Hakyll.
-}
module Site.Compilers
  ( compressJsCompiler
  , sassCompiler
  )
where

import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.ByteString.Lazy.Char8    as CL
import           Language.JavaScript.Parser
import           Language.JavaScript.Process.Minify
import           Hakyll

-- | Minifies JavaScript content.
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap minify <$> getResourceString
 where
  minify :: String -> String
  minify = CL.unpack . toLazyByteString . renderJS . minifyJS . readJs

sassCompiler :: Compiler (Item String)
sassCompiler =
  loadBody (fromFilePath "assets/scss/screen.scss")
    >>= makeItem
    >>= withItemBody (unixFilter "sassc" args)
  where args = ["-s", "-I", "assets/scss/"]
