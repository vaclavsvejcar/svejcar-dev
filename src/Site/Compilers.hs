{-|
Module      : Site.Compilers
Description : Extra compilers for Hakyll
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing extra compilers for Hakyll.
-}

module Site.Compilers
  ( compressJsCompiler
  , sassCompiler
  )
where

import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.ByteString.Lazy.Char8    as CL
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           Language.JavaScript.Parser
import           Language.JavaScript.Process.Minify


-- | Minifies JavaScript content.
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap minify <$> getResourceString
  where minify = CL.unpack . toLazyByteString . renderJS . minifyJS . readJs
