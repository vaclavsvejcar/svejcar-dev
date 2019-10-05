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
import           Hakyll.Web.Sass         hiding ( sassCompiler )


-- | Minifies JavaScript content.
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap minify <$> getResourceString
 where
  minify :: String -> String
  minify = CL.unpack . toLazyByteString . renderJS . minifyJS . readJs

-- | Compiles SASS file(s) into plain CSS
sassCompiler :: Maybe [FilePath] -> Compiler (Item String)
sassCompiler includePaths = getResourceBody >>= renderSassWith opts
 where
  opts = sassDefConfig { sassIncludePaths = includePaths
                       , sassOutputStyle  = SassStyleCompressed
                       }
