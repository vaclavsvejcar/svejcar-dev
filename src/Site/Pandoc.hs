{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Site.Pandoc
Description : Pandoc related functions specific for the site.
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing Pandoc related functions and data types for the site.
-}

module Site.Pandoc
  ( withSyntax
  , withTableOfContents
  )
where

import           Data.Functor.Identity          ( runIdentity )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Skylighting.Parser             ( addSyntaxDefinition )
import           Skylighting.Syntax             ( defaultSyntaxMap )
import           Skylighting.Types              ( Syntax )
import           Text.Pandoc.Options            ( WriterOptions
                                                , writerNumberSections
                                                , writerSyntaxMap
                                                , writerTOCDepth
                                                , writerTableOfContents
                                                , writerTemplate
                                                )
import           Text.Pandoc.Templates          ( Template
                                                , compileTemplate
                                                )


-- | Adds writer options for custom syntax highlighting.
withSyntax :: Syntax -> WriterOptions -> WriterOptions
withSyntax syntax options =
  options { writerSyntaxMap = addSyntaxDefinition syntax defaultSyntaxMap }


-- | Adds writer options for Table of Content rendering.
withTableOfContents :: WriterOptions -> WriterOptions
withTableOfContents options = options { writerNumberSections  = True
                                      , writerTableOfContents = True
                                      , writerTOCDepth        = 2
                                      , writerTemplate        = Just tocTemplate
                                      }


tocTemplate :: Template Text
tocTemplate = case runIdentity $ compileTemplate "" tmpl of
  Left  err      -> error err
  Right template -> template
 where
  tmpl = T.intercalate
    "\n"
    [ ""
    , "<div class=\"toc\"><div class=\"header\">Table of Contents</div>"
    , "$toc$"
    , "</div>"
    , "$body$"
    ]
