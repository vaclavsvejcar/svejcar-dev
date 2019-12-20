{-|
Module      : Site.Pandoc
Description : Pandoc related functions specific for the site.
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing Pandoc related functions and data types for the site.
-}
{-# LANGUAGE OverloadedStrings #-}
module Site.Pandoc
  ( withSyntax
  , withTableOfContents
  )
where

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


-- | Adds writer options for custom syntax highlighting.
withSyntax :: Syntax -> WriterOptions -> WriterOptions
withSyntax syntax options =
  options { writerSyntaxMap = addSyntaxDefinition syntax defaultSyntaxMap }

-- | Adds writer options for Table of Content rendering.
withTableOfContents :: WriterOptions -> WriterOptions
withTableOfContents options = options
  { writerNumberSections  = True
  , writerTableOfContents = True
  , writerTOCDepth        = 2
  , writerTemplate        =
    Just
      "\n<div class=\"toc\"><div class=\"header\">Table of Contents</div>\n$toc$\n</div>\n$body$"
  }
