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
  ( writerOptionsTOC
  )
where

import           Hakyll                  hiding ( tagCloudField )
import           Text.Pandoc.Options            ( WriterOptions
                                                , writerNumberSections
                                                , writerTableOfContents
                                                , writerTemplate
                                                , writerTOCDepth
                                                )

-- | Pandoc writer options for Table of Content rendering
writerOptionsTOC :: WriterOptions
writerOptionsTOC = defaultHakyllWriterOptions
  { writerNumberSections  = True
  , writerTableOfContents = True
  , writerTOCDepth        = 2
  , writerTemplate        =
    Just
      "\n<div class=\"toc\"><div class=\"header\">Table of Contents</div>\n$toc$\n</div>\n$body$"
  }
