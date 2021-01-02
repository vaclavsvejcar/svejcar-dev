{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Main logic for /Hakyll/
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module containing blog configuration for /Hakyll/.
-}

module Main where

import           Control.Monad                  ( filterM
                                                , (>=>)
                                                )
import           Hakyll                  hiding ( tagCloudField )
import           Site.Compilers
import           Site.Config                    ( SiteConfig(..) )
import           Site.Contexts                  ( postCtx
                                                , siteCtx
                                                )
import           Site.Core
import           Site.Pandoc                    ( withSyntax
                                                , withTableOfContents
                                                )
import           Site.Sitemap
import           Site.Types                     ( RenderMode(..) )
import           Skylighting.Parser             ( parseSyntaxDefinition )


siteConfig :: RenderMode -> SiteConfig
siteConfig mode = def { scGaId = gaId, scSiteRoot = "https://svejcar.dev/" }
 where
  gaId = case mode of
    Draft -> Nothing
    Prod  -> Just "UA-148507120-1"


main :: IO ()
main = do
  scalaSyntax <- parseSyntaxDefinition "syntax/scala.xml"
  runSite $ \mode -> do
    let postsPattern' = postsPattern mode

    -- syntax highlighting
    let scalaSyntax'  = either (error "Missing syntax/scala.xml") id scalaSyntax
        writerOptions = withSyntax scalaSyntax' defaultHakyllWriterOptions

    tags <- buildTags postsPattern' (fromCapture "tags/*/index.html")

    -- site configuration
    let siteConfig' = siteConfig mode
        siteCtx'    = siteCtx siteConfig' tags
        postCtx'    = postCtx siteConfig' tags
        postList'   = postList postsPattern' siteConfig' tags

    tagsRules tags $ \tag _ -> do
      route idRoute
      compile $ do
        list <- postList'
          (recentFirst >=> filterM (fmap (elem tag) . getTags . itemIdentifier))
        let ctx = mconcat
              [ boolField "page-blog"   (const True)
              , boolField "home-button" (const True)
              , constField "tag"         tag
              , constField "title"       ("Posts for tag: " <> tag)
              , constField "posts"       list
              , constField "description" ("Posts for tag: " <> tag)
              , siteCtx'
              ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html"    ctx
          >>= relativizeUrls
          >>= deIndexURLs

    match "content/index.html" $ do
      let ctx = boolField "page-blog" (const True) <> siteCtx'
      route stripContent
      compile $ do
        body <- fmap itemBody templateBodyCompiler
        loadAllSnapshots postsPattern' "teaser"
          >>= (fmap (take 100) . recentFirst)
          >>= applyTemplateList body (postCtx' <> bodyField "posts")
          >>= makeItem
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= deIndexURLs

    match postsPattern' $ do
      let ctx = mconcat
            [ boolField "home-button" (const True)
            , boolField "page-blog"   (const True)
            , siteCtx'
            ]
      route $ directorizeDate +||+ stripContent +||+ setExtension "html"
      compile $ do
        ident <- getUnderlying
        toc   <- getMetadataField ident "tableOfContents"
        let writerOptions' = maybe
              writerOptions
              (const $ withTableOfContents writerOptions)
              toc
        compiled <- pandocCompilerWith defaultHakyllReaderOptions writerOptions'
        full     <- loadAndApplyTemplate "templates/post.html" postCtx' compiled
        teaser   <- loadAndApplyTemplate "templates/post-teaser.html" postCtx'
          $ dropMore compiled
        _ <- saveSnapshot "content" full
        _ <- saveSnapshot "teaser" teaser
        loadAndApplyTemplate "templates/default.html" ctx full
          >>= relativizeUrls
          >>= deIndexURLs

    match "content/about/index.md" $ do
      let ctx =
            boolField "home-button" (const True)
              <> boolField "page-about" (const True)
              <> siteCtx'
      route $ stripContent +||+ setExtension "html"
      compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/static.html"  ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexURLs

    create ["archive/index.html"] $ do
      route idRoute
      compile $ do
        let archiveCtx = mconcat
              [ field "posts" (\_ -> postList' recentFirst)
              , constField "description" "Blog Archive"
              , constField "title"       "Blog Archive"
              , boolField "home-button"  (const True)
              , boolField "page-archive" (const True)
              , siteCtx'
              ]
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts-list.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html"    archiveCtx
          >>= relativizeUrls
          >>= deIndexURLs

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ sitemapCompiler def { sitemapBase     = scSiteRoot siteConfig'
                                    , sitemapRewriter = ('/' :) . stripIndex
                                    }

    scssDependency <- makePatternDependency "assets/css/**.scss"
    rulesExtraDependencies [scssDependency]
      . match "assets/css/screen.scss"
      $ do
          route $ setExtension "css"
          compile (fmap compressCss <$> sassCompiler)

    match "assets/images/**" $ do
      route idRoute
      compile copyFileCompiler

    match "assets/js/*.js" $ do
      route idRoute
      compile compressJsCompiler

    match "static/*" $ do
      route stripStatic
      compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "content/robots.txt" $ do
      route stripContent
      compile copyFileCompiler

