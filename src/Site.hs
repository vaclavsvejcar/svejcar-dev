{-# LANGUAGE OverloadedStrings #-}
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
import           Site.Pandoc                    ( writerOptionsTOC )
import           Site.Sitemap
import           Site.Types                     ( RenderMode(..) )

siteConfig :: RenderMode -> SiteConfig
siteConfig mode = def { scGaId = gaId, scSiteRoot = "https://svejcar.dev/" }
 where
  gaId = case mode of
    Draft -> Nothing
    Prod  -> Just "UA-148507120-1"

main :: IO ()
main = runSite $ \mode -> do
  let postsPattern' = postsPattern mode

  tags <- buildTags postsPattern' (fromCapture "tags/*/index.html")

  let siteConfig' = siteConfig mode
      siteCtx'    = siteCtx siteConfig' tags
      postCtx'    = postCtx siteConfig' tags
      postList'   = postList postsPattern' siteConfig' tags

  tagsRules tags $ \tag _ -> do
    route idRoute
    compile $ do
      list <- postList'
        (recentFirst >=> filterM (fmap (elem tag) . getTags . itemIdentifier))
      let ctx =
            boolField "page-blog" (const True)
              <> boolField "back-button" (const True)
              <> constField "tag"         tag
              <> constField "title"       ("Posts for tag: " ++ tag)
              <> constField "posts"       list
              <> constField "description" ("Posts for tag: " ++ tag)
              <> siteCtx'
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
    let ctx =
          boolField "back-button" (const True)
            <> boolField "page-blog" (const True)
            <> siteCtx'
    route $ directorizeDate +||+ stripContent +||+ setExtension "html"
    compile $ do
      ident <- getUnderlying
      toc   <- getMetadataField ident "withTOC"
      let writerOptions =
            maybe defaultHakyllWriterOptions (const writerOptionsTOC) toc
      compiled <- pandocCompilerWith defaultHakyllReaderOptions writerOptions
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
          boolField "back-button" (const True)
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
    let ctx =
          boolField "back-button" (const True)
            <> boolField "page-archive" (const True)
            <> siteCtx'
    route idRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList' recentFirst)
              <> constField "description" "Blog Archive"
              <> constField "title"       "Blog Archive"
              <> ctx
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
  rulesExtraDependencies [scssDependency] $ match "assets/css/screen.scss" $ do
    route $ setExtension "css"
    compile (fmap compressCss <$> sassCompiler)

  match "assets/images/*.jpg" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/js/*.js" $ do
    route idRoute
    compile compressJsCompiler

  match "templates/*" $ compile templateBodyCompiler

  match "content/robots.txt" $ do
    route stripContent
    compile copyFileCompiler
