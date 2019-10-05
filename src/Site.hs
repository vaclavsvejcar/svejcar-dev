{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                  ( filterM
                                                , (>=>)
                                                )
import           Data.Time                      ( UTCTime(..)
                                                , getCurrentTime
                                                )
import           Hakyll                  hiding ( tagCloudField )
import           Site.Compilers
import           Site.Config                    ( SiteConfig(..) )
import           Site.Core

siteConfig :: UTCTime -> SiteConfig
siteConfig builtAt' = SiteConfig { builtAt  = builtAt'
                                 , gaId     = "UA-148507120-1"
                                 , siteRoot = "https://svejcar.dev"
                                 }

main :: IO ()
main = do
  builtAt' <- getCurrentTime
  runSite $ \siteMode -> do
    let postsPattern' = postsPattern siteMode

    tags <- buildTags postsPattern' (fromCapture "tags/*/index.html")

    let siteConfig' = siteConfig builtAt'
        siteCtx'    = siteCtx siteConfig' tags
        postCtx'    = postCtx siteConfig' tags
        postList'   = postList postsPattern' siteConfig' tags

    tagsRules tags $ \tag _ -> do
      route idRoute
      compile $ do
        list <- postList'
          (recentFirst >=> filterM (fmap (elem tag) . getTags . itemIdentifier))
        let ctx =
              constField "tag" tag
                <> constField "title"     ("Posts for tag: " ++ tag)
                <> constField "posts"     list
                <> constField "page-blog" ""
                <> siteCtx'
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html"    ctx
          >>= relativizeUrls
          >>= deIndexUrls

    match "content/index.html" $ do
      let ctx = constField "page-blog" "" <> siteCtx'
      route stripContent
      compile $ do
        body <- fmap itemBody templateBodyCompiler
        loadAllSnapshots postsPattern' "teaser"
          >>= (fmap (take 100) . recentFirst)
          >>= applyTemplateList body (postCtx' <> bodyField "posts")
          >>= makeItem
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= deIndexUrls

    match postsPattern' $ do
      let ctx = constField "page-blog" "" <> siteCtx'
      route $ directorizeDate +||+ stripContent +||+ setExtension "html"
      compile $ do
        compiled <- pandocCompiler
        full     <- loadAndApplyTemplate "templates/post.html" postCtx' compiled
        teaser   <- loadAndApplyTemplate "templates/post-teaser.html" postCtx'
          $ dropMore compiled
        _ <- saveSnapshot "content" full
        _ <- saveSnapshot "teaser" teaser
        loadAndApplyTemplate "templates/default.html" ctx full
          >>= relativizeUrls
          >>= deIndexUrls

    match "content/about/index.md" $ do
      let ctx = constField "page-about" "" <> siteCtx'
      route $ stripContent +||+ setExtension "html"
      compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/static.html"  ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

    create ["archive/index.html"] $ do
      let ctx = constField "page-archive" "" <> siteCtx'
      route idRoute
      compile $ do
        let archiveCtx =
              field "posts" (\_ -> postList' recentFirst)
                <> constField "title" "Blog Archive"
                <> ctx
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts-list.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html"    archiveCtx
          >>= relativizeUrls
          >>= deIndexUrls

    match "assets/images/*.jpg" $ do
      route idRoute
      compile copyFileCompiler

    match "assets/scss/**.scss" $ compile getResourceBody

    scssDependencies <- makePatternDependency "assets/scss/**.scss"
    rulesExtraDependencies [scssDependencies]
      $ create ["assets/css/screen.css"]
      $ do
          route idRoute
          compile (fmap compressCss <$> sassCompiler)

    match "assets/js/*.js" $ do
      route idRoute
      compile compressJsCompiler

    match "templates/*" $ compile templateBodyCompiler
