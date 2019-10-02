{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                  ( filterM
                                                , when
                                                , (>=>)
                                                )
import           Hakyll                  hiding ( tagCloudField )
import           System.Environment             ( getArgs
                                                , withArgs
                                                )
import           Site.Compilers
import           Site.Config                    ( SiteConfig(..) )
import           Site.Core
import           System.Console.Pretty          ( Color(..)
                                                , color
                                                )

siteConfig :: SiteConfig
siteConfig =
  SiteConfig { gaId = "UA-148507120-1", siteRoot = "https://svejcar.dev" }

main :: IO ()
main = do
  args <- getArgs
  let draftMode  = length args == 2 && args !! 1 == "draft"
      hakyllConf = if draftMode
        then defaultConfiguration { destinationDirectory = "_draft"
                                  , storeDirectory       = "_draft_cache"
                                  , tmpDirectory         = "_draft_cache/tmp"
                                  }
        else defaultConfiguration
      postsPattern = if draftMode
        then "content/posts/*.md" .||. "content/drafts/*.md"
        else "content/posts/*.md"
      args' = take 1 args

  when draftMode
    $ putStrLn (color Yellow "!!!!!!!!! RUNNING IN DRAFT MODE !!!!!!!!!")
  withArgs args' $ hakyllWith hakyllConf $ do

    tags <- buildTags postsPattern (fromCapture "tags/*/index.html")

    let siteCtx'  = siteCtx siteConfig tags
    let postCtx'  = postCtx siteConfig tags
    let postList' = postList postsPattern siteConfig tags

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
        loadAllSnapshots postsPattern "teaser"
          >>= (fmap (take 100) . recentFirst)
          >>= applyTemplateList body (postCtx' <> bodyField "posts")
          >>= makeItem
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= deIndexUrls

    match postsPattern $ do
      let ctx = constField "page-blog" "" <> siteCtx'
      route
        $               directorizeDate
        `composeRoutes` stripContent
        `composeRoutes` setExtension "html"
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
      route $ stripContent `composeRoutes` setExtension "html"
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

    match "images/*.jpg" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.css" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.scss" $ do
      route $ setExtension "css"
      let compressCssItem = fmap compressCss
      compile (compressCssItem <$> sassCompiler)

    match "js/*.js" $ do
      route idRoute
      compile compressJsCompiler

    match "templates/*" $ compile templateBodyCompiler
