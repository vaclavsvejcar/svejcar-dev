{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                  ( filterM
                                                , when
                                                )
import           Data.List                      ( intersperse
                                                , isSuffixOf
                                                )
import           Data.List.Split                ( splitOn )
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           System.Environment             ( getArgs
                                                , withArgs
                                                )
import           Svejcar.Dev.JavaScript
import qualified Svejcar.Dev.Tags              as SDT
import           System.FilePath                ( splitExtension )

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

  when draftMode $ putStrLn "!!!!!!!!! RUNNING IN DRAFT MODE !!!!!!!!!"
  withArgs args' $ hakyllWith hakyllConf $ do

    tags <- buildTags postsPattern (fromCapture "tags/*/index.html")

    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        list <- postList
          postsPattern
          tags
          (\t -> recentFirst t
            >>= filterM (fmap (elem tag) . getTags . itemIdentifier)
          )
        let ctx =
              constField "tag" tag
                <> constField "title" ("Posts for tag: " ++ tag)
                <> constField "posts" list
                <> siteCtx tags
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html"     ctx
          >>= relativizeUrls
          >>= deIndexUrls

    match "content/index.html" $ do
      route stripContent
      compile $ do
        body <- fmap itemBody templateBodyCompiler
        loadAllSnapshots postsPattern "teaser"
          >>= (fmap (take 100) . recentFirst)
          >>= applyTemplateList body (postCtx tags <> bodyField "posts")
          >>= makeItem
          >>= loadAndApplyTemplate "templates/default.html" (siteCtx tags)
          >>= relativizeUrls
          >>= deIndexUrls

    match postsPattern $ do
      route
        $               directorizeDate
        `composeRoutes` stripContent
        `composeRoutes` setExtension "html"
      compile $ do
        compiled <- pandocCompiler
        full     <- loadAndApplyTemplate "templates/post.html"
                                         (postCtx tags)
                                         compiled
        teaser <-
          loadAndApplyTemplate "templates/post-teaser.html" (postCtx tags)
            $ dropMore compiled
        saveSnapshot "content" full
        saveSnapshot "teaser"  teaser
        loadAndApplyTemplate "templates/default.html" (siteCtx tags) full
          >>= relativizeUrls
          >>= deIndexUrls

    match "content/about/index.md" $ do
      route $ stripContent `composeRoutes` setExtension "html"
      compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/static.html"  (siteCtx tags)
        >>= loadAndApplyTemplate "templates/default.html" (siteCtx tags)
        >>= relativizeUrls
        >>= deIndexUrls

    create ["archive/index.html"] $ do
      route idRoute
      compile $ do
        let archiveCtx =
              field "posts" (\_ -> postList postsPattern tags recentFirst)
                <> constField "title" "Blog Archive"
                <> (siteCtx tags)
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts-list.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html"     archiveCtx
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

postList
  :: Pattern
  -> Tags
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList postsPattern tags sortFilter = do
  posts   <- sortFilter =<< loadAll postsPattern
  itemTpl <- loadBody "templates/post-link.html"
  list    <- applyTemplateList itemTpl (postCtx tags) posts
  return list

stripContent :: Routes
stripContent = gsubRoute "content/" $ const ""

directorizeDate :: Routes
directorizeDate = customRoute (directorize . toFilePath)
 where
  directorize path = dirs ++ "/index" ++ ext
   where
    (dirs, ext) =
      splitExtension
        $  concat
        $  intersperse "/" date
        ++ ["/"]
        ++ intersperse "-" rest
    (date, rest) = splitAt 3 $ splitOn "-" path

stripIndex :: String -> String
stripIndex url =
  if "index.html" `isSuffixOf` url && elem (head url) ("/." :: String)
    then take (length url - 10) url
    else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)

siteCtx :: Tags -> Context String
siteCtx tags = SDT.tagCloudField "cloud" 60 150 tags <> defaultContext

postCtx :: Tags -> Context String
postCtx tags =
  dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> (SDT.tagLinks getTags) "tags" tags
    <> siteCtx tags
