{-# LANGUAGE OverloadedStrings #-}
import qualified Svejcar.Dev.Tags              as SDT
import           Control.Monad                  ( filterM )
import           Data.List                      ( intersperse
                                                , isSuffixOf
                                                )
import           Data.List.Split                ( splitOn )
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           System.FilePath                ( splitExtension )

main :: IO ()
main = hakyll $ do

  tags <- buildTags "content/posts/*" (fromCapture "tags/*/index.html")

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      list <- postList
        tags
        (\t -> recentFirst t
          >>= filterM (fmap (elem tag) . getTags . itemIdentifier)
        )
      let ctx = constField "tag" tag <> constField "posts" list <> siteCtx tags
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts-by-tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"      ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "content/index.html" $ do
    route stripContent
    compile $ do
      body <- fmap itemBody templateBodyCompiler
      loadAllSnapshots "content/posts/*" "teaser"
        >>= (fmap (take 100) . recentFirst)
        >>= applyTemplateList body (postCtx tags <> bodyField "posts")
        >>= makeItem
        >>= loadAndApplyTemplate "templates/default.html" (siteCtx tags)
        >>= relativizeUrls
        >>= deIndexUrls

  match "content/posts/*" $ do
    route
      $               directorizeDate
      `composeRoutes` stripContent
      `composeRoutes` setExtension "html"
    compile $ do
      compiled <- pandocCompiler
      full <- loadAndApplyTemplate "templates/post.html" (postCtx tags) compiled
      teaser <- loadAndApplyTemplate "templates/post-teaser.html" (postCtx tags)
        $ dropMore compiled
      saveSnapshot "content" full
      saveSnapshot "teaser"  teaser
      loadAndApplyTemplate "templates/default.html" (siteCtx tags) full
        >>= relativizeUrls
        >>= deIndexUrls

  match "images/*.jpg" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.scss" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateBodyCompiler

postList :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags sortFilter = do
  posts   <- sortFilter =<< loadAll "content/posts/*"
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
