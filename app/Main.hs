{-# LANGUAGE OverloadedStrings #-}
import           Data.List                      ( intersperse, isSuffixOf )
import           Data.List.Split                ( splitOn )
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           System.FilePath                ( splitExtension )

main :: IO ()
main = hakyll $ do

  match "content/index.html" $ do
    route stripContent
    compile $ do
      body <- fmap itemBody templateBodyCompiler
      loadAllSnapshots "content/posts/*" "teaser"
        >>= fmap (take 100) . recentFirst
        >>= applyTemplateList body (defaultContext  `mappend` bodyField "posts")
        >>= makeItem
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
        >>= deIndexUrls
  
  match "content/posts/*" $ do
    route $ directorizeDate `composeRoutes` stripContent `composeRoutes` setExtension "html"
    compile $ do
      compiled <- pandocCompiler
      full <- loadAndApplyTemplate "templates/post.html" defaultContext  compiled
      teaser <- loadAndApplyTemplate "templates/post-teaser.html" defaultContext $ dropMore compiled
      saveSnapshot "content" full
      saveSnapshot "teaser" teaser
      loadAndApplyTemplate "templates/default.html" defaultContext full
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
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) ("/." :: String)
      then take (length url - 10) url else url
    
deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)
