{-# LANGUAGE OverloadedStrings #-}
module Site.Core where

import           Data.List                      ( intersperse
                                                , isSuffixOf
                                                )
import           Data.List.Split                ( splitOn )
import           Hakyll                  hiding ( tagCloudField )
import           Site.Config
import           Site.Meta                      ( buildVersion )
import           Site.Tags
import           System.FilePath                ( splitExtension )

postList
  :: Pattern
  -> SiteConfig
  -> Tags
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList postsPattern config tags sortFilter = do
  posts   <- sortFilter =<< loadAll postsPattern
  itemTpl <- loadBody "templates/post-link.html"
  applyTemplateList itemTpl (postCtx config tags) posts

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

siteCtx :: SiteConfig -> Tags -> Context String
siteCtx config tags =
  tagCloudField "cloud" 60 150 tags
    <> constField "buildVersion" buildVersion
    <> constField "gaId"         (gaId config)
    <> defaultContext

postCtx :: SiteConfig -> Tags -> Context String
postCtx config tags =
  dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> tagLinks getTags "tags" tags
    <> siteCtx config tags
