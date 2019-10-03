{-# LANGUAGE OverloadedStrings #-}
module Site.Core where

import           Control.Monad                  ( when )
import           Data.List                      ( intersperse
                                                , isSuffixOf
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Time
import           Hakyll                  hiding ( tagCloudField )
import           Site.Config
import           Site.Meta                      ( buildVersion )
import           Site.Tags
import           Site.Types                     ( RenderMode(..) )
import           System.Environment             ( getArgs
                                                , withArgs
                                                )
import           System.FilePath                ( splitExtension )
import           System.Console.Pretty          ( Color(..)
                                                , color
                                                )
runSite :: (RenderMode -> Rules ()) -> IO ()
runSite rules = do
  args <- getArgs
  let draftMode  = length args == 2 && args !! 1 == "draft"
      hakyllConf = if draftMode
        then defaultConfiguration { destinationDirectory = "_draft"
                                  , storeDirectory       = "_draft_cache"
                                  , tmpDirectory         = "_draft_cache/tmp"
                                  }
        else defaultConfiguration
      mode  = if draftMode then Draft else Prod
      args' = take 1 args

  when draftMode
    $ putStrLn (color Yellow "!!!!!!!!! RUNNING IN DRAFT MODE !!!!!!!!!")
  withArgs args' $ hakyllWith hakyllConf (rules mode)

postsPattern :: RenderMode -> Pattern
postsPattern Draft = "content/posts/*.md" .||. "content/drafts/*.md"
postsPattern Prod  = "content/posts/*.md"

postList
  :: Pattern
  -> SiteConfig
  -> Tags
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList postsPattern' config tags sortFilter = do
  posts   <- sortFilter =<< loadAll postsPattern'
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
    <> constField "buildTime"    formattedTime
    <> constField "buildVersion" buildVersion
    <> constField "gaId"         (gaId config)
    <> defaultContext
  where formattedTime = formatTime defaultTimeLocale "%F %T UTC" (builtAt config)

postCtx :: SiteConfig -> Tags -> Context String
postCtx config tags =
  dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> tagLinks getTags "tags" tags
    <> siteCtx config tags

(+||+) :: Routes -> Routes -> Routes
(+||+) = composeRoutes
