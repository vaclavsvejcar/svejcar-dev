{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Site.Core
Description : Core functionality for the site.
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing functions required to properly configure Hakyll for this site.
-}

module Site.Core
  ( deIndexURLs
  , directorizeDate
  , dropMore
  , postList
  , postsPattern
  , runSite
  , stripContent
  , stripStatic
  , stripIndex
  , (+||+)
  )
where

import           Control.Lens
import           Control.Monad                  ( when )
import           Data.List                      ( intersperse
                                                , isSuffixOf
                                                )
import           Data.List.Split                ( splitOn )
import           Hakyll                  hiding ( tagCloudField )
import           Site.Config
import           Site.Contexts                  ( postCtx )
import           Site.Types                     ( RenderMode(..) )
import           System.Console.Pretty          ( Color(..)
                                                , color
                                                )
import           System.Environment             ( getArgs
                                                , withArgs
                                                )
import           System.FilePath                ( splitExtension )


-- | Cleans up generated files used for Draft mode.
cleanDrafts :: IO ()
cleanDrafts = do
  remove "_draft"
  remove "_draft_cache"
 where
  remove dir = do
    putStrLn $ "Removing " <> dir <> "..."
    removeDirectory dir


deIndexURLs :: Item String -> Compiler (Item String)
deIndexURLs item = return $ fmap (withUrls stripIndex) item


directorizeDate :: Routes
directorizeDate = customRoute (directorize . toFilePath)
 where
  directorize path = dirs <> "/index" <> ext
   where
    (dirs, ext) =
      splitExtension
        .  concat
        $  intersperse "/" date
        <> ["/"]
        <> intersperse "-" rest
    (date, rest) = splitAt 3 $ splitOn "-" path


postList :: Pattern
         -> SiteConfig
         -> Tags
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList postsPattern' config tags sortFilter = do
  posts   <- sortFilter =<< loadAll postsPattern'
  itemTpl <- loadBody "templates/post-link.html"
  applyTemplateList itemTpl (postCtx config tags) posts


dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)


postsPattern :: RenderMode -> Pattern
postsPattern Draft = "content/posts/*.md" .||. "content/drafts/*.md"
postsPattern Prod  = "content/posts/*.md"


runSite :: (RenderMode -> Rules ()) -> IO ()
runSite rules = do
  args <- getArgs
  let draftMode  = length args == 2 && args !! 1 == "draft"
      action     = args ^? element 0
      hakyllConf = if draftMode
        then defaultConfiguration { destinationDirectory = "_draft"
                                  , storeDirectory       = "_draft_cache"
                                  , tmpDirectory         = "_draft_cache/tmp"
                                  }
        else defaultConfiguration
      mode  = if draftMode then Draft else Prod
      args' = take 1 args

  case action of
    Just "clean" -> cleanDrafts
    _            -> return ()

  when draftMode $ putStrLn (color Yellow "🚧 RUNNING IN DRAFT MODE 🚧")
  withArgs args' $ hakyllWith hakyllConf (rules mode)


stripContent :: Routes
stripContent = stripRoute "content/"


stripRoute :: String -> Routes
stripRoute ptrn = gsubRoute ptrn $ const ""


stripStatic :: Routes
stripStatic = stripRoute "static/"


-- | Strips "index.html" from given URL string.
stripIndex :: String -> String
stripIndex url =
  if "index.html" `isSuffixOf` url then take (length url - 10) url else url


-- | Infix version of 'composeRoutes'.
(+||+) :: Routes -> Routes -> Routes
(+||+) = composeRoutes
