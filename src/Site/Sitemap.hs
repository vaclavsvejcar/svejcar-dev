{-|
Module      : Site.Sitemap
Description : sitemap.xml generator for Hakyll
Copyright   : (c) 2019 Vaclav Svejcar

Stability   : experimental
Portability : portable

Module providing functions and data structures for generating sitemap.xml
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Site.Sitemap
  ( ChangeFrequency(..)
  , SitemapConfiguration(..)
  , def
  , sitemapCompiler
  )
where
import           Data.Char                      ( toLower )
import           Data.Default.Class
import           Data.Maybe                     ( catMaybes )
import           Data.Time
import           Hakyll
import           System.Directory               ( getModificationTime )
import           System.FilePath                ( takeExtensions )
import           System.IO.Error
import           Text.XML.Light


data SitemapConfiguration = SitemapConfiguration
  { sitemapExtensions :: [String]
  , sitemapChangeFreq :: FilePath -> ChangeFrequency
  , sitemapPriority   :: FilePath -> Double
  , sitemapBase       :: String
  , sitemapRewriter   :: FilePath -> FilePath
  }

data ChangeFrequency = Always
                     | Hourly
                     | Daily
                     | Weekly
                     | Monthly
                     | Yearly
                     | Never
                     deriving Show

instance Default SitemapConfiguration where
  def = SitemapConfiguration { sitemapExtensions = [".html"]
                             , sitemapChangeFreq = const Weekly
                             , sitemapPriority   = const 0.5
                             , sitemapBase       = "http://example.com/"
                             , sitemapRewriter   = ('/' :)
                             }

type SitemapRecord = (FilePath, String)

showFreq :: ChangeFrequency -> String
showFreq = map toLower . show

sitemapCompiler :: SitemapConfiguration -> Compiler (Item String)
sitemapCompiler config = do
  ids  <- getMatches "**"
  urls <- filter extFilter . catMaybes <$> mapM routeWithMod ids
  let urlset = xmlUrlSet config urls
  makeItem $ ppcTopElement prettyConfigPP urlset
 where
  exts = sitemapExtensions config
  extFilter (p, _) = takeExtensions p `elem` exts
  routeWithMod i = do
    mtime <- itemModTime i
    rt    <- getRoute i
    return $ fmap (, mtime) rt

itemModTime :: Identifier -> Compiler String
itemModTime i = do
  let path = toFilePath i
  mTimeUtc <- unsafeCompiler $ modTimeOrCurrent path
  return $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" mTimeUtc
 where
  modTimeOrCurrent path =
    catchIOError (getModificationTime path) (const getCurrentTime)

elementString :: String -> String -> Element
elementString name content = Element
  { elName    = unqual name
  , elAttribs = []
  , elContent = [Text (CData CDataText content Nothing)]
  , elLine    = Nothing
  }

element :: String -> [Element] -> Element
element name content = Element { elName    = unqual name
                               , elAttribs = []
                               , elContent = map Elem content
                               , elLine    = Nothing
                               }

xmlUrlSet :: SitemapConfiguration -> [SitemapRecord] -> Element
xmlUrlSet config = add_attr xmlns . element "urlset" . map (xmlUrl config)
 where
  xmlns = Attr (unqual "xmlns") "http://www.sitemaps.org/schemas/sitemap/0.9"

xmlUrl :: SitemapConfiguration -> SitemapRecord -> Element
xmlUrl conf r = element "url" [ f conf r | f <- sub ]
  where sub = [xmlLoc, xmlLastMod, xmlChangeFreq, xmlPriority]

xmlLoc, xmlLastMod, xmlChangeFreq, xmlPriority
  :: SitemapConfiguration -> SitemapRecord -> Element
xmlLastMod _ (_, m) = elementString "lastmod" m
xmlLoc config (r, _) = elementString "loc" loc
 where
  loc = sitemapBase config ++ drop 1 rew
  rew = sitemapRewriter config r
xmlChangeFreq config (r, _) = elementString "changefreq" freq
  where freq = showFreq $ sitemapChangeFreq config r
xmlPriority config (r, _) = elementString "priority" p
  where p = show $ sitemapPriority config r
