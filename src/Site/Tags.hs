{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Site.Tags
Description : Processing of blog post tags
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Logic for generating and processing blog post tags.
-}

module Site.Tags
  ( tagLinks
  , tagCloud
  , tagCloudField
  )
where

import           Control.Monad                  ( forM )
import           Data.List                      ( sort )
import           Data.Maybe                     ( catMaybes )
import           Hakyll                  hiding ( tagCloudField )
import           Text.Blaze.Html                ( preEscapedToHtml
                                                , toHtml
                                                , toValue
                                                , (!)
                                                )
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


tagLinks :: (Identifier -> Compiler [String]) -> String -> Tags -> Context a
tagLinks extractTags key tags = field key $ \item -> do
  tags' <- extractTags $ itemIdentifier item
  links <- forM (sort tags')
    $ \tag -> renderLink tag <$> getRoute (tagsMakeId tags tag)
  return . renderHtml . H.ul . mconcat . catMaybes $ links
 where
  renderLink tag tagRoute = pathToUrl <$> tagRoute
   where
    pathToUrl path =
      H.li . (H.a ! A.href (toValue . toUrl $ path)) $ toHtml tag

tagCloud :: Double -> Double -> Tags -> Compiler String
tagCloud = renderTagCloudWith makeLink joinLinks
 where
  joinLinks = renderHtml . H.ul . mconcat . fmap (H.li . preEscapedToHtml)
  makeLink minSize maxSize tag url count min' max' =
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ minSize + relative * (maxSize - minSize) :: Int
    in  renderHtml
          . ( H.a
            ! A.style (toValue $ "font-size: " <> show size <> "%")
            ! A.href (toValue url)
            )
          $ toHtml tag

tagCloudField :: String -> Double -> Double -> Tags -> Context String
tagCloudField key minSize maxSize tags =
  field key $ \_ -> tagCloud minSize maxSize tags
