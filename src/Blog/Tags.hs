{-# LANGUAGE OverloadedStrings #-}
module Blog.Tags where

import           Control.Monad                  ( forM )
import           Data.List                      ( intercalate
                                                , intersperse
                                                , sort
                                                )
import           Data.Maybe                     ( catMaybes )
import           Hakyll
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
  renderLink tag route = pathToUrl <$> route
   where
    pathToUrl path = H.li $ H.a ! A.href (toValue . toUrl $ path) $ toHtml tag

tagCloudField :: String -> Double -> Double -> Tags -> Context String
tagCloudField key minSize maxSize tags =
  field key $ \_ -> tagCloud minSize maxSize tags

tagCloud :: Double -> Double -> Tags -> Compiler String
tagCloud = renderTagCloudWith makeLink joinLinks
 where
  joinLinks = renderHtml . H.ul . mconcat . map (H.li . preEscapedToHtml)
  makeLink minSize maxSize tag url count min' max' =
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ minSize + relative * (maxSize - minSize) :: Int
    in  renderHtml
          $ H.a
          ! A.style (toValue $ "font-size: " ++ show size ++ "%")
          ! A.href (toValue url)
          $ toHtml tag
