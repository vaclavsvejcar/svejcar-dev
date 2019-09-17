module Blog.Tags where

import           Control.Monad                  ( forM )
import           Data.List                      ( intercalate
                                                , intersperse
                                                )
import           Data.Maybe                     ( catMaybes )
import           Hakyll
import           Text.Blaze.Html                ( toHtml
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
  links <- forM tags'
    $ \tag -> renderLink tag <$> getRoute (tagsMakeId tags tag)
  return . renderHtml . H.ul . mconcat . catMaybes $ links
 where
  renderLink tag route = pathToUrl <$> route
   where
    pathToUrl path = H.li $ H.a ! A.href (toValue . toUrl $ path) $ toHtml tag
