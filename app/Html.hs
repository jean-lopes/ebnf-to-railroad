{-# LANGUAGE OverloadedStrings #-}
module Html
    ( generateHtml
    ) where
import           Data.List.NonEmpty              (NonEmpty)
import           Data.Text                       (Text)
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5                (toHtml, (!))
import qualified Text.Blaze.Html5                as Html
import           Text.Blaze.Html5.Attributes

generateHtml :: String -> NonEmpty (Text, Text) -> String
generateHtml n xs = renderHtml $ Html.docTypeHtml $ do
    Html.head $ do
        Html.title $ toHtml n
        Html.meta ! charset "UTF-8"
        Html.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        Html.style $ toHtml baseStyle
        Html.link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/railroad-diagrams@1.0.0/railroad-diagrams.css"
        Html.script ! src "https://cdn.jsdelivr.net/npm/railroad-diagrams@1.0.0/railroad-diagrams.min.js" $ ""
    Html.body $ do
        mapM_ diagrams xs

diagrams :: (Text, Text) -> Html.Html
diagrams (n, js) = do
    Html.h1 $ toHtml n
    Html.div $ do
        Html.script $ toHtml js

baseStyle :: String
baseStyle = "body{background-color:#f4f2ef}h1{font-family:sans-serif;font-size:1em}"
