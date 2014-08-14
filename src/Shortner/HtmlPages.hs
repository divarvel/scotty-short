{-# LANGUAGE OverloadedStrings #-}

module Shortner.HtmlPages where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


templatePage title content = docTypeHtml $ do
    H.head $ do
        H.title title
    H.body $ do
        content



codeNotFoundPage = renderHtml . templatePage "Link not found" $ do
    H.h1 "Link not found"
    H.p "please check the URL"
