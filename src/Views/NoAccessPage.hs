{-# LANGUAGE OverloadedStrings #-}
module Views.NoAccessPage where

import qualified Text.Blaze.Bootstrap as H
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

import qualified Data.Text as T

import Views.PanelWithError

noAccessPage :: T.Text -> H.Html
noAccessPage msg = panelWithErrorView "No Access" Nothing (H.text $ msg)
