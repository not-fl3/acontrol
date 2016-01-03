{-# LANGUAGE OverloadedStrings #-}
module Views.PanelWithError where

import           Text.Blaze.XHtml5 ((!))
import qualified Data.Text as T
import qualified Text.Blaze.Bootstrap as H
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

import           Data.Monoid

panelWithErrorView :: T.Text -> Maybe T.Text -> H.Html -> H.Html
panelWithErrorView title mError ct =
    H.div ! A.class_ "panel panel-info" ! A.style "margin-top: 30px;" $
     do H.div ! A.class_ "panel-heading" $
         H.div ! A.class_ "panel-title" $ H.toHtml title
        H.div ! A.class_ "panel-body" $
         do case mError of
              Just errMsg ->
                  H.alertBox H.BootAlertDanger (H.toHtml errMsg)
              Nothing -> mempty
            H.div ct
