{-# LANGUAGE OverloadedStrings #-}
module Views.Site(site) where

import           Text.Blaze.XHtml5 ((!))

import qualified Data.Text as T
import qualified Text.Blaze.Bootstrap as H
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import           Data.Monoid

import           Types

site :: Site -> H.Html -> H.Html
site (Site u) html = H.html $
  do H.head $
       do H.title "whut?"
          H.meta ! A.charset "utf-8"
          H.meta ! A.httpEquiv "refresh" ! A.content "60"
          H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
          H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
          H.link ! A.href "/css/style.css" ! A.rel "stylesheet"
     H.body $
       do H.div ! A.class_ "masthead" $
            H.div ! A.class_ "container" $
              H.nav ! A.class_ "nav" $
              do H.a ! A.class_ "nav-item" ! A.href "/" $ "Home"
                 case u of
                   Nothing ->
                     do H.a ! A.class_ "nav-item" ! A.href "/login" $ "Login"
                        H.a ! A.class_ "nav-item" ! A.href "/register" $ "Register"
                   (Just _) ->
                     do  H.a ! A.class_ "nav-item" ! A.href "/addtask" $ "New task"
                         H.a ! A.class_ "nav-item" ! A.href "/logout" $ "Logout"
          H.div ! A.class_ "container" $ H.html $ html
          H.div ! A.class_ "footer" $
            H.p $ H.a ! A.href "https://github.com/not-fl3/acontrol" $ "repo"
     H.script ! A.href "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js" $ mempty
     H.script ! A.href "/js/bootstrap.min.js" $ mempty

