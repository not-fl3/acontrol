{-# LANGUAGE OverloadedStrings #-}
module Views.NotLoginned where

import           Control.Monad
import           Text.Blaze.XHtml5 ((!))

import qualified Data.Text as T
import qualified Text.Blaze.Bootstrap as H
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import           Data.Monoid


import           Types

notLoginned :: H.Html
notLoginned = H.div $ do
  H.p ! A.class_ "hello-title" $ H.toHtml ("Здравствуйте!" :: String) 
  H.p $ H.toHtml ("Это прекрасное средство для контроля времени." :: String)
  H.p $ H.toHtml ("После регистрации может понадобиться программа, следящяя за открытыми окнами" :: String)
  H.p $ H.toHtml ("Взять которую можно здесь." :: String)


