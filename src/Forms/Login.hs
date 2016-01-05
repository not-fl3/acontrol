{-# LANGUAGE OverloadedStrings #-}
module Forms.Login where

import           Text.Digestive
import qualified Data.Text as T

import           Forms.Common
import           Control.Applicative

data LoginRequest
   = LoginRequest
   { lr_user :: T.Text
   , lr_password :: T.Text
   } deriving (Show)


loginForm :: Monad m => Form T.Text m LoginRequest
loginForm =
    LoginRequest <$> "name" .: text Nothing
                 <*> "password" .: text Nothing
