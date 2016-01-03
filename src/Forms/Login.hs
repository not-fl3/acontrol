{-# LANGUAGE OverloadedStrings #-}
module Forms.Login where

import           Text.Blaze.Html (Html)
import           Text.Digestive
import           Text.Digestive.Bootstrap
import qualified Data.Text as T

import           Forms.Common
import           Control.Applicative

data LoginRequest
   = LoginRequest
   { lr_user :: T.Text
   , lr_password :: T.Text
   } deriving (Show)

loginForm :: Monad m => Form Html m LoginRequest
loginForm =
    LoginRequest <$> "name" .: usernameFormlet Nothing
                 <*> "password" .: passwordFormlet Nothing

loginFormSpec :: FormMeta
loginFormSpec =
    FormMeta
    { fm_method = POST
    , fm_target = "/login"
    , fm_elements =
        [ FormElement "name" (Just "Username") InputText
        , FormElement "password" (Just "Password") InputPassword
        ]
    , fm_submitText = "Login"
    }
