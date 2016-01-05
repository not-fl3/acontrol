{-# LANGUAGE OverloadedStrings #-}
module Forms.Register where

import Forms.Common

import Text.Digestive
import qualified Data.Text as T

import Control.Applicative

data RegisterRequest
   = RegisterRequest
   { rr_username :: T.Text
   , rr_password :: T.Text
   , rr_passwordConfirm :: T.Text
   , rr_email :: T.Text
   } deriving (Show)

registerForm :: Monad m => Form T.Text m RegisterRequest
registerForm =
    -- RegisterRequest <$> "name" .: usernameFormlet Nothing
    --                 <*> "password1" .: passwordFormlet Nothing
    --                 <*> "password2" .: passwordFormlet Nothing
    --                 <*> "email" .: emailFormlet Nothing
    RegisterRequest <$> "name" .: text Nothing
                    <*> "password1" .: text Nothing
                    <*> "password2" .: text Nothing
                    <*> "email" .: text Nothing

-- registerFormSpec :: FormMeta
-- registerFormSpec =
--     FormMeta
--     { fm_method = POST
--     , fm_target = "/register"
--     , fm_elements =
--         [ FormElement "name" (Just "Username") InputText
--         , FormElement "email" (Just "Email") InputText
--         , FormElement "password1" (Just "Password") InputPassword
--         , FormElement "password2" (Just "Repeat Password") InputPassword
--         ]
--     , fm_submitText = "Register"
--     }
