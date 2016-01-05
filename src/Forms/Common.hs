{-# LANGUAGE OverloadedStrings #-}
module Forms.Common where

import Data.Maybe
import Text.Digestive
import qualified Data.Text as T


-- usernameFormlet :: Monad m => Maybe T.Text -> Form H.Html m T.Text
-- usernameFormlet mTxt =
--     validate (minMaxLen (3, 12)) (text mTxt)

-- passwordFormlet :: Monad m => Maybe T.Text -> Form H.Html m T.Text
-- passwordFormlet mTxt =
--     validate (minMaxLen (6, 40)) (text mTxt)

-- emailFormlet :: Monad m => Maybe T.Text -> Form H.Html m T.Text
-- emailFormlet mTxt =
--     check "Not a valid email address" (isJust . T.find (== '@')) $
--     validate (minMaxLen(4, 50)) (text mTxt)
