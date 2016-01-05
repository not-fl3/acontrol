{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Hooks where

import           Data.HVect
import           Web.Spock.Safe hiding (SessionId)

import           Database.Persist hiding (get)
import           Database.Persist.MongoDB hiding (get)
import qualified Database.Persist.MongoDB as M
import qualified Data.Text as T

import           Data.Time

import           Control.Monad
import           Control.Monad.Trans

import           Types
import           Views

import           Utils

import           Db


loadUser :: SessionId -> Action IO (Maybe (UserId, User))
loadUser sessId =
    do mSess <- M.get sessId
       now <- liftIO getCurrentTime
       case mSess of
         Just sess | sessionValidUntil sess > now ->
             do mUser <- M.get (sessionUserId sess)
                return $ fmap (\u -> (sessionUserId sess, u)) mUser
         _ ->
             return Nothing

initHook :: AAction () (HVect '[])
initHook = return HNil

guestOnlyHook :: AAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = readSession >>= \session -> isGuest session =<< getContext
  where
    isGuest (Nothing) context = return $ IsGuest :&: context
    isGuest (Just _) _ = redirect "/"

authHook :: AAction (HVect xs) (HVect ((UserId, User) ': xs))
authHook = do
  sess <- readSession
  case sess of
    Nothing -> mkSiteText $ "Unknown user. Login first!"
    Just (sid, uid) ->
      do oldCtx <- getContext
         mUser <- runDb $ loadUser sid
         case mUser of
           Nothing ->
             mkSiteText $ "Unknown user. Login first!"
           Just val ->
             return (val :&: oldCtx)



