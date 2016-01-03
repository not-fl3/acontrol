{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Db(
  runDb,
  initDbConnection,
  loginUser,
  registerUser,
  CommonResponse(..),
  IsGuest(..),
  ConnectionPool,
  module Model.Types,
  module Model.TaskState) where

import           Control.Applicative
import           Database.Persist hiding (get)
import qualified Database.Persist.MongoDB as M
import           Database.Persist.MongoDB hiding (get)
import           Network (PortID(PortNumber))
import           Web.Spock.Safe hiding (SessionId)

import           System.Random

import           Utils
import qualified Data.Text as T

import           Control.Monad
import           Control.Monad.Trans

import           Model.TaskState
import           Model.Types

data IsGuest = IsGuest

data CommonResponse
   = CommonError T.Text
   | CommonSuccess T.Text
   deriving (Show, Eq)

initDbConnection :: ConnBuilder ConnectionPool
initDbConnection = ConnBuilder (createMongoDBPool "whut" "127.0.0.1" (PortNumber 27017) Nothing 5 5 10000) (const $ pure ()) (PoolCfg 5 5 10000)

runDb :: (HasSpock m, SpockConn m ~ ConnectionPool) => Action IO a -> m a
runDb action = runQuery $ \cnn ->
  runMongoDBPool master action cnn

loginUser :: T.Text -> T.Text -> M.Action IO (Maybe UserId)
loginUser username password =
    do mUserU <- getBy (UniqueUsername username)
       mUserE <- getBy (UniqueEmail username)
       case mUserU `mplus` mUserE of
         Just userEntity ->
             let user = entityVal userEntity
             in if userPassword user == (makeHex $ hashPassword password (decodeHex $ userSalt user))
                then return $ Just (entityKey userEntity)
                else return Nothing
         Nothing ->
             return Nothing

registerUser :: T.Text -> T.Text -> T.Text -> M.Action IO CommonResponse
registerUser username email password =
    do mUserU <- getBy (UniqueUsername username)
       mUserE <- getBy (UniqueEmail email)
       case (mUserU, mUserE) of
         (Just _, _) ->
             return (CommonError "Username already taken!")
         (_, Just _) ->
             return (CommonError "Email already registered!")
         (Nothing, Nothing) ->
             do g <- liftIO getStdGen
                let salt = randomBS 512 g
                    hash = hashPassword password salt
                _ <- insert (User username (makeHex hash) (makeHex salt) email)
                return (CommonSuccess "Signup complete. You may now login.")
