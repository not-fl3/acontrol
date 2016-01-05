{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Actions where

import           Data.HVect
import           Web.Spock.Safe hiding (SessionId)

import qualified Data.Text as T

import           Database.Persist.MongoDB hiding (get)
import qualified Database.Persist.MongoDB as M

import           Data.Time

import           Control.Monad
import           Control.Monad.Trans

import           Forms.AddTask
import           Forms.Login
import           Forms.Register
import           Views

import qualified Data.List as List
import           Data.Maybe

import           Model.Types
import           Model.TaskState

import           Db
import           Types

import           Text.Digestive.View

rootAction :: AAction (HVect xs) ()
rootAction = do
  s <- readSession
  case s of
    Nothing -> mkSiteTemplate "notloginned"
    Just (_, uid)  -> do
      allTasks <- runDb $ selectList [TaskAuthor ==. uid, TaskDone ==. False] [Desc TaskName]
      mkSiteTasks allTasks

writeAction :: (ListContains m (UserId, User) xs) => AAction (HVect xs) a
writeAction =
    do (Just (_, uid)) <- readSession
       time <- liftIO getCurrentTime
       f <- runForm "addTask" (addTaskForm uid time)
       case f of
         (view, Nothing) ->
           mkSiteForm "addTask" view
         (_, Just newPost) -> do
            _ <- runDb $ insert newPost
            redirect "/"




loginAction ::
  (ListContains n IsGuest xs, NotInList (UserId, User) xs ~ 'True) => AAction (HVect xs) a
loginAction = do
  (view, res) <- runForm "loginForm" loginForm
  case res of
    Nothing -> mkSiteForm "loginForm" view
    Just loginReq ->
      do loginRes <- runDb $ loginUser (lr_user loginReq) (lr_password loginReq)
         case loginRes of
           Just userId ->
             do sid <- runDb $ createSession userId
                writeSession (Just (sid, userId))
                redirect "/"
           Nothing ->
             mkSiteForm "loginForm" $ makeViewWithError view "Invalid login credentials!"

createSession userId =
    do now <- liftIO getCurrentTime
       insert (Session (addUTCTime (5 * 3600) now) userId)

registerAction :: (ListContains n IsGuest xs, NotInList (UserId, User) xs ~ 'True) => AAction (HVect xs) a
registerAction =
    do f <- runForm "registerForm" registerForm
       case f of
         (view, Nothing) ->
             mkSiteForm "registerForm" view
         (view, Just registerReq) ->
             if rr_password registerReq /= rr_passwordConfirm registerReq
             then mkSiteForm "registerForm" $ makeViewWithError view "Password dont match!"
             else do registerRes <-
                         runDb $ registerUser (rr_username registerReq) (rr_email registerReq) (rr_password registerReq)
                     case registerRes of
                       CommonError errMsg ->
                         mkSiteForm "registerForm" $ makeViewWithError view errMsg
                       CommonSuccess _ ->
                         mkSiteText  "Registered successeful!"

startTaskAction id = do
  let objectid = read id :: ObjectId
  time <- liftIO getCurrentTime
  runDb $ update (fromBackendKey (MongoKey objectid)) [TaskState =. Started, TaskStartTime =. Just time]
  redirect "/"

stopTaskAction id = do
  let key = fromBackendKey $ MongoKey $ read id
  task <- runDb $ M.get key
  updateTaskTime task Stopped key
  redirect "/"

doneAction id = do
  let objectid = read id :: ObjectId
  runDb $ update (fromBackendKey (MongoKey objectid)) [TaskDone =. True]
  redirect "/"


updateTaskTime task state key = do
  time <- liftIO getCurrentTime
  let diff = diffUTCTime time (fromJust $ taskStartTime (fromJust task))
  let newTime = addUTCTime diff (taskTimeSpent (fromJust task))
  runDb $ update key [TaskState =. state, TaskTimeSpent =. newTime, TaskStartTime =. Nothing]

checkWindowAction str = do
  Just (_,uid) <- readSession
  time <- liftIO getCurrentTime
  allTasks <- runDb $ selectList [TaskAuthor ==. uid, TaskDone ==. False] [Desc TaskName]
  let (goodTasks, badTasks) = List.partition (isGoodTask str) allTasks
  forM_ badTasks $ \t ->
    when (taskState (entityVal t) == Started) $
      updateTaskTime (Just (entityVal t)) Paused (entityKey t)
  forM_ goodTasks $ \t ->
    when (taskState (entityVal t) == Paused) $
      runDb $ update (entityKey t) [TaskState =. Started, TaskStartTime =. Just time]
  text (T.pack $ "OK: " ++ str)
  where
    isGoodTask str task = let allowed = taskAllowedWindows $ entityVal task
                              splitted = T.splitOn ";" allowed
                          in allowed == "" || any (\s -> s /= "" && T.isInfixOf s (T.pack str)) splitted
