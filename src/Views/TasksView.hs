{-# LANGUAGE OverloadedStrings #-}
module Views.TasksView where

import           Control.Monad
import           Data.Fixed
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Persist
import           Database.Persist.MongoDB

import           Model.TaskState
import           Model.Types

import           Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

data MyData = MyData (Int, Int, Int)
instance Show MyData where
  show (MyData (a, b, _)) = show a ++ " h, " ++ show b ++ "m"

tasksView :: [Entity Task] -> UTCTime -> H.Html
tasksView tasks time =
  H.div $
         forM_ tasks $ \task ->
         H.div ! A.class_ "row" $
           do H.p ! A.class_ "col-sm-8 task-title" $ H.toHtml $ taskName $ entityVal task
              H.div ! A.class_ "cols-sm-4" $
                do
                 case taskState $ entityVal task of
                   Started -> do
                     H.p ! A.class_ "task-status" $ H.toHtml ("active" :: String)
                     H.a ! A.class_ "task-action-item" !  A.href (mkLink "/stoptask/" task) $ "Stop"
                   Stopped -> do
                     H.p ! A.class_ "task-status" $ H.toHtml ("inactive" :: String)
                     H.a ! A.class_ "task-action-item" !  A.href (mkLink "/starttask/" task) $ "Start"
                   Paused -> H.p ! A.class_ "task-status" $ H.toHtml ("Paused (needed window not active)" :: String)
                 H.a ! A.class_ "task-action-item" ! A.href (mkLink "/done/" task) $ "Done!"
                 H.p ! A.class_ "task-time" $ H.toHtml $ show $ getTime time task

mkLink :: String -> Entity Task -> H.AttributeValue
mkLink str task = H.stringValue $ str ++ show (unMongoKey $ unTaskKey $ entityKey task)

getTime :: UTCTime -> Entity Task -> MyData
getTime time task | taskState (entityVal task) == Started = getActiveTime time (fromJust $ taskStartTime (entityVal task)) (taskTimeSpent $ entityVal task)
                  | otherwise = getStoppedTime (taskTimeSpent $ entityVal task)

getActiveTime :: UTCTime -> UTCTime -> UTCTime -> MyData
getActiveTime time startTime spentTime =
  let diff = diffUTCTime time startTime
      newTime = addUTCTime diff spentTime
      zeroTime = posixSecondsToUTCTime 0
      diffTime = diffUTCTime newTime zeroTime
      res  = realToFrac diffTime :: Double
  in MyData (floor(res / (60 * 60)), floor ((res `mod'` 3600) / 60), floor(res `mod'` 60))

getStoppedTime :: UTCTime ->  MyData
getStoppedTime spentTime =
  let zeroTime = posixSecondsToUTCTime 0
      diffTime = diffUTCTime spentTime zeroTime
      res  = realToFrac diffTime :: Double
  in MyData (floor(res / (60 * 60)), floor ((res `mod'` 3600) / 60), floor(res `mod'` 60))
