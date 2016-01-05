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

data MyData = MyData (Int, Int, Int)
instance Show MyData where
  show (MyData (a, b, _)) = show a ++ " h, " ++ show b ++ "m"

getMongoKey :: Entity Task -> String
getMongoKey task = show (unMongoKey $ unTaskKey $ entityKey task)

getTime :: UTCTime -> Task -> MyData
getTime time task | taskState (task) == Started = getActiveTime time (fromJust $ taskStartTime (task)) (taskTimeSpent $ task)
                  | otherwise = getStoppedTime (taskTimeSpent $ task)

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
