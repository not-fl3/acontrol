{-# LANGUAGE TemplateHaskell #-}
module Model.TaskState where
       
import           Data.Time
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Language.Haskell.TH

import qualified Data.Text as T

data TaskState = Started | Stopped | Paused
    deriving (Show, Read, Eq)
derivePersistField "TaskState"
