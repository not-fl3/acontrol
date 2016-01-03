{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Model.Types where

import qualified Data.Text as T
import           Data.Time
import           Database.Persist
import           Database.Persist.MongoDB
import           Database.Persist.TH
import           Language.Haskell.TH

import           Model.TaskState


let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False }
  in share [mkPersist mongoSettings, mkMigrate "migrateCore"] [persistLowerCase|
Session
     validUntil UTCTime
     userId UserId
     deriving Show
User json
     name T.Text
     password T.Text
     salt T.Text
     email T.Text
     UniqueUsername name
     UniqueEmail email
     deriving Show
Task
     name T.Text
     author UserId
     state TaskState
     done Bool
     timeSpent UTCTime
     startTime UTCTime Maybe
     allowedWindows T.Text
     deriving Show
|]
