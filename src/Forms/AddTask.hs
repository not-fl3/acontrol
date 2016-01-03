{-# LANGUAGE OverloadedStrings #-}
module Forms.AddTask where

import Text.Blaze.Html (Html)
import Text.Digestive hiding (Post)
import Text.Digestive.Bootstrap

import Control.Applicative

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Forms.Common

import Model.TaskState
import Model.Types

postForm :: Monad m => UserId -> UTCTime -> Form Html m Task
postForm uid time = createTask uid time
                   <$> ("name" .: text Nothing)
                   <*> ("active" .: bool Nothing)
                   <*> ("allowed" .: text Nothing)
  where createTask uid time name active allowed = Task {
          taskName = name,
          taskAuthor = uid,
          taskState = if active then Started else Stopped,
          taskDone = False,
          taskTimeSpent = posixSecondsToUTCTime 0,
          taskStartTime = if active then Just time else Nothing,
          taskAllowedWindows = allowed
          }

postFormSpec :: FormMeta
postFormSpec =
    FormMeta
    { fm_method = POST
    , fm_target = "/addtask"
    , fm_elements =
        [ FormElement "name" (Just "Title:") InputText,
          FormElement "active" (Just "Start now:") InputCheckbox,
          FormElement "allowed" (Just "Semicolumnt(;)-separated list of keywords, that must be in opened windows (empty for no restrictions):") InputText
        ]
    , fm_submitText = "Publish"
    }
