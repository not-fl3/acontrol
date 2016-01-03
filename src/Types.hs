module Types where

import Web.Spock.Safe hiding (SessionId)

import Db
import Data.Text

data Site = Site {
  user :: Maybe String
  }


type AAction ctx a = SpockActionCtx ctx ConnectionPool (Maybe (SessionId, UserId)) () a


