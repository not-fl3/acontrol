{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import           Web.Spock.Safe hiding (SessionId)
import           Network.Wai.Middleware.Static

import           Actions
import           Hooks
import           Db

main :: IO()
main = let config = defaultSpockCfg Nothing (PCConn initDbConnection) ()
       in runSpock 3000 $ spock config $ prehook initHook $
          do middleware (staticPolicy (addBase "static"))
             get "/" rootAction
             prehook guestOnlyHook $
               do getpost "/register" registerAction
                  getpost "/login" loginAction
             prehook authHook $
               do getpost "/addtask" writeAction
                  getpost ("/starttask" <//> var) startTaskAction
                  getpost ("/done" <//> var) doneAction
                  getpost ("/stoptask" <//> var) stopTaskAction
                  getpost ("/checkwindow/" <//> var) checkWindowAction

                  getpost "/logout" $ writeSession Nothing >> redirect "/"
