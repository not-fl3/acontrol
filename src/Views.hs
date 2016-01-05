{-# LANGUAGE OverloadedStrings #-}
module Views(
  getSiteData,
  taskSplice,
  template,
  runForm,
  mkSiteTemplate,
  mkSiteTasks,
  mkSiteText,
  mkSiteForm,
  module Views.TasksView,
  ) where

import           Web.Spock.Safe hiding (SessionId)

import qualified Data.Text as T

import           Types
import           Views.TasksView
import           Views.TasksView as TV

import           Database.Persist.MongoDB

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Encoding
import           Data.Time
import           Heist
import           Heist.Compiled as C
import           Heist.Internal.Types
import qualified Heist.Interpreted as I

import           Db

import qualified Text.Digestive.Heist as HE
import qualified Text.Digestive.Heist.Compiled as HE
import qualified Text.Digestive.View as HE

import           Text.Digestive.Types

getSiteData :: Maybe a -> Site
getSiteData Nothing  = Site Nothing
getSiteData (Just _) = Site $ Just "aaa"

taskSplice :: UTCTime -> [Entity Task] -> I.Splice IO
taskSplice time = I.mapSplices $ renderTask . ((,) <$> entityVal <*> getMongoKey)
     where
       renderTask (t, k) | taskState t == Started = I.callTemplate "startedtask" $ splicefuncs t k
                         | taskState t == Stopped = I.callTemplate "stoppedtask" $ splicefuncs t k
                         | taskState t == Paused  = I.callTemplate "pausedtask"  $ splicefuncs t k
       splicefuncs task k = do
         "name" ## (I.textSplice $ taskName task)
         "time" ## (I.textSplice $ T.pack $ show $ TV.getTime time task)
         "key" ## (I.textSplice $ T.pack k)

env :: MonadIO m => Env (ActionCtxT ctx m)
env path = parse TextInput params
  where parse f = liftM $ map (f . snd) . filter ((== name) . fst)
        name    = fromPath path

runForm formName form = do
  req <- reqMethod
  case req of
    GET -> do
      view <- HE.getForm formName form
--      mkSite'' [] [("container", HE.formSplice mempty mempty (return view))]
      mkSite'' [] [("container", template (T.unpack formName)), ("formdata", HE.formSplice mempty mempty (return view))]
      return (view, Nothing)
    POST -> HE.postForm formName form (const $ return env)


template :: String -> Splice IO
template = callTemplate . B8.pack

renderSplices :: (Splices (I.Splice IO), Splices (Splice IO)) -> IO B.ByteString
renderSplices (isplices, csplices) = do
  let sc = SpliceConfig (isplices `mappend` defaultInterpretedSplices)( defaultLoadTimeSplices `mappend` isplices) csplices mempty [loadTemplates "./templates/"]

  heistState <- either (error . show) id <$> runEitherT (initHeist $ HeistConfig sc "" False)
  builder <- maybe (error "oops") fst $
         renderTemplate heistState "site"
  return $ toByteString builder

mkSite'' i s = getSplices i s . getSiteData <$> readSession >>= liftIO . renderSplices >>= html . decodeUtf8

getSplices ilst lst (Site a) = let log = if isJust a then "fornotloginned" else "forloginned"
                                   splices = do
                                     mapM_ (uncurry (##)) lst
                                     log ## return (C.yieldRuntimeText $ return $ T.pack "")
                        in (mapM_ (uncurry (##)) ilst, splices)



mkSiteTasks :: [Entity Task] -> AAction a b
mkSiteTasks tasks = do
  time <- liftIO getCurrentTime
  mkSite'' [("task", taskSplice time tasks)] [("container", template "tasks_view")]

mkSiteTemplate :: T.Text -> AAction a b
mkSiteTemplate t = mkSite'' [] [("container", template $ T.unpack t)]

mkSiteText :: T.Text -> AAction a b
mkSiteText text = mkSite'' [("container", I.textSplice text)] []

mkSiteForm formName view = mkSite'' [] [("container", template (T.unpack formName)), ("formdata", HE.formSplice mempty mempty (return view))]


