module Views(
  mkSite',
  getSiteData,
  module Views.Site,
  module Views.TasksView,
  module Views.NotLoginned,
  module Views.PanelWithError,
  module Views.NoAccessPage
  ) where

import           Web.Spock.Safe hiding (SessionId)

import qualified Text.Blaze.XHtml5 as H
import qualified Data.Text as T

import           Text.Blaze.Html.Renderer.String

import           Types
import           Views.NoAccessPage
import           Views.NotLoginned
import           Views.PanelWithError
import           Views.Site
import           Views.TasksView

getSiteData :: Maybe a -> Site
getSiteData Nothing  = Site Nothing
getSiteData (Just _) = Site $ Just "aaa"

mkSite' :: H.Html -> AAction a b
mkSite' ss = readSession >>= \s -> html $ T.pack $ renderHtml $ site (getSiteData s) ss
