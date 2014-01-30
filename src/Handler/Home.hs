module Handler.Home where

import Import
import Yesod.Static

staticFiles "static"

handleHomeR :: Handler Html
handleHomeR = defaultLayout $ do
    setTitle "HKaido home page"
    getYesod >>= addScriptEither . urlJqueryJs
    addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
    $(widgetFile "home")