module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "HKaido home page"
    getYesod >>= addScriptEither . urlJqueryJs
    addStylesheetRemote "bootstrap/css/bootstrap-theme.min.css"
    addStylesheetRemote "bootstrap/css/bootstrap.min.css"
    $(widgetFile "home")