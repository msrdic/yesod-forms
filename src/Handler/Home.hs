module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "HKaido home page"
    getYesod >>= addScriptEither . urlJqueryJs
    addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
    $(widgetFile "home")