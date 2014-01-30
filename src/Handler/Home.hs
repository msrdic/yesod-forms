module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "HKaido home page"
    getYesod >>= addScriptEither . urlJqueryJs
    $(widgetFile "home")