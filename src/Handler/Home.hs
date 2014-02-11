module Handler.Home where

import          Yesod
import          Yesod.Default.Util
import          Yesod.Form.Jquery

import          Data.Default        (def)

import          Foundation

getHomeR :: Handler Html
getHomeR = do
    persons <- runDB $ selectList [] [Desc PersonName]
    (widget, enctype) <- generateFormPost personForm
    defaultLayout $ do
        addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
        -- addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap-theme.min.css"
        -- addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min.js"
        $(widgetFileReload def "home")