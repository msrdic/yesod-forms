module Handler.Home where

import          Yesod
import          Yesod.Default.Util
import          Yesod.Form.Jquery

import          Data.Default        (def)

import          Foundation

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost personForm
    defaultLayout $ do
        addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
        $(widgetFileReload def "home")