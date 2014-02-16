module Handler.Home where

import          Yesod
import          Yesod.Default.Util
import          Yesod.Form.Jquery

import          Data.Default        (def)

import          Foundation

getHomeR :: Handler Html
getHomeR = do
    (basicWidget, enctype) <- generateFormPost personForm
    (inlineWidget, enctype) <- generateFormPost personIForm
    (horizontalWidget, enctype) <- generateFormPost personHForm
    (largeWidget, enctype) <- generateFormPost largeDataForm
    defaultLayout $ do
        addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
        $(widgetFileReload def "home")