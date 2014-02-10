module Handler.Person where

import          Yesod
import          Yesod.Default.Util
import          Yesod.Form.Jquery

import          Data.Default        (def)

import          Foundation

postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout $(widgetFileReload def "person")
        _ -> defaultLayout $(widgetFileReload def "home")