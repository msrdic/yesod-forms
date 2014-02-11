module Handler.Person where

import          Yesod
import          Yesod.Default.Util
import          Yesod.Form.Jquery

import          Data.Default        (def)

import          Foundation
import          Database.Persist
import          Database.Persist.Sqlite

postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    persons <- runDB $ selectList [] [Desc PersonName]
    FormApp connectionPool <- getYesod
    case result of
        FormSuccess person -> do
            runDB $ insert person
            defaultLayout $(widgetFileReload def "person")
        _ -> defaultLayout $(widgetFileReload def "home")