module Foundation where

import              Yesod hiding (renderBootstrap)
import              Yesod.Form.Jquery
import              Yesod.Default.Util

import              Data.Default        (def)
import              Data.Text           (Text)

import              Control.Applicative ((<$>), (<*>))

import              Database.Persist
import              Database.Persist.TH
import              Database.Persist.Sqlite

import              Form.Bootstrap3

data FormApp = FormApp ConnectionPool

instance Yesod FormApp

instance YesodJquery FormApp where
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"

instance RenderMessage FormApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist FormApp where
    type YesodPersistBackend FormApp = SqlPersistT
    runDB dbAction = do
        FormApp connectionPool <- getYesod
        runSqlPool dbAction connectionPool

mkYesodData "FormApp" $(parseRoutesFile "config/routes")

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    surname Text
    deriving Show
|]

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap defaultFormConfig $ Person
    <$> areq textField (bootstrapFieldSettings "Name") Nothing
    <*> areq textField (bootstrapFieldSettings "Surname") Nothing