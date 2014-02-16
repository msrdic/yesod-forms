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

hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 0) (ColXs 4) (ColXs 2) }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm }
bConfig = BootstrapFormConfig { form = BootstrapBasicForm }

personHForm :: Html -> MForm Handler (FormResult Person, Widget)
personHForm = renderBootstrap hConfig $ Person
    <$> areq textField (bootstrapFieldSettings hConfig "Name" Nothing (Just "Person name") Nothing Nothing) Nothing
    <*> areq textField (bootstrapFieldSettings hConfig "Surname" Nothing (Just "Person surname") Nothing Nothing) Nothing

personIForm :: Html -> MForm Handler (FormResult Person, Widget)
personIForm = renderBootstrap iConfig $ Person
    <$> areq textField (bootstrapFieldSettings iConfig "Name" Nothing (Just "Person name") Nothing Nothing) Nothing
    <*> areq textField (bootstrapFieldSettings iConfig "Surname" Nothing (Just "Person surname") Nothing Nothing) Nothing

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap bConfig $ Person
    <$> areq textField (bootstrapFieldSettings bConfig "Name" Nothing (Just "Person name") Nothing Nothing) Nothing
    <*> areq textField (bootstrapFieldSettings bConfig "Surname" Nothing (Just "Person surname") Nothing Nothing) Nothing