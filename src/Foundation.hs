module Foundation where

import           Yesod                   hiding (renderBootstrap)
import           Yesod.Default.Util
import           Yesod.Form.Jquery

import           Data.Default            (def)
import           Data.Text               (Text)
import           Data.Time               (Day, TimeOfDay (..))

import           Control.Applicative     ((<$>), (<*>))

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           Form.Bootstrap3

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

LargeData
    textField       Text
    intField        Int
    doubleField     Double
    textAreaField   Textarea
    hiddenField     Text
    passwordField   Text
    emailField      Text
    htmlField       Html
    dayField        Day
    timeField       TimeOfDay
    searchField     Text
    urlField        Text
    selectField     Bool
    checkboxField   Bool
|]

hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2) }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm }
bConfig = BootstrapFormConfig { form = BootstrapBasicForm }

bootstrapFieldHelper config label placeholder = bootstrapFieldSettings config label Nothing placeholder Nothing Nothing

personHForm :: Html -> MForm Handler (FormResult Person, Widget)
personHForm = renderBootstrap hConfig $ Person
    <$> areq textField (bootstrapFieldHelper hConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper hConfig "Surname" (Just "Person surname")) Nothing

personIForm :: Html -> MForm Handler (FormResult Person, Widget)
personIForm = renderBootstrap iConfig $ Person
    <$> areq textField (bootstrapFieldHelper iConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper iConfig "Surname" (Just "Person surname")) Nothing

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap bConfig $ Person
    <$> areq textField (bootstrapFieldHelper bConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper bConfig "Surname" (Just "Person surname")) Nothing

largeDataForm :: Html -> MForm Handler (FormResult LargeData, Widget)
largeDataForm = renderBootstrap hConfig $ LargeData
    <$> areq textField (bootstrapFieldHelper hConfig "Text" (Just "Some text content")) Nothing
    <*> areq intField (bootstrapFieldHelper hConfig "Int" (Just "Some integer value")) Nothing
    <*> areq doubleField (bootstrapFieldHelper hConfig "Double" (Just "Some double value")) Nothing
    <*> areq textareaField (bootstrapFieldHelper hConfig "Area" (Just "Some text area content")) Nothing
    <*> areq hiddenField (bootstrapFieldHelper hConfig "Hidden" (Just "Hidden field")) Nothing
    <*> areq passwordField (bootstrapFieldHelper hConfig "Password" (Just "Password field")) Nothing
    <*> areq emailField (bootstrapFieldHelper hConfig "Email" (Just "Email field")) Nothing
    <*> areq htmlField (bootstrapFieldHelper hConfig "Html" (Just "Some HTML")) Nothing
    <*> areq dayField (bootstrapFieldHelper hConfig "Day" (Just "Some day")) Nothing
    <*> areq timeField (bootstrapFieldHelper hConfig "Time" (Just "Some time")) Nothing
    <*> areq (searchField False) (bootstrapFieldHelper hConfig "Search" (Just "Some search")) Nothing
    <*> areq urlField (bootstrapFieldHelper hConfig "URL" (Just "Some URL")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Bool" (Just "Some bool")) Nothing
    <*> areq checkBoxField (bootstrapFieldHelper hConfig "Checkbox" (Just "Some checkbox")) Nothing