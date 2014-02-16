module Foundation where

import           Yesod                   hiding (renderBootstrap)
import           Yesod.Form.Jquery

import           Data.Text               (Text)
import           Data.Time               (Day, TimeOfDay (..))

import           Control.Applicative     ((<$>), (<*>))

import           Form.Bootstrap3

data FormApp = FormApp

instance Yesod FormApp

instance YesodJquery FormApp where
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"

instance RenderMessage FormApp FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "FormApp" $(parseRoutesFile "config/routes")

data Person = Person { name :: Text, surname :: Text }
    deriving Show

data LargeData = LargeData {
    textField1       :: Text,
    intField1        :: Int,
    doubleField1     :: Double,
    textAreaField1   :: Textarea,
    hiddenField1     :: Text,
    passwordField1   :: Text,
    emailField1      :: Text,
    htmlField1       :: Html,
    dayField1        :: Day,
    timeField1       :: TimeOfDay,
    searchField1     :: Text,
    urlField1        :: Text,
    selectField1     :: Bool,
    checkboxField1   :: Bool
    }

hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Create user" }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm, submit = "Create user"}
bConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Create user" }
largeFormConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Submit large data" }

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