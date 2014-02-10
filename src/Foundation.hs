module Foundation where

import          Yesod
import          Yesod.Form.Jquery
import          Yesod.Default.Util

import          Data.Default        (def)
import          Data.Text           (Text)

import          Control.Applicative ((<$>), (<*>))

data FormApp = FormApp

instance Yesod FormApp

instance YesodJquery FormApp where
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"

instance RenderMessage FormApp FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "FormApp" $(parseRoutesFile "config/routes")

data Person = Person
    { personName          :: Text
    , personSurname       :: Text
    }
  deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap $ Person
    <$> areq textField "Name" Nothing
    <*> areq textField "Surname" Nothing