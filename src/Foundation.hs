module Foundation where

import Yesod
import Yesod.Default.Util
import Data.Default (def)
import Yesod.Form.Jquery

data HKaido = HKaido

instance Yesod HKaido

instance YesodJquery HKaido where
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"

instance RenderMessage HKaido FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "HKaido" $(parseRoutesFile "config/routes")