{-# LANGUAGE RecordWildCards #-}
module Form.Bootstrap3 (renderBootstrap,
                        bootstrapFieldSettings,
                        defaultFormConfig
                        ) where

import Yesod hiding (renderBootstrap)
import qualified Yesod.Form.Functions as YFF hiding (renderBootstrap)

import Control.Monad (liftM, join)
import Control.Arrow (second)
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import Data.Maybe (listToMaybe, fromMaybe)

import Data.Text (Text)
import qualified Data.Map as Map

import Text.Blaze.Html

data BootstrapFormConfig = BootstrapFormConfig {
    horizontal  :: Bool,
    form    :: BootstrapForm,
    version     :: Int
    }

defaultFormConfig = BootstrapFormConfig { horizontal = True, form = BootstrapHorizontalForm (ColXs 2) (ColXs 10), version = 3 }

renderBootstrap :: Monad m => BootstrapFormConfig -> FormRender m a
renderBootstrap formConfig aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
                $newline never
                \#{fragment}
                $forall view <- views
                    <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                        $case (form formConfig)
                            $of BootstrapBasicForm
                                <label for=#{fvId view}>#{fvLabel view}
                            $of BootstrapInlineForm
                                <label .sr-only for=#{fvId view}>#{fvLabel view}
                            $of BootstrapHorizontalForm labelClass fieldClass
                                <label .control-label .#{labelClass} for=#{fvId view}>#{fvLabel view}
                                <div .col-sm-4>
                        ^{fvInput view}
                        $maybe tt <- fvTooltip view
                            <span .help-block>#{tt}
                        $maybe err <- fvErrors view
                            <span .help-block>#{err}
                |]
    return (res, widget)

data GridOptions = ColXs Int | ColSm Int | ColMd Int | ColLg Int

instance Show GridOptions where
    show (ColXs columns) = "col-xs-" ++ (show columns)
    show (ColSm columns) = "col-sm-" ++ (show columns)
    show (ColMd columns) = "col-md-" ++ (show columns)
    show (ColLg columns) = "col-lg-" ++ (show columns)

instance ToMarkup GridOptions where
    toMarkup option = toMarkup $ show option

data BootstrapForm = BootstrapBasicForm | BootstrapInlineForm | BootstrapHorizontalForm GridOptions GridOptions

bootstrapFieldSettings :: SomeMessage site -> FieldSettings site
bootstrapFieldSettings msg = FieldSettings msg Nothing Nothing Nothing [("class", "form-control")]