module Form.Bootstrap3 (renderBootstrap,
                        bootstrapFieldSettings,
                        BootstrapFormConfig (BootstrapFormConfig, form),
                        GridOptions (ColXs, ColSm, ColMd, ColLg),
                        BootstrapForm (BootstrapBasicForm, BootstrapInlineForm, BootstrapHorizontalForm)
                        ) where

import              Yesod hiding (renderBootstrap)
import qualified    Yesod.Form.Functions as YFF hiding (renderBootstrap)

import              Control.Monad (liftM, join)
import              Control.Arrow (second)
import              Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import              Data.Maybe (listToMaybe, fromMaybe)

import              Data.Text (Text, pack)
import qualified    Data.Map as Map

import              Text.Blaze.Html

import              Text.Shakespeare.I18N

data GridOptions = ColXs Int | ColSm Int | ColMd Int | ColLg Int

instance Show GridOptions where
    show (ColXs 0) = ""
    show (ColXs columns) = "col-xs-" ++ show columns

    show (ColSm 0) = ""
    show (ColSm columns) = "col-sm-" ++ show columns

    show (ColMd 0) = ""
    show (ColMd columns) = "col-md-" ++ show columns

    show (ColLg 0) = ""
    show (ColLg columns) = "col-lg-" ++ show columns

toOffset :: GridOptions -> String
toOffset (ColXs 0) = ""
toOffset (ColSm 0) = ""
toOffset (ColMd 0) = ""
toOffset (ColLg 0) = ""
toOffset (ColXs columns) = "col-xs-offset-" ++ show columns
toOffset (ColSm columns) = "col-sm-offset-" ++ show columns
toOffset (ColMd columns) = "col-md-offset-" ++ show columns
toOffset (ColLg columns) = "col-lg-offset-" ++ show columns

instance ToMarkup GridOptions where
    toMarkup = toMarkup . show

data BootstrapForm = BootstrapBasicForm | BootstrapInlineForm
    | BootstrapHorizontalForm { containerOffset :: GridOptions, container :: GridOptions, label :: GridOptions }

data BootstrapFormConfig = BootstrapFormConfig { form :: BootstrapForm }

-- | Use a tooltip as a placeholder
bootstrapFieldSettings :: BootstrapFormConfig -> SomeMessage site -> Maybe (SomeMessage site) -> Maybe Text -> Maybe Text -> Maybe Text -> FieldSettings site
bootstrapFieldSettings formConfig msg tooltip placeholder id name = FieldSettings msg tooltip id name (attrsFromFormConfig formConfig placeholder)

attrsFromFormConfig :: BootstrapFormConfig -> Maybe Text -> [(Text, Text)]
attrsFromFormConfig _ Nothing = [("class", "form-control")]
attrsFromFormConfig _ (Just placeholder) = [("class", "form-control"), ("placeholder", placeholder)]

renderBootstrap :: Monad m => BootstrapFormConfig -> FormRender m a
renderBootstrap formConfig aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
                        \#{fragment}
                        $forall view <- views
                          <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                            $case (form formConfig)
                              $of BootstrapBasicForm
                                <label for=#{fvId view}>#{fvLabel view}
                                ^{fvInput view}
                                ^{helpWidget view}
                              $of BootstrapInlineForm
                                <label .sr-only for=#{fvId view}>#{fvLabel view}
                                ^{fvInput view}
                                ^{helpWidget view}
                              $of BootstrapHorizontalForm containerOffset containerClass labelClass
                                <label .control-label .#{labelClass} for=#{fvId view}>#{fvLabel view}
                                <div .#{containerClass}>
                                  ^{fvInput view}
                                ^{helpWidget view}

                        ^{submitWidget formConfig}
                |]
    return (res, widget)

submitWidget (BootstrapFormConfig (BootstrapHorizontalForm containerOffset containerClass labelClass)) = [whamlet|
<div .form-group>
    <div .#{toOffset containerOffset} .#{containerClass}>
      <button type=submit .btn .btn-default>Create user
|]
submitWidget _ = [whamlet|<button type=submit .btn .btn-default>Create user|]

helpWidget view = [whamlet|
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]