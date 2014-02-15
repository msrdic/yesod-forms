module Form.Bootstrap3 (renderBootstrap,
                        bootstrapFieldSettings,
                        hConfig,
                        bConfig,
                        iConfig
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

instance ToMarkup GridOptions where
    toMarkup = toMarkup . show

data BootstrapForm = BootstrapBasicForm
    | BootstrapInlineForm
    | BootstrapHorizontalForm { containerOffset :: GridOptions, container :: GridOptions, label :: GridOptions, field :: GridOptions }

bootstrapFieldSettings :: BootstrapFormConfig -> SomeMessage site -> FieldSettings site
bootstrapFieldSettings formConfig msg = FieldSettings msg Nothing Nothing Nothing (attrsFromFormConfig formConfig)

attrsFromFormConfig :: BootstrapFormConfig -> [(Text, Text)]
attrsFromFormConfig config = [("class",  pack ("form-control " ++ (show . field $ form config)))]

data BootstrapFormConfig = BootstrapFormConfig {
    form        :: BootstrapForm
    }

hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 0) (ColXs 7) (ColXs 2) (ColXs 4) }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm }
bConfig = BootstrapFormConfig { form = BootstrapBasicForm }

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
                        $of BootstrapHorizontalForm containerOffset containerClass labelClass fieldClass
                            <label .control-label .#{labelClass} for=#{fvId view}>#{fvLabel view}
                            <div .#{containerOffset} .#{containerClass}>

                                ^{fvInput view}

                            $maybe tt <- fvTooltip view
                                <span .help-block>#{tt}
                            $maybe err <- fvErrors view
                                <span .help-block>#{err}
                |]
    return (res, widget)