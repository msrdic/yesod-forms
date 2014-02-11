{-# LANGUAGE RecordWildCards #-}
module Form.Bootstrap3 (renderBootstrap,
                        areq,
                        aopt,
                        mreq,
                        mopt,
                        defaultFormConfig
                        ) where

import Yesod hiding (areq, aopt, mreq, mopt, renderBootstrap)
import qualified Yesod.Form.Functions as YFF hiding (renderBootstrap, areq, aopt, mreq, mopt)

import Control.Monad (liftM, join)
import Control.Arrow (second)
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import Data.Maybe (listToMaybe, fromMaybe)

import Data.Text (Text)
import qualified Data.Map as Map

data BootstrapFormConfig = BootstrapFormConfig {
    horizontal  :: Bool,
    version     :: Int
    }

defaultFormConfig = BootstrapFormConfig { horizontal = True, version = 3 }

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
                        <label .control-label $if (horizontal formConfig) .col-sm-2 for=#{fvId view}>#{fvLabel view}
                        $if (horizontal formConfig)
                        <div .col-sm-10>
                            ^{fvInput view}
                            $maybe tt <- fvTooltip view
                                <span .help-block>#{tt}
                            $maybe err <- fvErrors view
                                <span .help-block>#{err}
                |]
    return (res, widget)

areq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
     => Field m a
     -> FieldSettings site
     -> Maybe a
     -> AForm m a
areq a b = formToAForm . liftM (second return) . mreq a b

-- | Applicative equivalent of 'mopt'.
aopt :: MonadHandler m
     => Field m a
     -> FieldSettings (HandlerSite m)
     -> Maybe (Maybe a)
     -> AForm m (Maybe a)
aopt a b = formToAForm . liftM (second return) . mopt a b

-- | Converts a form field into monadic form. This field requires a value
-- and will return 'FormFailure' if left empty.
mreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
     => Field m a           -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> MForm m (FormResult a, FieldView site)
mreq field fs mdef = mhelper field fs mdef (\m l -> FormFailure [renderMessage m l MsgValueRequired]) FormSuccess True

-- | Converts a form field into monadic form. This field is optional, i.e.
-- if filled in, it returns 'Just a', if left empty, it returns 'Nothing'.
-- Arguments are the same as for 'mreq' (apart from type of default value).
mopt :: (site ~ HandlerSite m, MonadHandler m)
     => Field m a
     -> FieldSettings site
     -> Maybe (Maybe a)
     -> MForm m (FormResult (Maybe a), FieldView site)
mopt field fs mdef = mhelper field fs (join mdef) (const $ const $ FormSuccess Nothing) (FormSuccess . Just) False

mhelper :: (site ~ HandlerSite m, MonadHandler m)
        => Field m a
        -> FieldSettings site
        -> Maybe a
        -> (site -> [Text] -> FormResult b) -- ^ on missing
        -> (a -> FormResult b) -- ^ on success
        -> Bool -- ^ is it required?
        -> MForm m (FormResult b, FieldView site)

mhelper Field {..} FieldSettings {..} mdef onMissing onFound isReq = do
    tell fieldEnctype
    mp <- askParams
    name <- maybe newFormIdent return fsName
    theId <- lift $ maybe newIdent return fsId
    (_, site, langs) <- ask
    let mr2 = renderMessage site langs
    (res, val) <-
        case mp of
            Nothing -> return (FormMissing, maybe (Left "") Right mdef)
            Just p -> do
                mfs <- askFiles
                let mvals = fromMaybe [] $ Map.lookup name p
                    files = fromMaybe [] $ mfs >>= Map.lookup name
                emx <- lift $ fieldParse mvals files
                return $ case emx of
                    Left (SomeMessage e) -> (FormFailure [renderMessage site langs e], maybe (Left "") Left (listToMaybe mvals))
                    Right mx ->
                        case mx of
                            Nothing -> (onMissing site langs, Left "")
                            Just x -> (onFound x, Right x)
    return (res, FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = fieldView theId name (fsAttrs ++ [("class", "form-control")]) val isReq
        , fvErrors =
            case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
        , fvRequired = isReq
        })