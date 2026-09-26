-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE OverloadedRecordDot #-}
module Derive.Parse.AllocRecord where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector.Unboxed

import qualified Util.Lists as Lists
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Record as Record
import qualified Derive.REnv as REnv
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Perform.Midi.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.UiConfig as UiConfig

import           Global


type Error = Text

-- * parse

type Parse a = State.StateT (Map Text Record.RVal) (Except.Except [Error]) a

pAllocation :: InstT.Qualified -> Map Text Record.RVal
    -> Either Error UiConfig.Allocation
pAllocation allocQualified fields = run $ do
    allocConfig <- pConfig
    allocBackend <- pBackend
    rest <- State.get
    unless (Map.null rest) $
        Except.throwError ["extra fields: " <> Text.unwords (Map.keys rest)]
    pure $ UiConfig.Allocation
        { allocQualified, allocConfig, allocBackend }
    where
    run = first (Text.intercalate " -> ") . Except.runExcept
        . flip State.evalStateT fields

-- put in the inherited env as comments?
unAllocation :: UiConfig.Allocation -> Record.Record
unAllocation (UiConfig.Allocation { allocConfig, allocBackend }) =
    unConfig allocConfig ++ unBackend allocBackend

pConfig :: Parse Common.Config
pConfig = do
    env <- field "env" mempty pEnv
    controls <- field "controls" mempty pControls
    pure $ Common.Config env controls False False

unConfig :: Common.Config -> Record.Record
unConfig (Common.Config { configEnviron, configControls }) = concat
    [ [("env", env) | configEnviron /= mempty]
    , [("controls", controls) | configControls /= mempty]
    ]
    where
    env = unEnviron configEnviron
    controls = unControls configControls

unEnviron :: REnv.Environ -> Record.RVal
unEnviron = Record.Record . Map.toAscList . fmap unVal . REnv.toMap

unControls :: ScoreT.ControlValMap -> Record.RVal
unControls = Record.Record . Map.toAscList . fmap unNum
    . Map.mapKeys ScoreT.controlName

pEnv :: Record.RVal -> Parse REnv.Environ
pEnv r = do
    fields <- pRecord r
    REnv.Environ . Map.fromList <$> traverse (traverse pVal) fields

pControls :: Record.RVal -> Parse ScoreT.ControlValMap
pControls r = do
    fields <- pRecord r
    Map.fromList <$> mapM parse fields
    where parse (k, v) = (ScoreT.Control k, ) <$> pNum v
    -- traverse p_num $ Map.mapKeys ScoreT.Control fields

pBackend :: Parse UiConfig.Backend
pBackend = UiConfig.Midi <$> pMidiConfig

pMidiConfig :: Parse Patch.Config
pMidiConfig = do
    initialization <- fieldm "initialization" pEnum
    settings <- pSettings
    pure $ Patch.Config
        { configAllocation = [] -- filled in from the top line
        , configInitialization = initialization
        , configSettings = settings
        }

pSettings :: Parse Patch.Settings
pSettings = do
    configFlags <- fieldm "flags" (fmap Set.fromList . pList pEnum)
    configScale <- fieldm "scale" pScale
    configDecay <- fieldm "decay" (fmap RealTime.seconds . pNum)
    configPitchBendRange <- fieldm "pb_range" (pPair pInt)
    configControlDefaults <- fieldm "control_defaults" pControls
    pure $ Patch.Settings
        { configFlags, configScale, configDecay
        , configPitchBendRange, configControlDefaults
        }

-- | 'Patch.Scale's are from instrument patch definitions, ultimately from
-- Derive.Scale.  Unless it's useful to hand-tune, it's probably better to
-- have a global named registry like with scales, but too much bother right
-- now.
pScale :: Record.RVal -> Parse Patch.Scale
pScale val
    | Record.Record fields <- val
    , [("key_to_nn", nns), ("name", name)] <- Lists.sortOn fst fields = do
        nns <- pList pNum nns
        name <- pStr name
        pure $ Patch.makeScaleDense name nns
    | otherwise = throw "{name, key_to_nn}" val

unScale :: Patch.Scale -> Record.RVal
unScale (Patch.Scale { scaleName, scaleKeyToNn }) = Record.Record
    [ ("name", Record.Val $ DeriveT.str scaleName)
    , ("key_to_nn", Record.Val $ DeriveT.VList $ map DeriveT.num $
        Vector.Unboxed.toList scaleKeyToNn)
    ]

unBackend :: UiConfig.Backend -> Record.Record
unBackend = \case
    UiConfig.Midi config -> concat
        [ maybeField "initialization" (Record.Val . unEnum)
            configInitialization
        , maybeField "flags"
            (Record.Val . DeriveT.VList . map unEnum . Set.toList)
            configFlags
        -- Save it as just the name.  It should be factored out into an
        -- assignment by Instruments.un_instruments.
        , maybeField "scale"
            -- TODO Historically scale_name had spaces.  They should no longer
            -- have them, but this is so old scores still load.
            (Record.Val . DeriveT.str . Text.replace " " "-" . Patch.scaleName)
            configScale
        , maybeField "decay" (Record.Val . DeriveT.realTime) configDecay
        , maybeField "pb_range" (\(a, b) -> Record.list [int a, int b])
            configPitchBendRange
        , maybeField "control_defaults" unControls configControlDefaults
        ]
        where
        Patch.Config { configInitialization, configSettings } = config
        Patch.Settings
            { configFlags, configScale, configDecay
            , configPitchBendRange, configControlDefaults
            } = configSettings
    _ -> []
    -- if Nothing, I could emit a commented out line?

maybeField :: text -> (a -> b) -> Maybe a -> [(text, b)]
maybeField name convert mbVal = maybe [] ((:[]) . (name,) . convert) mbVal

fieldm :: Text -> (Record.RVal -> Parse a) -> Parse (Maybe a)
fieldm name parse = field name Nothing (fmap Just . parse)

field :: Text -> a -> (Record.RVal -> Parse a) -> Parse a
field name deflt parse =
    State.get >>= \fields -> case Map.lookup name fields of
        Nothing -> pure deflt
        Just val -> do
            State.put (Map.delete name fields)
            annotate name (parse val)

pRecord :: Record.RVal -> Parse Record.Record
pRecord = \case
    Record.Record rec -> pure rec
    a@(Record.Val _) -> throw "record" a

pStr :: Record.RVal -> Parse Text
pStr = \case
    Record.Val (DeriveT.VStr (Expr.Str s)) -> pure s
    a -> throw "str" a

pNum :: Record.RVal -> Parse Double
pNum = \case
    Record.Val (DeriveT.VSignal (ScoreT.Typed _ sig))
        | Just a <- Signal.constantVal sig -> pure a
    a -> throw "num" a

unNum :: Double -> Record.RVal
unNum = Record.Val . DeriveT.num

pInt :: Record.RVal -> Parse Int
pInt val = properFraction <$> pNum val >>= \case
    (n, 0) -> pure n
    _ -> throw "int" val

pEnum :: forall a. (Enum a, Bounded a, Show a) => Record.RVal -> Parse a
pEnum val = pStr val >>= \str -> case lookup str to of
    Just a -> pure a
    Nothing -> throw "enum" val
    where to = Lists.keyOn showt [minBound :: a ..]

unEnum :: Show a => a -> DeriveT.Val
unEnum = DeriveT.str . showt

pList :: (Record.RVal -> Parse a) -> Record.RVal -> Parse [a]
pList parse = \case
    Record.Val (DeriveT.VList vals) ->
        annotate "list" $ mapM (parse . Record.Val) vals
    a -> throw "list" a

pPair :: (Record.RVal -> Parse a) -> Record.RVal -> Parse (a, a)
pPair parse = \case
    Record.Val (DeriveT.VList [a, b]) ->
        annotate "pair" $ (,) <$> parse (Record.Val a) <*> parse (Record.Val b)
    a -> throw "list" a

-- | REnv.Vals are supposed to be the the parseable ones.  But the usual
-- parser produces DeriveT.Val, just a subset.  So this effectively narrows
-- the type.
pVal :: Record.RVal -> Parse REnv.Val
pVal = \case
    Record.Val val -> case REnv.demote val of
        Just val -> pure val
        Nothing -> Except.throwError
            ["val can't be converted to parseable: " <> ShowVal.showVal val]
    a@(Record.Record _) -> throw "val" a

unVal :: REnv.Val -> Record.RVal
unVal = Record.Val . REnv.promote

annotate :: Except.MonadError [e] m => e -> m a -> m a
annotate context = flip Except.catchError (Except.throwError . (context:))

throw :: Except.MonadError [Error] m => Text -> Record.RVal -> m a
throw expected val = Except.throwError
    ["expected " <> expected <> " but got " <> mconcat (Record.unRval val)]

int :: Int -> DeriveT.Val
int = DeriveT.num . fromIntegral
