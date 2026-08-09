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

p_allocation :: InstT.Qualified -> [(Text, Record.RVal)]
    -> Either Error UiConfig.Allocation
p_allocation alloc_qualified fields = run $ do
    alloc_config <- p_config
    alloc_backend <- p_backend
    rest <- State.get
    unless (Map.null rest) $
        Except.throwError ["extra fields: " <> Text.unwords (Map.keys rest)]
    return $ UiConfig.Allocation
        { alloc_qualified, alloc_config, alloc_backend }
    where
    run = first (Text.intercalate " -> ") . Except.runExcept
        . flip State.evalStateT (Map.fromList fields)

-- put in the inherited env as comments?
un_allocation :: UiConfig.Allocation -> [(Text, Record.RVal)]
un_allocation (UiConfig.Allocation { alloc_config, alloc_backend }) =
    un_config alloc_config ++ un_backend alloc_backend

p_config :: Parse Common.Config
p_config = do
    env <- field "env" mempty p_env
    controls <- field "controls" mempty p_controls
    return $ Common.Config env controls False False

un_config :: Common.Config -> [(Text, Record.RVal)]
un_config (Common.Config { config_environ, config_controls }) = concat
    [ [("env", env) | config_environ /= mempty]
    , [("controls", controls) | config_controls /= mempty]
    ]
    where
    env = Record.Record $ Map.toAscList $ un_val <$> REnv.to_map config_environ
    controls = un_controls config_controls

un_controls :: ScoreT.ControlValMap -> Record.RVal
un_controls = Record.Record . Map.toAscList . fmap un_num
    . Map.mapKeys ScoreT.control_name

p_env :: Record.RVal -> Parse REnv.Environ
p_env r = do
    fields <- p_record r
    REnv.Environ . Map.fromList <$> traverse (traverse p_val) fields

p_controls :: Record.RVal -> Parse ScoreT.ControlValMap
p_controls r = do
    fields <- p_record r
    Map.fromList <$> mapM parse fields
    where parse (k, v) = (ScoreT.Control k, ) <$> p_num v
    -- traverse p_num $ Map.mapKeys ScoreT.Control fields

p_backend :: Parse UiConfig.Backend
p_backend = UiConfig.Midi <$> p_midi_config

p_midi_config :: Parse Patch.Config
p_midi_config = do
    initialization <- fieldm "initialization" p_enum
    settings <- p_settings
    return $ Patch.Config
        { config_allocation = [] -- filled in from the top line
        , config_initialization = initialization
        , config_settings = settings
        }

p_settings :: Parse Patch.Settings
p_settings = do
    config_flags <- fieldm "flags" (fmap Set.fromList . p_list p_enum)
    config_scale <- fieldm "scale" p_scale
    config_decay <- fieldm "decay" (fmap RealTime.seconds . p_num)
    config_pitch_bend_range <- fieldm "pb_range" (p_pair p_int)
    config_control_defaults <- fieldm "control_defaults" p_controls
    return $ Patch.Settings
        { config_flags, config_scale, config_decay
        , config_pitch_bend_range, config_control_defaults
        }

-- | 'Patch.Scale's are from instrument patch definitions, ultimately from
-- Derive.Scale.  Unless it's useful to hand-tune, it's probably better to
-- have a global named registry like with scales, but too much bother right
-- now.
p_scale :: Record.RVal -> Parse Patch.Scale
p_scale val
    | Record.Record fields <- val
    , [("key_to_nn", nns), ("name", name), ("offset", offset)]
        <- Lists.sortOn fst fields
    = do
        -- These are often mostly -1, so abbreviate.
        offset <- floor <$> p_num offset
        nns <- p_list p_num nns
        let scale_key_to_nn = Vector.Unboxed.fromList $
                replicate offset (-1) <> nns
                <> replicate (len - offset - length nns) (-1)
        scale_name <- p_str name
        pure $ Patch.Scale { scale_name, scale_key_to_nn }
    | otherwise = throw "{name, key_to_nn}" val
    where
    len = 128

un_scale :: Patch.Scale -> Record.RVal
un_scale (Patch.Scale { scale_name, scale_key_to_nn }) = Record.Record
    [ ("name", Record.Val $ DeriveT.str scale_name)
    , ("offset", Record.Val $ int $ Vector.Unboxed.length pad)
    , ("key_to_nn", Record.Val $ DeriveT.VList $ map DeriveT.num $
        Vector.Unboxed.toList nns)
    ]
    where
    (pad, nns) = fmap (Vector.Unboxed.takeWhile (/= -1)) $
        Vector.Unboxed.span (== -1) scale_key_to_nn

un_backend :: UiConfig.Backend -> [(Text, Record.RVal)]
un_backend = \case
    UiConfig.Midi config -> concat
        [ maybe_field "initialization" (Record.Val . un_enum)
            config_initialization
        , maybe_field "flags"
            (Record.Val . DeriveT.VList . map un_enum . Set.toList)
            config_flags
        , maybe_field "scale" un_scale config_scale
        , maybe_field "decay" (Record.Val . DeriveT.real_time) config_decay
        , maybe_field "pb_range" (\(a, b) -> Record.list [int a, int b])
            config_pitch_bend_range
        , maybe_field "control_defaults" un_controls config_control_defaults
        ]
        where
        Patch.Config { config_initialization, config_settings } = config
        Patch.Settings
            { config_flags, config_scale, config_decay
            , config_pitch_bend_range, config_control_defaults
            } = config_settings
    _ -> []
    -- if Nothing, I could emit a commented out line?

maybe_field :: text -> (a -> b) -> Maybe a -> [(text, b)]
maybe_field name convert mb_val = maybe [] ((:[]) . (name,) . convert) mb_val

fieldm :: Text -> (Record.RVal -> Parse a) -> Parse (Maybe a)
fieldm name parse = field name Nothing (fmap Just . parse)

field :: Text -> a -> (Record.RVal -> Parse a) -> Parse a
field name deflt parse =
    State.get >>= \fields -> case Map.lookup name fields of
        Nothing -> return deflt
        Just val -> do
            State.put (Map.delete name fields)
            annotate name (parse val)

p_record :: Record.RVal -> Parse [(Text, Record.RVal)]
p_record = \case
    Record.Record rec -> return rec
    a@(Record.Val _) -> throw "record" a

p_str :: Record.RVal -> Parse Text
p_str = \case
    Record.Val (DeriveT.VStr (Expr.Str s)) -> return s
    a -> throw "str" a

p_num :: Record.RVal -> Parse Double
p_num = \case
    Record.Val (DeriveT.VSignal (ScoreT.Typed _ sig))
        | Just a <- Signal.constant_val sig -> return a
    a -> throw "num" a

un_num :: Double -> Record.RVal
un_num = Record.Val . DeriveT.num

p_int :: Record.RVal -> Parse Int
p_int val = properFraction <$> p_num val >>= \case
    (n, 0) -> return n
    _ -> throw "int" val

p_enum :: forall a. (Enum a, Bounded a, Show a) => Record.RVal -> Parse a
p_enum val = p_str val >>= \str -> case lookup str to of
    Just a -> return a
    Nothing -> throw "enum" val
    where to = Lists.keyOn showt [minBound :: a ..]

un_enum :: Show a => a -> DeriveT.Val
un_enum = DeriveT.str . showt

p_list :: (Record.RVal -> Parse a) -> Record.RVal -> Parse [a]
p_list parse = \case
    Record.Val (DeriveT.VList vals) ->
        annotate "list" $ mapM (parse . Record.Val) vals
    a -> throw "list" a

p_pair :: (Record.RVal -> Parse a) -> Record.RVal -> Parse (a, a)
p_pair parse = \case
    Record.Val (DeriveT.VList [a, b]) ->
        annotate "pair" $ (,) <$> parse (Record.Val a) <*> parse (Record.Val b)
    a -> throw "list" a

-- | REnv.Vals are supposed to be the the parseable ones.  But the usual
-- parser produces DeriveT.Val, just a subset.  So this effectively narrows
-- the type.
p_val :: Record.RVal -> Parse REnv.Val
p_val = \case
    Record.Val val -> case REnv.demote val of
        Just val -> pure val
        Nothing -> Except.throwError
            ["val can't be converted to parseable: " <> ShowVal.show_val val]
    a@(Record.Record _) -> throw "val" a

un_val :: REnv.Val -> Record.RVal
un_val = Record.Val . REnv.promote

annotate :: Except.MonadError [e] m => e -> m a -> m a
annotate context = flip Except.catchError (Except.throwError . (context:))

throw :: Except.MonadError [Error] m => Text -> Record.RVal -> m a
throw expected val = Except.throwError
    ["expected " <> expected <> " but got " <> mconcat (Record.un_rval val)]

int :: Int -> DeriveT.Val
int = DeriveT.num . fromIntegral
