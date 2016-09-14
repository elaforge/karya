-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Calls for Carnatic gamakam.
module Derive.Call.India.Gamakam3 (
    pitch_calls, control_calls, note_calls
-- #ifdef TESTING
    , parse_sequence, Expr_(..), ResolvedExpr, PitchCall(..)
    , parse_sequence2, resolve_exprs
    , dyn_call_map, pitch_call_map, pitch_call_map2
-- #endif
) where
import qualified Control.Monad.State as State
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Map as Map
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam3"

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map
    [ (Parse.unparsed_call, c_sequence False)
    , (Parse.unparsed_call, c_sequence True)
    ]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.generator_call_map
    [(Parse.unparsed_call, c_dyn_sequence)]

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map [("sahitya", c_sahitya)]

-- * sequence

c_sequence :: Bool -> Derive.Generator Derive.Pitch
c_sequence parse2 = Derive.generator1 (module_ <> if parse2 then "a" else "")
    "sequence" mempty sequence_doc
    $ Sig.call ((,) <$> Sig.required "sequence" sequence_arg_doc <*> config_env)
    $ \(text, (transition, dyn_transition)) args -> do
        (start, end) <- Args.range_or_note_end args
        maybe_state <- get_state parse2 transition dyn_transition args
        case maybe_state of
            Nothing -> return mempty
            Just state -> do
                Result pitches dyns_d <- Derive.at start $
                    pitch_sequence parse2 (end - start) state text
                real_start <- Derive.real start
                real_end <- Derive.real end
                let dyns = dropWhile Signal.null $ DList.toList dyns_d
                -- Extend the first sample to the start time, otherwise I wind
                -- up with whatever the previous dyn was.
                let initial = case Signal.head =<< Seq.head dyns of
                        Just (x, y) | x > real_start ->
                            Signal.signal [(real_start, y)]
                        _ -> mempty
                unless (null dyns) $
                    ControlUtil.modify Controls.dynamic real_end
                        (mconcat (initial : dyns))
                return $ mconcat $ DList.toList pitches
    where
    config_env :: Sig.Parser (Typecheck.Normalized, Typecheck.Normalized)
    config_env = (,)
        <$> Sig.environ "transition" Sig.Both (Typecheck.Normalized 0.5)
            "Time for each pitch movement, in proportion of the total time\
            \ available."
        <*> Sig.environ "dyn-transition" Sig.Both (Typecheck.Normalized 1)
            "Time for each dyn movement, in proportion of the total time\
            \ available."

get_state :: Bool -> Typecheck.Normalized -> Typecheck.Normalized
    -> Derive.PassedArgs Derive.Pitch -> Derive.Deriver (Maybe State)
get_state parse2 transition dyn_transition args =
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (get_pitch (Args.start args)) $ \cur -> do
        prev_event <- Args.prev_note
        let prev_event_pitch = fmap snd . PSignal.last
                . Score.event_untransformed_pitch =<< prev_event
            prev_pitch = snd <$> Args.prev_pitch args
        maybe_prev <- Args.lookup_prev_pitch args
        maybe_next <- Args.lookup_next_note_pitch args
        return $ Just $ if parse2
            then State
                { state_from_pitch =
                    fromMaybe cur (prev_pitch <|> prev_event_pitch)
                , state_transition = transition
                , state_current_pitch = cur
                , state_previous_pitch = fromMaybe cur maybe_prev
                , state_next_pitch = fromMaybe cur maybe_next
                , state_dyn = DynState
                    { state_from_dyn =
                        fromMaybe 1 $ lookup_last_dyn =<< prev_event
                    , state_dyn_transition = dyn_transition
                    }
                }
            else State
                { state_from_pitch = cur
                , state_transition = transition
                , state_current_pitch = cur
                , state_previous_pitch =
                    fromMaybe cur (prev_pitch <|> prev_event_pitch)
                , state_next_pitch = fromMaybe cur maybe_next
                , state_dyn = DynState
                    { state_from_dyn =
                        fromMaybe 1 $ lookup_last_dyn =<< prev_event
                    , state_dyn_transition = dyn_transition
                    }
                }
    where
    get_pitch = Derive.pitch_at <=< Derive.real

lookup_last_dyn :: Score.Event -> Maybe Signal.Y
lookup_last_dyn event = do
    sig <- Map.lookup Controls.dynamic $
        Score.event_untransformed_controls event
    snd <$> Signal.last (Score.typed_val sig)

sequence_doc :: Doc.Doc
sequence_doc = "doc doc\
    \ Currently the transition curve is hardcoded to a sigmoid curve, but\
    \ I could add a curve env var if necessary."

sequence_arg_doc :: Doc.Doc
sequence_arg_doc = "Abbreviated string of calls... TODO"

-- * dyn-sequence

c_dyn_sequence :: Derive.Generator Derive.Control
c_dyn_sequence = Derive.generator1 module_ "dyn-sequence" mempty doc
    $ Sig.call ((,) <$> Sig.required "sequence" arg_doc <*> config_env)
    $ \(text, dyn_transition) args -> do
        (start, end) <- Args.range_or_note_end args
        let state = DynState
                { state_from_dyn = maybe 0 snd (Args.prev_control args)
                , state_dyn_transition = dyn_transition
                }
        Derive.at start $ dyn_sequence (end - start) state text
    where
    doc = "doc doc"
    arg_doc = "blah blah"
    config_env :: Sig.Parser Typecheck.Normalized
    config_env = Sig.environ "dyn-transition" Sig.Both (Typecheck.Normalized 1)
        "Time for each dyn movement, in proportion of the total time available."

data DynState = DynState {
    state_from_dyn :: !Signal.Y
    , state_dyn_transition :: !Typecheck.Normalized
    } deriving (Show)

instance Pretty.Pretty DynState where
    format (DynState from_dyn dyn_transition) = Pretty.recordTitle "DynState"
        [ ("from_dyn", Pretty.format from_dyn)
        , ("dyn_transition", Pretty.format dyn_transition)
        ]

-- parsing:
-- <-->  <.5-->.2->
--
-- Like pitches, it's just a set of breakpoints.
--
-- I only allow numbers as args, and numbers always start with '.'.
-- Will that get in the way of extending with future calls?  They may want
-- non-numeric args, or at least >=1 args.
--
-- E.g. for impulse: [-]<>, or |>
--
-- To resolve ambiguity I can use spaces: .1>.5 .1>.5
-- Why does this not work for pitch?  Because in dyn I'm using an explicit 'go
-- to' > call, while pitch notation just has a movement.
--
-- Also I need to retain similarity to the pitch call notation.  That notation
-- allows any arg as long as it doesn't have 'valid_dcall_char's in it.

dyn_sequence :: ScoreTime -> DynState -> Text -> Derive.Deriver Signal.Control
dyn_sequence dur state arg = do
    exprs <- Derive.require_right (("parsing " <> showt arg <> ": ")<>) $
        resolve_dyn_exprs =<< parse_dyn_sequence arg
    let starts = slice_time dur (replicate (length exprs) 1)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM eval_dyn (zip ranges exprs)) state
    return $ mconcat results

type DynSequenceExpr call = (call, Text, Text)

eval_dyn :: ((ScoreTime, ScoreTime), DynSequenceExpr (Text, DynCall))
    -> M DynState Signal.Control
eval_dyn ((start, end), ((name_, DynCall _ sig1 sig2 func), arg1, arg2)) = do
    let name = Derive.CallName name_
    let ctx = Context
            { ctx_start = start
            , ctx_end = end
            , ctx_call_name = name
            }
    parsed_arg1 <- parse_args name arg1 sig1
    parsed_arg2 <- parse_args name arg2 sig2
    func parsed_arg1 parsed_arg2 ctx

-- ** parse

parse_dyn_sequence :: Text -> Either Text [DynSequenceExpr Text]
parse_dyn_sequence = ParseText.parse p_dyn_exprs

p_dyn_exprs :: Parser [DynSequenceExpr Text]
p_dyn_exprs = A.skipSpace *> A.many1 (p_dyn_expr <* A.skipSpace)

p_dyn_expr :: Parser (DynSequenceExpr Text)
p_dyn_expr = p_flat <|> p_expr
    where
    p_flat = do
        A.char '-'
        return ("-", "", "")
    p_expr = do
        arg1 <- A.takeWhile valid_dcall_arg
        sym <- A.takeWhile1 valid_dcall_char
        arg2 <- A.takeWhile valid_dcall_arg
        return (sym, arg1, arg2)

resolve_dyn_exprs :: [DynSequenceExpr Text]
    -> Either Text [DynSequenceExpr (Text, DynCall)]
resolve_dyn_exprs = mapM $ \(name, arg1, arg2) ->
    case Map.lookup name dyn_call_map of
        Nothing -> Left $ "dyn call not found: " <> showt name
        Just call -> return ((name, call), arg1, arg2)

-- * State

type M s a = State.StateT s Derive.Deriver a

data State = State {
    state_from_pitch :: !PSignal.Pitch
    , state_transition :: !Typecheck.Normalized
    , state_current_pitch :: !PSignal.Pitch
    , state_previous_pitch :: !PSignal.Pitch
    , state_next_pitch :: !PSignal.Pitch
    , state_dyn :: !DynState
    }

instance Pretty.Pretty State where
    format (State from_pitch transition cur prev next dyn) =
        Pretty.recordTitle "State"
            [ ("from_pitch", Pretty.format from_pitch)
            , ("transition", Pretty.format transition)
            , ("current_pitch", Pretty.format cur)
            , ("previous_pitch", Pretty.format prev)
            , ("next_pitch", Pretty.format next)
            , ("dyn", Pretty.format dyn)
            ]

set_pitch :: PSignal.Pitch -> M State ()
set_pitch p = State.modify $ \state -> state { state_from_pitch = p }

get_from :: M State PSignal.Pitch
get_from = State.gets state_from_pitch

-- * sequence

data Result =
    Result !(DList.DList PSignal.PSignal) !(DList.DList Signal.Control)
    deriving (Show)

instance Monoid Result where
    mempty = Result mempty mempty
    mappend (Result pitch1 dyn1) (Result pitch2 dyn2) =
        Result (pitch1<>pitch2) (dyn1<>dyn2)

pitch_sequence :: Bool -> ScoreTime -> State -> Text -> Derive.Deriver Result
pitch_sequence parse2 dur state arg = do
    exprs <- Derive.require_right (("parsing " <> showt arg <> ": ")<>) $
        resolve_extensions =<< resolve_exprs parse2
        =<< if parse2 then parse_sequence2 arg else parse_sequence arg
    let starts = slice_time dur (expr_durations exprs)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM eval (zip_exprs ranges exprs)) state
    return $ mconcat results

slice_time :: ScoreTime -> [Double] -> [ScoreTime]
slice_time dur slices = scanl (+) 0 $ map ((*one) . ScoreTime.double) slices
    where one = dur / ScoreTime.double (sum slices)

eval :: Expr_ (Text, DynCall) ((ScoreTime, ScoreTime), PitchCall)
    -> M State Result
eval (PitchExpr ((start, end), pcall) arg_) = case pcall_call pcall of
    PCall signature func -> do
        parsed_arg <- parse_args (ctx_call_name ctx) arg signature
        (\p -> Result (DList.singleton p) mempty) <$> func parsed_arg ctx
    where
    arg = pcall_arg pcall arg_
    ctx = Context
        { ctx_start = start
        , ctx_end = end
        , ctx_call_name = Derive.CallName $ Text.cons (pcall_name pcall) arg_
        }
eval (Group exprs) = concatMapM eval exprs
eval (DynExpr (name, call) arg1 arg2 exprs) = case (start, end) of
    (Just start, Just end) -> do
        state <- State.get
        (dyn, dyn_state) <- lift $ State.runStateT
            (eval_dyn ((start, end), ((name, call), arg1, arg2)))
            (state_dyn state)
        State.put $ state { state_dyn = dyn_state }
        pitch <- concatMapM eval exprs
        return $ Result mempty (DList.singleton dyn) <> pitch
    _ -> return mempty
    where
    start = Monoid.getFirst $
        foldMap (foldMap (Monoid.First . Just . fst . fst)) exprs
    end = Monoid.getLast $
        foldMap (foldMap (Monoid.Last . Just . snd . fst)) exprs

-- | DynExpr Name Arg1 Arg2 exprs | PitchExpr Name Arg
data Expr_ dyn pitch = DynExpr !dyn !Text !Text ![Expr_ dyn pitch]
    | PitchExpr !pitch !Text
    | Group [Expr_ dyn pitch]
    deriving (Eq, Show, Functor, Foldable, Traversable)
type ResolvedExpr = Expr_ (Text, DynCall) PitchCall
type Expr = Expr_ Text Char

expr_durations :: [ResolvedExpr] -> [Double]
expr_durations = map pcall_duration . concatMap Foldable.toList

zip_exprs :: [a] -> [Expr_ dyn b] -> [Expr_ dyn (a, b)]
zip_exprs xs exprs = fst $ State.runState (traverse go exprs) xs
    where
    go (DynExpr c arg1 arg2 exprs) = DynExpr c arg1 arg2 <$> traverse go exprs
    go (Group exprs) = Group <$> traverse go exprs
    go (PitchExpr call arg) = do
        x : xs <- State.get
        State.put xs
        return $ PitchExpr (x, call) arg

parse_sequence :: Text -> Either Text [Expr]
parse_sequence = ParseText.parse p_exprs

parse_sequence2 :: Text -> Either Text [Expr]
parse_sequence2 = ParseText.parse p_exprs2

resolve_exprs :: Bool -> [Expr] -> Either Text [ResolvedExpr]
resolve_exprs parse2 = concatMapM resolve
    where
    resolve (DynExpr c a1 a2 exprs) = case Map.lookup c dyn_call_map of
        Nothing -> Left $ "dynamic call not found: " <> showt c
        Just call -> do
            resolved <- resolve_exprs parse2 exprs
            return [DynExpr (c, call) a1 a2 resolved]
    resolve (Group exprs) =
        map (modify_duration (* (1 / fromIntegral (length exprs)))) <$>
            concatMapM resolve exprs
    resolve (PitchExpr c arg) = case Map.lookup c pcall_map of
        Nothing -> Left $ "pitch call not found: " <> showt c
        -- Apply the same argument to all of them.  But I should only get
        -- multiple PitchExprs for aliases, which expect no argument.
        Just calls -> Right [PitchExpr c arg | c <- calls]
    pcall_map = if parse2 then pitch_call_map2 else pitch_call_map

resolve_extensions :: [ResolvedExpr] -> Either Text [ResolvedExpr]
resolve_extensions = resolve <=< check_no_args
    where
    resolve [] = Right []
    resolve (expr : exprs)
        | is_extension expr = Left "extension with no preceding call"
        | otherwise = (modify_duration (+ fromIntegral (length pre)) expr :)
            <$> resolve post
        where (pre, post) = span is_extension exprs
    check_no_args exprs
        | null errs = Right exprs
        | otherwise =
            -- The parser won't parse 1_2 anyway, but let's check anyway.
            Left $ "_ calls can't have args: " <> Text.intercalate ", " errs
        where errs = concatMap has_arg exprs
    has_arg expr@(PitchExpr _ arg)
        | is_extension expr && arg /= mempty = [arg]
        | otherwise = []
    has_arg (Group exprs) = concatMap has_arg exprs
    has_arg _ = []
    is_extension (PitchExpr call _) = pcall_name call == extend_name
    is_extension _ = False

modify_duration :: (Double -> Double) -> ResolvedExpr -> ResolvedExpr
modify_duration modify = fmap $ \call ->
    if pcall_duration call > 0
        then call { pcall_duration = modify (pcall_duration call) }
        else call

-- * DynCall

data DynCall = forall a b. DynCall {
    _dcall_doc :: Text
    , _dcall_signature1 :: Sig.Parser a
    , _dcall_signature2 :: Sig.Parser b
    , _dcall_func :: a -> b -> Context -> M DynState Signal.Control
    }

dyn_call_map :: Map.Map Text DynCall
dyn_call_map = Map.fromList $ (("-", dc_flat) :) $
    validate_names $ moves ++
        [ ("=>", dc_attack)
        ]
    where
    moves = speed_permutations
        [ ("<", dc_move LT)
        , (">", dc_move GT)
        , ("=", dc_move EQ)
        ]
    validate_names = map $ first $ \name ->
        if Text.all valid_dcall_char name then name
            else errorStack $ "invalid name: " <> showt name
    speed_permutations calls =
        [ (name <> suffix, call speed)
        | (name, call) <- calls, (speed, suffix) <- speeds
        ]
    speeds = [(Nothing, ""), (Just Fast, "^"), (Just Medium, "="),
        (Just Slow, "_")]

dc_flat :: DynCall
dc_flat = DynCall "No movement." Sig.no_args Sig.no_args $ \() () ctx -> do
    prev <- State.gets state_from_dyn
    start <- lift $ Derive.real (ctx_start ctx)
    return $ Signal.signal [(start, prev)]

dc_move :: Ordering -> Maybe TransitionTime -> DynCall
dc_move ord speed = DynCall doc sig1 sig2 $ \from to args -> do
    (start, end) <- ctx_range args
    -- TODO these could come from an environ value
    transition <- case speed of
        Just Fast -> return $ Typecheck.Normalized 0.1
        Just Medium -> return $ Typecheck.Normalized 0.5
        Just Slow -> return $ Typecheck.Normalized 1
        Nothing -> State.gets state_dyn_transition
    from <- maybe get_from return from
    to <- ($to) $ case ord of
        LT -> return . fromMaybe 1
        GT -> return . fromMaybe 0
        -- 1 should be unreached since sig2 is required when ord == EQ.
        EQ -> return . fromMaybe 1
    move_dyn transition ord start from end to
    where
    get_from = State.gets state_from_dyn
    sig1 = Sig.defaulted "from" Nothing "From value."
    sig2 = case ord of
        EQ -> Just <$> Sig.required "to" "To value."
        _ -> Sig.defaulted "to" Nothing "To value."

    doc = "Hi, doc\
        \ < -> from prev, to 1, align to start\
        \ > -> from prev, to 0, align to end\
        \ = -> from prev, to arg, align to middle"

-- | This is like 'dc_move', but with default defaults for the args.
dc_attack :: DynCall
dc_attack = DynCall "Impulse for note attack."
    (Sig.defaulted "from" 1 "From value.")
    (Sig.defaulted "to" 0.5 "To value.") $ \from to ctx -> do
        (start, end) <- ctx_range ctx
        move_dyn (Typecheck.Normalized 0.5) EQ start from end to

move_dyn :: Typecheck.Normalized -> Ordering -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> M DynState Signal.Control
move_dyn (Typecheck.Normalized transition) align start from end to = do
    let curve = snd ControlUtil.sigmoid_curve $ case align of
            LT -> (0, weight)
            GT -> (weight, 0)
            EQ -> (weight, weight)
        weight = 1 - transition
    State.modify $ \state -> state { state_from_dyn = to }
    lift $ ControlUtil.make_segment curve start from end to

-- * PitchCall

data PitchCall = PitchCall {
    -- TODO take this out, as with DynCall
    -- then I can merge PCall into this, as with DynCall
    pcall_name :: !Char
    , _pcall_doc :: !Text
    , pcall_duration :: !Double
    -- | If True, cons 'pcall_name' on to the arg before parsing it.
    , pcall_parse_call_name :: !Bool
    , pcall_call :: !PCall
    }

pcall_arg :: PitchCall -> Text -> Text
pcall_arg pcall arg
    | pcall_parse_call_name pcall = Text.cons (pcall_name pcall) arg
    | otherwise = arg

data PCall = forall a. PCall {
    _pcall_signature :: Sig.Parser a
    , _pcall_func :: a -> Context -> M State PSignal.PSignal
    }

pitch_call_map :: Map.Map Char [PitchCall]
pitch_call_map = resolve $ Map.unique $ concat
    -- relative motion
    [ [ pcall '0' "Hold flat pitch." pc_flat
      , parse_name $ pcall '-' "Negative relative motion." (pc_relative False)
      ]
    , [parse_name $ pcall c "Relative motion." (pc_relative False)
        | c <- "123456789"]
    , [ alias 'b' ["-2"], alias 'a' ["-1"]
      , alias 'y' ["-1nn"], alias 'z' ["1nn"] -- relative motion by NN
      ]
    -- absolute motion
    -- I actually wanted to implement motion relative to the base pitch, but
    -- couldn't think of a nice mnemonic for the names.  In any case, maybe
    -- swaram-absolute is actually more intuitive.
    , [ pcall c "Absolute motion to swaram." (pc_absolute pc)
      | (pc, c) <- zip [0..] ("srgmpdn" :: [Char])
      ]
    , [ pcall 'c' "Absolute motion to current pitch."
        (pc_move_direction Current)
      , pcall 'v' "Absolute motion to next pitch." (pc_move_direction Next)
      -- compound motion
      , alias 'u' ["-1", "1"]
      , alias 'h' ["1", "-1"] -- single turn
      , pcall 'j' "Janta." pc_janta

      -- set config
      , config extend_name "Extend the duration of the previous call." pc_extend
      , config '<' "Set from pitch to previous." (pc_set_pitch Previous)
      , config '^' "Set from pitch to current." (pc_set_pitch Current)
      , config 'P' "Set from pitch to relative steps."
        (pc_set_pitch_relative False)
      , config 'F' "Fast transition time." (pc_set_transition_time Fast)
      , config 'M' "Medium transition time." (pc_set_transition_time Medium)
      , config 'S' "Slow transition time." (pc_set_transition_time Slow)
      ]
    ]
    where
    resolve (calls, duplicates)
        | null duplicates = either errorStack id (resolve_aliases calls)
        | otherwise =
            errorStack $ "duplicate calls: " <> pretty (map fst duplicates)
    parse_name = second $ second $ \g -> g { pcall_parse_call_name = True }
    alias name to = (name, Left to)
    pcall name doc c = (name, Right $ PitchCall name doc 1 False c)
    config name doc c = (name, Right $ PitchCall name doc 0 False c)

pitch_call_map2 :: Map.Map Char [PitchCall]
pitch_call_map2 = resolve $ Map.unique $ concat
    [ [pcall '=' "Hold flat pitch." pc_flat]
    -- relative motion
    , [parse_name $ pcall c "Relative motion." (pc_relative True)
        | c <- "0123456789"]
    , [parse_name $ pcall '-' "Negative relative motion." (pc_relative True)]
    -- 'd' would conflict with absolute motion
    , [alias c [showt n] | (c, n) <- zip "abc" [-1, -2 ..]]
    -- , [ alias 'a' ["-1"], alias 'b' ["-2"], alias 'c'
    --     -- TODO does't work if arg is just 1 character
    --   -- , alias 'y' ["-1nn"], alias 'z' ["1nn"] -- relative motion by NN
    --   ]
    -- absolute motion

    , [pcall 'e' "Pitch up by a little." (pc_relative_move (Pitch.Nn 1))]

    , [ pcall c "Absolute motion to swaram." (pc_absolute pc)
      | (pc, c) <- zip [0..] ("srgmpdn" :: [Char])
      ]
    , [ pcall 'h' "Absolute motion to current pitch."
        (pc_move_direction Current)
      , pcall 'v' "Absolute motion to next pitch." (pc_move_direction Next)

      -- set config
      , config extend_name "Extend the duration of the previous call." pc_extend
      , config '<' "Set from pitch to previous." (pc_set_pitch Previous)
      , config '^' "Set from pitch to current." (pc_set_pitch Current)
      , config 'P' "Set from pitch to relative steps."
        (pc_set_pitch_relative False)
      , config 'T' "Set from pitch relative to swaram."
        (pc_set_pitch_relative True)
      , config 'F' "Fast transition time." (pc_set_transition_time Fast)
      , config 'M' "Medium transition time." (pc_set_transition_time Medium)
      , config 'S' "Slow transition time." (pc_set_transition_time Slow)
      ]
    ]
    where
    resolve (calls, duplicates)
        | null duplicates = either errorStack id (resolve_aliases calls)
        | otherwise =
            errorStack $ "duplicate calls: " <> pretty (map fst duplicates)
    parse_name = second $ second $ \g -> g { pcall_parse_call_name = True }
    alias name to = (name, Left to)
    pcall name doc c = (name, Right $ PitchCall name doc 1 False c)
    config name doc c = (name, Right $ PitchCall name doc 0 False c)

resolve_aliases :: Map.Map Char (Either [Text] PitchCall)
    -> Either Text (Map.Map Char [PitchCall])
resolve_aliases call_map = Map.fromList <$> mapM resolve (Map.toList call_map)
    where
    resolve (name, Right call) = Right (name, [call])
    resolve (name, Left calls) = (,) name <$> mapM resolve1 calls
    resolve1 to = do
        (c, arg) <- justErr "empty alias" $ Text.uncons to
        call <- justErr ("not found: " <> showt c) $ Map.lookup c call_map
        call <- first (("alias to alias: "<>) . showt) call
        Right $ apply_arg call arg

apply_arg :: PitchCall -> Text -> PitchCall
apply_arg call arg = call
    { pcall_call = case pcall_call call of
        PCall signature func -> PCall ignore $ \_ ctx -> do
            parsed <- parse_args (ctx_call_name ctx) (pcall_arg call arg)
                signature
            func parsed ctx
    }
    where
    -- Accept anything for an argument but ignore it.  This is because
    -- I've already hardcoded the argument, but 'eval' will want to apply it
    -- anyway, since it can't tell the difference from an alias call and
    -- a normal call.
    ignore = Sig.defaulted "ignore" (BaseTypes.num 0) ""

-- ** PitchCall implementation

parse_args :: State.MonadTrans m => Derive.CallName -> Text -> Sig.Parser a
    -> m Derive.Deriver a
parse_args name arg sig = lift $ do
    vals <- Derive.require_right (("parsing " <> showt name <> ": ") <>) $
        if Text.null arg then return [] else (:[]) <$> Parse.parse_val arg
    Sig.require_right
        =<< Sig.parse_vals sig (Derive.dummy_context 0 1 (pretty name))
            name vals

-- | Here I am reinventing Derive.Call yet again.  This is the equivalent of
-- 'Derive.Context' and 'Derive.PassedArgs'.
data Context = Context {
    ctx_start :: !ScoreTime
    , ctx_end :: !ScoreTime
    -- | Complete call name, first char consed to arg.
    , ctx_call_name :: !Derive.CallName
    } deriving (Show)

ctx_range :: Context -> M s (RealTime, RealTime)
ctx_range ctx = lift $
    (,) <$> Derive.real (ctx_start ctx) <*> Derive.real (ctx_end ctx)

-- | This is just a place holder, its effect is applied in 'resolve_extensions'.
pc_extend :: PCall
pc_extend = PCall Sig.no_args $ \() _ctx -> return mempty

extend_name :: Char
extend_name = '_'

pc_flat :: PCall
pc_flat = PCall Sig.no_args $ \() ctx -> do
    pitch <- get_from
    (start, end) <- ctx_range ctx
    return $ PSignal.signal [(start, pitch), (end, pitch)]

pc_relative :: Bool -> PCall
pc_relative swaram_relative = PCall (Sig.required "to" "To pitch.")
    $ \arg ctx -> case arg of
        Left (BaseTypes.Symbol sym)
            | sym == "-" -> do
                (start, end) <- ctx_range ctx
                pitch <- get_from
                return $ PSignal.signal [(start, pitch), (end, pitch)]
            | otherwise -> lift $ Derive.throw $ "unknown move: " <> showt sym
        Right (Typecheck.DefaultDiatonic transpose) -> do
            from_pitch <- if swaram_relative
                then State.gets state_current_pitch else get_from
            move_to ctx (Pitches.transpose transpose from_pitch)

pc_relative_move :: Pitch.Transpose -> PCall
pc_relative_move transpose = PCall Sig.no_args $ \() ctx -> do
    from_pitch <- get_from
    move_to ctx (Pitches.transpose transpose from_pitch)

data PitchDirection = Previous | Current | Next deriving (Show, Eq)
instance Pretty.Pretty PitchDirection where pretty = showt

get_direction_pitch :: PitchDirection -> M State PSignal.Pitch
get_direction_pitch dir = case dir of
    Previous -> State.gets state_previous_pitch
    Current -> State.gets state_current_pitch
    Next -> State.gets state_next_pitch

pc_move_direction :: PitchDirection -> PCall
pc_move_direction dir = PCall Sig.no_args $ \() ctx ->
    move_to ctx =<< get_direction_pitch dir

pc_absolute :: Pitch.PitchClass -> PCall
pc_absolute pc = PCall Sig.no_args $ \() ctx -> do
    from_pitch <- get_from
    to_pitch <- lift $ find_closest_pc (ctx_start ctx) pc from_pitch
    move_to ctx to_pitch

-- | Given a PitchClass and a previous pitch, infer the octave for the
-- PitchClass which is closest to the previous pitch.
find_closest_pc :: ScoreTime -> Pitch.PitchClass -> PSignal.Pitch
    -> Derive.Deriver PSignal.Pitch
find_closest_pc start pc pitch = do
    (from_note, to_note, _) <- Call.get_pitch_functions
    scale <- Call.get_scale
    pitch <- Call.parse_pitch from_note (PSignal.apply mempty pitch)
    let unwrapped = Pitch.pitch oct pc
        wrapped = Pitch.pitch
            (if pc >= Pitch.pitch_pc pitch then oct - 1 else oct + 1) pc
        oct = Pitch.pitch_octave pitch
    per_octave <- Derive.require "scale must have octaves" $
        Scale.pc_per_octave (Scale.scale_layout scale)
    let to_pitch = if distance unwrapped <= distance wrapped
            then unwrapped else wrapped
        distance p = abs (Pitch.subtract_pitch per_octave pitch p)
    Call.eval_note start =<< Derive.require "to_note" (to_note to_pitch)

pc_janta :: PCall
pc_janta = PCall Sig.no_args $ \() _ctx -> lift $ Derive.throw "TODO janta"

pc_set_pitch :: PitchDirection -> PCall
pc_set_pitch dir = PCall Sig.no_args $ \() _ctx -> do
    set_pitch =<< get_direction_pitch dir
    return mempty

pc_set_pitch_relative :: Bool -> PCall
pc_set_pitch_relative from_current = PCall (Sig.required "to" "To pitch.") $
    \arg _ctx -> do
        transpose <- lift $ parse_transpose arg
        set_pitch . Pitches.transpose transpose
            =<< if from_current then State.gets state_current_pitch
                else get_from
        return mempty

parse_transpose :: Either Pitch.Transpose BaseTypes.Symbol
    -> Derive.Deriver Pitch.Transpose
parse_transpose (Left t) = return t
parse_transpose (Right (BaseTypes.Symbol sym)) = case untxt sym of
    [c] | 'a' <= c && c <= 'z' -> return $ Pitch.Diatonic $ fromIntegral $
        fromEnum 'a' - fromEnum c - 1
    _ -> Derive.throw $ "expected a lowercase letter: " <> showt sym

data TransitionTime = Slow | Medium | Fast deriving (Show, Eq)

pc_set_transition_time :: TransitionTime -> PCall
pc_set_transition_time time = PCall Sig.no_args $ \() _ctx -> do
    State.modify $ \state -> state { state_transition = ttime }
    return mempty
    where
    -- TODO these could come from an environ value
    ttime = Typecheck.Normalized $ case time of
        Fast -> 0.1
        Medium -> 0.5
        Slow -> 0.9

-- ** util

move_to :: Context -> PSignal.Pitch -> M State PSignal.PSignal
move_to ctx pitch = do
    (start, end) <- ctx_range ctx
    from_pitch <- get_from
    move_pitch start from_pitch end pitch

move_pitch :: RealTime -> PSignal.Pitch -> RealTime -> PSignal.Pitch
    -> M State PSignal.PSignal
move_pitch start from_pitch end to_pitch = do
    Typecheck.Normalized transition <- State.gets state_transition
    let curve = snd ControlUtil.sigmoid_curve (1-transition, 1-transition)
    set_pitch to_pitch
    lift $ PitchUtil.make_segment curve start from_pitch end to_pitch

-- * parser

type Parser a = A.Parser a

p_exprs :: Parser [Expr]
p_exprs = concat <$> (A.skipSpace *> A.many1 (p_expr <* A.skipSpace))

p_expr :: Parser [Expr]
p_expr = ((:[]) <$> p_dyn_sub_expr p_exprs) <|> (A.char '!' *> p_compact_exprs)
    <|> ((:[]) <$> p_pitch_expr)

-- | Parse dyn call with a bracketed sub-expression.
p_dyn_sub_expr :: Parser [Expr] -> Parser Expr
p_dyn_sub_expr sub_expr = do
    A.char '['
    exprs <- sub_expr
    A.char ']'
    arg1 <- A.takeWhile valid_dcall_arg
    sym <- A.takeWhile1 valid_dcall_char
    arg2 <- A.takeWhile valid_dcall_arg
    return $ DynExpr sym arg1 arg2 exprs

p_compact_exprs :: Parser [Expr]
p_compact_exprs = A.many1 (p_dyn_sub_expr p_compact_exprs <|> p_compact_pitch)

p_compact_pitch :: Parser Expr
p_compact_pitch = PitchExpr <$> A.satisfy valid_pcall_char <*> pure ""

p_pitch_expr :: Parser Expr
p_pitch_expr = PitchExpr <$> A.satisfy (\c -> c /= '!' && valid_pcall_char c)
    <*> A.takeWhile valid_pcall_char

valid_pcall_char :: Char -> Bool
valid_pcall_char c = c /= '[' && c /= ']' && c /= ' '

valid_dcall_char :: Char -> Bool
valid_dcall_char c = valid_pcall_char c && c `elem` ("=<>^_" :: [Char])

valid_dcall_arg :: Char -> Bool
valid_dcall_arg c = Char.isDigit c || c == '.'

-- * parser2

p_exprs2 :: Parser [Expr]
p_exprs2 = A.skipSpace *> A.many1 (p_expr2 <* A.skipSpace)

p_expr2 :: Parser Expr
p_expr2 = p_group <|> p_pitch_expr2

p_group :: Parser Expr
p_group = Group <$> (A.char '[' *> p_exprs2 <* A.char ']')

p_pitch_expr2 :: Parser Expr
p_pitch_expr2 = do
    c <- A.satisfy $ \c -> c /=' ' && c /= '[' && c /= ']'
    if is_argument_call c
        then PitchExpr c <$> p_pitch_expr_arg
        else return $ PitchExpr c ""

p_pitch_expr_arg :: Parser Text
p_pitch_expr_arg = do
    minus <- A.option False (A.char '-' >> return True)
    c <- A.satisfy (/=' ')
    return $ (if minus then ("-"<>) else id) (Text.singleton c)

is_argument_call :: Char -> Bool
is_argument_call c = Char.isUpper c || c == '-'


-- * misc

c_sahitya :: Derive.Taggable a => Derive.Transformer a
c_sahitya = Derive.transformer module_ "sahitya" mempty
    "Ignore the transformed deriver. Put this on a track to ignore its\
    \ contents, and put in sahitya."
    $ Sig.call0t $ \_args _deriver -> return Stream.empty
