-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{- | This module defines basic tracklang types.

    They all have to pretty much be here to avoid circular imports.  But to
    avoid this module becoming even larger than it already is, subsets are
    re-exported from "Derive.Score" and "Derive.PSignal".  PSignal is types
    related directly to pitches.  Score is for types required for
    'Derive.Score.Event'.  There is a third subset, which is types related to
    'Val', which used to be re-exported from a TrackLang module, but are now
    intended to be imported directly from here.  I eventually got rid of
    TrackLang because it just added a few small utilities but no additional
    dependencies, and since modules started directly using BaseTypes anyway
    to avoid dependencies.  Many Score types are further divided into
    "Derive.ScoreTypes", once again to avoid circular imports.

    Perhaps the simplest would be to get rid of all the re-export guff.

    Here are the names for various aspects of signals:

    >           numbers                   pitches                 both
    > scalar    Signal.Y                  PSignal.Y
    > name      Score.Control             Score.PControl
    > signal    Signal.Control            PSignal.PSignal
    > ref       BaseTypes.ControlRef      BaseTypes.PControlRef   Ref
-}
module Derive.BaseTypes where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Char as Char
import qualified Data.Coerce as Coerce
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector

import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Attrs as Attrs
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- This file is pretty much unreducable, as far as dependencies go:
-- For TrackLang: ControlFunction -> Environ -> Val <- ControlFunction
-- For PSignal: PitchConfig -> Environ -> Val <- Pitch <- PitchConfig
--
-- So 'ControlFunction', 'Pitch', and 'Val' must all be together.  'Signal'
-- also gets dragged in, and winds up being everything in this file.

-- * Derive.PSignal

-- | A pitch signal is similar to a 'Signal.Control', except that its values
-- are 'Pitch'es instead of plain floating point values.
newtype PSignal = PSignal { sig_vec :: TimeVector.Boxed Pitch }
    deriving (Show, Pretty.Pretty)

instance Monoid PSignal where
    mempty = PSignal mempty
    mappend s1 s2
        | TimeVector.null (sig_vec s1) = s2
        | TimeVector.null (sig_vec s2) = s1
        | otherwise = mconcat [s1, s2]
    mconcat [] = mempty
    mconcat sigs = PSignal (TimeVector.merge (map sig_vec sigs))

instance DeepSeq.NFData PSignal where
    rnf (PSignal vec) = vec `seq` ()

{- | This is an untransposed pitch.  All pitches have transposition signals
    from the dynamic state applied when they are converted to MIDI or whatever
    backend.  So if I want the final concrete pitch, I have to apply the
    transposition signals.  But if I want to emit a note with this pitch,
    I want the untransposed one, or the transposition will be applied twice.
    I use a phantom type parameter to keep them straight.
-}
type Pitch = RawPitch Untransposed_
-- | The transposed version of 'Pitch'.
type Transposed = RawPitch Transposed_
data Transposed_
data Untransposed_

-- | A pitch is an abstract value that can generate a 'Pitch.NoteNumber' or
-- symbolic 'Pitch.Note'.
data RawPitch a = Pitch {
    pitch_eval_nn :: !(PitchConfig -> Either PitchError Pitch.NoteNumber)
    , pitch_eval_note :: !(PitchConfig -> Either PitchError Pitch.Note)
    , pitch_scale :: !Scale
    , pitch_config :: !PitchConfig
    }

-- | Make an abstract Pitch.
pitch :: Scale
    -> (PitchConfig -> Either PitchError Pitch.NoteNumber)
    -> (PitchConfig -> Either PitchError Pitch.Note)
    -> PitchConfig -> Pitch
pitch scale nn note config = Pitch
    { pitch_eval_nn = nn
    , pitch_eval_note = note
    , pitch_scale = scale
    , pitch_config = config
    }

coerce :: RawPitch a -> RawPitch b
coerce = Coerce.coerce

-- | Usually I only want to evaluate a fully transposed pitch.  Exceptions
-- are documented by applying 'coerce'.
pitch_nn :: Transposed -> Either PitchError Pitch.NoteNumber
pitch_nn pitch = do
    nn <- pitch_eval_nn pitch (pitch_config pitch)
    first (annotate_out_of_range pitch (Just nn)) $
        if 0 <= nn && nn <= 127 then return nn else Left out_of_range

-- | Usually I only want to evaluate a fully transposed pitch.  Exceptions
-- are documented by applying 'coerce'.
pitch_note :: Transposed -> Either PitchError Pitch.Note
pitch_note pitch = first (annotate_out_of_range pitch Nothing) $
    pitch_eval_note pitch (pitch_config pitch)

annotate_out_of_range :: RawPitch a -> Maybe Pitch.NoteNumber -> PitchError
    -> PitchError
annotate_out_of_range pitch maybe_nn (OutOfRange _ old) =
    OutOfRange maybe_nn (filtered <> old)
    where
    filtered = Map.filterWithKey (\k v -> k `Set.member` transposers && v /= 0)
        cmap
    PitchConfig _ cmap = pitch_config pitch
    transposers = pscale_transposers (pitch_scale pitch)
annotate_out_of_range _ _ err = err

{- | A PitchConfig is the data that can continue to influence the pitch's
    frequency.

    Pitches are configured by controls and by an environ.  The controls
    are for values that change over time, such as transposition or tuning.
    They're combined additively, which is really only appropriate for
    transposition.  Controls are mostly applied only on conversion to the
    performer.  TODO I don't entirely remember why.  However, this leads to
    some trickiness because if I want to compare a pitch to an absolute
    NoteNumber, I need the final transposed value, but if I put it in an event
    it must be untransposed, or transposition will be applied twice.
    To avoid double.  To avoid this, there's a phantom type parameter to
    distinguish an untransposed 'Pitch' from a 'Transposed' one.

    The Environ is for symbolic configuration, such as key or tuning mode.
    Unlike controls, though, it's taken from the environ in scope when the pith
    is created.  Otherwise, you can't evaluate a pitch with a different key by
    setting the environ.
-}
data PitchConfig = PitchConfig !Environ !ScoreTypes.ControlValMap
    deriving (Show)

instance Monoid PitchConfig where
    mempty = PitchConfig mempty mempty
    mappend (PitchConfig env1 c1) (PitchConfig env2 c2) =
        PitchConfig (env1 <> env2) (Map.unionWith (+) c1 c2)

-- | PSignal can't take a Scale because that would be a circular import.
-- Fortunately it only needs a few fields.  However, because of the
-- circularity, the Scale.Scale -> PSignal.Scale constructor is in
-- "Derive.Derive".
data Scale = Scale {
    -- | It can be useful to see the scale of a pitch, e.g. to create more
    -- pitches in the same scale as an existing pitch.
    pscale_scale_id :: !Pitch.ScaleId
    -- | The set of transposer signals for this scale, as documented in
    -- 'Derive.Scale.scale_transposers'.
    --
    -- They are stored here because they're needed by 'to_nn'.  I could
    -- store them separately, e.g. in the 'Score.Event' alongside the
    -- event_pitch, but the scale at event creation time is not guaranteed to
    -- be the same as the one when the pitch was created, so the safest thing
    -- to do is keep it with the pitch itself.
    , pscale_transposers :: !(Set.Set ScoreTypes.Control)
    } deriving (Show)

instance Pretty.Pretty Scale where
    pretty = pretty . pscale_scale_id

-- | It can't be reduced since it has lambdas, but at least this way you can
-- easily rnf things that contain it.
instance DeepSeq.NFData (RawPitch a) where
    rnf _ = ()

instance Show (RawPitch a) where
    -- Show just the NN, so this is parseable by Util.PPrint.
    show p = either show prettys (pitch_nn (coerce p))

-- | Will look like: 62.95nn,4i(*wayang)
instance Pretty.Pretty (RawPitch a) where
    pretty p = either showt pretty (pitch_nn (coerce p)) <> ","
        <> either showt Pitch.note_text (pitch_note (coerce p))
        <> "(" <> pretty (pitch_scale p) <> ")"

-- | Pitches have no literal syntax, but I have to print something.
instance ShowVal.ShowVal (RawPitch a) where
    show_val pitch = "<pitch: " <> pretty pitch <> ">"

-- | Things that can go wrong evaluating a pitch.
data PitchError =
    UnparseableNote
    -- | Note out of the scale's range.  The values are transpositions from
    -- the environment, in case it was out of range because of a transposition.
    --
    -- Some scales have a restricted range, in which case they should throw
    -- 'out_of_range', which 'pitch_nn' and 'pitch_note' will annotate with the
    -- transposition signals.  Other scales have unlimited range, in which case
    -- they're limited by the backend.  In this case 'pitch_nn' checks 0--127,
    -- which happens to be MIDI's limitation.
    | OutOfRange !(Maybe Pitch.NoteNumber) !ScoreTypes.ControlValMap
    -- | Input note doesn't map to a scale note.
    | InvalidInput
    -- | A required environ value was missing or had the wrong type or value.
    -- The Text is a 'ShowVal.show_val' of the wrong Val.
    | EnvironError !Key !Text
    -- | The scale doesn't implement that operation.
    | NotImplemented
        -- The Text should be Val except that makes Eq not work.
    -- | Other kind of error.
    | PitchError !Text
    deriving (Eq, Ord, Show)

out_of_range :: PitchError
out_of_range = OutOfRange Nothing mempty

instance Pretty.Pretty PitchError where
    pretty err = case err of
        UnparseableNote -> "unparseable note"
        OutOfRange nn vals -> maybe "" ((<>" is ") . pretty) nn
            <> "out of range"
            <> if vals == mempty then "" else ": " <> pretty vals
        InvalidInput -> "invalid input"
        EnvironError key err ->
            "environ value for " <> pretty key <> ": " <> err
        NotImplemented -> "not implemented"
        PitchError msg -> msg

instance Pretty.Pretty PitchConfig where
    format (PitchConfig env controls) = Pretty.record "PitchConfig"
        [ ("environ", Pretty.format env)
        , ("controls", Pretty.format controls)
        ]

pitches_equal :: RawPitch a -> RawPitch a -> Bool
pitches_equal p1 p2 = either (const False) id $
    Pitch.nns_equal <$> pitch_nn (coerce p1) <*> pitch_nn (coerce p2)

-- * Duration

-- | Some calls can operate in either RealTime or ScoreTime.
data Duration = RealDuration RealTime
    | ScoreDuration ScoreTime
    deriving (Eq, Show)

instance ShowVal.ShowVal Duration where
    show_val (RealDuration x) = ShowVal.show_val x
    show_val (ScoreDuration x) = ShowVal.show_val x

instance Pretty.Pretty Duration where
    pretty (RealDuration t) = pretty t
    pretty (ScoreDuration t) = pretty t

-- | Duration can't be in Fractional since you can't multiple a RealDuration
-- by a ScoreDuration, but scaling operations are still useful.
multiply_duration :: Duration -> Double -> Duration
multiply_duration (RealDuration t) n = RealDuration (t * RealTime.seconds n)
multiply_duration (ScoreDuration t) n = ScoreDuration (t * ScoreTime.double n)

-- * tracklang types

newtype Environ = Environ (Map.Map Key Val)
    deriving (Show, Monoid, Pretty.Pretty, DeepSeq.NFData)

-- | Insert a val directly, with no typechecking.
insert :: Key -> Val -> Environ -> Environ
insert name val (Environ env) = Environ $ Map.insert name val env

lookup :: Key -> Environ -> Maybe Val
lookup name (Environ env) = Map.lookup name env

-- | Symbols to look up a val in the 'ValMap'.
type Key = Symbol

-- | Call used by the infix @=@ syntax.
c_equal :: CallId
c_equal = "="

-- ** Val

-- | This is the type of first class values in the tracklang.  It's main
-- purpose is the type for arguments to tracklang calls, and val calls' return
-- type.
data Val =
    -- | A number with an optional type suffix.  It also has a ratio style
    -- literal, though the output is still a floating point value, not a true
    -- ratio.
    --
    -- Literal: @42.23@, @-.4@, @1c@, @-2.4d@, @3/2@, @-3/2@, @0x7f@.
    VNum !ScoreTypes.TypedVal
    -- | A set of Attributes for an instrument.
    --
    -- Literal: @+attr@, @+attr1+attr2@.
    | VAttributes !Attrs.Attributes

    -- | A control name.  An optional value gives a default if the control
    -- isn't present.
    --
    -- Literal: @%control@, @%control,.4@
    | VControlRef !ControlRef
    -- | If a control name starts with a *, it denotes a pitch signal and the
    -- scale is taken from the environ.  Unlike a control signal, the empty
    -- string is a valid signal name and means the default pitch signal.
    --
    -- Literal: @\#pitch@, @(# pitch (4c))@
    | VPControlRef !PControlRef

    -- | No literal, but is returned from val calls, notably scale calls.
    | VPitch !Pitch

    -- | A parsed 'Pitch.Note'.  This is useful for things for which a textual
    -- 'Pitch.Note' is too high level and a numerical 'Pitch.NoteNumber' is too
    -- low level, like instrument ranges.
    --
    -- Literal: @(pitch 4 0 1)@ -> 4c#.
    | VNotePitch !Pitch.Pitch
    -- | Sets the instrument in scope for a note.  An empty instrument doesn't
    -- set the instrument, but can be used to mark a track as a note track.
    --
    -- Literal: @>@, @>inst@
    | VInstrument !ScoreTypes.Instrument

    -- | A string, which is interpreted as a call if it's at the front of an
    -- expression.  Parsing a symbol is somewhat complicated.  If it occurs
    -- at the front of an expression, it can have anything in it except
    -- spaces or parens: 'Derive.ParseBs.p_call_symbol'.  If it's in the
    -- argument position, it can be surrounded with single quotes and contain
    -- anything, and a single quote is encoded as two single quotes:
    -- 'Derive.ParseBs.p_string'.  Or if it starts with a hyphen, letter, or
    -- @*@, it doesn't need quotes at all: 'Derive.ParseBs.p_symbol'.
    --
    -- Literal: @func@, @\'hello\'@, @\'quinn\'\'s hat\'@
    | VSymbol !Symbol

    -- | A quoted expression.  Quoted calls are resolved by "Derive.Sig" when
    -- it typechecks arguments.  This way you can set an argument default to
    -- an expression that will be evaluated every time the call occurs.
    -- Derive.Sig expects that the expression is a valid val call, which means
    -- no pipes.
    --
    -- Literal: @\"(a b c)@
    | VQuoted !Quoted
    | VControlFunction !ControlFunction
    -- | An explicit not-given arg for functions so you can use positional
    -- args with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    -- | A token used as a separator when calls want to parse their argument
    -- lists via their own complicated means.
    --
    -- Literal: @;@
    | VSeparator
    -- | List of values.
    --
    -- Literal: @(list)@, @(list 1 2)@, @(list (x) (y))@
    | VList ![Val]
    deriving (Show)

-- | Return Nothing if the Vals can't be compared, and whether or not they're
-- equal otherwise.
vals_equal :: Val -> Val -> Maybe Bool
vals_equal x y = case (x, y) of
    (VNum a, VNum b) -> Just $ a == b
    (VAttributes a, VAttributes b) -> Just $ a == b
    (VControlRef a, VControlRef b) -> Just $ a == b
    -- This could use pitches_equal, but don't bother until I have a need for
    -- it.
    (VPControlRef _, VPControlRef _) -> Nothing
    (VPitch a, VPitch b) -> Just $ pitches_equal a b
    (VNotePitch a, VNotePitch b) -> Just $ a == b
    (VInstrument a, VInstrument b) -> Just $ a == b
    (VSymbol a, VSymbol b) -> Just $ a == b
    (VQuoted (Quoted a), VQuoted (Quoted b)) ->
        lists_equal calls_equal (NonEmpty.toList a) (NonEmpty.toList b)
    (VControlFunction _, VControlFunction _) -> Nothing
    (VNotGiven, VNotGiven) -> Just True
    (VSeparator, VSeparator) -> Just True
    (VList a, VList b) -> lists_equal vals_equal a b
    _ -> Nothing

lists_equal :: (a -> a -> Maybe Bool) -> [a] -> [a] -> Maybe Bool
lists_equal eq = go
    where
    go (a:as) (b:bs) = maybe Nothing
        (\t -> if t then go as bs else Just False) (eq a b)
    go _ _ = Just False

-- | This instance is actually invalid due to showing VPitch, which has no
-- literal, and for 'Val', showing 'PControlRef', which amounts to the same
-- thing.  I use this to treat any Val as a Symbol to re-evaluate it.  Being
-- invalid means that a VPitch or VPControlRef with a default will cause
-- a parse failure, but I'll have to see if this becomes a problem in practice.
instance ShowVal.ShowVal Val where
    show_val val = case val of
        VNum d -> ShowVal.show_val d
        VAttributes attrs -> ShowVal.show_val attrs
        VControlRef control -> ShowVal.show_val control
        VPControlRef control -> ShowVal.show_val control
        VPitch pitch -> ShowVal.show_val pitch
        VNotePitch pitch -> ShowVal.show_val pitch
        VInstrument inst -> ShowVal.show_val inst
        VSymbol sym -> ShowVal.show_val sym
        VQuoted quoted -> ShowVal.show_val quoted
        VControlFunction f -> ShowVal.show_val f
        VNotGiven -> "_"
        VSeparator -> ";"
        VList vals -> ShowVal.show_val vals

instance Pretty.Pretty Val where
    pretty = ShowVal.show_val

instance DeepSeq.NFData Val where
    rnf (VNum d) = DeepSeq.rnf d
    rnf (VSymbol (Symbol s)) = DeepSeq.rnf s
    rnf _ = ()

newtype Quoted = Quoted Expr deriving (Show)

-- | Unlike Exprs in general, a Quoted Expr should be representable with
-- show_val.  This is because a Quoted has only been parsed, not evaluated,
-- so it shouldn't have anything unshowable, like pitches.
instance ShowVal.ShowVal Quoted where
    show_val (Quoted expr) = "\"(" <> ShowVal.show_val expr <> ")"
instance Pretty.Pretty Quoted where pretty = ShowVal.show_val

newtype Symbol = Symbol Text
    deriving (Eq, Ord, Read, Show, DeepSeq.NFData, String.IsString,
        Serialize.Serialize)
instance Pretty.Pretty Symbol where pretty = ShowVal.show_val

unsym :: Symbol -> Text
unsym (Symbol sym) = sym

instance ShowVal.ShowVal Symbol where
    -- TODO This is actually kind of error prone.  The problem is that symbols
    -- at the beginning of an expression are parsed as-is and cannot have
    -- quotes.  Only ones as arguments need quotes.  Symbols are rarely
    -- arguments, but strings frequently are.  Maybe I should go back to
    -- separate types for symbols and strings?
    show_val (Symbol s)
        | parseable = s
        | otherwise = "'" <> Text.concatMap quote s <> "'"
        where
        -- This should be the same as ParseBs.p_symbol.  I can't use it
        -- directly because that would be a circular import.
        parseable = case Text.uncons s of
            Just (c, cs) -> (Char.isAlpha c || c == '-' || c == '*')
                && Text.all (\c -> c /= ' ' && c /= ')' && c /= '=') cs
            Nothing -> False
        quote '\'' = "''"
        quote c = Text.singleton c

-- | Show a symbol intended for call position.  Call position is special in
-- that it can contain any character except space and equals without quoting.
show_call_val :: Val -> Text
show_call_val (VSymbol (Symbol sym)) = sym
show_call_val val = ShowVal.show_val val

instance ShowVal.ShowVal Text where
    show_val = ShowVal.show_val . Symbol

-- ** val utils

-- | Make an untyped VNum.
num :: Double -> Val
num = VNum . ScoreTypes.untyped

str :: Text -> Val
str = VSymbol . Symbol

score_time :: ScoreTime -> Val
score_time = VNum . ScoreTypes.Typed ScoreTypes.Score . ScoreTime.to_double

real_time :: RealTime -> Val
real_time = VNum . ScoreTypes.Typed ScoreTypes.Real . RealTime.to_seconds

transposition :: Pitch.Transpose -> Val
transposition t = VNum $ case t of
    Pitch.Diatonic d -> ScoreTypes.Typed ScoreTypes.Diatonic d
    Pitch.Chromatic d -> ScoreTypes.Typed ScoreTypes.Chromatic d
    Pitch.Nn d -> ScoreTypes.Typed ScoreTypes.Nn d

to_scale_id :: Val -> Maybe Pitch.ScaleId
to_scale_id (VSymbol (Symbol a)) = Just (Pitch.ScaleId a)
to_scale_id _ = Nothing

quoted :: Symbol -> [Val] -> Quoted
quoted name args = Quoted $ literal_call name args :| []

-- ** Ref

data Ref control val =
    -- | A signal literal.
    ControlSignal val
    -- | If the control isn't present, use the given default.
    | DefaultedControl control val
    -- | Throw an exception if the control isn't present.
    | LiteralControl control
    deriving (Eq, Read, Show)

type ControlRef = Ref ScoreTypes.Control ScoreTypes.TypedControl
type PControlRef = Ref ScoreTypes.PControl PSignal

instance (Serialize.Serialize val, Serialize.Serialize control) =>
        Serialize.Serialize (Ref control val) where
    put val = case val of
        ControlSignal a -> Serialize.put_tag 0 >> Serialize.put a
        DefaultedControl a b -> Serialize.put_tag 1 >> Serialize.put a
            >> Serialize.put b
        LiteralControl a -> Serialize.put_tag 2 >> Serialize.put a
    get = Serialize.get_tag >>= \x -> case x of
        0 -> ControlSignal <$> Serialize.get
        1 -> DefaultedControl <$> Serialize.get <*> Serialize.get
        2 -> LiteralControl <$> Serialize.get
        n -> Serialize.bad_tag "BaseTypes.Ref" n

-- | This can only represent constant signals, since there's no literal for an
-- arbitrary signal.  Non-constant signals will turn into a constant of
-- whatever was at 0.
instance ShowVal.ShowVal ControlRef where
    show_val = show_control $ \(ScoreTypes.Typed typ sig) ->
        ShowVal.show_val (Signal.at 0 sig) <> ScoreTypes.type_to_code typ

instance Pretty.Pretty ControlRef where pretty = ShowVal.show_val

-- | There's no way to convert a pitch back into the expression that produced
-- it, so this is the best I can do.
--
-- Similar to ShowVal 'ControlRef', there's no signal literal so I use the
-- value at 0.  A pitch can be turned into an expression, but not necessarily
-- accurately since it doesn't take things like pitch interpolation into
-- account.
instance ShowVal.ShowVal PControlRef where
    show_val = show_control
        (maybe "<none>" ShowVal.show_val . TimeVector.at 0 . sig_vec)

instance Pretty.Pretty PControlRef where pretty = ShowVal.show_val

show_control :: ShowVal.ShowVal control => (sig -> Text) -> Ref control sig
    -> Text
show_control sig_text control = case control of
    ControlSignal sig -> sig_text sig
    DefaultedControl control deflt ->
        ShowVal.show_val control <> "," <> sig_text deflt
    LiteralControl control -> ShowVal.show_val control

-- | Defaulted control from a RealTime.
real_control :: ScoreTypes.Control -> RealTime -> ControlRef
real_control c deflt = DefaultedControl c $
    ScoreTypes.untyped $ Signal.constant (RealTime.to_seconds deflt)

constant_control :: Signal.Y -> ControlRef
constant_control = ControlSignal . ScoreTypes.untyped . Signal.constant

-- ** Call

-- | Symbols used in function call position.  This is just to document that
-- a symbol is expected to be looked up in the scope.
type CallId = Symbol

-- | The only operator is @|@, so a list suffices for an AST.
type Expr = NonEmpty Call
data Call = Call CallId [Term] deriving (Show)
-- | TODO this is weird, in that Literal is evaluated but ValCall is not.
-- Why is it so weird?
data Term = ValCall Call | Literal Val deriving (Show)

calls_equal :: Call -> Call -> Maybe Bool
calls_equal (Call sym1 args1) (Call sym2 args2)
    | sym1 /= sym2 = Just False
    | otherwise = lists_equal terms_equal args1 args2

terms_equal :: Term -> Term -> Maybe Bool
terms_equal (ValCall call1) (ValCall call2) = calls_equal call1 call2
terms_equal (Literal val1) (Literal val2) = vals_equal val1 val2
terms_equal _ _ = Just False

-- | This is just a 'Call', but it's expected to return a VPitch.
type PitchCall = Call

instance ShowVal.ShowVal Expr where
    show_val expr = Text.stripEnd $
        Text.intercalate " | " (map ShowVal.show_val (NonEmpty.toList expr))
instance ShowVal.ShowVal Call where
    show_val (Call (Symbol sym) terms) =
        sym <> if null terms then ""
            else " " <> Text.unwords (map ShowVal.show_val terms)
instance ShowVal.ShowVal Term where
    show_val (ValCall call) = "(" <> ShowVal.show_val call <> ")"
    show_val (Literal val) = ShowVal.show_val val

instance Pretty.Pretty Call where pretty = ShowVal.show_val

instance DeepSeq.NFData Call where
    rnf (Call call_id terms) = call_id `seq` DeepSeq.rnf terms
instance DeepSeq.NFData Term where
    rnf (ValCall call) = DeepSeq.rnf call
    rnf (Literal val) = DeepSeq.rnf val

-- *** call utils

sym_to_scale_id :: Symbol -> Pitch.ScaleId
sym_to_scale_id (Symbol s) = Pitch.ScaleId s

scale_id_to_sym :: Pitch.ScaleId -> Symbol
scale_id_to_sym (Pitch.ScaleId s) = Symbol s

-- | Transform the Symbols in an expression.  This affects both symbols in call
-- position, and argument symbols.
map_symbol :: (Symbol -> Symbol) -> Expr -> Expr
map_symbol f = fmap call
    where
    call (Call sym terms) = Call (f sym) (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal (VSymbol sym)) = Literal (VSymbol (f sym))
    term (Literal lit) = Literal lit

-- | Transform the arguments in an expression.  This affects only vals in
-- argument position.
map_args :: (Val -> Val) -> Expr -> Expr
map_args f = fmap call
    where
    call (Call sym terms) = Call sym (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal lit) = Literal (f lit)

map_call_id :: (CallId -> CallId) -> Call -> Call
map_call_id f (Call call args) = Call (f call) args

-- | Transform only the CallId in the generator position.
map_generator :: (CallId -> CallId) -> Expr -> Expr
map_generator f (call1 :| calls) = case calls of
    [] -> map_call_id f call1 :| []
    _ : _ -> call1 :| Seq.map_last (map_call_id f) calls

-- | Convenient constructor for Call.
call :: Symbol -> [Term] -> Call
call sym = Call sym

call0 :: Symbol -> Call
call0 sym = Call sym []

literal_call :: Symbol -> [Val] -> Call
literal_call sym args = call sym (map Literal args)

inst :: Text -> Term
inst = Literal . VInstrument . ScoreTypes.Instrument

val_call :: Symbol -> [Val] -> Term
val_call sym args = ValCall (literal_call sym args)


-- * Derive.Score

-- ** ControlMap

type ControlMap = Map.Map ScoreTypes.Control ScoreTypes.TypedControl
type ControlFunctionMap = Map.Map ScoreTypes.Control ControlFunction
type PitchMap = Map.Map ScoreTypes.PControl PSignal

-- * ControlFunction

{- | Another representation of a signal, complementary to 'Signal.Control'.
    It's more powerful because it has access to a subset of the Dynamic state,
    as well as the 'Control' is was originally bound to.  However, it's also
    less powerful because you can't inspect it to see if it's constant, or emit
    exactly the samples present without resorting to sampling, or draw it on
    the UI.  This is the ubiquitous code vs. data tradeoff.

    In addition, the main motivation to add control functions was to randomize
    values, which means that, unlike signals, they're not actually functions at
    all, and thus couldn't be rendered as a continuous signal.  This means that
    functions are only suitable for sampling at points, not for slicing over
    time ranges.

    Having both signals and functions is awkward because then some calls may
    ignore a control function if they require a signal, which is inconsistent
    and confusing.  This is the case for all control generators since the
    signal usually is on a control track and will wind up being rendered on the
    UI.  So the convention is that control functions are generally just
    modifications of an underlying signal, rather than synthesizing a signal.

    Another awkward thing about ControlFunction is that it really wants to
    be in Deriver, but can't, due to circular imports.  The alternative is
    a giant hs-boot file, or lumping thousands of lines into
    "Derive.Deriver.Monad".  Currently it's a plain function but if I want
    logging and exceptions I could use "Derive.Deriver.DeriveM".  It still
    wouldn't solve the main problem, which is that I can't reuse the Deriver
    functions, and instead have to rewrite them.

    See NOTE [control-function].
-}
data ControlFunction =
    -- | Control is the control name this function was bound to, if it was
    -- bound to one.  Dynamic is a stripped down Derive State.  For
    -- ControlFunctions that represent a control signal, the RealTime is the
    -- desired X value, otherwise it's just some number.
    ControlFunction !Text
        !(ScoreTypes.Control -> Dynamic -> RealTime -> ScoreTypes.TypedVal)

instance Show ControlFunction where show = untxt . ShowVal.show_val
instance Pretty.Pretty ControlFunction where pretty = showt
-- | Not parseable.
instance ShowVal.ShowVal ControlFunction where
    show_val (ControlFunction name _) = "((ControlFunction " <> name <> "))"
instance DeepSeq.NFData ControlFunction where
    rnf (ControlFunction a b) = a `seq` b `seq` ()

call_control_function :: ControlFunction -> ScoreTypes.Control -> Dynamic
    -> RealTime -> ScoreTypes.TypedVal
call_control_function (ControlFunction _ f) = f

-- | Modify the underlying function, presumably to compose something onto the
-- input or output.
modify_control_function ::
    ((RealTime -> ScoreTypes.TypedVal) -> (RealTime -> ScoreTypes.TypedVal))
    -> ControlFunction -> ControlFunction
modify_control_function modify (ControlFunction name f) =
    ControlFunction name (\dyn control -> modify (f dyn control))

-- | A stripped down "Derive.Deriver.Monad.Dynamic" for ControlFunctions
-- to use.  The duplication is unfortunate, see 'ControlFunction'.
data Dynamic = Dynamic {
    dyn_controls :: !ControlMap
    , dyn_control_functions :: !ControlFunctionMap
    , dyn_pitches :: !PitchMap
    , dyn_pitch :: !PSignal
    , dyn_environ :: !Environ
    -- | This is from 'Derive.Deriver.Monad.state_event_serial'.
    , dyn_event_serial :: !Int
    , dyn_warp :: !ScoreTypes.Warp
    , dyn_ruler :: Ruler.Marklists -- intentionally lazy
    } deriving (Show)

empty_dynamic :: Dynamic
empty_dynamic = Dynamic
    { dyn_controls = mempty
    , dyn_control_functions = mempty
    , dyn_pitches = mempty
    , dyn_pitch = mempty
    , dyn_environ = mempty
    , dyn_event_serial = 0
    , dyn_warp = ScoreTypes.id_warp
    , dyn_ruler = mempty
    }

{- NOTE [control-function]

    Control functions add unwanted complexity, but I couldn't think of
    a simpler way to randomized or synthesized control values.  Here's the
    history:

    . One way would be look for a corresponding <>"-rnd" control for a range.
      But what about distribution?
    . If I pass RealTime to a Quoted value, I could write the parameter as
      "(rnd-signal x y).  But it would have to be curried to still accept the
      "at" parameter, or have access to it through Context.  It could look at
      the stack, which is how the random seed works.  Seems complicated.  But
      replacing a signal with a function seems like a good idea.
    . Can I replace the Signal.Control itself with something possibly wrapped
      in a function?  That function would need (RandomSeed, RealTime).  But
      that doesn't work for notes that take a slice of the signal.  I think
      I only want to randomize Signal.at access.
    . Or a separate 'Map Score.Control ValCall' and the Signal.at functions in
      derive first look in there.  This is similar to the -rnd control idea,
      except more flexible.  I could use it not just for different
      distributions, but to invert a signal, combine two signals, etc.  Or
      have an entirely synthesized signal, with no backing Signal.Control.
    . dur-rnd could be replaced by randomization on sus-abs.  start-rnd gets
      replaced by a start-offset control, which can then be randomized.
      start-offset could be also useful for e.g. swing time or instruments
      that are aggressively before the beat.
    . At this point I could get rid of the "normal" control map, replacing
      with Signal.at ValCalls.  Except that I still need non-randomized signals
      to slice and send to MIDI perform, and to display on the UI.
    . Also, ValCalls can log and fail and do the other Deriver things.  Of
      course they also have access to State, which they need, or at least the
      Dynamic part.  But to bind in tracklang I still need some sort of
      currying, e.g. '%control = (rnd .1 sig)'.  But the call is
      (rnd, Control, RealTime).  So I could save the Call and append RealTime
      at the end.  I could optimize a bit by looking up 'rnd' at the binding
      time.  But I can't typecheck the args unless I introspect... which
      I actually can do, given the documentation.  I would have to map each
      ArgDoc back to its parser, then apply each parser.  But it would be hard
      to store the types untagged, maybe possible with an existential type but
      seems tricky, and I need access to the function directly.
    . I could also use plain haskell functions, but it would be yet another
      namespace, and I'd still need a way to typecheck and pass args from
      tracklang.  Val calls already do all that, I should reuse it.
    . A better way might be to add a VControlFunction, which is just
      RealTime -> Signal.Y, this would also eliminate typechecking the return
      value.  That means I also don't have to quote, e.g. '%x = "(f)'
-}
