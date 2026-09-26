-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{- | This module defines basic tracklang types.

    The Derive.PSignal section is re-exported from "Derive.PSignal".  I'd rather
    move it to PSignal, but it needs to be here to avoid circular imports.

    Here are the names for various aspects of signals:

    >           numbers                   pitches                 both
    > scalar    Signal.Y                  PSignal.Y
    > name      ScoreT.Control            ScoreT.PControl
    > signal    Signal.Control            PSignal.PSignal
    > ref       DeriveT.ControlRef        DeriveT.PControlRef     Ref
-}
module Derive.DeriveT (
    -- * Derive.PSignal
    PSignal(..)
    , _signal
    , interpolate
    , Pitch
    , Transposed
    , RawPitch(..)
    , pitch
    , coerce
    , pitchNn, pitchNote
    , PitchConfig(..)
    , Scale(..)
    , detailedError
    , PitchError(..)
    , OutOfRange(..)
    , outOfRange, outOfRangeError
    , pitchesEqual

    -- * Duration
    , Duration(..)
    , TimeType(..)
    , multiplyDuration

    -- * Environ
    , Environ(..)
    , null
    , insert
    , lookup
    , environAttributes

    -- * Val
    , Val(..)
    , valsEqual
    , typesEqual
    , valToMini
    , constantPitch
    , Quoted(..)
    , showCallVal
    -- ** val utils
    , num
    , constantVal
    , constant
    , scoreTime, realTime
    , transposition
    , str
    , toScaleId
    , quoted, quoted0
    -- * Ref
    , ControlRef, PControlRef, Ref(..)

    -- * Expr
    , Expr, Call, Term
    , PitchCall
    -- ** call utils
    , mapStr

    -- * type aliases
    , PitchMap
    , PitchFunction

    -- * ControlFunction
    , CFunction(..)
    , callCfunction
    , PFunction(..)
    , Dynamic(..)
    , emptyDynamic
) where
import           Prelude hiding (lookup, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Coerce as Coerce
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Segment as Segment
import qualified Util.Serialize as Serialize

import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Warp as Warp

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


{-
    This file is pretty much unreducable, as far as dependencies go:
    For TrackLang: ControlFunction -> Environ -> Val <- ControlFunction
    For PSignal: PitchConfig -> Environ -> Val <- Pitch <- PitchConfig

    So 'ControlFunction', 'Pitch', and 'Val' must all be together.  'Signal'
    also gets dragged in, and winds up being everything in this file.

    The real key is Val, which has a ControlFunction, which means
    ControlFunction can't be in Deriver (since Deriver.Monad imports this),
    and PControlRef, which requires PSignal.
-}

-- * Derive.PSignal

-- | A pitch signal is similar to a 'Signal.Control', except that its values
-- are 'Pitch'es instead of plain floating point values.
newtype PSignal = PSignal (Segment.Signal Vector.Vector Pitch)
    deriving (Show, Pretty)

_signal :: PSignal -> Segment.Signal Vector.Vector Pitch
_signal (PSignal sig) = sig

instance Semigroup PSignal where
    s1 <> s2
        | Segment.null (_signal s1) = s2
        | Segment.null (_signal s2) = s1
        | otherwise = mconcat [s1, s2]
instance Monoid PSignal where
    mempty = PSignal Segment.empty
    mappend = (<>)
    mconcat [] = mempty
    mconcat sigs = PSignal $
        Segment.concat Nothing interpolate $ filter (not . Segment.null) $
        map _signal sigs

instance DeepSeq.NFData PSignal where
    rnf (PSignal vec) = vec `seq` ()

-- | A pitch interpolated a certain distance between two other pitches.
interpolate :: Segment.Interpolate Pitch
interpolate (Segment.Sample x1 p1) (Segment.Sample x2 p2) x
    | x <= x1 = p1
    | x >= x2 = p2
    | otherwise = Pitch
        { pitchEvalNn = nn
        , pitchEvalNote = note
        , pitchScale = pitchScale p1
        , pitchConfig = mempty
        }
    where
    nn config = do
        p1Nn <- pitchNn $ coerce $ applyConfig config p1
        p2Nn <- pitchNn $ coerce $ applyConfig config p2
        pure $ Num.scale p1Nn p2Nn $
            Pitch.NoteNumber $ RealTime.toSeconds $ Num.normalize x1 x2 x
    note config = pitchNote $ coerce $ applyConfig config $
        if x < x1 then p1 else p2
    applyConfig c pitch = pitch { pitchConfig = c <> pitchConfig pitch }

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
data RawPitch a = Pitch
    { pitchEvalNn :: PitchConfig -> Either PitchError Pitch.NoteNumber
    , pitchEvalNote :: PitchConfig -> Either PitchError Pitch.Note
    , pitchScale :: Scale
    , pitchConfig :: PitchConfig
    }

-- | Make an abstract Pitch.
pitch :: Scale
    -> (PitchConfig -> Either PitchError Pitch.NoteNumber)
    -> (PitchConfig -> Either PitchError Pitch.Note)
    -> PitchConfig -> Pitch
pitch scale nn note config = Pitch
    { pitchEvalNn = nn
    , pitchEvalNote = note
    , pitchScale = scale
    , pitchConfig = config
    }

coerce :: RawPitch a -> RawPitch b
coerce = Coerce.coerce

-- | Usually I only want to evaluate a fully transposed pitch.  Exceptions
-- are documented by applying 'coerce'.
pitchNn :: Transposed -> Either PitchError Pitch.NoteNumber
pitchNn pitch = do
    nn <- pitchEvalNn pitch (pitchConfig pitch)
    first (annotateOutOfRange pitch (Just nn)) $
        if 0 <= nn && nn <= 127 then pure nn
            else Left $ OutOfRangeError outOfRange

-- | Usually I only want to evaluate a fully transposed pitch.  Exceptions
-- are documented by applying 'coerce'.
pitchNote :: Transposed -> Either PitchError Pitch.Note
pitchNote pitch = first (annotateOutOfRange pitch Nothing) $
    pitchEvalNote pitch (pitchConfig pitch)

annotateOutOfRange :: RawPitch a -> Maybe Pitch.NoteNumber -> PitchError
    -> PitchError
annotateOutOfRange pitch maybeNn = modifyOutOfRange $ \err -> err
    { oorNn = maybeNn
    , oorTransposers = filtered <> oorTransposers err
    }
    where
    filtered = Map.filterWithKey (\k v -> k `Set.member` transposers && v /= 0)
        cmap
    PitchConfig _ cmap = pitchConfig pitch
    transposers = pscaleTransposers (pitchScale pitch)

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
data PitchConfig = PitchConfig Environ ScoreT.ControlValMap
    deriving (Show)

instance Semigroup PitchConfig where
    PitchConfig env1 c1 <> PitchConfig env2 c2 =
        PitchConfig (env1 <> env2) (Map.unionWith (+) c1 c2)

instance Monoid PitchConfig where
    mempty = PitchConfig mempty mempty
    mappend = (<>)

-- | PSignal can't take a Scale because that would be a circular import.
-- Fortunately it only needs a few fields.  However, because of the
-- circularity, the Scale.Scale -> PSignal.Scale constructor is in
-- "Derive.Derive".
data Scale = Scale
    -- | It can be useful to see the scale of a pitch, e.g. to create more
    -- pitches in the same scale as an existing pitch.
    { pscaleScaleId :: Pitch.ScaleId
    -- | The set of transposer signals for this scale, as documented in
    -- 'Derive.Scale.scale_transposers'.
    --
    -- They are stored here because they're needed by 'to_nn'.  I could
    -- store them separately, e.g. in the 'Score.Event' alongside the
    -- event_pitch, but the scale at event creation time is not guaranteed to
    -- be the same as the one when the pitch was created, so the safest thing
    -- to do is keep it with the pitch itself.
    , pscaleTransposers :: Set ScoreT.Control
    } deriving (Show)

instance Pretty Scale where
    pretty = pretty . pscaleScaleId

-- | It can't be reduced since it has lambdas, but at least this way you can
-- easily rnf things that contain it.
instance DeepSeq.NFData (RawPitch a) where
    rnf _ = ()

instance Show (RawPitch a) where
    -- Show just the NN, so this is parseable by Util.PPrint.
    show p = either show prettys (pitchNn (coerce p))

-- | Will look like: 62.95nn,4i(*wayang)
instance Pretty (RawPitch a) where
    pretty p = either showt pretty (pitchNn (coerce p)) <> ","
        <> either showt Pitch.noteText (pitchNote (coerce p))
        <> "(" <> pretty (pitchScale p) <> ")"

-- | Pitches have no literal syntax, but I have to print something.
instance ShowVal.ShowVal (RawPitch a) where
    showVal pitch = "<pitch: " <> pretty pitch <> ">"

-- | Annotate a PitchError with additional info.  TODO I should probably
-- accumulate info all the way up to get a full "stack trace" of what happened
-- to a pitch (e.g. interpolation), which maybe means abandon PitchError and
-- just use Text, or go ever further with structure?  Meanwhile, this seems
-- to do ok practically speaking.
detailedError :: RawPitch a -> PitchError -> Text
detailedError pitch err = mconcat
    [ pretty (pscaleScaleId scale)
    , ":"
    , either pretty pretty $ pitchEvalNote pitch mempty
    -- TODO the cmap includes non-transposing
    , if Map.null cmap then "" else " with transposition: " <> pretty cmap
    , ": ", pretty err
    ]
    where
    PitchConfig _env cmapAll = pitchConfig pitch
    cmap = Map.intersection cmapAll
        (Map.fromSet (const ()) (pscaleTransposers scale))
    scale = pitchScale pitch

-- | Things that can go wrong evaluating a pitch.
data PitchError
    = UnparseableNote Pitch.Note
    | OutOfRangeError OutOfRange
    -- | Input note doesn't map to a scale note.
    | InvalidInput
    -- | A required environ value was missing or had the wrong type or value.
    -- Nothing if the value is missing, otherwise a Text description.
    | EnvironError EnvKey.Key (Maybe Text)
    -- | Same as EnvironError, but for control vals.
    | ControlError ScoreT.Control Text
    -- | The scale doesn't implement that operation.
    | NotImplemented
    -- | Other kind of error.
    | PitchError Text
    deriving (Eq, Ord, Show)

{- | Note out of the scale's range.  The values are transpositions from
    the environment, in case it was out of range because of a transposition.

    Some scales have a restricted range, in which case they should throw
    'out_of_range', which 'pitch_nn' and 'pitch_note' will annotate with the
    transposition signals.  Other scales have unlimited range, in which case
    they're limited by the backend.  In this case 'pitch_nn' checks 0--127,
    which happens to be MIDI's limitation.
-}
data OutOfRange = OutOfRange
    { oorNn :: Maybe Pitch.NoteNumber
    , oorDegree :: Maybe Pitch.FSemi
    , oorValid :: Maybe (Int, Int)
    , oorTransposers :: ScoreT.ControlValMap
    } deriving (Eq, Ord, Show)

outOfRange :: OutOfRange
outOfRange = OutOfRange Nothing Nothing Nothing mempty

modifyOutOfRange :: (OutOfRange -> OutOfRange) -> PitchError -> PitchError
modifyOutOfRange modify (OutOfRangeError err) = OutOfRangeError (modify err)
modifyOutOfRange _ err = err

outOfRangeError :: Real a => a -> (Int, Int) -> PitchError
outOfRangeError semi valid = OutOfRangeError $ OutOfRange
    { oorNn = Nothing
    , oorDegree = Just (realToFrac semi)
    , oorValid = Just valid
    , oorTransposers = mempty
    }

instance Pretty PitchError where
    pretty err = case err of
        UnparseableNote note -> "unparseable note: " <> pretty note
        OutOfRangeError err -> pretty err
        InvalidInput -> "invalid input"
        EnvironError key err ->
            "environ value for " <> pretty key <> ": "
                <> fromMaybe "not found" err
        ControlError control err ->
            "control value for " <> pretty control <> ": " <> err
        NotImplemented -> "not implemented"
        PitchError msg -> msg

instance Pretty OutOfRange where
    pretty (OutOfRange nn semi valid transposers) =
        Text.unwords $ filter (not . Text.null)
            [ "pitch"
            , maybe "" pretty nn
            , maybe "" (\n -> "scale degree " <> pretty n) semi
            , "out of range"
            , maybe "" (\(a, b) -> pretty a <> "--" <> pretty b) valid
            , if transposers == mempty then "" else pretty transposers
            ]

instance Pretty PitchConfig where
    format (PitchConfig env controls) = Pretty.record "PitchConfig"
        [ ("environ", Pretty.format env)
        , ("controls", Pretty.format controls)
        ]

pitchesEqual :: RawPitch a -> RawPitch a -> Bool
pitchesEqual p1 p2 = either (const False) id $
    Pitch.nnsEqual <$> pitchNn (coerce p1) <*> pitchNn (coerce p2)

-- * Duration

-- | Some calls can operate in either RealTime or ScoreTime.
data Duration = RealDuration RealTime | ScoreDuration ScoreTime
    deriving (Eq, Show)

data TimeType = Real | Score deriving (Eq, Show)

instance ShowVal.ShowVal Duration where
    showVal (RealDuration x) = ShowVal.showVal x
    showVal (ScoreDuration x) = ShowVal.showVal x

instance Pretty Duration where
    pretty (RealDuration t) = pretty t
    pretty (ScoreDuration t) = pretty t

-- | Duration can't be in Fractional since you can't multiple a RealDuration
-- by a ScoreDuration, but scaling operations are still useful.
multiplyDuration :: Duration -> Double -> Duration
multiplyDuration (RealDuration t) n = RealDuration (t * RealTime.seconds n)
multiplyDuration (ScoreDuration t) n =
    ScoreDuration (t * ScoreTime.fromDouble n)

-- * Environ

newtype Environ = Environ (Map EnvKey.Key Val)
    deriving (Show, Semigroup, Monoid, DeepSeq.NFData)

-- Environ keys are always Text, and it's annoying to have quotes on them.
instance Pretty Environ where
    format (Environ env) = Pretty.formatMap
        . map (bimap Pretty.text Pretty.format) . Map.toList $ env

null :: Environ -> Bool
null (Environ env) = Map.null env

-- | Insert a val directly, with no typechecking.
insert :: EnvKey.Key -> Val -> Environ -> Environ
insert name val (Environ env) = Environ $ Map.insert name val env

lookup :: EnvKey.Key -> Environ -> Maybe Val
lookup name (Environ env) = Map.lookup name env

environAttributes :: Environ -> Attrs.Attributes
environAttributes environ =
    case lookup EnvKey.attributes environ of
        Just (VAttributes attrs) -> attrs
        _ -> mempty

-- * Val

-- | This is the type of first class values in the tracklang.  It's main
-- purpose is the type for arguments to tracklang calls, and val calls' return
-- type.
data Val
    -- | Numbers are merged with signals, a constant number is just a constant
    -- signal.  The signal type internally recognizes a constant and can
    -- apply various optimizations.
    --
    -- A number with an optional type suffix.  It also has a ratio style
    -- literal, though the output is still a floating point value, not a true
    -- ratio.
    --
    -- Constant literal: @42.23@, @-.4@, @1c@, @-2.4d@, @3/2@, @-3/2@, @0x7f@.
    --
    -- Signal literal: @(signal d 0 0 1 1)@.
    = VSignal (ScoreT.Typed Signal.Control)
    -- | No literal, but constants are returned from val calls, notably scale
    -- calls.
    | VPSignal PSignal
    -- | A set of Attributes for an instrument.
    --
    -- Literal: @+attr@, @+attr1+attr2@.
    | VAttributes Attrs.Attributes

    -- | A control name.  An optional value gives a default if the control
    -- isn't present.
    --
    -- Literal: @%control@, @%control,.4@
    | VControlRef ControlRef
    -- | A pitch control name.  The scale is taken from the environ.  Unlike
    -- a control signal, the empty string is a valid signal name and means the
    -- default pitch signal.  The @#@ val call is needed to make a pitch signal
    -- with a default.
    --
    -- Literal: @\#@, @\#pitch@, @(# pitch (4c))@
    | VPControlRef PControlRef

    -- | A parsed 'Pitch.Note'.  This is useful for things for which a textual
    -- 'Pitch.Note' is too high level and a numerical 'Pitch.NoteNumber' is too
    -- low level, like instrument ranges.
    --
    -- Literal: @(pitch 4 0 1)@ -> 4c#.
    | VNotePitch Pitch.Pitch

    -- | A string.  There is an unquoted and a quoted form, parsed at
    -- 'Derive.Parse.p_unquoted_str' and 'Derive.Parse.p_str'.
    --
    -- Literal: @func@, @\'hello\'@, @\'quinn\'\'s hat\'@
    | VStr Expr.Str

    -- | A quoted expression.  Quoted calls are resolved by "Derive.Sig" when
    -- it typechecks arguments.  This way you can set an argument default to
    -- an expression that will be evaluated every time the call occurs.
    -- Derive.Sig expects that the expression is a valid val call, which means
    -- no pipes.
    --
    -- Literal: @\"(a b c)@
    | VQuoted Quoted
    | VCFunction CFunction
    | VPFunction PFunction
    -- | An explicit not-given arg for functions so you can use positional
    -- args with defaults.
    --
    -- Literal: @_@
    | VNotGiven
    -- | A token used as a separator when calls want to parse their argument
    -- lists via their own complicated means.  TODO only used by old gamakam,
    -- get rid of this
    --
    -- Literal: @;@
    | VSeparator
    -- | List of values.
    --
    -- Literal: @(list)@, @(list 1 2)@, @(list (x) (y))@
    | VList [Val]
    deriving (Show)

-- | Return Nothing if the Vals can't be compared, and whether or not they're
-- equal otherwise.
valsEqual :: Val -> Val -> Maybe Bool
valsEqual x y = case (x, y) of
    (VSignal a, VSignal b) -> Just $ a == b
    (VPSignal sig1, VPSignal sig2)
        | Just p1 <- constantPitch sig1, Just p2 <- constantPitch sig2 ->
            Just $ pitchesEqual p1 p2
        -- I'm not going to implement this right now.  vals_equal is only used
        -- in Conditional anyway, and who is going to be comparing pitch
        -- signals?
        | otherwise -> Nothing
    (VAttributes a, VAttributes b) -> Just $ a == b
    (VControlRef a, VControlRef b) -> Just $ a == b
    -- This could use pitches_equal, but don't bother until I have a need for
    -- it.
    (VPControlRef _, VPControlRef _) -> Nothing
    (VNotePitch a, VNotePitch b) -> Just $ a == b
    (VStr a, VStr b) -> Just $ a == b
    (VQuoted (Quoted a), VQuoted (Quoted b)) ->
        listsEqual callsEqual (NonEmpty.toList a) (NonEmpty.toList b)
    (VCFunction _, VCFunction _) -> Nothing
    (VNotGiven, VNotGiven) -> Just True
    (VSeparator, VSeparator) -> Just True
    (VList a, VList b) -> listsEqual valsEqual a b
    _ -> Nothing

typesEqual :: Val -> Val -> Bool
typesEqual x y = case (x, y) of
    (VSignal {}, VSignal {}) -> True
    (VPSignal {}, VPSignal {}) -> True
    (VAttributes {}, VAttributes {}) -> True
    (VControlRef {}, VControlRef {}) -> True
    (VPControlRef {}, VPControlRef {}) -> True
    (VNotePitch {}, VNotePitch {}) -> True
    (VStr {}, VStr {}) -> True
    (VQuoted {}, VQuoted {}) -> True
    (VCFunction {}, VCFunction {}) -> True
    (VPFunction {}, VPFunction {}) -> True
    (VNotGiven, VNotGiven) -> True
    (VSeparator, VSeparator) -> True
    (VList {}, VList {}) -> True
    _ -> False

listsEqual :: (a -> a -> Maybe Bool) -> [a] -> [a] -> Maybe Bool
listsEqual eq = go
    where
    go (a:as) (b:bs) = maybe Nothing
        (\t -> if t then go as bs else Just False) (eq a b)
    go _ _ = Just False

valToMini :: Val -> Maybe Expr.MiniVal
valToMini = \case
    VStr a -> Just (Expr.VStr a)
    VSignal sig -> Expr.VNum <$> traverse Signal.constantVal sig
    _ -> Nothing

-- | This instance is actually invalid due to showing VPSignal, which has no
-- literal, and for 'Val', showing 'PControlRef', which amounts to the same
-- thing.  I use this to treat any Val as a Str to re-evaluate it.  Being
-- invalid means that a VPSignal or VPControlRef with a default will cause
-- a parse failure, but I'll have to see if this becomes a problem in practice.
instance ShowVal.ShowVal Val where
    showVal = \case
        VSignal sig -> showSignal sig
        VPSignal sig -> showPsignal sig
        VAttributes attrs -> ShowVal.showVal attrs
        VControlRef ref -> ShowVal.showVal ref
        VPControlRef ref -> ShowVal.showVal ref
        VNotePitch pitch -> ShowVal.showVal pitch
        VStr str -> ShowVal.showVal str
        VQuoted quoted -> ShowVal.showVal quoted
        VCFunction f -> ShowVal.showVal f
        VPFunction f -> ShowVal.showVal f
        VNotGiven -> "_"
        VSeparator -> ";"
        VList vals -> ShowVal.showVal vals

instance ShowVal.ShowVal (ScoreT.Typed Signal.Control) where
    showVal = showSignal

-- | ShowVal for VSignal.
--
-- > 1c -- constant with chromatic type
-- > (signal 0 1 1 1) -- [(0, 1), (1, 1)], untyped
showSignal :: ScoreT.Typed Signal.Control -> Text
showSignal (ScoreT.Typed typ sig)
    | Just c <- Signal.constantVal sig = ShowVal.showVal (ScoreT.Typed typ c)
    | otherwise = Text.unwords $ Lists.mapLast (<>")") $
        "(signal" : filter (/="") [ShowVal.showVal typ] ++
            [ ShowVal.showVal v
            | (x, y) <- Signal.toPairs sig, v <- [RealTime.toSeconds x, y]
            ]

instance ShowVal.ShowVal PSignal where showVal = showPsignal

-- | ShowVal for VPSignal.
--
-- TODO probably bogus? ShowVal (RawPitch a) just punts and uses <>s
-- Which this also uses.  Ideal if I can get back to symbolic, like (4c).
-- Otherwise, convert to nn, since symbolic depends on what's in scope?
-- The fact that I never trip on <>s implies that improper ShowVal for
-- pitch is not practically a problem.
showPsignal :: PSignal -> Text
showPsignal sig
    | Just c <- constantPitch sig = ShowVal.showVal c
    | otherwise = Text.unwords $ Lists.mapLast (<>")") $
        "(psignal" :
            [ s
            | (x, y) <- toPairs sig
            , s <- [ShowVal.showVal x, ShowVal.showVal y]
            ]
    where
    toPairs = Segment.toPairs . _signal

-- These are in PSignal, but have to be here too to avoid a circular
-- import.
constantPitch :: PSignal -> Maybe Pitch
constantPitch = Segment.constantVal . _signal

instance Pretty Val where
    pretty = ShowVal.showVal

instance DeepSeq.NFData Val where
    rnf (VStr s) = DeepSeq.rnf s
    rnf _ = ()

newtype Quoted = Quoted Expr deriving (Show)

-- | Unlike Exprs in general, a Quoted Expr should be representable with
-- show_val.  This is because a Quoted has only been parsed, not evaluated,
-- so it shouldn't have anything unshowable, like pitches.
instance ShowVal.ShowVal Quoted where
    showVal (Quoted expr) = "\"(" <> ShowVal.showVal expr <> ")"
instance Pretty Quoted where pretty = ShowVal.showVal

-- | Show a str intended for call position.  Call position is special in
-- that it can contain any character except space and equals without quoting.
showCallVal :: Val -> Text
showCallVal (VStr (Expr.Str sym)) = sym
showCallVal val = ShowVal.showVal val

-- ** val utils

-- | Make an untyped VSignal.
num :: Double -> Val
num = constant ScoreT.Untyped

constantVal :: Val -> Maybe (ScoreT.Typed Signal.Y)
constantVal (VSignal (ScoreT.Typed typ sig)) =
    ScoreT.Typed typ <$> Signal.constantVal sig
constantVal _ = Nothing

constant :: ScoreT.Type -> Signal.Y -> Val
constant typ = VSignal . ScoreT.Typed typ . Signal.constant

scoreTime :: ScoreTime -> Val
scoreTime = constant ScoreT.Score . ScoreTime.toDouble

realTime :: RealTime -> Val
realTime = constant ScoreT.Real . RealTime.toSeconds

transposition :: Pitch.Transpose -> Val
transposition t = case t of
    Pitch.Diatonic d -> constant ScoreT.Diatonic d
    Pitch.Chromatic d -> constant ScoreT.Chromatic d
    Pitch.Nn d -> constant ScoreT.Nn d

str :: Text -> Val
str = VStr . Expr.Str

toScaleId :: Val -> Maybe Pitch.ScaleId
toScaleId (VStr (Expr.Str a)) = Just (Pitch.ScaleId a)
toScaleId _ = Nothing

quoted :: Expr.Symbol -> [Val] -> Quoted
quoted sym args = Quoted $ Expr.generator (Expr.call sym args)

quoted0 :: Expr.Symbol -> Quoted
quoted0 sym = quoted sym []

-- * Ref

type ControlRef = Ref ScoreT.Control ScoreT.TypedSignal
type PControlRef = Ref ScoreT.PControl PSignal

data Ref control val = Ref control (Maybe val)
    deriving (Eq, Show)

instance (Serialize.Serialize control, Serialize.Serialize val) =>
        Serialize.Serialize (Ref control val) where
    put (Ref a b) = Serialize.putTag 3 >> Serialize.put a >> Serialize.put b
    {-
        This has to be careful to maintain compatibility with the old Ref:

        data Ref control val =
            ControlSignal val
            | DefaultedControl control val
            | LiteralControl control
        get = Serialize.get_tag >>= \case
            0 -> ControlSignal <$> Serialize.get
            1 -> DefaultedControl <$> Serialize.get <*> Serialize.get
            2 -> LiteralControl <$> Serialize.get
    -}
    get = Serialize.getTag >>= \case
        0 -> fail "new Ref doesn't have ControlSignal"
        1 -> Ref <$> Serialize.get <*> (Just <$> Serialize.get)
        2 -> Ref <$> Serialize.get <*> pure Nothing
        3 -> Ref <$> Serialize.get <*> Serialize.get
        n -> Serialize.badTag "DeriveT.Ref" n

instance ShowVal.ShowVal ControlRef where
    showVal = showRef (Text.cons '%' . ScoreT.controlName)

instance Pretty ControlRef where pretty = ShowVal.showVal

-- | There's no way to convert a pitch back into the expression that produced
-- it, so this is the best I can do.
--
-- Similar to ShowVal 'ControlRef', there's no signal literal so I use the
-- value at 0.  A pitch can be turned into an expression, but not necessarily
-- accurately since it doesn't take things like pitch interpolation into
-- account.
instance ShowVal.ShowVal PControlRef where
    showVal = showRef (Text.cons '#' . ScoreT.pcontrolName)

instance Pretty PControlRef where pretty = ShowVal.showVal

showRef :: ShowVal.ShowVal sig => (control -> Text) -> Ref control sig -> Text
showRef refText (Ref control deflt) =
        refText control <> maybe "" (("," <>) . ShowVal.showVal) deflt

-- * Expr

type Expr = Expr.Expr Val
type Call = Expr.Call Val
type Term = Expr.Term Val

instance ShowVal.ShowVal (Expr.Expr Val) where showVal = Expr.showValExpr
instance ShowVal.ShowVal (Expr.Call Val) where
    showVal = Expr.showValCall $ \case
        VStr (Expr.Str op) -> Just op
        _ -> Nothing
instance ShowVal.ShowVal (Expr.Term Val) where showVal = Expr.showValTerm

instance Pretty (Expr.Call Val) where pretty = ShowVal.showVal
instance Pretty (Expr.Term Val) where pretty = ShowVal.showVal

callsEqual :: Call -> Call -> Maybe Bool
callsEqual (Expr.Call sym1 args1) (Expr.Call sym2 args2)
    | sym1 /= sym2 = Just False
    | otherwise = listsEqual termsEqual args1 args2

termsEqual :: Term -> Term -> Maybe Bool
termsEqual (Expr.ValCall call1) (Expr.ValCall call2) = callsEqual call1 call2
termsEqual (Expr.Literal val1) (Expr.Literal val2) = valsEqual val1 val2
termsEqual _ _ = Just False

-- | This is just a 'Call', but it's expected to return a constant VPSignal.
type PitchCall = Call

-- ** call utils

-- | Transform the Symbols in a Call.
mapStr :: (Expr.Str -> Expr.Str) -> Call -> Call
mapStr f = call
    where
    call (Expr.Call sym terms) = Expr.Call sym (map term terms)
    term (Expr.ValCall c) = Expr.ValCall (call c)
    term (Expr.Literal (VStr str)) = Expr.Literal (VStr (f str))
    term (Expr.Literal lit) = Expr.Literal lit

-- * type aliases

type PitchMap = Map ScoreT.PControl PSignal
type PitchFunction = RealTime -> Maybe Pitch

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

    Another awkward thing about CFunction is that it really wants to
    be in Deriver, but can't, due to circular imports.  The alternative is
    a giant hs-boot file, or lumping thousands of lines into
    "Derive.Deriver.Monad".  Currently it's a plain function but if I want
    logging and exceptions I could use "Derive.Deriver.DeriveM".  It still
    wouldn't solve the main problem, which is that I can't reuse the Deriver
    functions, and instead have to rewrite them.

    See NOTE [control-function].
-}
data CFunction = CFunction
    -- | Human readable name.
    --
    -- TODO I thought of making it the expression that created this, for
    -- serialization, but not implemented yet.
    { cfName :: Text
    , cfSignal :: ScoreT.TypedSignal
    {- | This is modifying an underlying signal.

        The function may be created before the signal it modifies, e.g.
        @dyn=(cf-rnd .2)@ will apply to whatever values the @dyn@ signal later
        takes. There is special hackery in Env.put_val to merge a signal into a
        CFunction if present.  The signal should start at const 0 by
        convention, since many functions make sense against 0 too, and it would
        be annoying to plumb out an error from 'call_cfunction'.
    -}
    , cfFunction :: Dynamic -> Signal.Control -> RealTime -> Signal.Y
    }

-- | A simple pure function.
data PFunction = PFunction
    { pfName :: Text
    , pfFunction :: ScoreT.TypedFunction
    }

instance Show PFunction where show = prettys
instance Pretty PFunction where pretty = ShowVal.showVal
instance ShowVal.ShowVal PFunction where showVal = pfName

instance DeepSeq.NFData CFunction where
    rnf _ = () -- bogus instance so Derive.Dynamic can have one
instance Show CFunction where show = untxt . pretty
instance Pretty CFunction where
    pretty cf = "((CFunction " <> cfName cf <> "))"

-- | TODO this isn't a real ShowVal, I'd have to record the whole expression.
instance ShowVal.ShowVal CFunction where
    showVal = cfName

callCfunction :: Dynamic -> CFunction -> ScoreT.TypedFunction
callCfunction cfDyn cf = ScoreT.Typed typ (cfFunction cf cfDyn signal)
    where ScoreT.Typed typ signal = cfSignal cf

-- | A stripped down "Derive.Deriver.Monad.Dynamic" for ControlFunctions
-- to use.  The duplication is unfortunate, see 'CFunction'.
data Dynamic = Dynamic
    { dynPitch :: PSignal
    , dynEnviron :: Environ
    -- | This is from 'Derive.Deriver.Monad.state_event_serial'.
    , dynEventSerial :: Int
    , dynWarp :: Warp.Warp
    , dynRuler :: ~Ruler.Marklists -- intentionally lazy
    } deriving (Show)

emptyDynamic :: Dynamic
emptyDynamic = Dynamic
    { dynPitch = mempty
    , dynEnviron = mempty
    , dynEventSerial = 0
    , dynWarp = Warp.identity
    , dynRuler = mempty
    }

{- NOTE [control-function]

    . The UI is what I want, which is that I write a start=(rnd .1 .3), so
    let's keep that.  Then it gets serialized as an expression (rnd .1 .3).

    Stuff I don't like:

    . The UI where it's in a different namespace than signals, so dyn=(rnd) vs
    %dyn=.5.  Maybe this comes from the signal vs env namespace though.

    . UI where they have an implicit signal backing.  I think I don't need
    that, if it's a random addition it should be explicit, like
    dyn=%dyn + rnd .5.  If they are in the same namespace then something like
    dyn=(+ dyn (rnd .5)), or even dyn=+(rnd .5)

    . Also plenty is wrong with the implementation, extra Dynamic.

    Problems to solve:
    - Unify signal and env namespaces.
    - Unify Val and RVal.  Make Val serializable.
    - Make so calls can coerce to ScoreT.TypedFunction.
    - Unify ControlFunction with ValCall.
      . Why though?  Can't it be its own thing?

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
    . Or a separate 'Map ScoreT.Control ValCall' and the Signal.at functions in
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
    . A better way might be to add a VCFunction, which is just
      RealTime -> Signal.Y, this would also eliminate typechecking the return
      value.  That means I also don't have to quote, e.g. '%x = "(f)'
-}
