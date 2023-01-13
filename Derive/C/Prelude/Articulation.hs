-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Calls that are similar to staff-notation articulations, or could be.
    This means calls that modify notes in relatively straightforward ways,
    either by adding an attribute or modifying their environment.

    There is already general purpose syntax to add attributes to notes, e.g.
    @attr = +x@ or @n +x@ or just @+x@, and instruments may supply special
    calls for their attributes, but there are several attributes which look
    nice with their own calls and are used by many instruments.

    TODO There are too many ways to apply attributes to notes, and they work in
    inconsistent ways.
-}
module Derive.C.Prelude.Articulation where
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StringUtil as StringUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import           Derive.Sig (defaulted)
import qualified Derive.Symbols as Symbols
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.both
        [ ("o", c_harmonic)
        , (Symbols.mute, Make.attributed_note Module.prelude Attrs.mute)
        , (".", Make.attributed_note Module.prelude Attrs.staccato)
        , ("{", Make.attributed_note Module.prelude Attrs.porta)
        -- I'd use '>', but then it overrides the empty instrument call in note
        -- tracks.  Besides, this way it has a nice symmetry with '^'.
        , (Symbols.accent, c_accent)
        , (Symbols.weak, c_weak)

        , ("-", c_shorten_lengthen True)
        , ("+", c_shorten_lengthen False)
        ]
    , Library.generators
        [ ("(", c_slur Nothing)
        -- These do different things in lilypond mode, but in normal
        -- performance they are just the same as a slur.
        , ("^(", c_slur (Just Call.Up))
        , ("_(", c_slur (Just Call.Down))
        ]
    , Library.transformers
        [ ("sus-a", c_sustain_abs)
        , ("sus", c_sustain)
        ]
    , Library.pattern pattern_attr_generator
    , Library.pattern pattern_attr_transformer
    ]

-- * attr pattern

pattern_attr_generator :: Derive.PatternCall (Derive.Generator Derive.Note)
pattern_attr_generator = make_pattern_attr $ \attrs ->
    Library.generator $ Make.attributed_note Module.prelude attrs

pattern_attr_transformer :: Derive.PatternCall (Derive.Transformer Derive.Note)
pattern_attr_transformer = make_pattern_attr $ \attrs ->
    Library.transformer $ Make.attributed_note Module.prelude attrs

make_pattern_attr :: (Attrs.Attributes -> call) -> Derive.PatternCall call
make_pattern_attr call = Derive.PatternCall
    { pat_description = "attribute starting with `+` or `=`"
    , pat_doc = Derive.extract_doc $ Library.generator $
        Make.attributed_note Module.prelude (Attrs.attr "example-attr")
    , pat_function = \(Expr.Symbol sym) -> parse_symbol sym
    }
    where
    parse_symbol sym = case Text.uncons sym of
        Just (c, _) | c == '+' || c == '=' -> case Parse.parse_val sym of
            Right (DeriveT.VAttributes attrs) -> return $ Just (call attrs)
            _ -> return Nothing
        _ -> return Nothing

-- * harmonic

c_harmonic :: Library.Calls Derive.Note
c_harmonic = Make.transform_notes Module.prelude "harmonic"
    (Tags.attr <> Tags.ly)
    "Harmonic, with lilypond for natural and artificial harmonic notation."
    ((,)
    <$> Sig.defaulted "type" (Nothing :: Maybe Sig.Dummy) "Type of harmonic."
    <*> lily_harmonic_sig
    ) $ \(htype, lily_args) deriver -> Ly.when_lilypond
            (lily_harmonic lily_args (fromMaybe Natural htype) deriver)
            (Call.add_attributes (Attrs.harm <> harm_attrs htype) deriver)
    where
    harm_attrs htype = case htype of
        Nothing -> mempty
        Just Natural -> Attrs.natural
        Just Artificial -> Attrs.artificial

data HarmonicType = Natural | Artificial
    deriving (Eq, Show, Bounded, Enum)

instance Typecheck.Typecheck HarmonicType
instance ShowVal.ShowVal HarmonicType where
    show_val Natural = "nat"
    show_val Artificial = "art"

-- | Args for 'lily_harmonic'.
lily_harmonic_sig :: Sig.Parser ([PSignal.Pitch], Maybe PSignal.Pitch, Bool)
lily_harmonic_sig = (,,)
    <$> Sig.environ_key EnvKey.open_strings ([] :: [Sig.Dummy])
        "Pitches of open strings."
    <*> Sig.environ_key EnvKey.string (Nothing :: Maybe Sig.Dummy)
        "Play on this string."
    <*> Sig.environ_key "harmonic-force-diamond" False
        "If true, use string+diamond notation even for the 2nd natural\
        \ harmonic."

lily_harmonic :: ([PSignal.Pitch], Maybe PSignal.Pitch, Bool)
    -> HarmonicType -> Derive.NoteDeriver -> Derive.NoteDeriver
lily_harmonic (open_strings, string, force_diamond) htype deriver = do
    open_strings <- mapM StringUtil.string open_strings
    string <- traverse StringUtil.string string
    Post.emap_m_ id
        (lily_harmonic_event force_diamond htype open_strings string)
        =<< deriver
    -- Ly should have one that skips code events

lily_harmonic_event :: Bool -> HarmonicType -> [StringUtil.String]
    -> Maybe StringUtil.String -> Score.Event -> Derive.Deriver [Score.Event]
lily_harmonic_event force_diamond htype open_strings string event = do
    nn <- Derive.require "no pitch" $ Score.initial_nn event
    (string, harmonic) <- Derive.require_right id $ case htype of
        Natural -> natural_harmonic open_strings string nn
        Artificial -> artificial_harmonic lowest nn
            where
            lowest = maybe 0 StringUtil.str_nn $
                string <|> Seq.head open_strings
    -- When the lilypond backend sees Attrs.harm it knows it's inherently nv.
    let add_harm = Score.add_attributes Attrs.harm
    map add_harm <$> if harmonic <= 2 && not force_diamond
        then return
            [Ly.add_note_code (Ly.append Constants.All, "-\\flageolet") event]
        else do
            interval <- Derive.require
                ("harmonic not supported: " <> showt harmonic)
                (touch_interval harmonic)
            return $ harmonic_code string (string + interval) event

harmonic_code :: Pitch.NoteNumber -> Pitch.NoteNumber -> Score.Event
    -> [Score.Event]
harmonic_code stopped touched event =
    [ with_pitch stopped
    , Ly.add_note_code (Ly.note_append Constants.All, "\\harmonic") $
        with_pitch touched
    ]
    where
    with_pitch nn =
        Score.set_pitch (PSignal.constant (Twelve.nn_pitch nn)) event

-- | Where should I touch the string to play the nth harmonic of a base
-- frequency?
touch_interval :: StringUtil.Harmonic -> Maybe Pitch.NoteNumber
touch_interval harmonic = case harmonic of
    2 -> Just 12
    3 -> Just 7
    4 -> Just 5
    5 -> Just 4
    6 -> Just 3
    _ -> Nothing
    -- In principle I want the interval that corresponds to 1/harmonic of
    -- the string.  In practice, I need to show an integral pitch number, and
    -- high harmonics get too close and should be notated via some other means.
    -- So I'll just hard code some low harmonics and deal with high ones if
    -- I need them some day.

highest_harmonic :: StringUtil.Harmonic
highest_harmonic = 6

-- | If string is given, try to find this pitch in the harmonics of that
-- string.  Otherwise, find the string from open_strings which has this as
-- its lowest harmonic.
natural_harmonic :: [StringUtil.String] -> Maybe StringUtil.String
    -> Pitch.NoteNumber -> Either Text (Pitch.NoteNumber, StringUtil.Harmonic)
    -- ^ (selected string, harmonic)
natural_harmonic [] Nothing nn = Right (Pitch.modify_hz (/2) nn, 2)
natural_harmonic open_strings maybe_string nn =
    first StringUtil.str_nn <$>
        StringUtil.find_harmonic False highest_harmonic open_strings
            maybe_string nn

-- | Pick the lowest harmonic which is above the given lowest string.
artificial_harmonic :: Pitch.NoteNumber -> Pitch.NoteNumber
    -> Either Text (Pitch.NoteNumber, StringUtil.Harmonic)
    -- ^ (stopped pitch, harmonic)
artificial_harmonic lowest_string nn =
    tryJust ("artificial harmonic for " <> pretty nn
        <> " must be above lowest string " <> pretty lowest_string) $
    fmap (first (Pitch.nn . round)) $
        Seq.head $ filter ((>lowest_string) . fst) $
        -- I assume the octave is not convenient for an artificial harmonic,
        -- but that's not true in higher pitches.  Maybe I could allow it, but
        -- make it least preferred?
        Seq.key_on base_of [3..highest_harmonic]
    where base_of h = Pitch.modify_hz (/ fromIntegral h) nn

-- * slur

-- | I'm not really sure how fancy calls should be.  On one hand, high level
-- calls should get a nice result automatically.  On the other hand, they're
-- not very composable if they override things like %sus-abs.
c_slur :: Maybe Call.UpDown -> Derive.Generator Derive.Note
c_slur direction = Derive.generator Module.prelude "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
    "Play the transformed notes legato. This just makes all but the last\
    \ overlap slightly.\
    \\nYou can combine this with other controls to get fancier phrasing.\
    \ For example, you can be detached by default but have legato connect\
    \ notes, by setting `%legato-overlap = .05 | %sus-abs = -.05`.\
    \\nOtherwise, you can use the `detach` and `dyn` args.\
    \\nThe `^` and `_` variants are the same in normal performance, but force\
    \ lilypond slurs to go above or below, respectively."
    $ Sig.call ((,,)
    <$> defaulted "overlap" (Sig.typed_control "legato-overlap" 0.1 ScoreT.Real)
        "All notes but the last have their durations extended by this amount."
    <*> defaulted "detach" (Nothing :: Maybe Sig.Dummy)
        "Shorten the final note by this amount, by setting `%sus-abs`.\
        \ The distinction between not given and 0 is important, because 0\
        \ will still override `%sus-abs`, which you may not want."
    <*> defaulted "dyn" (1 :: Double)
        "Scale dyn for notes after the first one by this amount."
    ) $ \(overlap, maybe_detach, dyn) args ->
    Ly.when_lilypond (lily_slur direction args) $ do
        overlap <- Call.real_time_at overlap =<< Args.real_start args
        note_slur overlap maybe_detach dyn =<< Sub.sub_events args

note_slur :: RealTime -> Maybe RealTime -> Signal.Y -> [[SubT.Event]]
    -> Derive.NoteDeriver
note_slur overlap maybe_detach dyn = Sub.derive . concatMap apply
    where
    apply = Seq.map_init (fmap (set_sustain overlap))
        . apply_dyn dyn . maybe id apply_detach maybe_detach

lily_slur :: Maybe Call.UpDown -> Derive.PassedArgs d -> Derive.NoteDeriver
lily_slur direction =
    Ly.notes_around_ly (Ly.append Constants.First, prefix <> "(")
        (Ly.append Constants.Last, ")")
    where
    prefix = case direction of
        Nothing -> ""
        Just Call.Up -> "^"
        Just Call.Down -> "_"

{- NOTE [legato]
    Previously, it would set @+legato@, and the default note deriver would
    then respond by overlapping with the next note.  The theory was that it
    would allow more flexibility since I could then swap out the default
    note deriver.  However, in practice, the note deriver doesn't know about
    the extent of the legato phrase, so it would need @+legato@ on all but
    the last note.  And I wound up swapping out the legato call itself since
    samplers with legato samples need the legato keyswitch on all notes, not
    just all-but-the-last, so I'd have to swap out both the legato call and
    the note call.  In addition, I added features like @detach@ and @dyn@
    and delegating note overlap to the note didn't make so much sense.
-}

-- | This is not in 'note_calls', instruments that support this are expected to
-- override @(@ with it.
c_attr_slur :: Attrs.Attributes -> Attrs.Attributes
    -> Derive.Generator Derive.Note
c_attr_slur first_attr rest_attr = Derive.generator Module.instrument "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
    "Make a phrase legato by applying the `+legato` attribute. This is for\
    \ instruments that understand it, for instance with a keyswitch for\
    \ transition samples."
    $ Sig.call ((,)
    <$> defaulted "detach" (Nothing :: Maybe Sig.Dummy)
        "If set, shorten the final note by this\
        \ amount. This is to avoid triggering legato from the previous note."
    <*> defaulted "dyn" (1 :: Double)
        "Scale dyn for notes after the first one by\
        \ this amount. Otherwise, transition samples can be too loud."
    ) $ \(detach, dyn) args -> Ly.when_lilypond (lily_slur Nothing args) $
        note_slur 0.02 detach dyn
            . map (Seq.map_head (fmap (Call.add_attributes first_attr)))
            . map (Seq.map_tail (fmap (Call.add_attributes rest_attr)))
                =<< Sub.sub_events args

apply_detach :: RealTime -> [SubT.Event] -> [SubT.Event]
apply_detach detach = Seq.map_last (fmap (set_sustain (-detach)))

apply_dyn :: Signal.Y -> [SubT.Event] -> [SubT.Event]
apply_dyn dyn = Seq.map_tail (fmap (Call.multiply_dynamic dyn))

set_sustain :: RealTime -> Derive.Deriver a -> Derive.Deriver a
set_sustain = Call.with_constant Controls.sustain_abs . RealTime.to_seconds

-- * misc

-- | This is the same as 'c_lengthen', but it's here for symmetry with
-- 'c_sustain'.  Also, conceptually this is lower level, while c_lengthen
-- is meant to be modified to whatever is locally appropriate.
c_sustain_abs :: Derive.Transformer Derive.Note
c_sustain_abs = Derive.transformer Module.prelude "sus-a" mempty
    ("Simple legato, extend the duration of the transformed notes by the given\
    \ amount. This works by setting " <> ShowVal.doc Controls.sustain_abs
    <> "."
    ) $ Sig.callt (Sig.defaulted "time" (Typecheck.real 0.25)
        "Add this duration to the note.")
    $ \(Typecheck.DefaultReal time) args deriver -> do
        time <- Call.real_duration (Args.end args) time
        set_sustain time deriver

c_sustain :: Derive.Transformer Derive.Note
c_sustain = Derive.transformer Module.prelude "sus" mempty
    ("Simple legato, extend the duration of the transformed notes by the given\
    \ amount. This works by setting " <> ShowVal.doc Controls.sustain
    <> "."
    ) $ Sig.callt (Sig.defaulted "amount" (1.5 :: Double)
        "Multiply the note's duration by this.")
    $ \amount _args -> Call.with_constant Controls.sustain amount

c_shorten_lengthen :: Bool -> Library.Calls Derive.Note
c_shorten_lengthen shorten = Make.transform_notes Module.prelude
    (if shorten then "shorten" else "lengthen") mempty
    ("Lengthen or Shorten a note duration, by adding to or subtracting from "
        <> ShowVal.doc Controls.sustain_abs <> ".")
    (defaulted "time" (0.15 :: Double) "Subtract this duration.") $ \time ->
        Call.with_constant Controls.sustain_abs
            (if shorten then -time else time)

c_accent :: Library.Calls Derive.Note
c_accent = Make.transform_notes Module.prelude "accent" Tags.ly
    "Accent the note by multiplying its dynamic."
    (defaulted "dyn" (1.5 :: Double) "Multiply dynamic.") $ \dyn ->
        -- Adding Attrs.accent makes lilypond attach a '>'.
        Call.add_attributes Attrs.accent . Call.multiply_dynamic dyn

c_weak :: Library.Calls Derive.Note
c_weak = Make.transform_notes Module.prelude "weak" mempty
    "Weaken the note by multiplying its dynamic."
    (defaulted "dyn" (0.35 :: Double) "Multiply dynamic.") $ \dyn ->
        Call.multiply_dynamic dyn
