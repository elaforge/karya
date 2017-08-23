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
module Derive.Call.Prelude.Articulation where
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.Symbols as Symbols
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Make.call_maps
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
    <> Derive.call_maps
        [ ("(", c_slur Nothing)
        -- These do different things in lilypond mode, but in normal
        -- performance they are just the same as a slur.
        , ("^(", c_slur (Just Call.Up))
        , ("_(", c_slur (Just Call.Down))
        ]
        [ ("sus-a", c_sustain_abs)
        , ("sus", c_sustain)
        ]
    <> (mempty :: Derive.CallMaps Derive.Note)
        { Derive.scopes_generator = [lookup_attr_generator]
        , Derive.scopes_transformer = [lookup_attr_transformer]
        }

-- * lookp attr

lookup_attr_generator :: Derive.LookupCall (Derive.Generator Derive.Note)
lookup_attr_generator = make_lookup_attr $ \attrs ->
    fst $ Make.attributed_note Module.prelude attrs

lookup_attr_transformer :: Derive.LookupCall (Derive.Transformer Derive.Note)
lookup_attr_transformer = make_lookup_attr $ \attrs ->
    snd $ Make.attributed_note Module.prelude attrs

make_lookup_attr :: (Attrs.Attributes -> call) -> Derive.LookupCall call
make_lookup_attr call =
    Derive.LookupPattern "attribute starting with `+` or `=`" doc $
        \(Expr.Symbol sym) -> parse_symbol sym
    where
    parse_symbol sym = case Text.uncons sym of
        Just (c, _) | c == '+' || c == '=' -> case Parse.parse_val sym of
            Right (BaseTypes.VAttributes attrs) -> return $ Just (call attrs)
            _ -> return Nothing
        _ -> return Nothing
    doc = Derive.extract_doc $ fst $
        Make.attributed_note Module.prelude (Attrs.attr "example-attr")

-- * harmonic

c_harmonic :: Make.Calls Derive.Note
c_harmonic = Make.transform_notes Module.prelude "harmonic"
    (Tags.attr <> Tags.ly)
    "Harmonic, with lilypond for artificial harmonic notation."
    ((,,,)
    <$> Sig.defaulted "type" Nothing "Type of harmonic."
    <*> Sig.environ_key EnvKey.open_strings [] "Pitches of open strings."
    <*> Sig.environ_key EnvKey.string Nothing "Play on this string."
    <*> Sig.environ_key "harmonic-force-diamond" False
        "If true, use string+diamond notation even for the 2nd natural\
        \ harmonic."
    ) $ \(htype, open_strings, string, force_diamond) deriver ->
        Ly.when_lilypond
            (lily_harmonic force_diamond htype open_strings string deriver)
            (Call.add_attributes (Attrs.harm <> harm_attrs htype) deriver)
    where
    harm_attrs htype = case htype of
        Nothing -> mempty
        Just Natural -> Attrs.natural
        Just Artificial -> Attrs.artificial

data HarmonicType = Natural | Artificial
    deriving (Eq, Show, Bounded, Enum)

instance Typecheck.Typecheck HarmonicType
instance Typecheck.TypecheckSymbol HarmonicType
instance ShowVal.ShowVal HarmonicType where
    show_val Natural = "nat"
    show_val Artificial = "art"

-- | Harmonic number.
type Harmonic = Int

lily_harmonic :: Bool -> Maybe HarmonicType -> [Pitch.NoteNumber]
    -> Maybe Pitch.NoteNumber -> Derive.NoteDeriver -> Derive.NoteDeriver
lily_harmonic force_diamond htype open_strings string deriver = do
    Post.emap_m_ id
        (lily_harmonic_event force_diamond htype open_strings string)
        =<< deriver
    -- Ly should have one that skips code events

lily_harmonic_event :: Bool -> Maybe HarmonicType -> [Pitch.NoteNumber]
    -> Maybe Pitch.NoteNumber -> Score.Event -> Derive.Deriver [Score.Event]
lily_harmonic_event force_diamond htype open_strings string event = do
    nn <- Derive.require "no pitch" $ Score.initial_nn event
    let msg = "can't find " <> pretty nn <> " as a harmonic of "
            <> maybe ("open strings: " <> pretty open_strings) pretty string
    (string, harmonic) <- Derive.require msg $
        case fromMaybe Artificial htype of
            Natural -> natural_harmonic open_strings string nn
            Artificial -> artificial_harmonic lowest nn
                where lowest = fromMaybe 0 $ string <|> Seq.head open_strings
    if harmonic <= 2 && not force_diamond
        then return [Score.add_attributes Attrs.harm event]
        else do
            interval <- Derive.require
                ("harmonic not supported: " <> showt harmonic)
                (touch_interval harmonic)
            return $ harmonic_code string (string + interval) event

harmonic_code :: Pitch.NoteNumber -> Pitch.NoteNumber -> Score.Event
    -> [Score.Event]
harmonic_code stopped touched event =
    [ with_pitch stopped
    , Ly.add_event_code (Ly.NoteAppendAll, "\\harmonic") $ with_pitch touched
    ]
    where
    with_pitch nn =
        Score.set_pitch (PSignal.constant (PSignal.nn_pitch nn)) event

-- | Where should I touch the string to play the nth harmonic of a base
-- frequency?
touch_interval :: Harmonic -> Maybe Pitch.NoteNumber
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

highest_harmonic :: Harmonic
highest_harmonic = 6

-- | If string is given, try to find this pitch in the harmonics of that
-- string.  Otherwise, find the string from open_strings which has this as
-- its lowest harmonic.
natural_harmonic :: [Pitch.NoteNumber] -> Maybe Pitch.NoteNumber
    -> Pitch.NoteNumber -> Maybe (Pitch.NoteNumber, Harmonic)
    -- ^ (selected string, harmonic)
natural_harmonic open_strings maybe_string nn = case maybe_string of
    Just string -> (string,) <$> harmonic_of highest_harmonic string nn
    Nothing
        | null open_strings -> Just (Pitch.modify_hz (/2) nn, 2)
        | otherwise -> fmap Tuple.swap $ Seq.minimum_on fst $
            Seq.key_on_just harm_of open_strings
            where harm_of string = harmonic_of highest_harmonic string nn

-- | Pick the lowest harmonic which is above the given lowest string.
artificial_harmonic :: Pitch.NoteNumber -> Pitch.NoteNumber
    -> Maybe (Pitch.NoteNumber, Harmonic) -- ^ (stopped pitch, harmonic)
artificial_harmonic lowest_string nn =
    fmap (first (Pitch.nn . round)) $
        Seq.head $ filter ((>lowest_string) . fst) $
        Seq.key_on base_of [3..highest_harmonic]
    where base_of h = Pitch.modify_hz (/ fromIntegral h) nn

harmonic_of :: Harmonic -> Pitch.NoteNumber -> Pitch.NoteNumber
    -> Maybe Harmonic
harmonic_of limit base pitch = (2+) <$> List.findIndex (close pitch) harmonics
    where
    harmonics = take limit $ map (Pitch.nn_to_hz base *) [2..]
    close nn hz = Pitch.nns_close 50 nn (Pitch.hz_to_nn hz)

-- * slur

-- | I'm not really sure how fancy calls should be.  On one hand, high level
-- calls should get a nice result automatically.  On the other hand, they're
-- not very composable if they override things like %sus-abs.
c_slur :: Maybe Call.UpDown -> Derive.Generator Derive.Note
c_slur direction = Derive.generator Module.prelude "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
    "Play the transformed notes legato.  This just makes all but the last\
    \ overlap slightly.\
    \\nYou can combine this with other controls to get fancier phrasing.\
    \ For example, you can be detached by default but have legato connect\
    \ notes, by setting `%legato-overlap = .05 | %sus-abs = -.05`.\
    \\nOtherwise, you can use the `detach` and `dyn` args.\
    \\nThe `^` and `_` variants are the same in normal performance, but force\
    \ lilypond slurs to go above or below, respectively."
    $ Sig.call ((,,)
    <$> defaulted "overlap" (Sig.typed_control "legato-overlap" 0.1 Score.Real)
        "All notes but the last have their durations extended by this amount."
    <*> defaulted "detach" Nothing "Shorten the final note by this amount,\
        \ by setting `%sus-abs`.\
        \ The distinction between not given and 0 is important, because 0\
        \ will still override `%sus-abs`, which you may not want."
    <*> defaulted "dyn" 1 "Scale dyn for notes after the first one by this\
        \ amount."
    ) $ \(overlap, maybe_detach, dyn) args ->
    Ly.when_lilypond (lily_slur direction args) $ do
        overlap <- Call.real_time_at overlap =<< Args.real_start args
        note_slur overlap maybe_detach dyn =<< Sub.sub_events args

note_slur :: RealTime -> Maybe RealTime -> Signal.Y -> [[Sub.Event]]
    -> Derive.NoteDeriver
note_slur overlap maybe_detach dyn = Sub.derive . concatMap apply
    where
    apply = Seq.map_init (fmap (set_sustain overlap))
        . apply_dyn dyn . maybe id apply_detach maybe_detach

lily_slur :: Maybe Call.UpDown -> Derive.PassedArgs d -> Derive.NoteDeriver
lily_slur direction =
    Ly.notes_around_ly (Ly.AppendFirst, prefix <> "(") (Ly.AppendLast, ")")
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
c_attr_slur :: Derive.Generator Derive.Note
c_attr_slur = Derive.generator Module.instrument "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
    "Make a phrase legato by applying the `+legato` attribute. This is for\
    \ instruments that understand it, for instance with a keyswitch for\
    \ transition samples."
    $ Sig.call ((,)
    <$> defaulted "detach" Nothing "If set, shorten the final note by this\
        \ amount. This is to avoid triggering legato from the previous note."
    <*> defaulted "dyn" 1 "Scale dyn for notes after the first one by\
        \ this amount. Otherwise, transition samples can be too loud."
    ) $ \(detach, dyn) args -> Ly.when_lilypond (lily_slur Nothing args) $
        Call.add_attributes Attrs.legato . note_slur 0.02 detach dyn
            =<< Sub.sub_events args

apply_detach :: RealTime -> [Sub.Event] -> [Sub.Event]
apply_detach detach = Seq.map_last (fmap (set_sustain (-detach)))

apply_dyn :: Signal.Y -> [Sub.Event] -> [Sub.Event]
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
    ) $ Sig.callt (Sig.defaulted "amount" 1.5
        "Multiply the note's duration by this.")
    $ \amount _args -> Call.with_constant Controls.sustain amount

c_shorten_lengthen :: Bool -> Make.Calls Derive.Note
c_shorten_lengthen shorten = Make.transform_notes Module.prelude
    (if shorten then "shorten" else "lengthen") mempty
    ("Lengthen or Shorten a note duration, by adding to or subtracting from "
        <> ShowVal.doc Controls.sustain_abs <> ".")
    (defaulted "time" 0.15 "Subtract this duration.") $ \time ->
        Call.with_constant Controls.sustain_abs
            (if shorten then -time else time)

c_accent :: Make.Calls Derive.Note
c_accent = Make.transform_notes Module.prelude "accent" Tags.ly
    "Accent the note by multiplying its dynamic."
    (defaulted "dyn" 1.5 "Multiply dynamic.") $ \dyn ->
        -- Adding Attrs.accent makes lilypond attach a '>'.
        Call.add_attributes Attrs.accent . Call.multiply_dynamic dyn

c_weak :: Make.Calls Derive.Note
c_weak = Make.transform_notes Module.prelude "weaken" mempty
    "Weaken the note by multiplying its dynamic."
    (defaulted "dyn" 0.35 "Multiply dynamic.") $ \dyn ->
        Call.multiply_dynamic dyn
