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
module Derive.Call.Articulation where
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


lookup_attr_generator :: Derive.LookupCall (Derive.Generator Derive.Note)
lookup_attr_generator = make_lookup_attr $ \attrs ->
    fst $ Make.transform_notes ("add attrs: " <> ShowVal.show_val attrs)
        Tags.attr "Doc unused." Sig.no_args
        (\() -> Util.add_attrs attrs)

lookup_attr_transformer :: Derive.LookupCall (Derive.Transformer Derive.Note)
lookup_attr_transformer = make_lookup_attr $ \attrs ->
    snd $ Make.transform_notes ("add attrs: " <> ShowVal.show_val attrs)
        Tags.attr "Doc unused." Sig.no_args
        (\() -> Util.add_attrs attrs)

make_lookup_attr :: (Score.Attributes -> call) -> Derive.LookupCall call
make_lookup_attr call =
    Derive.pattern_lookup "attribute starting with `+`" doc $
        \(TrackLang.Symbol sym) -> parse_symbol sym
    where
    parse_symbol sym = case Text.uncons sym of
        Just (c, _) | c == '+' || c == '=' -> case ParseBs.parse_val sym of
            Right (TrackLang.VAttributes attrs) -> return $ Just (call attrs)
            _ -> return Nothing
        _ -> return Nothing
    doc = Derive.extract_doc $ fst $
        Make.attributed_note (Score.attr "example-attr")

note_calls :: Derive.CallMaps Derive.Note
note_calls = Make.call_maps
    [ ("o", Make.attributed_note Attrs.harm)
    , ("m", Make.attributed_note Attrs.mute)
    , (".", Make.attributed_note Attrs.staccato)
    , ("{", Make.attributed_note Attrs.porta)
    , ("D", c_detach) -- for symmetry with 'd', which delays the start
    ]
    [ ("(", c_legato)
    -- These do different things in lilypond mode, but in normal performance
    -- they are just the same as a slur.
    , ("^(", c_legato)
    , ("_(", c_legato)
    ]
    []

-- * legato

-- | I'm not really sure how fancy calls should be.  On one hand, high level
-- calls should get a nice result automatically.  On the other hand, they're
-- not very composable if they override things like %sus-abs.
c_legato :: Derive.Generator Derive.Note
c_legato = Derive.make_call "legato" (Tags.attr <> Tags.subs <> Tags.ly)
    "Play the transformed notes legato.  This just makes all but the last\
    \ overlap slightly.\
    \\nYou can combine this with other controls to get fancier phrasing.\
    \ For example, you can be detached by default but have legato connect\
    \ notes, by setting `%legato-overlap = .05 | %sus-abs = -.05`.\
    \\nOtherwise, you can use the `detach` and `dyn` args."
    $ Sig.call ((,,)
    <$> defaulted "overlap" (Sig.typed_control "legato-overlap" 0.1 Score.Real)
        "All notes but the last have their durations extended by this amount."
    <*> defaulted "detach" Nothing "Shorten the final note by this amount,\
        \ by setting `%sus-abs`.\
        \ The distinction between not given and 0 is important, because 0\
        \ will still override `%sus-abs`, which you may not want."
    <*> defaulted "dyn" 1 "Scale dyn for notes after the first one by this\
        \ amount."
    ) $ \(overlap, maybe_detach, dyn) args -> do
        overlap <- Util.real_time_at overlap =<< Args.real_start args
        note_legato overlap maybe_detach dyn =<< Sub.sub_events args

note_legato :: RealTime -> Maybe RealTime -> Signal.Y -> [[Sub.Event]]
    -> Derive.EventDeriver
note_legato overlap maybe_detach dyn = Sub.place . concat . map apply
    where
    apply = Seq.map_init (Sub.map_event (set_sustain overlap))
        . apply_dyn dyn . maybe id apply_detach maybe_detach

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

c_ly_slur :: Derive.Generator Derive.Note
c_ly_slur = Derive.make_call "ly-slur" (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "(") (Lily.SuffixLast, ")")

c_ly_slur_up :: Derive.Generator Derive.Note
c_ly_slur_up = Derive.make_call "ly-slur-up" (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur, forced to be above." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "^(") (Lily.SuffixLast, ")")

c_ly_slur_down :: Derive.Generator Derive.Note
c_ly_slur_down = Derive.make_call "ly-slur-down"
    (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur, forced to be below." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "_(") (Lily.SuffixLast, ")")

c_attr_legato :: Derive.Generator Derive.Note
c_attr_legato = Derive.make_call "legato" (Tags.attr <> Tags.subs)
    "Make a phrase legato by applying the `+legato` attribute. This is for\
    \ instruments that understand it, for instance with a keyswitch for\
    \ transition samples."
    $ Sig.call ((,)
    <$> defaulted "detach" Nothing "If set, shorten the final note by this\
        \ amount. This is to avoid triggering legato from the previous note."
    <*> defaulted "dyn" 1 "Scale dyn for notes after the first one by\
        \ this amount. Otherwise, transition samples can be too loud."
    ) $ \(detach, dyn) ->
        Util.add_attrs Attrs.legato . note_legato 0.02 detach dyn
            <=< Sub.sub_events

apply_detach :: RealTime -> [Sub.Event] -> [Sub.Event]
apply_detach detach = Seq.map_last (Sub.map_event (set_sustain (-detach)))

apply_dyn :: Signal.Y -> [Sub.Event] -> [Sub.Event]
apply_dyn dyn = Seq.map_tail (Sub.map_event (Util.multiply_dynamic dyn))

-- | Apply detach and dyn arguments.  This sets 'Controls.sustain_abs' instead
-- of adding to it because it feels like legato should override any sustain
-- environment in place.
apply_phrase :: RealTime -> Signal.Y -> [Sub.Event] -> [Sub.Event]
apply_phrase detach dyn =
    Seq.map_tail (Sub.map_event (Util.multiply_dynamic dyn))
    . Seq.map_last (Sub.map_event (set_sustain (-detach)))

set_sustain :: RealTime -> Derive.Deriver a -> Derive.Deriver a
set_sustain = Util.with_constant Controls.sustain_abs . RealTime.to_seconds

-- * misc

c_detach :: Make.Calls Derive.Note
c_detach = Make.transform_notes "detach" mempty
    ("Detach the notes slightly, by setting "
        <> ShowVal.show_val Controls.sustain_abs <> ".")
    (defaulted "time" 0.15 "Set control to `-time`.") $ \time ->
        Util.with_constant Controls.sustain_abs (-time)
