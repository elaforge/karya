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


lookup_attr :: Derive.LookupCall Derive.NoteCall
lookup_attr = Derive.pattern_lookup "attribute starting with `+`" doc $
    \(TrackLang.Symbol sym) -> parse_symbol sym
    where
    parse_symbol sym = case Text.uncons sym of
        Just (c, _) | c == '+' || c == '=' -> case ParseBs.parse_val sym of
            Right (TrackLang.VAttributes attrs) -> return $ Just $ call attrs
            _ -> return Nothing
        _ -> return Nothing
    call attrs = Make.transform_notes ("add attrs: " <> ShowVal.show_val attrs)
        Tags.attr "Doc unused." Sig.no_args
        (\() -> Util.add_attrs attrs)
    doc = Derive.extract_doc $ Make.attributed_note (Score.attr "example-attr")

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("o", Make.attributed_note Attrs.harm)
    , ("m", Make.attributed_note Attrs.mute)
    , (".", Make.attributed_note Attrs.staccato)
    , ("(", c_legato)
    -- These do different things in lilypond mode, but in normal performance
    -- they are just the same as a slur.
    , ("^(", c_legato)
    , ("_(", c_legato)
    , ("{", Make.attributed_note Attrs.porta)
    , ("D", c_detach) -- for symmetry with 'd', which delays the start
    ]

-- * legato

c_legato :: Derive.NoteCall
c_legato = Derive.stream_generator "legato" (Tags.attr <> Tags.subs <> Tags.ly)
    "Play the transformed notes legato."
    $ Sig.call ((,,)
    <$> defaulted "overlap" (Sig.typed_control "legato-overlap" 0.1 Score.Real)
        "All notes but the last have their durations extended by this amount."
    <*> defaulted "detach" 0.05 "Shorten the final note by this amount."
    <*> defaulted "dyn" 0.75 "Scale dyn for notes after the first one by this\
        \ amount."
    ) $ \(overlap, detach, dyn) args -> do
        overlap <- Util.real_time_at overlap =<< Args.real_start args
        note_legato overlap detach dyn =<< Sub.sub_events args
    -- Note [legato]
    -- Previously, it would set @+legato@, and the default note deriver would
    -- then respond by overlapping with the next note.  The theory was that it
    -- would allow more flexibility since I could then swap out the default
    -- note deriver.  However, in practice, the note deriver doesn't know about
    -- the extent of the legato phrase, so it would need @+legato@ on all but
    -- the last note.  And I wound up swapping out the legato call itself since
    -- samplers with legato samples need the legato keyswitch on all notes, not
    -- just all-but-the-last, so I'd have to swap out both the legato call and
    -- the note call.  In addition, I added features like @detach@ and @dyn@
    -- and delegating note overlap to the note didn't make so much sense.

note_legato :: RealTime -> RealTime -> Signal.Y -> [[Sub.Event]]
    -> Derive.EventDeriver
note_legato overlap detach dyn = Sub.place . concat . map apply
    where
    apply = Seq.map_init (Sub.map_event (set_sustain overlap))
        . apply_legato detach dyn

c_ly_slur :: Derive.NoteCall
c_ly_slur = Derive.stream_generator "ly-slur" (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "(") (Lily.SuffixLast, ")")

c_ly_slur_up :: Derive.NoteCall
c_ly_slur_up = Derive.stream_generator "ly-slur-up" (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur, forced to be above." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "^(") (Lily.SuffixLast, ")")

c_ly_slur_down :: Derive.NoteCall
c_ly_slur_down = Derive.stream_generator "ly-slur-down"
    (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur, forced to be below." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "_(") (Lily.SuffixLast, ")")

c_attr_legato :: Derive.NoteCall
c_attr_legato = Derive.stream_generator "legato" (Tags.attr <> Tags.subs)
    "Make a phrase legato by applying the `+legato` attribute. This is for\
    \ instruments that understand it, for instance with a keyswitch for\
    \ transition samples.\
    \\nIf you use this, you should definitely turn off `Note.config_legato`.\
    \ Otherwise, the detach argument won't work."
    $ Sig.call ((,)
    <$> defaulted "detach" 0.05 "Shorten the final note by this amount.\
        \ This is to avoid triggering legato from the previous note."
    <*> defaulted "dyn" 0.75 "Scale dyn for notes after the first one by this\
        \ amount. Otherwise, transition samples can be too loud."
    ) $ \(detach, dyn) -> attr_legato detach dyn <=< Sub.sub_events

attr_legato :: RealTime -> Signal.Y -> [[Sub.Event]] -> Derive.EventDeriver
attr_legato detach dyn = Sub.place . concatMap apply
    where
    apply = add_attr . add_overlap . apply_legato detach dyn
    add_attr = Sub.map_events (Util.add_attrs Attrs.legato)
    add_overlap = Seq.map_init (Sub.map_event (set_sustain 0.02))

-- | Apply detach and dyn arguments.  This sets 'Controls.sustain_abs' instead
-- of adding to it because it feels like legato should override any sustain
-- environment in place.
apply_legato :: RealTime -> Signal.Y -> [Sub.Event] -> [Sub.Event]
apply_legato detach dyn =
    Seq.map_tail (Sub.map_event (Util.multiply_dynamic dyn))
    . Seq.map_last (Sub.map_event (set_sustain (-detach)))

set_sustain :: RealTime -> Derive.Deriver a -> Derive.Deriver a
set_sustain = Util.with_constant Controls.sustain_abs . RealTime.to_seconds

-- * misc

c_detach :: Derive.NoteCall
c_detach = Make.transform_notes "detach" mempty
    ("Detach the notes slightly, by setting "
        <> ShowVal.show_val Controls.sustain_abs <> ".")
    (defaulted "time" 0.15 "Set control to `-time`.") $ \time ->
        Util.with_constant Controls.sustain_abs (-time)
