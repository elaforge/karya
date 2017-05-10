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
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Make.call_maps
    [ ("o", Make.attributed_note Module.prelude Attrs.harm)
    , (mute_call, Make.attributed_note Module.prelude Attrs.mute)
    , (".", Make.attributed_note Module.prelude Attrs.staccato)
    , ("{", Make.attributed_note Module.prelude Attrs.porta)
    -- I'd use '>', but then it overrides the empty instrument call in note
    -- tracks.  Besides, this way it has a nice symmetry with '^'.
    , ("v", c_accent)
    , ("^", c_weak)

    , ("-", c_shorten_lengthen True)
    , ("+", c_shorten_lengthen False)
    ]
    <> Derive.call_maps
        [ ("(", c_legato)
        -- These do different things in lilypond mode, but in normal
        -- performance they are just the same as a slur.
        , ("^(", c_legato)
        , ("_(", c_legato)
        ]
        [ ("sus-a", c_sustain_abs)
        , ("sus", c_sustain)
        ]
    <> Derive.CallMaps [lookup_attr_generator] [lookup_attr_transformer]

mute_call :: Expr.Symbol
mute_call = "m"

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

-- * legato

-- | I'm not really sure how fancy calls should be.  On one hand, high level
-- calls should get a nice result automatically.  On the other hand, they're
-- not very composable if they override things like %sus-abs.
c_legato :: Derive.Generator Derive.Note
c_legato = Derive.generator Module.prelude "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
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
        overlap <- Call.real_time_at overlap =<< Args.real_start args
        note_legato overlap maybe_detach dyn =<< Sub.sub_events args

note_legato :: RealTime -> Maybe RealTime -> Signal.Y -> [[Sub.Event]]
    -> Derive.NoteDeriver
note_legato overlap maybe_detach dyn = Sub.derive . concatMap apply
    where
    apply = Seq.map_init (fmap (set_sustain overlap))
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
c_ly_slur = Derive.generator Module.ly "ly-slur" Tags.subs
    "Add a lilypond slur." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "(") (Lily.SuffixLast, ")")

c_ly_slur_up :: Derive.Generator Derive.Note
c_ly_slur_up = Derive.generator Module.ly "ly-slur-up" Tags.subs
    "Add a lilypond slur, forced to be above." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "^(") (Lily.SuffixLast, ")")

c_ly_slur_down :: Derive.Generator Derive.Note
c_ly_slur_down = Derive.generator Module.ly "ly-slur-down" Tags.subs
    "Add a lilypond slur, forced to be below." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "_(") (Lily.SuffixLast, ")")

-- | This is not in 'note_calls', instruments that support this are expected to
-- override @(@ with it.
c_attr_legato :: Derive.Generator Derive.Note
c_attr_legato = Derive.generator Module.instrument "legato"
    (Tags.attr <> Tags.subs)
    "Make a phrase legato by applying the `+legato` attribute. This is for\
    \ instruments that understand it, for instance with a keyswitch for\
    \ transition samples."
    $ Sig.call ((,)
    <$> defaulted "detach" Nothing "If set, shorten the final note by this\
        \ amount. This is to avoid triggering legato from the previous note."
    <*> defaulted "dyn" 1 "Scale dyn for notes after the first one by\
        \ this amount. Otherwise, transition samples can be too loud."
    ) $ \(detach, dyn) ->
        Call.add_attributes Attrs.legato . note_legato 0.02 detach dyn
            <=< Sub.sub_events

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
