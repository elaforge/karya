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
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Note as Note
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
    ("Play the transformed notes legato.  This sets `+legato` on all notes\
    \ except the last one. The default note deriver will respond to `+legato`\
    \ and " <> ShowVal.doc_val Controls.legato_overlap <> "."
    ) $ Sig.call0 $ init_attr Attrs.legato

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
    "This is for instruments that understand the `+legato` attribute,\
    \ for instance with a keyswitch for transition samples.\
    \\nIf you use this, you should definitely turn off `Note.config_legato`."
    $ Sig.call (defaulted "detach" 0.05 "Shorten the final note by this amount.\
        \ This is to avoid triggering legato from the previous note.")
    attr_legato

attr_legato :: RealTime -> Derive.PassedArgs d -> Derive.EventDeriver
attr_legato detach = Note.place . concat . map apply <=< Note.sub_events
    where
    apply notes = case Seq.viewr notes of
        Just (pre, post) -> Note.map_events (Util.add_attrs Attrs.legato) $
            pre ++ [Note.map_event shorten post]
        Nothing -> []
    shorten = Util.with_constant Controls.sustain_abs
        (- RealTime.to_seconds detach)

-- | Apply the attributes to the init of the sub-events, i.e. every one but the
-- last.
init_attr :: Score.Attributes -> Derive.PassedArgs d -> Derive.EventDeriver
init_attr attr = Note.place . concatMap add <=< Note.sub_events
    where
    add notes = case Seq.viewr notes of
        Just (notes, last) ->
            Note.map_events (Util.add_attrs attr) notes ++ [last]
        Nothing -> []

-- * misc

c_detach :: Derive.NoteCall
c_detach = Make.transform_notes "detach" mempty
    ("Detach the notes slightly, by setting "
        <> ShowVal.show_val Controls.sustain_abs <> ".")
    (defaulted "time" 0.15 "Set control to `-time`.") $ \time ->
        Util.with_constant Controls.sustain_abs (-time)
