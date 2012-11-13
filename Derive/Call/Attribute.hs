-- | Calls that just apply attributes to the notes.  There is already general
-- purpose syntax for this, e.g. @attr = +x@ or @n +x@, and instruments may
-- supply special calls for their attributes, but there are several attributes
-- which look nice with their own calls and are used by many instruments.
--
-- TODO There are too many ways to apply attributes to notes, and they work
-- in inconsistent ways.
module Derive.Call.Attribute where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, typed_control)
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("o", attributed_note Attrs.harmonic)
    , ("m", attributed_note Attrs.mute)
    , ("(", c_legato)
    ]


-- TODO: in lilypond mode, ignore the overlap but apply the attribute
c_legato :: Derive.NoteCall
c_legato = Derive.stream_generator "legato"
    ("Play the transformed notes legato.  This extends their duration and\
     \ applies `+legato`."
    ) $ CallSig.call1g
    (optional "overlap" (typed_control "legato" 0.1 Score.Real)
        "All notes except the last one overlap with the next note by this\
        \ amount."
    ) $ \overlap args -> Lily.note_transformer args Attrs.legato $ do
        overlap <- Util.real_duration Util.Real (Args.start args)
            =<< Util.typed_control_at overlap =<< Args.real_start args
        mconcat $ map (legato overlap) (Note.sub_events args)

legato :: RealTime -> [Note.Event] -> Derive.EventDeriver
legato overlap = fmap (Util.map_around_asc (extend_duration overlap))
    . Note.place . Note.map_events (Util.add_attrs Attrs.legato)

extend_duration :: RealTime -> [Score.Event] -> Score.Event -> [Score.Event]
    -> Score.Event
extend_duration _ _ cur [] = cur
extend_duration overlap _prev cur (next:_) = Score.set_duration dur cur
    where dur = Score.event_start next - Score.event_start cur + overlap

-- | Map on all elts except the last.
map_1 :: (a -> a) -> [a] -> [a]
map_1 _ [] = []
map_1 f (x:xs)
    | null xs = [x]
    | otherwise = f x : map_1 f xs

attributed_note :: Attrs.Attributes -> Derive.NoteCall
attributed_note attrs = Derive.Call
    { Derive.call_name = "note with " ++ ShowVal.show_val attrs
    , Derive.call_generator = Just $ Derive.generator_call
        ("Apply attributes to notes. When applied as a note transformer\
        \ (i.e. it has notes in child tracks) it applies its attributes to\
        \ those notes. Otherwise, it applies its attributes to the null note\
        \ call."
        ) generator
    , Derive.call_transformer = Just $ Derive.transformer_call
        "Apply attributes to the transformed deriver." transformer
    }
    where
    generator = CallSig.call0g $ \args -> case Note.sub_events args of
        [] -> add_attrs $ Call.reapply_call args (TrackLang.call "" [])
        subs -> Note.place (Note.map_events add_attrs (concat subs))
    transformer = CallSig.call0t $ \_ deriver -> add_attrs deriver
    add_attrs = Util.add_attrs attrs
