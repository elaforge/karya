-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Lily where
import Util.Control
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score


-- | Replace a note generator call with one that generates a single note having
-- the given attributes.
note :: Derive.PassedArgs d -> Score.Attributes
    -> Derive.EventDeriver -> Derive.EventDeriver
note args attrs = when_lilypond (Util.place args (Util.attr_note attrs))

-- | Replace a note transformer with one that derives its children as-is,
-- but adds the given attributes.
note_transformer :: Derive.PassedArgs d -> Score.Attributes
    -> Derive.EventDeriver -> Derive.EventDeriver
note_transformer args attrs = when_lilypond $
    Note.place $ Note.map_events (Util.add_attrs attrs) $
        concat (Note.sub_events args)

when_lilypond :: Derive.Deriver a -> Derive.Deriver a -> Derive.Deriver a
when_lilypond = ifM Derive.is_lilypond_derive
