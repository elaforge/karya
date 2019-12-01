-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
module Derive.Call.SubT where
import qualified Util.Pretty as Pretty
import qualified Derive.Deriver.Monad as Derive
import qualified Ui.Id as Id

import           Global
import           Types


data Track = Track {
    -- | Usually this comes from a sliced track with a TrackId, but sometimes
    -- from 'Derive.ctx_sub_events', or an unnamed track.
    _source :: !(Either Text TrackId)
    , _events :: ![Event]
    }

instance Pretty Track where
    format (Track source events) = Pretty.record "Track"
        [ ("source", Pretty.format source)
        , ("events", Pretty.format (const () <$> events))
        ]

show_track :: Track -> Text
show_track (Track source _) = "subtrack:" <> either id Id.ident_text source

-- | Sliced sub-events are represented as a start, duration, and opaque
-- deriver.  This is a compromise between a plain NoteDeriver, which is fully
-- abstract but also fully opaque, and some kind of note data structure, which
-- is fully concrete (and thus inflexible), but also transparent to
-- modification.
type Event = EventT Derive.NoteDeriver

data EventT a = EventT {
    _start :: !ScoreTime
    , _duration :: !ScoreTime
    , _note :: !a
    } deriving (Show, Functor)

instance Pretty a => Pretty (EventT a) where
    pretty (EventT start dur note) =
        "Event " <> showt start <> " " <> showt dur
            <> " (" <> pretty note <> ")"

end :: EventT a -> ScoreTime
end event = _start event + _duration event

overlaps :: ScoreTime -> EventT a -> Bool
overlaps pos (EventT start dur _)
    | dur == 0 = pos == start
    | otherwise = start <= pos && pos < start + dur

place :: ScoreTime -> ScoreTime -> EventT a -> EventT a
place shift factor (EventT start dur note) =
    EventT ((start - shift) * factor + shift) (dur * factor) note

stretch :: ScoreTime -> EventT a -> EventT a
stretch factor = place 0 factor

at :: ScoreTime -> EventT a -> EventT a
at shift (EventT start dur note) = EventT (start + shift) dur note
