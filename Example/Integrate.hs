-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Examples for integrating with external scores.  This parses a file in
-- a simple format, but if there generator is in haskell it can also be
-- directly imported.
--
-- The easiest way to call the functions in here is to import it into
-- Local/Repl.hs, which should then show up in the REPL.
module Example.Integrate where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.FilePath as FilePath
import qualified Text.Read as Read

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Manual as Manual
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import qualified Derive.ShowVal as ShowVal
import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Meter.Meters as Meters
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Ui as Ui

import           Global
import           Types



-- | Read score from the file, and create a new block if it's the first time,
-- or integrate changes the next time.  Changes made to the file should be
-- merged into the block it creates.
integrate_file :: Cmd.CmdT IO (Maybe BlockId)
integrate_file = do
    score <- read_score fname
    mb_block_id <- integrate_score (txt (FilePath.takeFileName fname)) score
    whenJust mb_block_id (void . Create.view)
    return mb_block_id
    where
    fname = "Example/integrate-score"

integrate_score :: Ui.M m => Text -> Score -> m (Maybe BlockId)
integrate_score name score = do
    block_id <- Ui.require "invalid name" $ Id.make $ Id.id namespace name
    -- Make a m44 ruler up to the give end time.  Rulers are not quite as
    -- unreasonably complicated as they used to be.
    let end = ScoreTime.from_double $ maximum [s + d | (s, d, _, _) <- score]
    ruler_id <- RulerUtil.replace block_id $ const $ Right $
        Ruler.meter_ruler $ RulerUtil.meter_until Meters.m44 1 4 end
    Manual.block source_key block_id ruler_id block_title [(note, controls)]
    where
    (note, controls) = Manual.convert_note_track source_key (note_track 0 score)
    block_title = ""
    -- See Block.SourceKey
    source_key = name
    -- IDs have a namespace, originally so you could merge different scores
    -- without name collisions.  It's also convenient to put all generated
    -- score into its own namespace.
    namespace = Id.namespace "ex"

-- | Insert 'score' at the selection position.  This is a plain insert, no
-- fancy integration.  It won't clear out any existing notes, so it'll get
-- haphazardly merged if stuff is already there.  This is because it's
-- part of ModifyNotes, which is a general way to do score transformations,
-- and it expects its caller 'ModifyNotes.selection' to have cleared the old
-- notes.
-- insert_at_selection :: Cmd.M m => FilePath -> m ()
insert_at_selection :: FilePath -> Cmd.CmdT IO ()
insert_at_selection fname = do
    score <- read_score fname
    (block_id, _, track_id, at) <- Selection.get_insert
    insert score block_id track_id at

insert :: Ui.M m => Score -> BlockId -> TrackId -> TrackTime -> m ()
insert score block_id track_id at =
    ModifyNotes.write_tracks block_id [track_id] [note_track at score]

-- | Convert the simple score to ModifyNotes.NoteTrack, which is a generic
-- high level representation of the track structure.  Since karya represents
-- pitch and control tracks separately from the notes, they have to be
-- extracted.
note_track :: TrackTime -> Score -> ModifyNotes.NoteTrack
note_track offset score = ModifyNotes.NoteTrack (mk_events notes) control_tracks
    where
    control_tracks = Map.fromList
        [ (ModifyNotes.Pitch Pitch.empty_scale, mk_events pitches)
        , (ModifyNotes.Control "dyn", mk_events dyns)
        ]
    notes = [(start, dur, "") | (start, dur, _, _) <- score]
    pitches = [(start, 0, pitch) | (start, _, pitch, _) <- score]
    dyns = [(start, 0, ShowVal.show_val dyn) | (start, _, _, dyn) <- score]
    mk_events = Events.from_list . map mk_event
    mk_event (start, dur, text) =
        Event.event (offset + realToFrac start) (realToFrac dur) text


-- | Simple score with (start, dur, pitch, dyn).
type Score = [(Double, Double, Text, Double)]

score :: Score
score =
    [ (0, 1, "4c", 1)
    , (1, 1, "4d", 0.75)
    ]

read_score :: FilePath -> Cmd.CmdT IO Score
read_score fname = do
    score <- liftIO $ Text.IO.readFile fname
    Cmd.require_right id $ parse_score score

parse_score :: Text -> Either Text Score
parse_score = mapM parse . Text.lines
    where
    parse line = case Text.words line of
        [start, dur, pitch, dyn] -> maybe fail Right $
            (,,,) <$> p start <*> p dur <*> p pitch <*> p dyn
        _ -> fail
        where
        fail = Left $ "can't parse: " <> showt line
    p :: Read a => Text -> Maybe a
    p = Read.readMaybe . untxt
