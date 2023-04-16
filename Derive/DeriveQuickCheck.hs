-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Quickcheck tests for derive.

    This has never been used because it's not done yet.  Maybe I should some
    day...

    TODO complete

    * make Arbitrary for BlockSpecs
    + make a simple deriver that creates event and midi output skeletons
    - test simple deriver to make sure it's accurate itself
    - assert that the reduced deriver output equals the simple deriver output
    - basic pitches: If the score was created with notes aligned to note
      starts, then every NoteOn should have the appropriate key, there should
      be no pitch bends, and "same note" should be the only reason for
      a channel split
    - basic controls: Given randomly placed control events, notes have the
      correct control curves.  Don't worry about times or midi.
    - slicing: Given some simple note parents (tuple, place, ...),
      pitches and controls are still associated with the right notes as above.
      Don't worry about times, just that the right notes and the right
      controls.
    - block call property: a couple levels of nesting for block calls, notes
      still have the expected pitches and controls as above
    - inversion: as 'basic pitches' and 'basic controls', but controls are
      below the note tracks, results should be the same
-}
module Derive.DeriveQuickCheck where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Test.QuickCheck as Q

import qualified Util.ParseText as ParseText
import qualified Util.Lists as Lists
import qualified Util.Trees as Trees

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest

import           Global
import           Types


type Block = (UiTest.BlockSpec, [Skeleton.Edge])
type Track = UiTest.TrackSpec
type Events = [(ScoreTime, ScoreTime, Text)]

-- | Random length track of \"\" notes with pitches.
simple_pitch :: Q.Gen Block
simple_pitch = do
    ranges <- granges
    pitches <- Q.vectorOf (length ranges) gpitch
    let ptrack = [(p, 0, Pitch.note_text n)
            | (p, n) <- zip (map fst ranges) pitches]
    return (("block", [(">", map (to_spec "") ranges), ("*", ptrack)]),
        [(1, 2)])

granges :: Q.Gen [(ScoreTime, ScoreTime)]
granges = Q.sized $ \n -> go 0 =<< Q.choose (0, n)
    where
    go _ 0 = return []
    go prev n = do
        dur <- abs <$> Q.arbitrary
        gap <- Q.oneof [return 0, abs <$> Q.arbitrary]
        let start = prev + gap
        rest <- go (start + dur) (n - 1)
        return $ (start, start + dur) : rest

gpitch :: Q.Gen Pitch.Note
gpitch = Q.oneof $ map return (Map.keys note_to_nn)

-- | Random list of non-overlapping events, some of which are adjacent.
gevents :: Q.Gen [Event.Event]
gevents = Events.ascending . Events.from_list <$> Q.listOf (gevent "")

gevent :: Text -> Q.Gen Event.Event
gevent text = do
    start <- Q.arbitrarySizedFractional
    dur <- Q.arbitrarySizedFractional
    return $ Event.event (abs start) (abs dur) text

to_spec :: Text -> (ScoreTime, ScoreTime) -> (ScoreTime, ScoreTime, Text)
to_spec text (start, dur) = (start, dur, text)

instance Q.Arbitrary ScoreTime where
    arbitrary = ScoreTime.from_double <$> Q.arbitrarySizedFractional


-- * simple derive

-- | Simplified deriver.
simple_derive :: [Block] -> [Score.Event]
simple_derive [] = []
simple_derive (((_, tracks), skel) : blocks) =
    derive_block initial_state (mkblocks blocks) (Skeleton.make skel) tracks

type Blocks = Map Text Block

mkblocks :: [Block] -> Blocks
mkblocks blocks = Map.fromList $ zip (map (fst . fst) blocks) blocks

-- | Derive a block.  Use the skeleton to make a tree, then find the pitch and
-- control tracks by looking up and down the tree.
derive_block :: State -> Blocks -> Skeleton.Skeleton -> [Track]
    -> [Score.Event]
derive_block state blocks skel tracks =
    concatMap (derive_note_track state blocks) (extract_notes skel tracks)

-- TODO why is blocks unused?
derive_note_track :: State -> Blocks -> NoteTrack -> [Score.Event]
derive_note_track state blocks (notes, samples) =
    Lists.mergeLists Score.event_start $ snd $
        List.mapAccumL go (state, samples) (map to_score notes)
    where
    to_score (start, dur, text) =
        (RealTime.from_score start, RealTime.from_score dur, text)
    go (prev_state, prev_samples) (start, dur, text)
        | text == "" = ((state, samples), [event state start dur])
        | otherwise = error $ "call not supported: " ++ show text
        where (state, samples) = update_state prev_samples start prev_state
    event state start dur = Score.empty_event
        { Score.event_start = start
        , Score.event_duration = dur
        , Score.event_environ = Env.from_controls $ state_control_map state
        , Score.event_pitch = state_psignal state
        }

data Sample = Sample {
    sample_name :: Text -- ^ @*@ for pitch
    , sample_pos :: RealTime
    , sample_val :: Text
    } deriving (Show)

-- | (note track, pitch track, controls)
type NoteTrack = (Events, [Sample])

data State = State {
    state_pitch :: Pitch.NoteNumber
    , state_controls :: Map ScoreT.Control Signal.Y
    } deriving (Show)

initial_state :: State
initial_state = State 0 mempty

state_control_map :: State -> DeriveT.ControlMap
state_control_map = Map.map (ScoreT.untyped . Signal.constant) . state_controls

state_psignal :: State -> PSignal.PSignal
state_psignal = PSignal.constant . mknote . state_pitch
    where
    scale = PSignal.Scale Twelve.scale_id
        (Scale.scale_transposers Twelve.scale)
    mknote nn = PSignal.pitch scale (const (return nn))
        (const $ return $ Pitch.Note $ showt nn) mempty

update_state :: [Sample] -> RealTime -> State -> (State, [Sample])
update_state samples pos state = (foldl' go state pre, post)
    where
    (pre, post) = break ((>pos) . sample_pos) samples
    go state (Sample name _ val)
        | name == "*" = state { state_pitch = parse_pitch val }
        | otherwise = state
            { state_controls = Map.insert (ScoreT.unchecked_control name)
                (parse_control val) (state_controls state)
            }

parse_pitch :: Text -> Pitch.NoteNumber
parse_pitch text =
    fromMaybe (error $ "unparseable pitch: " ++ show text) $
        Map.lookup (Pitch.Note text) note_to_nn

note_to_nn :: Map Pitch.Note Pitch.NoteNumber
note_to_nn = Map.fromList
    [(note, nn) | (Just note, nn) <- zip (map Twelve.show_nn nns) nns]
    where nns = Lists.range 1 127 1

parse_control :: Text -> Signal.Y
parse_control text = fromMaybe (error $ "unparseable control: " ++ show text)
    (ParseText.float text)

block1 =
    [ ("c1", [(0, 0, "1")])
    , (">", [(0, 1, "")]), ("*", [(0, 0, "4c")])
    , (">", [(1, 1, "")]), ("*", [(1, 0, "5c")])
    ]
skel1 = [(0, 1), (1, 2), (0, 3), (3, 4)]

extract_notes :: Skeleton.Skeleton -> [Track] -> [NoteTrack]
extract_notes skel tracks
    | not (null missing) = error $ "extract_notes: skel " ++ show skel
        ++ " has missing tracknums: " ++ show missing
    | otherwise = do
        (track, parents, children) <- Trees.flatPaths trees
        guard (is_note track)
        let tracks = children ++ parents
        let pitch = fromMaybe ("*", []) (List.find is_pitch tracks)
        return (snd track, make_samples (pitch : filter is_control tracks))
    where
    (trees, missing) = TrackTree.resolve_track_tree
        (Map.fromList (zip [0..] tracks))
        (Skeleton.to_forest (length tracks) skel)
    is_note = (==">") . fst
    is_pitch = (=="*") . fst
    is_control = (`notElem` [">", "*", "tempo"]) . fst

make_samples :: [Track] -> [Sample]
make_samples = Lists.mergeLists sample_pos . map make
    where
    make (title, events) = [Sample title (RealTime.from_score pos) val
        | (pos, _, val) <- events]

