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

import qualified Util.Lists as Lists
import qualified Util.ParseText as ParseText
import qualified Util.Trees as Trees

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
simplePitch :: Q.Gen Block
simplePitch = do
    ranges <- granges
    pitches <- Q.vectorOf (length ranges) gpitch
    let ptrack = [(p, 0, Pitch.noteText n)
            | (p, n) <- zip (map fst ranges) pitches]
    pure (("block", [(">", map (toSpec "") ranges), ("*", ptrack)]),
        [(1, 2)])

granges :: Q.Gen [(ScoreTime, ScoreTime)]
granges = Q.sized $ \n -> go 0 =<< Q.choose (0, n)
    where
    go _ 0 = pure []
    go prev n = do
        dur <- abs <$> Q.arbitrary
        gap <- Q.oneof [pure 0, abs <$> Q.arbitrary]
        let start = prev + gap
        rest <- go (start + dur) (n - 1)
        pure $ (start, start + dur) : rest

gpitch :: Q.Gen Pitch.Note
gpitch = Q.oneof $ map pure (Map.keys noteToNn)

-- | Random list of non-overlapping events, some of which are adjacent.
gevents :: Q.Gen [Event.Event]
gevents = Events.ascending . Events.fromList <$> Q.listOf (gevent "")

gevent :: Text -> Q.Gen Event.Event
gevent text = do
    start <- Q.arbitrarySizedFractional
    dur <- Q.arbitrarySizedFractional
    pure $ Event.event (abs start) (abs dur) text

toSpec :: Text -> (ScoreTime, ScoreTime) -> (ScoreTime, ScoreTime, Text)
toSpec text (start, dur) = (start, dur, text)

instance Q.Arbitrary ScoreTime where
    arbitrary = ScoreTime.fromDouble <$> Q.arbitrarySizedFractional


-- * simple derive

-- | Simplified deriver.
simpleDerive :: [Block] -> [Score.Event]
simpleDerive [] = []
simpleDerive (((_, tracks), skel) : blocks) =
    deriveBlock initialState (mkblocks blocks) (Skeleton.make skel) tracks

type Blocks = Map Text Block

mkblocks :: [Block] -> Blocks
mkblocks blocks = Map.fromList $ zip (map (fst . fst) blocks) blocks

-- | Derive a block.  Use the skeleton to make a tree, then find the pitch and
-- control tracks by looking up and down the tree.
deriveBlock :: State -> Blocks -> Skeleton.Skeleton -> [Track]
    -> [Score.Event]
deriveBlock state blocks skel tracks =
    concatMap (deriveNoteTrack state blocks) (extractNotes skel tracks)

-- TODO why is blocks unused?
deriveNoteTrack :: State -> Blocks -> NoteTrack -> [Score.Event]
deriveNoteTrack state blocks (notes, samples) =
    Lists.mergeLists Score.eventStart $ snd $
        List.mapAccumL go (state, samples) (map toScore notes)
    where
    toScore (start, dur, text) =
        (RealTime.fromScore start, RealTime.fromScore dur, text)
    go (prevState, prevSamples) (start, dur, text)
        | text == "" = ((state, samples), [event state start dur])
        | otherwise = error $ "call not supported: " ++ show text
        where (state, samples) = updateState prevSamples start prevState
    event state start dur = Score.emptyEvent
        { Score.eventStart = start
        , Score.eventDuration = dur
        , Score.eventEnviron = Env.fromControls $ stateControlMap state
        , Score.eventPitch = statePsignal state
        }

data Sample = Sample
    { sampleName :: Text -- ^ @*@ for pitch
    , samplePos :: RealTime
    , sampleVal :: Text
    } deriving (Show)

-- | (note track, pitch track, controls)
type NoteTrack = (Events, [Sample])

data State = State
    { statePitch :: Pitch.NoteNumber
    , stateControls :: Map ScoreT.Control Signal.Y
    } deriving (Show)

initialState :: State
initialState = State 0 mempty

stateControlMap :: State -> ScoreT.ControlMap
stateControlMap = Map.map (ScoreT.untyped . Signal.constant) . stateControls

statePsignal :: State -> PSignal.PSignal
statePsignal = PSignal.constant . mknote . statePitch
    where
    scale = PSignal.Scale Twelve.scaleId
        (Scale.scaleTransposers Twelve.scale)
    mknote nn = PSignal.pitch scale (const (pure nn))
        (const $ pure $ Pitch.Note $ showt nn) mempty

updateState :: [Sample] -> RealTime -> State -> (State, [Sample])
updateState samples pos state = (foldl' go state pre, post)
    where
    (pre, post) = break ((>pos) . samplePos) samples
    go state (Sample name _ val)
        | name == "*" = state { statePitch = parsePitch val }
        | otherwise = state
            { stateControls = Map.insert (ScoreT.Control name)
                (parseControl val) (stateControls state)
            }

parsePitch :: Text -> Pitch.NoteNumber
parsePitch text =
    fromMaybe (error $ "unparseable pitch: " ++ show text) $
        Map.lookup (Pitch.Note text) noteToNn

noteToNn :: Map Pitch.Note Pitch.NoteNumber
noteToNn = Map.fromList
    [(note, nn) | (Just note, nn) <- zip (map Twelve.showNn nns) nns]
    where nns = Lists.range 1 127 1

parseControl :: Text -> Signal.Y
parseControl text = fromMaybe (error $ "unparseable control: " ++ show text)
    (ParseText.float text)

block1 =
    [ ("c1", [(0, 0, "1")])
    , (">", [(0, 1, "")]), ("*", [(0, 0, "4c")])
    , (">", [(1, 1, "")]), ("*", [(1, 0, "5c")])
    ]
skel1 = [(0, 1), (1, 2), (0, 3), (3, 4)]

extractNotes :: Skeleton.Skeleton -> [Track] -> [NoteTrack]
extractNotes skel tracks
    | not (null missing) = error $ "extract_notes: skel " ++ show skel
        ++ " has missing tracknums: " ++ show missing
    | otherwise = do
        (track, parents, children) <- Trees.flatPaths trees
        guard (isNote track)
        let tracks = children ++ parents
        let pitch = fromMaybe ("*", []) (List.find isPitch tracks)
        pure (snd track, makeSamples (pitch : filter isControl tracks))
    where
    (trees, missing) = TrackTree.resolveTrackTree
        (Map.fromList (zip [0..] tracks))
        (Skeleton.toForest (length tracks) skel)
    isNote = (==">") . fst
    isPitch = (=="*") . fst
    isControl = (`notElem` [">", "*", "tempo"]) . fst

makeSamples :: [Track] -> [Sample]
makeSamples = Lists.mergeLists samplePos . map make
    where
    make (title, events) = [Sample title (RealTime.fromScore pos) val
        | (pos, _, val) <- events]

