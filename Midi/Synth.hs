-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Simulate a MIDI synth and turn low level MIDI msgs back into a medium
-- level form.  This is a bit like \"unperform\".
module Midi.Synth where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

import qualified Util.Num as Num
import qualified Util.Lists as Lists
import qualified Midi.Midi as Midi
import qualified Midi.State as MState
import           Midi.State (Addr)

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import           Global
import           Types


-- * analyze

initialPitches :: [Note] -> [(Int, Pitch.NoteNumber)]
initialPitches notes = Lists.sortOn fst $ map Tuple.swap $ Map.toList $
    Map.fromListWith (+) [(initialPitch note, 1) | note <- notes ]

nonconstantPitches :: [Note] -> [Note]
nonconstantPitches = filter $ not . null . notePitches
    -- filter out ones after note-off
    -- take a decay time

initialPitch :: Note -> Pitch.NoteNumber
initialPitch = roundCents . notePitch

roundCents :: Pitch.NoteNumber -> Pitch.NoteNumber
roundCents = (/100) . Pitch.nn . round . (*100)

-- * compute

data State = State
    -- constant
    { statePbRange :: !(Map Addr PbRange)

    , stateChannel :: !MState.State
    -- | Notes still sounding.  This retains notes for 'deactivate_time' after
    -- their note-off to capture controls during the decay.
    , stateActive :: !(Map Addr [SoundingNote])
    , stateNotes :: ![Note]
    , stateWarns :: ![(Midi.WriteMessage, Text)]
    } deriving (Show)

emptyState :: State
emptyState = State
    { statePbRange = mempty
    , stateChannel = MState.empty
    , stateActive = mempty
    , stateNotes = []
    , stateWarns = []
    }

-- | (down, up)
type PbRange = (Pitch.NoteNumber, Pitch.NoteNumber)

getPbRange :: Addr -> State -> PbRange
getPbRange addr = Map.findWithDefault (-2, 2) addr . statePbRange
    -- This (-2, 2) thing is probably wrong, maybe I should crash.

-- | SoundingNotes may still be open.
type SoundingNote = NoteT (Maybe RealTime)
type Note = NoteT RealTime

data NoteT end = Note
    { noteStart :: RealTime
    , noteEnd :: end
    , noteKey :: Midi.Key
    , noteVel :: Midi.Velocity
    , notePitch :: Pitch.NoteNumber
    , notePitches :: [(RealTime, Pitch.NoteNumber)]
    , noteControls :: ControlMap
    , noteAddr :: Addr
    } deriving (Eq, Show)

type ControlMap = Map MState.Control [(RealTime, Midi.ControlValue)]

-- | Keep the current msg for 'warn'.
type SynthM a = Reader.ReaderT Midi.WriteMessage
    (State.StateT State Identity.Identity) a

modify :: (State -> State) -> SynthM ()
modify f = do
    st <- State.get
    State.put $! f st

run :: State -> [Midi.WriteMessage] -> State
run state msgs = postproc $ runState (mapM_ msg1 (Lists.zipPrev msgs))
    where
    runState = Identity.runIdentity . flip State.execStateT state
    msg1 (prev, wmsg) = flip Reader.runReaderT wmsg $ do
        let prevT = maybe (-RealTime.larger) Midi.wmsgTs prev
        when (Midi.wmsgTs wmsg < prevT) $
            warn $ "timestamp less than previous: " <> pretty prevT
        runMsg wmsg

postproc :: State -> State
postproc state_ = state
    { stateActive = Map.filterWithKey (\_ ns -> not (null ns)) $
        Map.map (map postprocNote) (stateActive state)
    , stateNotes = reverse $ map postprocNote (stateNotes state)
    , stateWarns = reverse $ stateWarns state
    }
    where
    state = deactivate 9999999 state_
    postprocNote note = note
        { noteControls = Map.map reverse (noteControls note)
        , notePitches = reverse (notePitches note)
        }

runMsg :: Midi.WriteMessage -> SynthM ()
runMsg wmsg@(Midi.WriteMessage dev ts (Midi.ChannelMessage chan msg)) = do
    let addr = (dev, chan)
    modify $ updateChannelState wmsg . deactivate ts
    case normalizeMsg msg of
        Midi.NoteOff key _ -> noteOff addr ts key
        Midi.NoteOn key vel -> noteOn addr ts key vel
        Midi.Aftertouch _ _ -> warn "aftertouch not supported"
        Midi.ControlChange c val -> control addr ts (MState.CC c) val
        Midi.ProgramChange _ -> warn "program change not supported"
        Midi.ChannelPressure val -> control addr ts MState.Pressure val
        Midi.PitchBend val -> pitchBend addr ts val
        _ -> warn "unhandled msg"
runMsg _ = pure ()

updateChannelState :: Midi.WriteMessage -> State -> State
updateChannelState wmsg state =
    state { stateChannel = MState.process (stateChannel state) msg }
    where msg = (Midi.wmsgDev wmsg, Midi.wmsgMsg wmsg)

normalizeMsg :: Midi.ChannelMessage -> Midi.ChannelMessage
normalizeMsg (Midi.NoteOn key 0) = Midi.NoteOff key 1
normalizeMsg msg = msg

-- | After notes have had a note-off time for a certain amount of time, move
-- them from 'state_active' to 'state_notes'.  The certain amount of time
-- should be the note's decay time, but since I don't really know that, just
-- pick an arbitrary constant.
deactivate :: RealTime -> State -> State
deactivate now state = state
    { stateActive = stillActive
    , stateNotes = mapMaybe close (concat done) ++ stateNotes state
    }
    where
    (addrs, (done, active)) = second unzip $ unzip $
        map (second (List.partition noteDone)) $
        Map.toList (stateActive state)
    stillActive = Map.fromList $ filter (not . null . snd) $ zip addrs active
    noteDone note = case noteEnd note of
        Just d -> now >= d + deactivateTime
        Nothing -> False
    close note = case noteEnd note of
        Nothing -> Nothing
        Just d -> Just $ note { noteEnd = d }

deactivateTime :: RealTime
deactivateTime = 1

-- * msgs

noteOn :: Addr -> RealTime -> Midi.Key -> Midi.Velocity -> SynthM ()
noteOn addr ts key vel = do
    active <- State.gets $ Map.findWithDefault [] addr . stateActive
    let sounding = filter (keySounding key) active
    unless (null sounding) $
        warn $ "sounding notes: " <> pretty sounding
    channel <- State.gets $ MState.getChannel addr . stateChannel
    pbRange <- State.gets $ getPbRange addr
    modifyNotes addr (makeNote pbRange channel addr ts key vel :)

makeNote :: PbRange -> MState.Channel -> Addr -> RealTime -> Midi.Key
    -> Midi.Velocity -> SoundingNote
makeNote pbRange state addr start key vel = Note
    { noteStart = start
    , noteEnd = Nothing
    , noteKey = key
    , noteVel = vel
    , notePitch = convertPitch pbRange key (MState.chanPb state)
    , notePitches = []
    , noteControls = here <$> MState.chanControls state
    , noteAddr = addr
    }
    where here val = [(start, val)]

noteOff :: Addr -> RealTime -> Midi.Key -> SynthM ()
noteOff addr ts key = do
    active <- State.gets $ Map.findWithDefault [] addr . stateActive
    let (sounding, rest) = List.partition (keySounding key) active
    case sounding of
        [] -> warn "no sounding notes"
        n : ns -> do
            unless (null ns) $
                warn $ "multiple sounding notes: " <> pretty sounding
            when (ts - noteStart n < 0.01) $
                warn $ "short note: dur " <> pretty (ts - noteStart n)
                    <> " of " <> pretty n
            modifyNotes addr $ const $
                n { noteEnd = Just ts } : rest

keySounding :: Midi.Key -> SoundingNote -> Bool
keySounding key n = noteEnd n == Nothing && noteKey n == key

-- | Append a CC change to all sounding notes.
control :: Addr -> RealTime -> MState.Control -> Midi.ControlValue -> SynthM ()
control addr ts control val = modifyNotes addr (map insert)
    where
    insert note = note { noteControls =
        Map.insertWith (++) control [(ts, val)] (noteControls note) }

-- | Append pitch bend to all sounding notes.
pitchBend :: Addr -> RealTime -> Midi.PitchBendValue -> SynthM ()
pitchBend addr ts val = do
    pbRange <- State.gets $ getPbRange addr
    modifyNotes addr (map (insert pbRange))
    where
    insert pbRange note = note { notePitches =
        (ts, convertPitch pbRange (noteKey note) val) : notePitches note }

convertPitch :: PbRange -> Midi.Key -> Midi.PitchBendValue -> Pitch.NoteNumber
convertPitch (down, up) key val = Midi.fromKey key + convert val
    where
    convert v
        | v >= 0 = Pitch.nn (Num.f2d v) * up
        | otherwise = Pitch.nn (- (Num.f2d v)) * down

modifyNotes :: Addr -> ([SoundingNote] -> [SoundingNote]) -> SynthM ()
modifyNotes addr f = do
    active <- State.gets stateActive
    let notes = Map.findWithDefault [] addr active
    modify $ \state -> state
        { stateActive = Map.insert addr (f notes) (stateActive state) }

-- * util

warn :: Text -> SynthM ()
warn msg = do
    wmsg <- Reader.ask
    State.modify $ \state ->
        state { stateWarns = (wmsg, msg) : stateWarns state }


-- * pretty

-- | Format synth state in an easier to read way.
prettyState :: State -> Text
prettyState (State _pbRange _chan active notes warns) =
    Text.intercalate "\n" $ concatMap (\(a, b) -> "  " <> a : b) $
    filter (not . null . snd)
        [ ("active:", map pretty (concat (Map.elems active)))
        , ("warns:", map prettyWarn warns)
        , ("notes:", map pretty notes)
        ]

instance Pretty end => Pretty (NoteT end) where
    pretty (Note start end key vel pitch0 pitches1 controls (dev, chan)) =
        Text.unwords $ filter (not . Text.null)
            [ addrS, pretty key
            , pretty start <> "--" <> pretty end <> ":"
            , "vel:" <> Num.hex 2 vel
            , if null pitches then "" else "p:" <> pretty pitches
            , if Map.null controls then ""
                else "c:" <> prettyControls controls
            ]
        where
        pitches = dropWhile ((== Midi.fromKey key) . snd) $
            (start, pitch0) : pitches1
        addrS = pretty dev <> ":" <> showt chan

prettyControls :: ControlMap -> Text
prettyControls controls = Text.intercalate "\n\t"
    [showt cont <> ":" <> pretty vals | (cont, vals) <- Map.assocs controls]

prettyWarn :: (Midi.WriteMessage, Text) -> Text
prettyWarn (Midi.WriteMessage dev ts (Midi.ChannelMessage chan msg), warn) =
    pretty ts <> " " <> pretty dev <> ":" <> showt chan
        <> " " <> pretty msg <> ": " <> warn
prettyWarn (Midi.WriteMessage dev ts msg, warn) =
    pretty ts <> " " <> pretty dev <> ":" <> pretty msg <> ": " <> warn
