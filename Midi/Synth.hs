-- | Simulate a MIDI synth and turn low level MIDI msgs back into a medium
-- level form.  This is a bit like \"unperform\".
--
-- TODO I'll probably want a more efficient signal format eventually.
module Midi.Synth where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State

import qualified Data.Map as Map
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime
import Types


data State = State {
    -- | Notes still sounding.
    state_active :: Map.Map Addr [Note]
    -- | Notes already complete.  I suppose I should take the Maybe off
    -- the duration but can't be bothered.
    , state_notes :: [Note]
    , state_warns :: [(Midi.WriteMessage, String)]
    -- | Decay time of instruments by addr.
    , state_decay :: Map.Map Addr RealTime
    -- | Pitch bend range for instruments by addr.
    , state_pb_range :: Map.Map Addr (Double, Double)
    } deriving (Show)
empty_state = State mempty [] [] mempty mempty

default_decay = RealTime.seconds 1
default_pb_range = (-1, 1)

data Note = Note {
    note_start :: RealTime
    -- | Nothing if there was no NoteOff.
    , note_duration :: Maybe RealTime
    , note_key :: Midi.Key
    , note_vel :: Midi.Velocity
    , note_pitch :: [(RealTime, Double)]
    , note_controls :: ControlMap
    , note_addr :: Addr
    } deriving (Eq, Show)

make_note :: Addr -> RealTime -> Midi.Key -> Midi.Velocity -> Note
make_note addr start key vel =
    Note start Nothing key vel [(start, fromIntegral key)] Map.empty addr

note_end :: Note -> Maybe RealTime
note_end n = (+ note_start n) <$> note_duration n

data Control = CC Midi.Control | Aftertouch | Pressure deriving (Eq, Ord, Show)

type Addr = (Midi.WriteDevice, Midi.Channel)
type ControlMap = Map.Map Control [(RealTime, Midi.ControlValue)]

type SynthM a = Reader.ReaderT Midi.WriteMessage
    (State.StateT State Identity.Identity) a

-- | Each note will be given all controls in its duration + 1 second after
run :: State -> [Midi.WriteMessage] -> State
run state msgs = postproc $ run_state (mapM_ msg1 (Seq.zip_prev msgs))
    where
    run_state = Identity.runIdentity . flip State.execStateT state
    msg1 (prev, wmsg) = Reader.runReaderT
        (run_msg (maybe 0 Midi.wmsg_ts prev) wmsg) wmsg

postproc :: State -> State
postproc st0 = state
    { state_active = Map.filterWithKey (\_ ns -> not (null ns))
        (Map.map (map postproc_note) (state_active state))
    , state_notes = reverse (map postproc_note (state_notes state))
    , state_warns = reverse (state_warns state)
    }
    where
    state = deactivate (RealTime.seconds 10000) st0
    postproc_note note = note
        { note_controls = Map.map reverse (note_controls note)
        , note_pitch = reverse (note_pitch note)
        }

run_msg :: RealTime -> Midi.WriteMessage -> SynthM ()
run_msg prev_ts (Midi.WriteMessage dev ts (Midi.ChannelMessage chan msg))
        = do
    State.modify $ deactivate ts
    when (ts < prev_ts) $
        warn $ "timestamp less than previous: " ++ Pretty.pretty prev_ts
    case msg of
        Midi.NoteOff key _ -> ifM (is_active addr key)
            (note_off addr ts key)
            (warn "note off not preceded by note on")
        Midi.NoteOn key vel -> ifM (is_active addr key)
            (do warn "double note on"
                note_off addr ts key
                note_on addr ts key vel)
            (note_on addr ts key vel)
        Midi.Aftertouch _ _ -> warn "aftertouch not supported"
        Midi.ControlChange c val -> control addr ts (CC c) val
        Midi.ProgramChange _ -> warn "program change not supported"
        Midi.ChannelPressure val -> control addr ts Pressure val
            -- add to all notes with this addr that still sound
        Midi.PitchBend val -> pitch_bend addr ts val
            -- look up pb range, add to pitch of notes with this addr
        _ -> warn "unhandled msg"
    where addr = (dev, chan)
run_msg _ _ = return ()

is_active :: Addr -> Midi.Key -> SynthM Bool
is_active addr key = do
    active <- State.gets state_active
    return $ case Map.lookup addr active of
        Just notes -> any ((==key) . note_key) notes
        Nothing -> False

note_on :: Addr -> RealTime -> Midi.Key -> Midi.Velocity -> SynthM ()
note_on addr ts key vel =
    modify_notes Nothing addr (make_note addr ts key vel :)

note_off :: Addr -> RealTime -> Midi.Key -> SynthM ()
note_off addr ts key = modify_notes Nothing addr (map set_dur)
    where
    set_dur note
        | note_key note == key = note { note_duration = Just ts }
        | otherwise = note

control :: Addr -> RealTime -> Control -> Midi.ControlValue -> SynthM ()
control addr ts control val = modify_notes (Just warning) addr (map insert)
    where
    insert note = note { note_controls =
        Map.insertWith (++) control [(ts, val)] (note_controls note) }
    warning = show control ++ " without note"

pitch_bend :: Addr -> RealTime -> Midi.PitchBendValue -> SynthM ()
pitch_bend addr ts val = do
    (up, down) <- Map.findWithDefault default_pb_range addr <$>
        State.gets state_pb_range
    let rel_pitch = if val >= 0 then Num.f2d val * up else Num.f2d val * (-down)
    modify_notes (Just "pitch bend without note") addr (map (insert rel_pitch))
    where
    insert rel_pitch note = note { note_pitch =
        (ts, fromIntegral (note_key note) + rel_pitch) : note_pitch note }

modify_notes :: Maybe String -> Addr -> ([Note] -> [Note]) -> SynthM ()
modify_notes maybe_msg addr f = do
    active <- State.gets state_active
    let notes = Map.findWithDefault [] addr active
    case (maybe_msg, notes) of
        (Just msg, []) -> warn msg
        _ -> State.modify $ \state  -> state
            { state_active = Map.insert addr (f notes) (state_active state) }

warn :: String -> SynthM ()
warn msg = do
    wmsg <- Reader.ask
    State.modify $ \state ->
        state { state_warns = (wmsg, msg) : state_warns state }

deactivate :: RealTime -> State -> State
deactivate ts state = state
    { state_active = Map.map (filter (not . decayed)) (state_active state)
    , state_notes = decayed_notes ++ state_notes state
    }
    where
    decayed_notes = filter decayed (concat (Map.elems (state_active state)))
    decayed n = case note_end n of
        Nothing -> False
        Just end -> end + decay_of (note_addr n) <= ts
    decay_of addr = Map.findWithDefault default_decay addr (state_decay state)


-- * pretty

-- | Format synth state in an easier to read way.
pretty_state :: State -> String
pretty_state (State active notes warns _ _) = Seq.join "\n" $
    ["active:"]
    ++ map Pretty.pretty (concat (Map.elems active))
    ++ ["", "warns:"]
    ++ map pretty_warn warns
    ++ ["", "notes:"]
    ++ map Pretty.pretty notes

pretty_controls :: ControlMap -> String
pretty_controls controls = Seq.join "\n\t"
    [show cont ++ ":" ++ Pretty.pretty vals
        | (cont, vals) <- Map.assocs controls]

pretty_warn :: (Midi.WriteMessage, String) -> String
pretty_warn (Midi.WriteMessage dev ts (Midi.ChannelMessage chan msg), warn) =
    Pretty.pretty ts ++ " " ++ Pretty.pretty dev ++ ":" ++ show chan ++ " "
        ++ show msg ++ ": " ++ warn
pretty_warn (Midi.WriteMessage dev ts msg, warn) =
    Pretty.pretty ts ++ " " ++ Pretty.pretty dev ++ ":" ++ show msg
        ++ ": " ++ warn

instance Pretty.Pretty Note where
    pretty (Note start dur _key vel pitch controls (dev, chan)) =
        Printf.printf "%s %s--%s: %s V:%x C: %s" addr_s (Pretty.pretty start)
            (maybe "" Pretty.pretty dur) (Pretty.pretty pitch)
            vel (pretty_controls controls)
        where
        addr_s = Pretty.pretty dev ++ ":" ++ show chan
