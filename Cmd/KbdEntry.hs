{-# LANGUAGE ViewPatterns #-}
{- | This module implements kbd entry by intercepting kbd events and
    re-emitting them as either Notes or ReadMessages.  The former are handed
    off to the PitchTrack entry commands since those understand Notes and not
    midi, while the latter go off to 'Cmd.MidiThru.cmd_midi_thru' which then
    thinks you are playing a midi keyboard and acts accordingly.

    It might be cleaner to translate midi and kbd entry into only Notes.  Then
    midi thru takes the Note msgs and turns them into the appropriate midi.
    This way I don't have to do separate conversions to Notes for input and
    midi for thru.

    However, then I have to have NoteOn, NoteOff, and NoteControl at the least.
    This also precludes me from using a more efficient low-level thru, which
    will be essential with e.g. continuum input.

    But I have to remap pitch and possibly controllers anyway if I want to
    support scales.  If I separate NoteOn from NotePitch then I can have the
    former create a note event and the latter create a pitch track event.
    I should be able to touch a continuum note and have the correct pitch go
    into the pitch track.  Also, it would be cool to advance the insert point
    only on NoteOff.  That would also be a good way to input chords.  TODO
    KeyNumbers shouldn't be limited to integers anyway

    So how can I get the speed of the former with the correct behaviour of the
    latter?

    I should find out what makes the responder stack so slow.  Can I profile
    that?

    - The sync afterwards: Some mechanism to find out if no Ui.State changes
    have happened and skip sync.

    - Marshalling the cmd list: cache the expensive parts.  The only changing
    bit is the focus cmds, so keep those until focus changes.  Or if it's the
    skeleton parse, that might be fixed by moving to the explicit skeleton.

    If responder stack speedup isn't enough, then I can run thru on a separate
    short-circuit threod:

    midi >> to_input >> input_to_note >> note_to_midi
    - midi -> input (scale-independent representation)
    - input -> note (lookup current inst, lookup note in scale)
    - note -> nn (lookup note in scale)

    The nice thing about this is it unifies the "(note_c, vel_c)->midi" backend
    so they have consistent results.  Of course they're not really unified
    since I don't want to call the player for every note, but they are
    operating from the same signals.

    This way, the differences between controllers are all abstracted out and
    adding non-midi-like controllers is natural.  This chain just needs
    cur_inst for scale, controllers, and addr, so I should be able to stick it
    in a short-circuit thread.

    However, it's much more complicated than just fiddling the channel and
    device and sending it back out.  A single NoteOn turns into three msgs
    (pitch, vel, note) and turns back into a single NoteOn.

    to_input (NoteOn nn vel) =
        [KeyNum oct degree offset, Control "vel" vel, NoteOn]
        where
        (oct, degree, offset) = nn_to_input controller_pb_range nn

    input_to_note cur_inst_scale input = ...
    note_to_midi note_c vel_c = note_to_nn note_c

    Requirements:

    1. remap addr based on current inst

    2. low latency, can handle continuum input stream without noticeable lag

    3. can tune notes as they are being entered, record velocity, insertion
    point only advances on keyup

    4. retune notes going thru based on current scale

    5. emit keyswitch and program change if necessary based on current inst

    Thru requires:

    thru controller inst chan_state (NoteOn nn vel) = NoteOn nn vel
        where
            -- some multiplication
            -- (oct, degree, offset)
        input = pitch_from_midi (c_pb_range controller) (chan_pb chan_state) nn
            -- lookup input in scale, twelve should be fast since input is
            -- already even tempered
        note = input_to_note (inst_scale inst) input
        note_to_midi (inst_pb_range inst)

    3 scales: controller scale, ptrack scale, inst scale

    pelog   pelog   twelve
    ding    ding    4c-+42

    twelve  pelog   twelve
    4c-     ding-42 4c-

    twelve  twelve  pelog
    4c-     4c-     ding-42
    64      64      pitch_to_nn inst_scale (note_to_pitch "4c-")

    Switching the controller scale is useful to enter pitches from different
    scales.  Switching ptrack scale is useful to play various scales when the
    underlying inst isn't.  Switching inst scale is useful to optimize for
    a certain scale by tuning on the inst end, or if samples are in that scale.

    However, for thru only the controller and inst scales need to be taken into
    account.  For note entry, controller and ptrack scales are needed.  For
    performance, ptrack and inst scales are needed.

    midi/kbd -> to_input c_scale -> input_to_note ptrack_scale -> track
                                 -> input_to_midi inst_scale -> midi out

    TODO
    - complete current hybrid Note / ReadMessage approach with only simple thru
      (supports 1 and maybe 4)
    - do skeleton redesign, hopefully this speeds up get_focus_cmds
    implementation
      - 2 requires some profiling and possibly optimization, but also depends on
        the rest of the implementation.
      - 3 requires NoteOn NoteOff and Control msgs, also Twelve etc. should
        support fractional notes.  I also need to know the scale and pb_range
        of the controller, so implement ControllerInfo in the static config.
      - 4 requires input_to_midi, this should use the same code as the
        performer when it turns a pitch signal into nn+pb.
      - 5 requires the ChannelMap to thread through midi_thru and the same
        logic as the performer

    Midi and kbd filters convert into Note msgs.
    Notes are turned into track edits, have to keep track of note_id
    Notes are also turned into midi via thru

    If this is too slow: as above, but insert a filter on get_msg that
    converts midi in directly into midi thru.
-}
module Cmd.KbdEntry where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Midi.Midi as Midi
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Key as Key

import qualified Cmd.Keymap as Keymap
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp

{- TODO
with_midi generates ReadMessages, which go to thru
with_note generates Notes, which go to PitchTrack editing stuff

The problem is that there are two functions to call, so when with_note is to be
called, with_midi can't return Done, but if with_note won't be called and
with_midi returns Continue, keys will fall through to other cmds.

One way around is a single cmd that captures keys and calls both thru and
PitchTrack cmds, but this makes default_cmds uglier

Another way is a "post cmd" which swallows alphanum keys if kbd_entry is set,
but this seems inefficient.

In the future to avoid thru latency I may short circuit the whole thu side
which will also eliminate this problem, so revisit this when the thu issue is
settled.
-}

-- * with_note

type Translator = Msg.Msg -> Maybe Msg.Msg

-- | Take a Key (if @kbd_entry@ is True) or a ReadMessage to a Ui.Note.
with_note :: Bool -> Cmd.Cmd -> Cmd.Cmd
with_note kbd_entry cmd msg = do
    has_mods <- are_modifiers_down
    kbd_note <- if kbd_entry && not has_mods
        then do
            octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
            return (note_from_kbd octave msg)
        else return Nothing
    let maybe_new_msg = kbd_note `mplus` note_from_midi msg
    case maybe_new_msg of
        Just new_msg -> cmd new_msg
        Nothing -> cmd msg

note_from_kbd :: Pitch.Octave -> Translator
note_from_kbd octave (Msg.key -> Just key) =
    fmap Msg.InputKey (key_to_input octave key)
note_from_kbd _ _ = Nothing

note_from_midi :: Translator
note_from_midi (Msg.midi -> Just (Midi.ChannelMessage _ (Midi.NoteOn nn _))) =
    Just $ Msg.InputKey (nn_to_input nn)
note_from_midi _ = Nothing

-- * with_midi

-- | Take a Key to a ReadMessage, for midi thru.
with_midi :: Pitch.ScaleId -> Cmd.Cmd -> Cmd.Cmd
with_midi scale_id cmd msg = do
    has_mods <- are_modifiers_down
    when has_mods Cmd.abort
    scale <- Cmd.get_scale "with_midi" scale_id
    octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    let new_msg = midi_from_kbd scale octave msg
    case new_msg of
        Nothing -> Cmd.abort
        Just msg -> cmd msg

midi_from_kbd :: Pitch.Scale -> Pitch.Octave -> Msg.Msg -> Maybe Msg.Msg
midi_from_kbd scale octave msg = do
    (state, key) <- key_char msg
    input <- key_to_input octave key
    chan_msg <- midi_of scale state input
    return $ kbd_read_message chan_msg

key_char (Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd state key)))) =
    Just (state, key)
key_char _ = Nothing

kbd_read_message :: Midi.ChannelMessage -> Msg.Msg
kbd_read_message chan_msg = Msg.Midi $
    Midi.ReadMessage (Midi.ReadDevice "kbd_entry") Timestamp.immediately
        (Midi.ChannelMessage 0 chan_msg)

midi_of :: Pitch.Scale -> UiMsg.KbdState -> Pitch.InputKey
    -> Maybe Midi.ChannelMessage
midi_of scale state input = do
    nn <- input_to_nn scale input
    return $ case state of
        UiMsg.KeyDown -> Midi.NoteOn nn 100
        UiMsg.KeyUp -> Midi.NoteOff nn 100

input_to_nn :: Pitch.Scale -> Pitch.InputKey -> Maybe Midi.Key
input_to_nn scale input = do
    note <- Pitch.scale_input_to_note scale input
    Pitch.NoteNumber nn <- Pitch.scale_note_to_nn scale note
    -- TODO send PitchBend, but this will eventually be unified with MidiThru
    -- anyway
    return $ floor nn

nn_to_input :: Midi.Key -> Pitch.InputKey
nn_to_input nn = Pitch.InputKey (fromIntegral oct, fromIntegral degree)
    where (oct, degree) = nn `divMod` 12

are_modifiers_down :: (Monad m) => Cmd.CmdT m Bool
are_modifiers_down = fmap (not . Set.null) Keymap.mods_down

-- * implementation

key_to_input :: Pitch.Octave -> Key.Key -> Maybe Pitch.InputKey
key_to_input oct_transpose (Key.KeyChar c) =
    fmap (add_oct oct_transpose) (Map.lookup c note_map)
key_to_input _ _ = Nothing

add_oct :: Pitch.Octave -> Pitch.InputKey -> Pitch.InputKey
add_oct oct (Pitch.InputKey (o, n)) = Pitch.InputKey (oct+o, n)

note_map :: Map.Map Char Pitch.InputKey
note_map = Map.fromList (upper_notes ++ lower_notes)

-- | These are the 10 keys from left to right, not including @[]-=@ etc. since
-- those don't appear on the lower \"manual\" and they're useful for other
-- things anyway.
--
-- Qwerty's @z@ and @q@ should be the "middle C" of the scale in two different
-- octaves, whatever that means.
--
-- This maps @a@ and @1@ to degree -1.  Even if the scale doesn't understand
-- that, it keeps those keys from leaking through to trigger some other
-- command.
lower_notes, upper_notes :: [(Char, Pitch.InputKey)]
lower_notes = make_key_map 0 "azsxdcfvgbhnjmk,l.;/"
upper_notes = make_key_map 1 "1q2w3e4r5t6y7u8i9o0p"

make_key_map :: Pitch.Octave -> [Char] -> [(Char, Pitch.InputKey)]
make_key_map oct = map mk_input . zip [-1..]
    where
    mk_input (n, c) =
        (Keymap.hardcoded_kbd_layout Map.! c, Pitch.InputKey (oct, n))
