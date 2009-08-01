{-# LANGUAGE ViewPatterns #-}
{- | This module implements kbd entry by intercepting kbd events and
re-emitting them as either Notes or ReadMessages.  The former are handed off to
the PitchTrack entry commands since those understand Notes and not midi, while
the latter go off to 'Cmd.MidiThru.cmd_midi_thru' which then thinks you are
playing a midi keyboard and acts accordingly.

It might be cleaner to translate midi and kbd entry into only Notes.  Then midi
thru takes the Note msgs and turns them into the appropriate midi.  This way
I don't have to do separate conversions to Notes for input and midi for thru.

However, then I have to have NoteOn, NoteOff, and NoteControl at the least.
This also precludes me from using a more efficient low-level thru, which will
be essential with e.g. continuum input.

But I have to remap pitch and possibly controllers anyway if I want to support
scales.  If I separate NoteOn from NotePitch then I can have the former create
a note event and the latter create a pitch track event.  I should be able to
touch a continuum note and have the correct pitch go into the pitch track.
Also, it would be cool to advance the insert point only on NoteOff.  That would
also be a good way to input chords.
TODO KeyNumbers shouldn't be limited to integers anyway

So how can I get the speed of the former with the correct behaviour of the
latter?

I should find out what makes the responder stack so slow.  Can I profile that?

- The sync afterwards: Some mechanism to find out if no Ui.State changes have
happened and skip sync.

- Marshalling the cmd list: cache the expensive parts.  The only changing bit
is the focus cmds, so keep those until focus changes.  Or if it's the skeleton
parse, that might be fixed by moving to the explicit skeleton.

If responder stack speedup isn't enough, then I can run thru on a separate
short-circuit threod:

midi >> to_keynum >> keynum_to_note >> note_to_midi
- midi -> keynum (scale-independent representation)
- keynum -> note (lookup current inst, lookup note in scale)
- note -> nn (lookup note in scale)

The nice thing about this is it unifies the "(note_c, vel_c)->midi" backend so
they have consistent results.  Of course they're not really unified since
I don't want to call the player for every note, but they are operating from the
same signals.

This way, the differences between controllers are all abstracted out and adding
non-midi-like controllers is natural.  This chain just needs cur_inst for
scale, controllers, and addr, so I should be able to stick it in
a short-circuit thread.

However, it's much more complicated than just fiddling the channel and device
and sending it back out.  A single NoteOn turns into three msgs (pitch, vel,
note) and turns back into a single NoteOn.

to_keynum (NoteOn nn vel) =[KeyNum oct degree offset, Control "vel" vel, NoteOn]
    where
    (oct, degree, offset) = nn_to_keynum controller_pb_range nn

keynum_to_note cur_inst_scale keynum = ...
note_to_midi note_c vel_c = note_to_nn note_c

Requirements:

1. remap addr based on current inst

2. low latency, can handle continuum input stream without noticeable lag

3. can tune notes as they are being entered, record velocity, insertion point
only advances on keyup

4. retune notes going thru based on current scale

5. emit keyswitch and program change if necessary based on current inst

Thru requires:

thru controller inst chan_state (NoteOn nn vel) = NoteOn nn vel
    where
        -- some multiplication
        -- (oct, degree, offset)
    keynum = pitch_from_midi (c_pb_range controller) (chan_pb chan_state) nn
        -- lookup keynum in scale, twelve should be fast since keynum is
        -- already even tempered
    note = keynum_to_note (inst_scale inst) keynum
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

midi/kbd -> to_keynum c_scale -> keynum_to_note ptrack_scale -> track
                              -> keynum_to_midi inst_scale -> midi out

TODO
- complete current hybrid Note / ReadMessage approach with only simple thru
  (supports 1 and maybe 4)
- do skeleton redesign, hopefully this speeds up get_focus_cmds
implementation
  - 2 requires some profiling and possibly optimization, but also depends on
    the rest of the implementation.
  - 3 requires NoteOn NoteOff and Control msgs, also Twelve etc. should support
    fractional notes.  I also need to know the scale and pb_range of the
    controller, so implement ControllerInfo in the static config.
  - 4 requires keynum_to_midi, this should use the same code as the performer
    when it turns a pitch signal into nn+pb.
  - 5 requires the ChannelMap to thread through midi_thru and the same logic as
    the performer

-}
module Cmd.KbdEntry where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Midi.Midi as Midi
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Key as Key

import qualified Cmd.Keymap as Keymap
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp

import qualified Util.Log as Log

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

-- | Eat kbd_entry keys so they don't fall through to other cmds.
cmd_eat_keys :: Cmd.Cmd
cmd_eat_keys msg = do
    mods <- modifiers_down
    when (not (null mods)) Cmd.abort -- keys with mods go through
    case Msg.key msg of
        Just (Key.KeyChar c) | c `Map.member` note_map -> return Cmd.Done
        _ -> return Cmd.Continue

-- | @Nothing@ means no match, @Just Nothing@ means matched, but no Note should
-- be emitted, and @Just (Just Note)@ means to emit the given Note.
type Translator = Msg.Msg -> Maybe (Maybe Msg.Msg)

-- | Take a Key (if @kbd_entry@ is True) or a ReadMessage to a Ui.Note.
with_note :: Bool -> Cmd.Cmd -> Cmd.Cmd
with_note kbd_entry cmd msg = do
    kbd_note <- if kbd_entry
        then do
            mods <- modifiers_down
            octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
            return (note_from_kbd mods octave msg)
        else return Nothing
    let midi_note = note_from_midi msg
    let maybe_new_msg = kbd_note `mplus` midi_note
    case maybe_new_msg of
        Just (Just new_msg) -> cmd new_msg
        Just Nothing -> Cmd.abort
        Nothing -> cmd msg

note_from_kbd :: [Cmd.Modifier] -> Cmd.Octave -> Translator
note_from_kbd mods octave (Msg.key -> Just key)
    | not (null mods) = Nothing
    | otherwise = case key_to_keynum octave key of
        Just (Just keynum) -> Just (Just (Msg.Note keynum))
        Just Nothing -> Just Nothing
        Nothing -> Nothing
note_from_kbd _ _ _ = Nothing

note_from_midi :: Translator
note_from_midi (Msg.midi -> Just (Midi.ChannelMessage _ (Midi.NoteOn nn _))) =
    Just $ Just $ Msg.Note (nn_to_keynum nn)
note_from_midi _ = Nothing

-- * with_midi

-- | Take a Key to a ReadMessage, for midi thru.
with_midi :: Cmd.Cmd -> Cmd.Cmd
with_midi cmd msg = do
    mods <- modifiers_down
    when (not (null mods)) Cmd.abort
    octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    let new_msg = midi_from_kbd octave msg
    case new_msg of
        Nothing -> Cmd.abort
        Just msg -> cmd msg

key_char (Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd state key)))) =
    Just (state, key)
key_char _ = Nothing

midi_from_kbd :: Cmd.Octave -> Msg.Msg -> Maybe Msg.Msg
midi_from_kbd octave msg = case key_char msg of
    Just (state, key) -> case key_to_keynum octave key of
        Just (Just keynum) -> Just (kbd_read_message (midi_of state keynum))
        Just Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

kbd_read_message :: Midi.ChannelMessage -> Msg.Msg
kbd_read_message chan_msg = Msg.Midi $
    Midi.ReadMessage (Midi.ReadDevice "kbd_entry") Timestamp.immediately
        (Midi.ChannelMessage 0 chan_msg)

midi_of :: UiMsg.KbdState -> Pitch.KeyNumber -> Midi.ChannelMessage
midi_of state keynum = case state of
    UiMsg.KeyDown -> Midi.NoteOn nn vel
    UiMsg.KeyUp -> Midi.NoteOff nn vel
    where
    nn = keynum_to_nn keynum
    vel = 100

-- TODO deal with out of range KeyNumbers
keynum_to_nn :: Pitch.KeyNumber -> Midi.Key
keynum_to_nn (octave, degree) = fromIntegral (octave * 12 + degree)

nn_to_keynum :: Midi.Key -> Pitch.KeyNumber
nn_to_keynum nn = (fromIntegral oct, fromIntegral degree)
    where (oct, degree) = nn `divMod` 12

modifiers_down :: (Monad m) => Cmd.CmdT m [Cmd.Modifier]
modifiers_down = do
    keys_down <- fmap Map.elems Cmd.keys_down
    return $ filter (not . Keymap.is_char_mod) keys_down

-- * implementation

key_to_keynum :: Cmd.Octave -> Key.Key -> Maybe (Maybe Pitch.KeyNumber)
key_to_keynum octave (Key.KeyChar c) =
    (fmap . fmap) (\(oct, n) -> (oct+octave, n)) $ Map.lookup c note_map
key_to_keynum _ _ = Nothing
note_map = Map.fromList (ignore_keys ++ upper_notes ++ lower_notes)

-- | Kbd entry doesn't map all the keys, and it's annoying when you
-- accidentally hit a key that falls through and does some function you don't
-- want.  So find all the alphanumeric keys that aren't mapped and give them
-- a dummy command.  It's only alphanum because zoom, play, etc. are still
-- useful.
ignore_keys, lower_notes, upper_notes :: [(Char, Maybe Pitch.KeyNumber)]
ignore_keys = zip "abcdefghijklmnopqrstuvwxyz01234567890" (repeat Nothing)

lower_notes = make_key_map 0
    [ 'z', 's' -- C
    , 'x', 'd' -- D
    , 'c'
    , 'v', 'g' -- F
    , 'b', 'h'
    , 'n', 'j' -- A
    , 'm'
    , ',' -- C
    ]

upper_notes = make_key_map 1
    [ 'q', '2' -- C
    , 'w', '3' -- D
    , 'e'
    , 'r', '5' -- F
    , 't', '6'
    , 'y', '7' -- A
    , 'u'
    , 'i' -- C
    ]

make_key_map :: Cmd.Octave -> [Char] -> [(Char, Maybe Pitch.KeyNumber)]
make_key_map base_oct = map mk_keynum . zip [0..]
    where
    mk_keynum (n0, c) =
        (Keymap.hardcoded_kbd_layout Map.! c, Just (base_oct + oct, n1))
        where (oct, n1) = (n0 `divMod` 12)
