-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Spicy guitar, free at http://www.spicyguitar.com/
module Local.Instrument.Spicy where
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.StateConfig as StateConfig
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.InstTypes as InstTypes
import Global


synth_name :: InstTypes.SynthName
synth_name = "spicy"

synth :: MidiInst.Synth
synth = MidiInst.synth synth_name "Spicy Guitar, http://www.spicyguitar.com" $
    MidiInst.synth_controls controls patches

patches :: [MidiInst.Patch]
patches = (:[]) $
    MidiInst.code #= MidiInst.note_calls (MidiInst.null_call note_call) $
    MidiInst.make_patch $
    MidiInst.add_flag Patch.HoldKeyswitch $
    Patch.attribute_map #= Patch.single_keyswitches keyswitches $
    Patch.patch (-3, 3) Patch.default_name


-- | WARNING: changing these while playing tends to crash the VST.
controls :: [(Midi.Control, Score.Control)]
controls =
    [ (20, "position") -- 0 for bridge, 1 for middle
    , (21, "finger") -- 0 for finger plucking, 1 for pick
    , (22, "inharmonicity")
    , (23, "twang")
    , (24, "color")
    , (25, "impedance")
    , (26, "vibrato") -- speed of vibrato
    , (27, "mute") -- amount of palm mute effect
    , (28, "harm")
    ]

keyswitches :: [(Attrs.Attributes, Midi.Key)]
keyswitches =
    [ (Attrs.legato, Key.b2)
    , (Attrs.mute, Key.c3)
    , (Attrs.harm, Key.cs3)
    ]

note_call :: Derive.Generator Derive.Note
note_call = Note.transformed_note
    ("If given a string-name attribute in " <> attrs_doc <> ", suffix the"
        <> " instrument with the string name.  When combined with the proper"
        <> " midi config, this will redirect the note to the proper channel"
        <> " for that string.") mempty
    (const transform)
    where
    attrs_doc = Doc.Doc $ Text.intercalate ", " ["`" <> a <> "`" | a <- strings]
    transform deriver = do
        attrs <- Call.get_attributes
        inst <- Call.lookup_instrument
        let string = Seq.head
                [ string
                | attr <- Attrs.to_list attrs, string <- strings
                , attr == string
                ]
        case (inst, string) of
            (Just inst, Just string) ->
                Derive.with_instrument (string_inst inst string) deriver
            _ -> deriver
    string_inst inst string =
        Score.Instrument $ Score.instrument_name inst <> "-" <> string

strings :: [Text]
strings = ["e1", "a", "d", "g", "b", "e2"]

-- | Create the proper midi config to work with the string attrs used by
-- 'note_call'.
allocations :: Text -> InstTypes.Name -> StateConfig.Allocations
allocations dev_name name = StateConfig.midi_allocations $
    inst name 0 : [inst (name <> "-" <> string) chan
        | (string, chan) <- zip strings [1..]]
    where
    inst name chan =
        ( Score.instrument name
        , (InstTypes.Qualified synth_name name, MidiInst.config1 dev chan)
        )
    dev = Midi.write_device dev_name
