-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.Util as CUtil
import qualified Cmd.Keymap as Keymap

import Derive.Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Instrument.Util as DUtil

import qualified Perform.Midi.Instrument as Instrument

import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "kkt" (Just "loop1") (-12, 12) [])
        { MidiInst.extra_patches = patches }

patches = MidiInst.with_code hang_code
    [ inst "hang1" hang_keyswitches
    , inst "hang2" hang_keyswitches
    ]
    where
    inst name ks = Instrument.set_keyswitches ks $
        Instrument.patch $ Instrument.instrument name [] (-12, 12)

hang_code = MidiInst.empty_code
    { MidiInst.note_calls = [Derive.make_lookup hang_calls]
    , MidiInst.cmds = [hang_cmd]
    }

-- | The order is important because it determines attr lookup priority.
hang_strokes :: [(Score.Attributes, Midi.Key, Maybe String, Maybe Char)]
hang_strokes =
    [ (center,  36, Just "",            Just 'Z')
    , (edge,    37, Just "`pang2`",     Just 'X')
    , (slap,    38, Just "`da3`",       Just 'C')
    , (middle,  39, Just "`zhong1`",    Just 'V')
    , (knuckle, 40, Just "`zhi3`",      Just 'B')
    , (no_attrs,36, Nothing,            Nothing)
    ]

hang_keyswitches = [(attrs, key) | (attrs, key, _, _) <- hang_strokes]

hang_calls :: Derive.NoteCallMap
hang_calls = Derive.make_calls
    [(text, DUtil.with_attrs attrs) | (attrs, _, Just text, _) <- hang_strokes,
        -- Make sure to not shadow the default "" call.
        not (null text)]

hang_cmd :: Cmd.Cmd
hang_cmd = CUtil.keyswitches [(Keymap.physical_key char, text, key)
    | (_, key, Just text, Just char) <- hang_strokes]
