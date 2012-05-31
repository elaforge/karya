-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.Util as CUtil
import qualified Cmd.Keymap as Keymap

import Derive.Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Instrument.Util as DUtil
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "kkt" (-12, 12) [])
        { MidiInst.extra_patches = patches }

patches :: [MidiInst.Patch]
patches = MidiInst.with_empty_code
    [ Instrument.set_scale wayang_umbang $ inst "wayang-umbang" wayang_ks
    , Instrument.set_scale wayang_isep $ inst "wayang-isep" wayang_ks
    ]
    ++ MidiInst.with_code hang_code
        [ inst "hang1" hang_ks
        , inst "hang2" hang_ks
        ]
    where
    inst name ks = Instrument.set_keyswitches ks $
        Instrument.patch $ Instrument.instrument name [] (-12, 12)

-- * hang

hang_code :: MidiInst.Code
hang_code = MidiInst.empty_code
    { MidiInst.note_calls = [Derive.make_lookup hang_calls]
    , MidiInst.cmds = [hang_cmd]
    }

hang_calls :: Derive.NoteCallMap
hang_calls = Derive.make_calls
    [(text, DUtil.with_attrs attrs) | (attrs, _, Just text, _) <- hang_strokes,
        -- Make sure to not shadow the default "" call.
        not (null text)]

hang_cmd :: Cmd.Cmd
hang_cmd = CUtil.keyswitches [(Keymap.physical_key char, text, key)
    | (_, key, Just text, Just char) <- hang_strokes]

-- | The order is important because it determines attr lookup priority.
hang_strokes :: [(Score.Attributes, Midi.Key, Maybe String, Maybe Char)]
hang_strokes =
    [ (center,  Key.c2,     Just "",            Just 'Z')
    , (edge,    Key.cs2,    Just "`pang2`",     Just 'X')
    , (slap,    Key.d2,     Just "`da3`",       Just 'C')
    , (middle,  Key.ds2,    Just "`zhong1`",    Just 'V')
    , (knuckle, Key.e2,     Just "`zhi3`",      Just 'B')
    , (no_attrs,Key.c2,     Nothing,            Nothing)
    ]

hang_ks :: [(Score.Attributes, Midi.Key)]
hang_ks = [(attrs, key) | (attrs, key, _, _) <- hang_strokes]


-- * gender wayang

-- TODO cmds for muted and open attrs?  Or I can say dur=0 means muted.
wayang_ks :: [(Score.Attributes, Midi.Key)]
wayang_ks = [(open, Key.c2), (muted, Key.cs2)]

wayang_umbang :: Instrument.PatchScale
wayang_umbang =
    Instrument.make_patch_scale $ zip wayang_keys Wayang.note_numbers_umbang

wayang_isep :: Instrument.PatchScale
wayang_isep =
    Instrument.make_patch_scale $ zip wayang_keys Wayang.note_numbers_isep

wayang_keys :: [Midi.Key]
wayang_keys =
    [         Key.d2, Key.e2, Key.g2, Key.a2
    , Key.c3, Key.d3, Key.e3, Key.g3, Key.a3
    , Key.c4
    ]
