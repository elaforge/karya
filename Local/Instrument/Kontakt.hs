{- | Instrument db for the native instruments Kontakt sampler.

Unfortunately the instruments here have to be hardcoded unless I want to figure
out how to parse .nki files or something.
-}
module Local.Instrument.Kontakt where
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller
import qualified Instrument.MidiDb as MidiDb

load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return (synth, MidiDb.merge_patch_maps
    (MidiDb.wildcard_patch_map patch_template)
    (fst $ MidiDb.patch_map patches))

patch_template = Instrument.patch $
    Instrument.instrument synth_name "" Nothing Controller.empty_map (-96, 96)

synth_name = "kkt"
synth = Instrument.synth synth_name "kontakt" []

patches =
    [ mkpatch "hang1" hang_keyswitches
    , mkpatch "hang2" hang_keyswitches
    ]

hang_keyswitches = Instrument.make_keyswitches
    [ ("center", 36)
    , ("edge", 37), ("slap", 38), ("mid", 39), ("knuckle", 40)
    , ("", 36)
    ]

mkpatch inst_name ks_map = patch_template
    { Instrument.patch_instrument =
        Instrument.set_instrument_name synth_name inst_name Nothing inst
    , Instrument.patch_keyswitches = ks_map
    }
    where
    inst = Instrument.patch_instrument patch_template
