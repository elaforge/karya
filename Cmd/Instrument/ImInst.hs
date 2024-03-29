-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is the 音 variant of "Cmd.Instrument.MidiInst".  All of the the
-- generic bits can be re-exported.
module Cmd.Instrument.ImInst (
    module Cmd.Instrument.ImInst, module MidiInst
) where
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.MidiInst as MidiInst
import           Cmd.Instrument.MidiInst
    (Code, allocations, both, cmd, generator, inst_range, make_code, note_calls,
     note_generators, note_transformers, null_call, postproc, transformer,
     val_calls)
import qualified Cmd.MidiThru as MidiThru

import qualified Derive.EnvKey as EnvKey
import qualified Derive.REnv as REnv
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Perform.Im.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Thru as Thru
import qualified Ui.UiConfig as UiConfig

import           Global


type Synth = Inst.SynthDecl Cmd.InstrumentCode

synth :: InstT.SynthName -> Text -> [(InstT.Name, Patch)] -> Synth
synth name doc patches =
    Inst.SynthDecl name doc (map (second make_inst) patches)

data Patch = Patch {
    patch_patch :: Patch.Patch
    , patch_dummy :: Maybe Text
    , patch_common :: Common.Common Code
    }

make_patch :: Patch.Patch -> Patch
make_patch p = Patch
    { patch_patch = p
    , patch_dummy = Nothing
    , patch_common = Common.common mempty
    }

patch = Lens.lens patch_patch (\f r -> r { patch_patch = f (patch_patch r) })
common = Lens.lens patch_common
    (\f r -> r { patch_common = f (patch_common r) })

-- | Cause this to have a Dummy backend.  It's a bit sloppy in that the
-- contents of 'patch_patch' will be ignored, but it's convenient in that
-- it lets me reuse all the functions in here for dummies too.
dummy :: Text -> Patch -> Patch
dummy msg patch = patch { patch_dummy = Just msg }

code :: Lens Patch Code
code = common # Common.code

doc :: Lens Patch Doc.Doc
doc = common # Common.doc

make_inst :: Patch -> Inst.Inst Cmd.InstrumentCode
make_inst (Patch patch dummy common) = Inst.Inst
    { inst_backend = case dummy of
        Nothing -> Inst.Im patch
        Just msg -> Inst.Dummy msg
    , inst_common = MidiInst.make_code <$> common
    }

-- TODO: these are copy paste from MidiInst, only 'common' is different.
-- I should be able to share the code.

-- | The instrument will also set the given environ when it comes into scope.
environ :: REnv.ToVal a => EnvKey.Key -> a -> Patch -> Patch
environ name val = common %= cenviron name val

cenviron :: REnv.ToVal a => EnvKey.Key -> a
    -> Common.Common code -> Common.Common code
cenviron name val =
    Common.environ %= (REnv.from_list [(name, REnv.to_val val)] <>)

-- | Set instrument range.
range :: Scale.Range -> Patch -> Patch
range range = common %= crange range

crange :: Scale.Range -> Common.Common code -> Common.Common code
crange range =
    cenviron EnvKey.instrument_bottom (Scale.range_bottom range)
    . cenviron EnvKey.instrument_top (Scale.range_top range)

nn_range :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Patch -> Patch
nn_range (bottom, top) =
    environ EnvKey.instrument_bottom bottom
    . environ EnvKey.instrument_top top

-- | Adapt a 'Thru.ThruFunction' to 'Cmd.ThruFunction'.
thru :: Thru.ThruFunction -> Code
thru thru_f = MidiInst.thru convert
    where
    convert scale attrs input = do
        inst <- Cmd.abort_unless =<< EditUtil.lookup_instrument
        MidiThru.convert_input inst scale input >>= \case
            InputNote.NoteOn _ pitch velocity ->
                case thru_f [Thru.Note pitch velocity attrs 0] of
                    Left err -> Cmd.throw err
                    Right msg -> return [Cmd.ImThru msg]
            _ -> return []

add_flag :: Common.Flag -> Patch -> Patch
add_flag flag = common#Common.flags %= Set.insert flag

triggered :: Patch -> Patch
triggered = add_flag Common.Triggered

im_allocations :: [(ScoreT.Instrument, Text, Common.Config -> Common.Config)]
    -- ^ (inst, qualified, set_config)
    -> UiConfig.Allocations
im_allocations = UiConfig.make_allocations . map (_make_allocation UiConfig.Im)

dummy_allocations :: [(ScoreT.Instrument, Text, Common.Config -> Common.Config)]
    -- ^ (inst, qualified, set_config)
    -> UiConfig.Allocations
dummy_allocations =
    UiConfig.make_allocations . map (_make_allocation (UiConfig.Dummy ""))

_make_allocation :: UiConfig.Backend
    -> (a, Text, Common.Config -> Common.Config) -> (a, UiConfig.Allocation)
_make_allocation backend (name, qualified, set_config) =
    ( name
    , UiConfig.Allocation
        { alloc_qualified = InstT.parse_qualified qualified
        , alloc_config = set_config Common.empty_config
        , alloc_backend = backend
        }
    )
