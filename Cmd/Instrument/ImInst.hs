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
    (Code, allocations, both, cmd, extractWarns, generator, instRange,
     makeCode, noteCalls, noteGenerators, noteTransformers, nullCall,
     postproc, transformer, valCalls)
import qualified Cmd.MidiThru as MidiThru

import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
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
synth name doc patchMap = Inst.SynthDecl
    { synthdName = name
    , synthdDoc = doc
    , synthdPatches
    , synthdWarns
    }
    where
    (synthdPatches, synthdWarns) = extractWarns $
        map (second makeInst) patchMap

data Patch = Patch
    { patchPatch :: Patch.Patch
    , patchDummy :: Maybe Text
    , patchCommon :: Common.Common Code
    }

makePatch :: Patch.Patch -> Patch
makePatch p = Patch
    { patchPatch = p
    , patchDummy = Nothing
    , patchCommon = Common.common mempty
    }

patch = Lens.lens patchPatch (\r a -> r { patchPatch = a })
common = Lens.lens patchCommon (\r a -> r { patchCommon = a })

-- | Cause this to have a Dummy backend.  It's a bit sloppy in that the
-- contents of 'patch_patch' will be ignored, but it's convenient in that
-- it lets me reuse all the functions in here for dummies too.
dummy :: Text -> Patch -> Patch
dummy msg patch = patch { patchDummy = Just msg }

code :: Lens Patch Code
code = common # Common.code

doc :: Lens Patch Doc.Doc
doc = common # Common.doc

makeInst :: Patch -> (Inst.Inst Cmd.InstrumentCode, [Library.Shadowed])
makeInst (Patch patch dummy common) =
    ( Inst.Inst
        { instBackend = case dummy of
            Nothing -> Inst.Im patch
            Just msg -> Inst.Dummy msg
        , instCommon = common { Common.commonCode = code }
        }
    , shadowed
    )
    where (code, shadowed) = makeCode (Common.commonCode common)

-- TODO: these are copy paste from MidiInst, only 'common' is different.
-- I should be able to share the code.

-- | The instrument will also set the given environ when it comes into scope.
environ :: REnv.ToVal a => EnvKey.Key -> a -> Patch -> Patch
environ name val = common %= cenviron name val

cenviron :: REnv.ToVal a => EnvKey.Key -> a
    -> Common.Common code -> Common.Common code
cenviron name val =
    Common.environ %= (REnv.fromList [(name, REnv.toVal val)] <>)

-- | Set instrument range.
range :: Scale.Range -> Patch -> Patch
range range = common %= crange range

crange :: Scale.Range -> Common.Common code -> Common.Common code
crange range =
    cenviron EnvKey.instrumentBottom (Scale.rangeBottom range)
    . cenviron EnvKey.instrumentTop (Scale.rangeTop range)

nnRange :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Patch -> Patch
nnRange (bottom, top) =
    environ EnvKey.instrumentBottom bottom
    . environ EnvKey.instrumentTop top

-- | Adapt a 'Thru.ThruFunction' to 'Cmd.ThruFunction'.
thru :: Thru.ThruFunction -> Code
thru thruF = MidiInst.thru convert
    where
    convert scale attrs input = do
        inst <- Cmd.abortUnless =<< EditUtil.lookupInstrument
        -- I drop InputNote.NodeId for both of these, because im doesn't
        -- support multiple notes yet.
        MidiThru.convertInput inst scale input >>= \case
            InputNote.NoteOn _ pitch velocity ->
                case thruF [Thru.Note pitch velocity attrs 0] of
                    Left err -> Cmd.throw err
                    Right msg -> pure [Cmd.ImThru msg]
            InputNote.NoteOff _ _ -> pure [Cmd.ImThru Thru.Stop]
            _ -> pure []

addFlag :: Common.Flag -> Patch -> Patch
addFlag flag = common#Common.flags %= Set.insert flag

triggered :: Patch -> Patch
triggered = addFlag Common.Triggered

imAllocations :: [(ScoreT.Instrument, Text, Common.Config -> Common.Config)]
    -- ^ (inst, qualified, set_config)
    -> UiConfig.Allocations
imAllocations = UiConfig.makeAllocations . map (_makeAllocation UiConfig.Im)

dummyAllocations :: [(ScoreT.Instrument, Text, Common.Config -> Common.Config)]
    -- ^ (inst, qualified, set_config)
    -> UiConfig.Allocations
dummyAllocations =
    UiConfig.makeAllocations . map (_makeAllocation (UiConfig.Dummy ""))

_makeAllocation :: UiConfig.Backend
    -> (a, Text, Common.Config -> Common.Config) -> (a, UiConfig.Allocation)
_makeAllocation backend (name, qualified, setConfig) =
    ( name
    , UiConfig.Allocation
        { allocQualified = InstT.parseQualified qualified
        , allocConfig = setConfig Common.emptyConfig
        , allocBackend = backend
        }
    )
