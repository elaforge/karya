-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.ReyongCode where
import qualified Data.List as List

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Post as Post
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Instrument.Common as Common
import qualified Synth.Sampler.Patch.Util as Util

import Types


code :: ImInst.Code
code = mconcat
    [ -- ImInst.null_call note
    ]

    -- infer insts = infer_damp_simple gap

-- * TODO copy paste

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (cek <> open, CekOpen)
    , (cek, CekClosed)
    , (mute <> open, MuteOpen)
    , (mute, MuteClosed)
    , (mempty, Open)
    ]
    where
    mute = Attrs.mute
    open = Attrs.open
    -- TODO from Derive.C.Bali.Reyong, or from a common attrs module
    cek = Attrs.attr "cek"

data Articulation = CekClosed | CekOpen | MuteClosed | MuteOpen | Open
    deriving (Eq, Ord, Show, Enum, Bounded)
