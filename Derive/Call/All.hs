-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect the various calls into one place.
module Derive.Call.All (library) where
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.Call.Bali.Sekar as Sekar
import qualified Derive.Call.Block as Block
import qualified Derive.Call.China.Zheng as Zheng
import qualified Derive.Call.Control as Control
import qualified Derive.Call.ControlFunction as ControlFunction
import qualified Derive.Call.Echo as Echo
import qualified Derive.Call.Equal as Equal
import qualified Derive.Call.Europe.Chord as Chord
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Highlight as Highlight
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.Idiom.Wind as Wind
import qualified Derive.Call.Import as Import
import qualified Derive.Call.India.Gamakam as Gamakam
import qualified Derive.Call.India.Gamakam2 as Gamakam2
import qualified Derive.Call.InferTrackVoice as InferTrackVoice
import qualified Derive.Call.Integrate as Integrate
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Misc as Misc
import qualified Derive.Call.Note as Note
import qualified Derive.Call.NoteTransformer as NoteTransformer
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.Call.PitchHigh as PitchHigh
import qualified Derive.Call.Post.Idiom as Idiom
import qualified Derive.Call.Post.Move as Move
import qualified Derive.Call.Post.Reverse as Post.Reverse
import qualified Derive.Call.Random as Random
import qualified Derive.Call.SignalTransform as SignalTransform
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Val as Val
import qualified Derive.Derive as Derive

import Global


library :: Derive.Library
library = Derive.Library
    { Derive.lib_note = note_maps
    , Derive.lib_control = control_maps
    , Derive.lib_pitch = pitch_maps
    , Derive.lib_val = val_map
    }

note_maps :: Derive.CallMaps Derive.Note
note_maps = mconcat
    [ Articulation.note_calls
    , Block.note_calls
    , Chord.note_calls
    , Echo.note_calls
    , Equal.note_calls
    , Gamakam2.note_calls
    , Gangsa.note_calls
    , Gender.note_calls
    , Grace.note_calls
    , Highlight.note_calls
    , Idiom.note_calls
    , Import.calls
    , InferTrackVoice.note_calls
    , Integrate.note_calls
    , Lily.note_calls
    , Misc.note_calls
    , Note.note_calls
    , NoteTransformer.note_calls
    , PitchHigh.note_calls
    , Post.Reverse.note_calls
    , Random.note_calls
    , Move.note_calls
    , Reyong.note_calls
    , Sekar.note_calls
    , SignalTransform.note_calls
    , String.note_calls
    , Trill.note_calls
    , Wind.note_calls
    , Zheng.note_calls
    ]

control_maps :: Derive.CallMaps Derive.Control
control_maps = mconcat
    [ Block.control_calls
    , Control.control_calls
    , Equal.control_calls
    , Gamakam.control_calls
    , Import.calls
    , Misc.control_calls
    , Random.control_calls
    , SignalTransform.control_calls
    , Trill.control_calls
    ]

pitch_maps :: Derive.CallMaps Derive.Pitch
pitch_maps = mconcat
    [ Equal.pitch_calls
    , Gamakam.pitch_calls
    , Gamakam2.pitch_calls
    , Grace.pitch_calls
    , Import.calls
    , Misc.pitch_calls
    , Pitch.pitch_calls
    , PitchHigh.pitch_calls
    , Random.pitch_calls
    , SignalTransform.pitch_calls
    , Trill.pitch_calls
    , Zheng.pitch_calls
    ]

val_map :: [Derive.LookupCall Derive.ValCall]
val_map = concat [ControlFunction.val_calls, Random.val_calls, Val.val_calls]
