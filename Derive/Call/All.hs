-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect the various calls into one place.
module Derive.Call.All (library) where
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Bali.Gong as Gong
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.Call.Bali.Sekar as Sekar
import qualified Derive.Call.China.Zheng as Zheng
import qualified Derive.Call.Europe.Chord as Chord
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.Idiom.Wind as Wind
import qualified Derive.Call.India.Gamakam as Gamakam
import qualified Derive.Call.India.Gamakam2 as Gamakam2
import qualified Derive.Call.India.Gamakam3 as Gamakam3
import qualified Derive.Call.India.Gamakam4 as Gamakam4
import qualified Derive.Call.India.Gamakam5 as Gamakam5
import qualified Derive.Call.India.Mridangam as Mridangam
import qualified Derive.Call.India.Pakhawaj as Pakhawaj
import qualified Derive.Call.Post.Idiom as Idiom
import qualified Derive.Call.Post.Map as Map
import qualified Derive.Call.Post.Postproc as Postproc
import qualified Derive.Call.Post.Rearticulate as Rearticulate
import qualified Derive.Call.Post.Retune as Retune
import qualified Derive.Call.Post.Reverse as Reverse
import qualified Derive.Call.Prelude.Articulation as Articulation
import qualified Derive.Call.Prelude.Block as Block
import qualified Derive.Call.Prelude.Conditional as Conditional
import qualified Derive.Call.Prelude.Config as Config
import qualified Derive.Call.Prelude.Control as Control
import qualified Derive.Call.Prelude.ControlFunction as ControlFunction
import qualified Derive.Call.Prelude.Delay as Delay
import qualified Derive.Call.Prelude.Equal as Equal
import qualified Derive.Call.Prelude.Highlight as Highlight
import qualified Derive.Call.Prelude.Import as Import
import qualified Derive.Call.Prelude.InferTrackVoice as InferTrackVoice
import qualified Derive.Call.Prelude.Integrate as Integrate
import qualified Derive.Call.Prelude.Lily as Lily
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Prelude.NoteTransformer as NoteTransformer
import qualified Derive.Call.Prelude.Parent as Parent
import qualified Derive.Call.Prelude.Pitch as Pitch
import qualified Derive.Call.Prelude.PitchHigh as PitchHigh
import qualified Derive.Call.Prelude.Random as Random
import qualified Derive.Call.Prelude.SignalTransform as SignalTransform
import qualified Derive.Call.Prelude.Trill as Trill
import qualified Derive.Call.Prelude.Val as Val
import qualified Derive.Derive as Derive


library :: Derive.Library
library = Derive.Library
    { lib_note = note_maps
    , lib_control = control_maps
    , lib_pitch = pitch_maps
    , lib_val = val_map
    , lib_instrument_aliases = mempty
    }

note_maps :: Derive.CallMaps Derive.Note
note_maps = mconcat
    [ Articulation.note_calls
    , Block.note_calls
    , Chord.note_calls
    , Conditional.note_calls
    , Config.note_calls
    , Delay.note_calls
    , Equal.note_calls
    , Gamakam2.note_calls
    , Gamakam3.note_calls
    , Gamakam4.note_calls
    , Gamakam5.note_calls
    , Gangsa.note_calls
    , Gender.note_calls
    , Gong.note_calls
    , Grace.note_calls
    , Highlight.note_calls
    , Idiom.note_calls
    , Import.calls
    , InferTrackVoice.note_calls
    , Integrate.note_calls
    , Lily.note_calls
    , Map.note_calls
    , Mridangam.note_calls
    , Note.note_calls
    , NoteTransformer.note_calls
    , Pakhawaj.note_calls
    , Parent.note_calls
    , PitchHigh.note_calls
    , Postproc.note_calls
    , Random.note_calls
    , Rearticulate.note_calls
    , Retune.note_calls
    , Reverse.note_calls
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
    , Config.control_calls
    , Control.control_calls
    , Equal.control_calls
    , Gamakam.control_calls
    , Gamakam3.control_calls
    , Gamakam4.control_calls
    , Gamakam5.control_calls
    , Import.calls
    , Conditional.control_calls
    , Random.control_calls
    , SignalTransform.control_calls
    , Trill.control_calls
    ]

pitch_maps :: Derive.CallMaps Derive.Pitch
pitch_maps = mconcat
    [ Config.pitch_calls
    , Equal.pitch_calls
    , Gamakam.pitch_calls
    , Gamakam2.pitch_calls
    , Gamakam3.pitch_calls
    , Gamakam4.pitch_calls
    , Grace.pitch_calls
    , Import.calls
    , Conditional.pitch_calls
    , Pitch.pitch_calls
    , PitchHigh.pitch_calls
    , Random.pitch_calls
    , SignalTransform.pitch_calls
    , Trill.pitch_calls
    , Zheng.pitch_calls
    ]

val_map :: [Derive.LookupCall Derive.ValCall]
val_map = concat
    [ ControlFunction.val_calls
    , Random.val_calls
    , Val.val_calls
    ]
