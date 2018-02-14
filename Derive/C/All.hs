-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect the various calls into one place.
module Derive.C.All (builtins, shadowed) where
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.C.Bali.Gender as Gender
import qualified Derive.C.Bali.Gong as Gong
import qualified Derive.C.Bali.Reyong as Reyong
import qualified Derive.C.Bali.Sekar as Sekar
import qualified Derive.C.China.Zheng as Zheng
import qualified Derive.C.Europe.Chord as Chord
import qualified Derive.C.Idiom.String as String
import qualified Derive.C.Idiom.Wind as Wind
import qualified Derive.C.India.Gamakam as Gamakam
import qualified Derive.C.India.Gamakam2 as Gamakam2
import qualified Derive.C.India.Gamakam5 as Gamakam5
import qualified Derive.C.India.Gamakam6 as Gamakam6
import qualified Derive.C.India.Mridangam as Mridangam
import qualified Derive.C.India.Pakhawaj as Pakhawaj
import qualified Derive.C.Lily as Lily
import qualified Derive.C.Post.Idiom as Idiom
import qualified Derive.C.Post.Map as Map
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.C.Post.Rearticulate as Rearticulate
import qualified Derive.C.Post.Retune as Retune
import qualified Derive.C.Post.Reverse as Reverse
import qualified Derive.C.Prelude.Articulation as Articulation
import qualified Derive.C.Prelude.Block as Block
import qualified Derive.C.Prelude.Conditional as Conditional
import qualified Derive.C.Prelude.Config as Config
import qualified Derive.C.Prelude.Control as Control
import qualified Derive.C.Prelude.ControlFunction as ControlFunction
import qualified Derive.C.Prelude.Delay as Delay
import qualified Derive.C.Prelude.Equal as Equal
import qualified Derive.C.Prelude.Grace as Grace
import qualified Derive.C.Prelude.Highlight as Highlight
import qualified Derive.C.Prelude.Import as Import
import qualified Derive.C.Prelude.InferTrackVoice as InferTrackVoice
import qualified Derive.C.Prelude.Integrate as Integrate
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.C.Prelude.NoteTransformer as NoteTransformer
import qualified Derive.C.Prelude.Parent as Parent
import qualified Derive.C.Prelude.Pitch as Pitch
import qualified Derive.C.Prelude.PitchHigh as PitchHigh
import qualified Derive.C.Prelude.Random as Random
import qualified Derive.C.Prelude.SignalTransform as SignalTransform
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.C.Prelude.Val as Val
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library


builtins :: Derive.Builtins
shadowed :: [Library.Shadowed]
(builtins, shadowed) = Library.compile $ mconcat
    [ Articulation.library
    , Block.library
    , Chord.library
    , Conditional.library
    , Config.library
    , ControlFunction.library
    , Control.library
    , Delay.library
    , Equal.library
    , Gamakam2.library
    , Gamakam5.library
    , Gamakam6.library
    , Gamakam.library
    , Gangsa.library
    , Gender.library
    , Gong.library
    , Grace.library
    , Highlight.library
    , Idiom.library
    , Import.library
    , InferTrackVoice.library
    , Integrate.library
    , Lily.library
    , Map.library
    , Mridangam.library
    , Note.library
    , NoteTransformer.library
    , Pakhawaj.library
    , Parent.library
    , PitchHigh.library
    , Pitch.library
    , Postproc.library
    , Random.library
    , Rearticulate.library
    , Retune.library
    , Reverse.library
    , Reyong.library
    , Sekar.library
    , SignalTransform.library
    , String.library
    , Trill.library
    , Val.library
    , Wind.library
    , Zheng.library
    ]
