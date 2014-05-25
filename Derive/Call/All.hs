-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect the various calls into one place.
module Derive.Call.All (library, shadowed) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Bali.Kotekan as Kotekan
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.Call.Bali.Sekar as Sekar
import qualified Derive.Call.Block as Block
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Echo as Echo
import qualified Derive.Call.Equal as Equal
import qualified Derive.Call.Europe.Chord as Chord
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Idiom.String as String
import qualified Derive.Call.Import as Import
import qualified Derive.Call.India.Gamakam as Gamakam
import qualified Derive.Call.Integrate as Integrate
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Note as Note
import qualified Derive.Call.NoteTransformer as NoteTransformer
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.Call.PitchHigh as PitchHigh
import qualified Derive.Call.Post.ArrivalNote as ArrivalNote
import qualified Derive.Call.Post.Idiom as Idiom
import qualified Derive.Call.Post.Reverse as Post.Reverse
import qualified Derive.Call.Random as Random
import qualified Derive.Call.SignalTransform as SignalTransform
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Val as Val
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang


library :: Derive.Library
library = Derive.Library
    { Derive.lib_note = note_maps
    , Derive.lib_control = control_maps
    , Derive.lib_pitch = pitch_maps
    , Derive.lib_val = val_map
    }

-- | Warnings for shadowed symbols.
type Shadowed = ((Text, Module.Module), [TrackLang.CallId])

shadowed :: Derive.Library -> [Shadowed]
shadowed (Derive.Library note control pitch val) =
    filter (not . null . snd) $ concat
    [ call_maps "note" note
    , call_maps "control" control
    , call_maps "pitch" pitch
    , add "val" $ get_shadows Derive.vcall_doc val
    ]
    where
    call_maps tag (Derive.CallMaps gen trans) = concat
        [ add (tag <> " generator") $ get_shadows Derive.call_doc gen
        , add (tag <> " transformer") $ get_shadows Derive.call_doc trans
        ]
    add tag shadows = [((tag, module_), calls) | (module_, calls) <- shadows]

get_shadows :: (call -> Derive.CallDoc) -> [Derive.LookupCall call]
    -> [(Module.Module, [TrackLang.CallId])]
get_shadows get_doc = filter (not . null . snd) . map (second duplicates)
    . Seq.group_fst . concatMap (call_module get_doc)

duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe extract . Seq.group_on id
    where
    extract (x : _ : _) = Just x
    extract _ = Nothing

call_module :: (call -> Derive.CallDoc) -> Derive.LookupCall call
    -> [(Module.Module, TrackLang.CallId)]
call_module _ (Derive.LookupPattern {}) = []
call_module get_doc (Derive.LookupMap calls) =
    [ (Derive.cdoc_module (get_doc call), call_id)
    | (call_id, call) <- Map.toList calls
    ]

note_maps :: Derive.CallMaps Derive.Note
note_maps = mconcat
    [ Articulation.note_calls
    , Equal.note_calls
    , Gender.note_calls
    , Kotekan.note_calls
    , Reyong.note_calls
    , Block.note_calls
    , Echo.note_calls
    , Chord.note_calls
    , Grace.note_calls
    , String.note_calls
    , Import.calls
    , Integrate.note_calls
    , Lily.note_calls
    , Note.note_calls
    , NoteTransformer.note_calls
    , PitchHigh.note_calls
    , ArrivalNote.note_calls
    , Idiom.note_calls
    , Post.Reverse.note_calls
    , Random.note_calls
    , Sekar.note_calls
    , SignalTransform.note_calls
    , Trill.note_calls
    ]

control_maps :: Derive.CallMaps Derive.Control
control_maps = mconcat
    [ Block.control_calls
    , Control.control_calls
    , Equal.control_calls
    , Gamakam.control_calls
    , Import.calls
    , Random.control_calls
    , SignalTransform.control_calls
    , Trill.control_calls
    ]

pitch_maps :: Derive.CallMaps Derive.Pitch
pitch_maps = mconcat
    [ Equal.pitch_calls
    , Gamakam.pitch_calls
    , Grace.pitch_calls
    , Import.calls
    , Pitch.pitch_calls
    , PitchHigh.pitch_calls
    , Random.pitch_calls
    , SignalTransform.pitch_calls
    , Trill.pitch_calls
    ]

val_map :: [Derive.LookupCall Derive.ValCall]
val_map = concat [Random.val_calls, Val.val_calls]
