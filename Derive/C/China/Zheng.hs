-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for 箏.
module Derive.C.China.Zheng (library) where
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.C.Idiom.String as String
import qualified Derive.C.India.Gamakam as Gamakam
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import Global
import Types


module_ :: Module.Module
module_ = "china" <> "zheng"

library :: Library.Library
library = mconcat
    [ Library.generators $
        [ ("gliss-a", Derive.set_module module_ String.c_gliss_absolute)
        , ("gliss", Derive.set_module module_ String.c_gliss)
        ] ++ trill_variations c_note_trill
    , Library.transformers
        [ ("bent-string", Derive.set_module module_ String.c_bent_string)
        ]
    , Library.generators $ trill_variations c_pitch_trill
    ]

trill_variations :: (Maybe Trill.Direction -> call) -> [(Expr.Symbol, call)]
trill_variations make =
    [ (Expr.Symbol $ "tr" <> Trill.direction_affix end, make end)
    | end <- dirs
    ]
    where dirs = [Nothing, Just Trill.High, Just Trill.Low]

-- * trill

c_note_trill :: Maybe Trill.Direction -> Derive.Generator Derive.Note
c_note_trill start_dir = Derive.generator module_ "tr" Tags.ly
    "A trill with smooth transitions."
    $ Sig.call ((,,) <$> neighbor_arg <*> speed_arg <*> Trill.hold_env
    ) $ \(neighbor, speed, hold) -> Sub.inverting $ \args ->
    Ly.note_code (Ly.append Constants.First, "\\trill") args $ do
        pitch <- Call.get_pitch =<< Args.real_start args
        sig <- trill_signal start_dir pitch neighbor speed hold args
        Derive.with_pitch sig $ Call.placed_note args

c_pitch_trill :: Maybe Trill.Direction -> Derive.Generator Derive.Pitch
c_pitch_trill start_dir = Derive.generator1 module_ "tr" mempty
    "A trill with smooth transitions."
    $ Sig.call ((,,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> neighbor_arg <*> speed_arg <*> Trill.hold_env
    ) $ \(pitch, neighbor, speed, hold) args ->
        trill_signal start_dir pitch neighbor speed hold args

trill_signal :: Maybe Trill.Direction -> PSignal.Pitch
    -> BaseTypes.ControlRef -> BaseTypes.ControlRef -> BaseTypes.Duration
    -> Derive.PassedArgs a -> Derive.Deriver PSignal.PSignal
trill_signal start_dir pitch neighbor speed hold args = do
    (neighbor, control) <- Call.to_transpose_function Typecheck.Nn neighbor
    transpose <- Gamakam.kampita start_dir Nothing Trill.Shorten neighbor
        speed transition hold lilt args
    start <- Args.real_start args
    return $ PSignal.apply_control control
        (Score.untyped transpose) $ PSignal.signal [(start, pitch)]
    where
    transition :: RealTime
    transition = 0.08
    lilt = 0

neighbor_arg :: Sig.Parser BaseTypes.ControlRef
neighbor_arg = Sig.defaulted "neighbor"
    (Sig.typed_control "tr-neighbor" 1 Score.Nn)
    "Alternate with a pitch at this interval."

speed_arg :: Sig.Parser BaseTypes.ControlRef
speed_arg = Sig.defaulted "speed" (Sig.typed_control "tr-speed" 20 Score.Real)
    "Alternate pitches at this speed."
