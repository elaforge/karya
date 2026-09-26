-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for 箏.
module Derive.C.China.Zheng (library) where
import qualified Derive.Args as Args
import qualified Derive.C.Idiom.String as String
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants


module_ :: Module.Module
module_ = "china" <> "zheng"

library :: Library.Library
library = mconcat
    [ Library.generators $
        [ ("gliss-a", Derive.setModule module_ String.cGlissAbsolute)
        , ("gliss", Derive.setModule module_ String.cGliss)
        ] ++ trillVariations cNoteTrill
    , Library.transformers
        [ ("bent-string", Derive.setModule module_ String.cBentString)
        ]
    , Library.generators $ trillVariations cPitchTrill
    ]

trillVariations :: (Maybe Trill.Direction -> call) -> [(Expr.Symbol, call)]
trillVariations make =
    [ (Expr.Symbol $ "tr" <> Trill.directionAffix end, make end)
    | end <- dirs
    ]
    where dirs = [Nothing, Just Trill.High, Just Trill.Low]

-- * trill

cNoteTrill :: Maybe Trill.Direction -> Derive.Generator Derive.Note
cNoteTrill startDir = Derive.generator module_ "tr" Tags.ly
    "A trill with smooth transitions."
    $ Sig.call ((,,) <$> neighborArg <*> speedArg <*> Trill.holdEnv
    ) $ \(neighbor, speed, hold) -> Sub.inverting $ \args ->
    Ly.noteCode (Ly.append Constants.First, "\\trill") args $ do
        pitch <- Call.getPitch =<< Args.realStart args
        sig <- trillSignal startDir pitch neighbor speed hold args
        Derive.withPitch sig $ Call.placedNote args

cPitchTrill :: Maybe Trill.Direction -> Derive.Generator Derive.Pitch
cPitchTrill startDir = Derive.generator1 module_ "tr" mempty
    "A trill with smooth transitions."
    $ Sig.call ((,,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> neighborArg <*> speedArg <*> Trill.holdEnv
    ) $ \(pitch, neighbor, speed, hold) args ->
        trillSignal startDir pitch neighbor speed hold args

trillSignal :: Maybe Trill.Direction -> PSignal.Pitch
    -> Typecheck.NnTransposeFunctionT -> Typecheck.RealTimeFunctionT
    -> DeriveT.Duration -> Derive.PassedArgs a
    -> Derive.Deriver PSignal.PSignal
trillSignal startDir pitch (Typecheck.NnTransposeFunctionT ttype neighbor)
        speed hold args = do
    transpose <- Trill.getTrillControlSmooth config curve
        (Args.rangeOrNext args) neighbor
    start <- Args.realStart args
    pure $ PSignal.applyControl (Typecheck.transposeControl ttype)
        (ScoreT.untyped transpose) (PSignal.fromSample start pitch)
    where
    config = Trill.Config
        { _speed = speed
        , _startDir = startDir
        , _endDir = Nothing
        , _hold = hold
        , _adjust = Trill.Stretch
        , _bias = 0
        , _includeEnd = True
        }
    curve = ControlUtil.Linear
    -- A sigmoid curve looks nice, but as far as I can tell, it sounds the same
    -- as linear, and linear should be cheaper.
    -- curve = ControlUtil.Function $ ControlUtil.sigmoid 0.5 0.5

neighborArg :: Sig.Parser Typecheck.NnTransposeFunctionT
neighborArg = Sig.defaulted "neighbor" (1 :: Int)
    "Alternate with a pitch at this interval."

speedArg :: Sig.Parser Typecheck.RealTimeFunctionT
speedArg = Sig.defaulted "speed" (20 :: Int) "Alternate pitches at this speed."
