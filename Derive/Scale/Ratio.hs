-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This \"scale\" generates pitches which are ratios of the pitches in
-- another pitch signal, @#ratio-source@.  The intent is to tune one instrument
-- relative another.
--
-- It's not a usual scale because there is no notion of scale degrees or
-- transposition or anything like that.  The degrees are ratios, e.g. @3/2@ or
-- @-9/8@.  A positive ratio will multiply with the source pitch, a negative
-- one will divide.  The source pitch is only sampled at the beginning of the
-- relative pitch, so if the source is moving the relative one won't move with
-- it.
module Derive.Scale.Ratio where
import qualified Data.Attoparsec.Text as A

import qualified Util.ParseA as ParseA
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import           Derive.Sig (defaulted)

import qualified Perform.Pitch as Pitch


scales :: [Scale.Definition]
scales = map Scale.Simple [scale]

scale :: Scale.Scale
scale = Scale.Scale
    { scaleId = "ratio"
    , scalePattern = "[+-]?\\d+/\\d+ e.g. 2/5 or -4/3"
    , scaleSymbols = []
    , scaleTransposers = mempty
    , scaleRead = \_ _ -> Left DeriveT.NotImplemented
    , scaleShow = \_ _ -> Left DeriveT.NotImplemented
    , scaleBottom = Pitch.pitch 0 0
    , scaleLayout = Scale.noOctaves
    , scaleTranspose = Scales.nonTransposing
    , scaleEnharmonics = Scales.noEnharmonics
    , scaleNoteToCall = noteToCall
    -- Since this isn't a proper scale, I can't think of any sensible way to
    -- input this with a music keyboard, so we'll have to use the computer
    -- keyboard.
    , scaleInputToNote = \_ _ -> Left DeriveT.InvalidInput
    , scaleInputToNn = Scales.directInputToNn
    , scaleCallDoc = Derive.extractDoc $ noteCall (Pitch.Note "1/1") id
    }

noteToCall :: Pitch.Note -> Maybe Derive.ValCall
noteToCall note = noteCall note <$>
    ParseA.maybeParse pNote (Pitch.noteText note)

noteCall :: Pitch.Note -> (Double -> Double) -> Derive.ValCall
noteCall note ratio = Derive.valCall Module.scale "ratio" mempty
    ( "Generate a frequency that is the ratio of the frequency of the "
    <> ShowVal.doc pcontrolRef
    <> " signal. A negative ratio divides, a positive one multiplies."
    ) $ Sig.call
    (defaulted "hz" (0 :: Double) "Add an absolute hz value to the output.") $
    \hz args -> do
        start <- Args.realStart args
        env <- Derive.getEnviron
        nn <- Derive.require
            ("ratio scale requires " <> ShowVal.showVal pcontrolRef)
            =<< Derive.namedNnAt pcontrol start
        let outNn = Pitch.hzToNn $ ratio (Pitch.nnToHz nn) + hz
        pure $ PSignal.pitch
            pscale (const $ pure outNn) (const $ pure note)
            (PSignal.PitchConfig env mempty)
    where
    pcontrolRef = DeriveT.Ref control Nothing :: DeriveT.PControlRef
    control = "ratio-source"
    pcontrol = "ratio-source" -- TODO remove
    pscale = Pitches.scale scale

-- | Ratios look like @2/5@, @-4/3@.  A negative ratio divides, a positive one
-- multiplies.
pNote :: ParseA.Parser (Double -> Double)
pNote = do
    num <- ParseA.pInt
    A.char '/'
    denom <- ParseA.pNat
    let ratio = fromIntegral (abs num) / fromIntegral denom
    pure $ if num < 0 then (/ ratio) else (* ratio)
