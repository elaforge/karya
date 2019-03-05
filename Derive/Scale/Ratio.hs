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

import qualified Util.ParseText as ParseText
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

import           Global


scales :: [Scale.Definition]
scales = map Scale.Simple [scale]

scale :: Scale.Scale
scale = Scale.Scale
    { scale_id = "ratio"
    , scale_pattern = "[+-]?\\d+/\\d+ e.g. 2/5 or -4/3"
    , scale_symbols = []
    , scale_transposers = mempty
    , scale_read = \_ _ -> Left DeriveT.NotImplemented
    , scale_show = \_ _ -> Left DeriveT.NotImplemented
    , scale_bottom = Pitch.pitch 0 0
    , scale_layout = Scale.no_octaves
    , scale_transpose = Scales.non_transposing
    , scale_enharmonics = Scales.no_enharmonics
    , scale_note_to_call = note_to_call
    -- Since this isn't a proper scale, I can't think of any sensible way to
    -- input this with a music keyboard, so we'll have to use the computer
    -- keyboard.
    , scale_input_to_note = \_ _ -> Left DeriveT.InvalidInput
    , scale_input_to_nn = Scales.direct_input_to_nn
    , scale_call_doc = Derive.extract_val_doc $ note_call (Pitch.Note "1/1") id
    }

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = note_call note <$>
    ParseText.maybe_parse p_note (Pitch.note_text note)

note_call :: Pitch.Note -> (Double -> Double) -> Derive.ValCall
note_call note ratio = Derive.val_call Module.scale "ratio" mempty
    ( "Generate a frequency that is the ratio of the frequency of the "
    <> ShowVal.doc pcontrol_ref
    <> " signal. A negative ratio divides, a positive one multiplies."
    ) $ Sig.call
    (defaulted "hz" 0 "Add an absolute hz value to the output.") $
    \hz args -> do
        start <- Args.real_start args
        env <- Derive.get_environ
        nn <- Derive.require
            ("ratio scale requires " <> ShowVal.show_val pcontrol_ref)
            =<< Derive.named_nn_at pcontrol start
        let out_nn = Pitch.hz_to_nn $ ratio (Pitch.nn_to_hz nn) + hz
        return $ PSignal.pitch
            pscale (const $ return out_nn) (const $ return note)
            (PSignal.PitchConfig env mempty)
    where
    pcontrol_ref = DeriveT.LiteralControl control :: DeriveT.PControlRef
    control = "ratio-source"
    pcontrol = "ratio-source" -- TODO remove
    pscale = Pitches.scale scale

-- | Ratios look like @2/5@, @-4/3@.  A negative ratio divides, a positive one
-- multiplies.
p_note :: ParseText.Parser (Double -> Double)
p_note = do
    num <- ParseText.p_int
    A.char '/'
    denom <- ParseText.p_nat
    let ratio = fromIntegral (abs num) / fromIntegral denom
    return $ if num < 0 then (/ ratio) else (* ratio)
