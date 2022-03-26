-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for Balinese instruments.
module Cmd.Instrument.Bali where
import qualified Data.List as List

import qualified Util.Doc as Doc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Args as Args
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale as Scale
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Global
import Types


pasang_msg :: Text
pasang_msg = "must be realized via `unison`, `kempyung`, `k`, &co"

-- | Emit events for both polos and sangsih.
pasang_code :: MidiInst.Code
pasang_code = MidiInst.thru pasang_thru

-- | Dispatch MIDI through to both polos and sangsih instruments.
pasang_thru :: Cmd.ThruFunction
pasang_thru scale _attrs input = do
    track <- Selection.track
    polos <- Perf.lookup_val track Gangsa.inst_polos
    sangsih <- Perf.lookup_val track Gangsa.inst_sangsih
    p_thru <- case polos of
        Nothing -> return []
        Just inst -> do
            attrs <- Cmd.get_instrument_attributes inst
            MidiThru.for_instrument inst scale attrs input
    s_thru <- case sangsih of
        Nothing -> return []
        Just inst -> do
            attrs <- Cmd.get_instrument_attributes inst
            MidiThru.for_instrument inst scale attrs $
                InputNote.offset_note_id 1 input
    return $ p_thru ++ s_thru

zero_dur_mute :: Signal.Y -> MidiInst.Code
zero_dur_mute dyn = zero_dur_mute_with "" (\_ -> Call.multiply_dynamic dyn)
    (Note.default_note Note.use_attributes)

gangsa_note :: Signal.Y -> Maybe Scale.Range -> MidiInst.Code
gangsa_note dyn maybe_range = zero_dur_mute_with (doc maybe_range)
    (\_ -> Call.multiply_dynamic dyn)
    $ \args -> maybe id (\top -> wrap top (Args.start args)) maybe_range $
        Note.default_note Note.use_attributes args
    where
    doc Nothing = ""
    doc (Just (Scale.Range bottom top)) =
        " Any pitch below " <> Doc.pretty bottom <> " or above "
        <> Doc.pretty top <> " will be transposed down by octaves until it\
        \ fits in the instrument's range."

zero_dur_mute_with :: Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (Derive.NoteArgs -> Derive.NoteDeriver)
    -> MidiInst.Code
zero_dur_mute_with doc transform note =
    MidiInst.null_call $ DUtil.zero_duration "note"
        ("When the event has zero duration, dispatch to the "
            <> ShowVal.doc Symbols.mute <> " call." <> doc)
        -- This only needs to invert if the transform needs it.  Otherwise,
        -- mute should do that.
        (Sub.inverting $ \args -> transform args $ reapply_mute args)
        (Sub.inverting note)

reapply_mute :: Derive.NoteArgs -> Derive.NoteDeriver
reapply_mute args = Eval.reapply_call (Args.context args) Symbols.mute []

wrap :: Scale.Range -> ScoreTime -> Derive.Deriver a -> Derive.Deriver a
wrap range start deriver = do
    (parse_p, show_p, _) <- Call.get_pitch_functions
    from_pitch <- Call.get_parsed_pitch parse_p =<< Derive.real start
    to_pitch <- Call.eval_pitch show_p start (wrap_octaves range from_pitch)
    Call.with_transposed_pitch to_pitch deriver

wrap_octaves :: Scale.Range -> Pitch.Pitch -> Pitch.Pitch
wrap_octaves (Scale.Range bottom top) pitch
    | pitch > top = try [octave top, octave top - 1]
    | pitch < bottom = try [octave bottom, octave bottom + 1]
    | otherwise = pitch
    where
    try = fromMaybe pitch . List.find in_range
        . map (\oct -> pitch { Pitch.pitch_octave = oct })
    octave = Pitch.pitch_octave
    in_range p = bottom <= p && p <= top
