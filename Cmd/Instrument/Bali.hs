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


pasangMsg :: Text
pasangMsg = "must be realized via `unison`, `kempyung`, `k`, &co"

-- | Emit events for both polos and sangsih.
pasangCode :: MidiInst.Code
pasangCode = MidiInst.thru pasangThru

-- | Dispatch MIDI through to both polos and sangsih instruments.
pasangThru :: Cmd.ThruFunction
pasangThru scale _attrs input = do
    track <- Selection.track
    polos <- Perf.lookupVal track Gangsa.instPolos
    sangsih <- Perf.lookupVal track Gangsa.instSangsih
    pThru <- case polos of
        Nothing -> pure []
        Just inst -> do
            attrs <- Cmd.getInstrumentAttributes inst
            MidiThru.forInstrument inst scale attrs input
    sThru <- case sangsih of
        Nothing -> pure []
        Just inst -> do
            attrs <- Cmd.getInstrumentAttributes inst
            MidiThru.forInstrument inst scale attrs $
                InputNote.offsetNoteId 1 input
    pure $ pThru ++ sThru

zeroDurMute :: Signal.Y -> MidiInst.Code
zeroDurMute dyn = zeroDurMuteWith "" (\_ -> Call.multiplyDynamic dyn)
    (Note.defaultNote Note.useAttributes)

gangsaNote :: Signal.Y -> Maybe Scale.Range -> MidiInst.Code
gangsaNote dyn maybeRange = zeroDurMuteWith (doc maybeRange)
    (\_ -> Call.multiplyDynamic dyn)
    $ \args -> maybe id (\top -> wrap top (Args.start args)) maybeRange $
        Note.defaultNote Note.useAttributes args
    where
    doc Nothing = ""
    doc (Just (Scale.Range bottom top)) =
        " Any pitch below " <> Doc.pretty bottom <> " or above "
        <> Doc.pretty top <> " will be transposed down by octaves until it\
        \ fits in the instrument's range."

zeroDurMuteWith :: Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (Derive.NoteArgs -> Derive.NoteDeriver)
    -> MidiInst.Code
zeroDurMuteWith doc transform note =
    MidiInst.nullCall $ DUtil.zeroDuration "note"
        ("When the event has zero duration, dispatch to the "
            <> ShowVal.doc Symbols.mute <> " call." <> doc)
        -- This only needs to invert if the transform needs it.  Otherwise,
        -- mute should do that.
        (Sub.inverting $ \args -> transform args $ reapplyMute args)
        (Sub.inverting note)

reapplyMute :: Derive.NoteArgs -> Derive.NoteDeriver
reapplyMute args = Eval.reapplyCall (Args.context args) Symbols.mute []

wrap :: Scale.Range -> ScoreTime -> Derive.Deriver a -> Derive.Deriver a
wrap range start deriver = do
    (parseP, showP, _) <- Call.getPitchFunctions
    fromPitch <- Call.getParsedPitch parseP =<< Derive.real start
    toPitch <- Call.evalPitch showP start (wrapOctaves range fromPitch)
    Call.withTransposedPitch toPitch deriver

wrapOctaves :: Scale.Range -> Pitch.Pitch -> Pitch.Pitch
wrapOctaves (Scale.Range bottom top) pitch
    | pitch > top = try [octave top, octave top - 1]
    | pitch < bottom = try [octave bottom, octave bottom + 1]
    | otherwise = pitch
    where
    try = fromMaybe pitch . List.find inRange
        . map (\oct -> pitch { Pitch.pitchOctave = oct })
    octave = Pitch.pitchOctave
    inRange p = bottom <= p && p <= top
