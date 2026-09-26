-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Operations on pitch tracks.
--
-- TODO It seems a little arbitrary to divide these cmds out like this.
-- However, they are distinct from Cmd.PitchTrack, so a separate module is
-- good.  I suppose if I need these functions elsewhere I can more them to more
-- generic places.
module Cmd.Repl.LPitch where
import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import           Global


-- | Turn an nn back to a human-readable note name.
nnToNote :: Int -> Maybe Pitch.Note
nnToNote = Twelve.showNn . fromIntegral

-- * scales

patchScale :: Cmd.M m => Pitch.ScaleId -> m Patch.Scale
patchScale scaleId = (Cmd.requireRight id <=< Perf.derive) $ do
    scale <- Derive.getScale scaleId
    nns <- Scale.noteNumbers scale mempty
    pure $ Scale.patchScale scaleId nns


-- * transpose

-- | Chromatic transposition.
transposeC :: Cmd.M m => Pitch.Step -> ModifyEvents.Track m
transposeC = transpose Scale.Chromatic 0

-- | Diatonic transposition.
transposeD :: Cmd.M m => Pitch.Step -> ModifyEvents.Track m
transposeD = transpose Scale.Diatonic 0

-- | Octave transposition.
transposeO :: Cmd.M m => Pitch.Octave -> ModifyEvents.Track m
transposeO octaves = transpose Scale.Chromatic octaves 0

transpose :: Cmd.M m => Scale.Transposition -> Pitch.Octave -> Pitch.Step
    -> ModifyEvents.Track m
transpose transposition octs steps =
    PitchTrack.pitchTracks $ PitchTrack.transpose transposition octs steps

-- * modify pitches

modifyPitch :: Cmd.M m => (Pitch.Pitch -> Pitch.Pitch)
    -> ModifyEvents.Track m
modifyPitch modify = PitchTrack.pitchTracks $ \scale env note -> do
    pitch <- first pretty $ Scale.scaleRead scale env note
    first pretty $ Scale.scaleShow scale env $ modify pitch

-- | Change notes from one scale to another.  This only makes sense if the
-- scales have the same number of notes per octave.
--
-- TODO it would be nice to change the track title too, but ModifyEvents.Track
-- doesn't support that.  Of course, maybe I want the scale in the block or
-- global transform.
changeScale :: Cmd.M m => Pitch.ScaleId -> m (ModifyEvents.Track m)
changeScale toScaleId = do
    track <- Selection.track
    toScale <- Cmd.require "scale not found"
        =<< Perf.lookupScale track toScaleId
    pure $ PitchTrack.pitchTracks $ \fromScale env note -> do
        pitch <- first pretty $ Scale.scaleRead fromScale env note
        first pretty $ Scale.scaleShow toScale env pitch

-- | Convert the selected absolute pitch track into a relative one by
-- subtracting all the notes from the given base note.
--
-- TODO as above, would be nice to set thet track title.
toRelative :: Cmd.M m => Bool -> Pitch.Note -> ModifyEvents.Track m
toRelative diatonic base = PitchTrack.pitchTracks $ \scale env note -> do
    base <- first pretty $ Scale.scaleRead scale env base
    pitch <- first pretty $ Scale.scaleRead scale env note
    let layout = Scale.scaleLayout scale
    let d = if diatonic then Scale.diatonicDifference layout pitch base
            else Scale.chromaticDifference layout pitch base
    pure $ Pitch.Note $ ShowVal.showVal d


-- * enharmonics

-- | Respell enharmonics according to the key.
respell :: Cmd.M m => ModifyEvents.Track m
respell = PitchTrack.pitchTracks $ \scale env note -> first pretty $ do
    pitch <- Scale.scaleRead scale env note
    Scale.scaleInputToNote scale env (toPianoInput pitch)

toPianoInput :: Pitch.Pitch -> Pitch.Input
toPianoInput pitch = Pitch.Input Pitch.PianoKbd (toSharps pitch) 0
    where
    toSharps = Theory.semisToPitchSharps Theory.pianoLayout
        . Theory.pitchToSemis Theory.pianoLayout

-- | Convert all sharps to their enharmonic flats.
sharpsToFlats :: Cmd.M m => ModifyEvents.Track m
sharpsToFlats = PitchTrack.pitchTracks $ \scale env note -> first pretty $ do
    pitch <- Scale.scaleRead scale env note
    if Pitch.pitchAccidentals pitch <= 0 then pure note else do
        notes <- Scale.scaleEnharmonics scale env note
        pitches <- mapM (Scale.scaleRead scale env) notes
        justErr (DeriveT.PitchError $ "no flats for " <> pretty note) $
            eSharpsToFlats (zip pitches notes)

eSharpsToFlats :: [(Pitch.Pitch, a)] -> Maybe a
eSharpsToFlats = fmap snd . Lists.maximumOn accs . filter ((<0) . accs)
    where accs = Pitch.pitchAccidentals . fst
