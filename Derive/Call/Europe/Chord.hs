-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls which are somewhat specific to European music.
module Derive.Call.Europe.Chord where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("chord", c_chord Unison)
    , ("chord^", c_chord Up)
    , ("chord_", c_chord Down)
    , ("stack", c_stack Unison)
    , ("stack^", c_stack Up)
    , ("stack_", c_stack Down)
    ]

data Direction = Unison | Up | Down deriving (Show)

time_env :: Sig.Parser TrackLang.RealOrScore
time_env = TrackLang.default_real <$>
    Sig.environ "time" Sig.Prefixed (TrackLang.real 0.08)
    "Time between notes, if they aren't unison."

-- * chord

c_chord :: Direction -> Derive.Generator Derive.Note
c_chord dir = Derive.make_call "chord" Tags.europe
    "Make a chord based on a specialized chord notation. It's different from\
    \ the usual kind of chord notation because the base pitch is given\
    \ separately, and it has to be non-ambiguous, but the idea is the same."
    $ Sig.call ((,)
    <$> Sig.defaulted "name" "" "Chord name."
    <*> time_env
    ) $ \(name, time) -> Sub.inverting $ \args -> do
        base <- Util.get_pitch =<< Args.real_start args
        intervals <- Derive.require_right id $ parse_chord base name
        from_intervals dir base intervals time args

-- Default to major, 'm' is minor, 'd' diminished, 'a' augmented, sus2 and sus4.
-- Sevenths: dim, dom, aug
-- Ninths:
-- Arguments for modification: inversion, transpose 3 5 7 9 up an octave,
-- drop 3 5 7 9.
parse_chord :: PitchSignal.Pitch -> Text -> Either String [PitchSignal.Pitch]
parse_chord _base _name = Left "not implemented" -- TODO


-- * stack

-- TODO terrible name, can't I come up with something better?
c_stack :: Direction -> Derive.Generator Derive.Note
c_stack dir = Derive.make_call "stack" Tags.europe
    "Make a chord by stacking intervals on a base pitch. There are variants\
    \ that arpeggiate upwards or downwards, in addition to playing in unison."
    $ Sig.call ((,)
    <$> Sig.many "interval" "Relative intervals, starting from the base pitch.\
        \ Can be absolute pitch, diatonic intervals, or a chromatic interval.\
        \ Chromatic intervals are notated `m3`, `M3`, `p4` for minor third,\
        \ major third, and perfect fourth respectively."
    <*> time_env
    ) $ \(intervals, time) -> Sub.inverting $ \args -> do
        base <- Util.get_pitch =<< Args.real_start args
        intervals <- resolve_intervals base intervals
        from_intervals dir base intervals time args

from_intervals :: Direction -> PitchSignal.Pitch -> [PitchSignal.Pitch]
    -> TrackLang.RealOrScore -> Derive.PassedArgs a -> Derive.NoteDeriver
from_intervals dir base intervals time args = do
    let start = Args.start args
    dur <- min (Args.duration args / fromIntegral (length intervals + 1)) <$>
        Util.duration_from start time
    let ts = case dir of
            Unison -> repeat start
            Up -> Seq.range_ start dur
            Down -> Seq.range_
                (start + dur * fromIntegral (length intervals)) (-dur)
    mconcat [Derive.d_place t (Args.end args - t) (Util.pitched_note pitch)
        | (t, pitch) <- zip ts (base : intervals)]

type Interval = Either PitchSignal.Pitch (Either Pitch.Step Text)

resolve_intervals :: PitchSignal.Pitch -> [Interval]
    -> Derive.Deriver [PitchSignal.Pitch]
resolve_intervals b = fmap snd . Seq.mapAccumLM resolve b
    where
    resolve _ (Left pitch) = return (pitch, pitch)
    resolve base (Right (Left steps)) = return (p, p)
        where p = Pitches.transpose_d (steps - 1) base
    resolve base (Right (Right sym)) = do
        p <- maybe
            (Derive.throw $ "can't parse: " <> untxt (ShowVal.show_val sym))
            (return . flip Pitches.transpose_c base) (parse_interval sym)
        return (p, p)

parse_interval :: Text -> Maybe Pitch.Step
parse_interval = flip Map.lookup intervals
    where
    intervals = Map.fromList $ flip zip [1..]
        [ "m2", "M2", "m3", "M3", "p4", "a4", "p5", "m6", "M6", "m7", "M7"
        , "p8", "M9", "m10", "M10", "p11", "a11", "p12", "m13", "M13", "m14"
        , "M14"
        ]
