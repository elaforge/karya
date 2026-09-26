-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls which are somewhat specific to European music.
module Derive.C.Europe.Chord (library) where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch

import           Global


library :: Library.Library
library = Library.generators
    [ ("chord", cChord Unison)
    , ("chord^", cChord Up)
    , ("chord_", cChord Down)
    , ("stack", cStack Unison)
    , ("stack^", cStack Up)
    , ("stack_", cStack Down)
    ]

data Direction = Unison | Up | Down deriving (Show)

timeEnv :: Sig.Parser DeriveT.Duration
timeEnv = Typecheck._real <$>
    Sig.environ "time" Sig.Prefixed (Typecheck.real 0.08)
    "Time between notes, if they aren't unison."

-- * chord

cChord :: Direction -> Derive.Generator Derive.Note
cChord dir = Derive.generator Module.europe "chord" mempty
    "Make a chord based on a specialized chord notation. It's different from\
    \ the usual kind of chord notation because the base pitch is given\
    \ separately, and it has to be non-ambiguous, but the idea is the same."
    $ Sig.call ((,)
    <$> Sig.defaulted "name" ("" :: Text) "Chord name."
    <*> timeEnv
    ) $ \(name, time) -> Sub.inverting $ \args -> do
        base <- Call.getPitch =<< Args.realStart args
        intervals <- Derive.requireRight id $ parseChord base name
        fromIntervals dir base intervals time args

-- Default to major, 'm' is minor, 'd' diminished, 'a' augmented, sus2 and sus4.
-- Sevenths: dim, dom, aug
-- Ninths:
-- Arguments for modification: inversion, transpose 3 5 7 9 up an octave,
-- drop 3 5 7 9.
parseChord :: PSignal.Pitch -> Text -> Either Text [PSignal.Pitch]
parseChord _base _name = Left "not implemented" -- TODO


-- * stack

-- TODO terrible name, can't I come up with something better?
cStack :: Direction -> Derive.Generator Derive.Note
cStack dir = Derive.generator Module.europe "stack" mempty
    "Make a chord by stacking intervals on a base pitch. There are variants\
    \ that arpeggiate upwards or downwards, in addition to playing in unison."
    $ Sig.call ((,)
    <$> Sig.many "interval" "Relative intervals, starting from the base pitch.\
        \ Can be absolute pitch, diatonic intervals, or a chromatic interval.\
        \ Chromatic intervals are notated `m3`, `M3`, `p4` for minor third,\
        \ major third, and perfect fourth respectively."
    <*> timeEnv
    ) $ \(intervals, time) -> Sub.inverting $ \args -> do
        base <- Call.getPitch =<< Args.realStart args
        intervals <- resolveIntervals base intervals
        fromIntervals dir base intervals time args

fromIntervals :: Direction -> PSignal.Pitch -> [PSignal.Pitch]
    -> DeriveT.Duration -> Derive.PassedArgs a -> Derive.NoteDeriver
fromIntervals dir base intervals time args = do
    let start = Args.start args
    dur <- min (Args.duration args / fromIntegral (length intervals + 1)) <$>
        Call.scoreDuration start time
    let ts = case dir of
            Unison -> repeat start
            Up -> Lists.range_ start dur
            Down -> Lists.range_
                (start + dur * fromIntegral (length intervals)) (-dur)
    mconcat [Derive.place t (Args.end args - t) (Call.pitchedNote pitch)
        | (t, pitch) <- zip ts (base : intervals)]

type Interval = Either PSignal.Pitch (Either Pitch.Step Text)

resolveIntervals :: PSignal.Pitch -> [Interval]
    -> Derive.Deriver [PSignal.Pitch]
resolveIntervals b = fmap snd . Lists.mapAccumLM resolve b
    where
    resolve _ (Left pitch) = pure (pitch, pitch)
    resolve base (Right (Left steps)) = pure (p, p)
        where p = Pitches.transposeD (steps - 1) base
    resolve base (Right (Right sym)) = do
        p <- maybe
            (Derive.throw $ "can't parse: " <> ShowVal.showVal sym)
            (pure . flip Pitches.transposeC base) (parseInterval sym)
        pure (p, p)

parseInterval :: Text -> Maybe Pitch.Step
parseInterval = flip Map.lookup intervals
    where
    intervals = Map.fromList $ flip zip [1..]
        [ "m2", "M2", "m3", "M3", "p4", "a4", "p5", "m6", "M6", "m7", "M7"
        , "p8", "M9", "m10", "M10", "p11", "a11", "p12", "m13", "M13", "m14"
        , "M14"
        ]
