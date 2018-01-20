-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-process notes to add artifacts characteristic of wind instruments.
module Derive.C.Idiom.Wind (library, find_harmonic) where
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import Global
import Types


library :: Library.Library
library = Library.transformers
    [ ("wind-idiom", c_wind_idiom)
    ]

module_ :: Module.Module
module_ = "idiom" <> "wind"

c_wind_idiom :: Derive.Transformer Derive.Note
c_wind_idiom = Derive.transformer module_ "wind-idiom"
    (Tags.postproc <> Tags.inst)
    "Post-process events to play in a monophonic wind-like idiom.\
    \\nThis tweaks the ends of the pitch signals of notes depending on the\
    \ following note.  When a note is played, a (fundamental, harmonic) pair\
    \ is chosen that best fits it.  When a subsequent note is played, a new\
    \ pair is chosen for it.\
    \\nThe decay of the note will be shifted to the corresponding harmonic\
    \ of the the fundamental of the subsequent note.  If the fundamentals are\
    \ the same then the pitch will remain constant, of course."
    $ Sig.callt fundamentals_env $ \fundamentals args deriver -> do
        start <- Args.real_start args
        fundamentals <- mapM
            (either return (Pitches.pitch_nn <=< Derive.resolve_pitch start))
            fundamentals
        wind_idiom (map Pitch.nn_to_hz fundamentals) =<< deriver

type Fundamentals = [Pitch.Hz]

wind_idiom :: Fundamentals -> Stream.Stream Score.Event -> Derive.NoteDeriver
wind_idiom fundamentals = return . Post.emap1_ (process fundamentals)
    . Stream.zip_on (map Seq.head . Post.nexts)

fundamentals_env :: Sig.Parser [Either Pitch.NoteNumber PSignal.Pitch]
fundamentals_env = Sig.check non_empty $
    Sig.environ "fundamentals" Sig.Unprefixed []
        "Fundamentals for this instrument."
    where
    non_empty xs
        | null xs = Just "fundamentals required"
        | otherwise = Nothing

-- | On each event, find the closest (fundamental, harmonic) for this pitch and
-- the next event's pitch.  On the next note's start time, shift the pitch to
-- same harmonic on the next note's fundamental.
process :: Fundamentals -> (Maybe Score.Event, Score.Event) -> Score.Event
process fundamentals (maybe_next, event) = fromMaybe event $ do
    next <- maybe_next
    (next_fundamental, _) <- harmonic_of fundamentals next
    (_, this_harmonic) <- harmonic_of fundamentals event
    return $ tweak_pitch (Score.event_start next)
        (next_fundamental *# this_harmonic) event

harmonic_of :: Fundamentals -> Score.Event -> Maybe (Pitch.Hz, Int)
harmonic_of fundamentals =
    find_harmonic fundamentals . Pitch.nn_to_hz <=< Score.initial_nn

find_harmonic :: Fundamentals -> Pitch.Hz -> Maybe (Pitch.Hz, Int)
find_harmonic fundamentals hz = closest $ map find fundamentals
    where
    -- I always pick the closest match, but I also want a lower harmonic.
    -- Hopefully this doesn't matter in practice, since fundamentals generally
    -- aren't multiples of each other.
    closest = fmap snd . Seq.minimum_on (abs . fst)
    find f = go 1
        where
        go harm
            | f *# (harm+1) - eta * (fromIntegral harm + 1) > hz =
                (hz - f *# harm, (f, harm))
            | otherwise = go (harm+1)
    -- Hz is a little imprecise, so while I want to pick the closest harmonic
    -- below 'hz', I can pick one above if it's only a little above.  Also, any
    -- imprecision in the original fundamental will be multiplied with the
    -- harmonic, so I can multiply the eta too.
    eta = 0.25

-- | Hz raised to the nth harmonic.
(*#) :: Pitch.Hz -> Int -> Pitch.Hz
fundamental *# harmonic = fundamental * fromIntegral harmonic

tweak_pitch :: RealTime -> Pitch.Hz -> Score.Event -> Score.Event
tweak_pitch pos hz event =
    Score.set_pitch (Score.event_pitch event <> tweak) event
    where tweak = PSignal.from_sample pos (PSignal.nn_pitch (Pitch.hz_to_nn hz))
