-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-process notes to add artifacts characteristic of wind instruments.
module Derive.C.Idiom.Wind (library, findHarmonic) where
import qualified Util.Lists as Lists
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

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("wind-idiom", cWindIdiom)
    ]

module_ :: Module.Module
module_ = "idiom" <> "wind"

cWindIdiom :: Derive.Transformer Derive.Note
cWindIdiom = Derive.transformer module_ "wind-idiom"
    (Tags.postproc <> Tags.inst)
    "Post-process events to play in a monophonic wind-like idiom.\
    \\nThis tweaks the ends of the pitch signals of notes depending on the\
    \ following note.  When a note is played, a (fundamental, harmonic) pair\
    \ is chosen that best fits it.  When a subsequent note is played, a new\
    \ pair is chosen for it.\
    \\nThe decay of the note will be shifted to the corresponding harmonic\
    \ of the the fundamental of the subsequent note.  If the fundamentals are\
    \ the same then the pitch will remain constant, of course."
    $ Sig.callt fundamentalsEnv $ \fundamentals args deriver -> do
        start <- Args.realStart args
        fundamentals <- mapM
            (either pure (Pitches.pitchNn <=< Derive.resolvePitch start))
            fundamentals
        windIdiom (map Pitch.nnToHz fundamentals) =<< deriver

type Fundamentals = [Pitch.Hz]

windIdiom :: Fundamentals -> Stream.Stream Score.Event -> Derive.NoteDeriver
windIdiom fundamentals = pure . Post.emap1_ (process fundamentals)
    . Stream.zipOn (map Lists.head . Post.nexts)

fundamentalsEnv :: Sig.Parser [Either Pitch.NoteNumber PSignal.Pitch]
fundamentalsEnv = Sig.check nonEmpty $
    Sig.environ "fundamentals" Sig.Unprefixed ([] :: [Pitch.NoteNumber])
        "Fundamentals for this instrument."
    where
    nonEmpty xs
        | null xs = Just "fundamentals required"
        | otherwise = Nothing

-- | On each event, find the closest (fundamental, harmonic) for this pitch and
-- the next event's pitch.  On the next note's start time, shift the pitch to
-- same harmonic on the next note's fundamental.
process :: Fundamentals -> (Maybe Score.Event, Score.Event) -> Score.Event
process fundamentals (maybeNext, event) = fromMaybe event $ do
    next <- maybeNext
    (nextFundamental, _) <- harmonicOf fundamentals next
    (_, thisHarmonic) <- harmonicOf fundamentals event
    pure $ tweakPitch (Score.eventStart next)
        (nextFundamental *# thisHarmonic) event

harmonicOf :: Fundamentals -> Score.Event -> Maybe (Pitch.Hz, Int)
harmonicOf fundamentals =
    findHarmonic fundamentals . Pitch.nnToHz <=< Score.initialNn

findHarmonic :: Fundamentals -> Pitch.Hz -> Maybe (Pitch.Hz, Int)
findHarmonic fundamentals hz = closest $ map find fundamentals
    where
    -- I always pick the closest match, but I also want a lower harmonic.
    -- Hopefully this doesn't matter in practice, since fundamentals generally
    -- aren't multiples of each other.
    closest = fmap snd . Lists.minimumOn (abs . fst)
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

tweakPitch :: RealTime -> Pitch.Hz -> Score.Event -> Score.Event
tweakPitch pos hz event =
    Score.setPitch (Score.eventPitch event <> tweak) event
    where tweak = PSignal.fromSample pos (PSignal.nnPitch (Pitch.hzToNn hz))
