module Ness.Guitar.Convert where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Ness.Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.Patch as Patch


-- | Scale dynamic=1 to this.
maxAmp :: Newtons
maxAmp = 0.65

type Error = Text

{- | I want to do explicit damping like reyong, so ignore all durations, but
    treat a special +mute note.

    . Pick a predefined instrument based on 'Note.instrument'.
    . Pick a string based on the lowest pitch.
    . Map %location.
    . Convert the pitch curve to finger movements.
    . Convert +mute notes to a finger lift.

    This means I have to group together all notes on the same string, so I can
    assign a single finger movement.

    How do I know which string for +mute?  Use the pitch as usual.
-}
convert :: Guitar.Instrument -> [Note.Note] -> Either Error Guitar.Score
convert inst =
    fmap (uncurry makeScore) . collectFingers <=< mapM (convertNote inst)

makeScore :: [Guitar.Note] -> [Guitar.Finger] -> Guitar.Score
makeScore notes fingers = Guitar.Score
    { sDecay = 4
    , sHighpass = True
    , sNotes = notes
    , sFingers = fingers
    }

convertNote :: Guitar.Instrument -> Note.Note -> Either Error Note
convertNote inst note = first ((pretty note <> ": ")<>) $ do
    pitch <- tryJust "no pitch" $ Map.lookup Control.pitch $ Note.controls note
    finger <- tryJust "no finger" $
        Map.lookup Patch.c_finger $ Note.controls note
    dyn <- tryJust "no amp" $ Note.controlVal Control.dynamic note
    loc <- tryJust "no location" $ Note.controlVal Patch.c_location note
    string <- tryJust ("no string: " <> Note.element note) $
        List.find ((== Note.element note) . pretty . Guitar.sNn)
            (Guitar.iStrings inst)
    let muted = Attrs.contain (Note.attributes note) Attrs.mute
    let converted = Note
            { _instrument = inst
            , _string = string
            , _start = Note.start note
            , _duration = Note.duration note
            , _pitch = pitch
            , _finger = finger
            , _dynamic = dyn
            , _location = loc
            }
    return $ if muted then muteNote (Note.start note) converted else converted

-- | Mute by touching lightly higher up on the string.
muteNote :: RealTime.RealTime -> Note -> Note
muteNote start note = note
    { _start = start
    , _pitch = Signal.constant $
        fromMaybe 0 (Signal.at (_start note) (_pitch note))
            + Pitch.nn_to_double muteOffset
    , _finger = Signal.constant 0.01
    , _dynamic = 0
    , _location = 0
    }

muteOffset :: Pitch.NoteNumber
muteOffset = 0.25

data Note = Note {
    _instrument :: !Guitar.Instrument
    , _string :: !Guitar.String
    , _start :: !RealTime.RealTime
    -- | Since strings are only damped explicitly, this is only used to
    -- define the pitch range when selecting a string.
    , _duration :: !RealTime.RealTime
    , _pitch :: !Signal.Signal
    -- | Finger touch strength.
    , _finger :: !Signal.Signal
    -- | If 0, there is no corresponding pluck, so this is just a finger
    -- movement.
    , _dynamic :: !Newtons
    , _location :: !Location
    } deriving (Show)

assignString :: [(Pitch.NoteNumber, string)] -> Note
    -> Either Error ((Pitch.NoteNumber, string), Note)
assignString strings note = first ((pretty note <> ": ")<>) $ do
    let pitches = Signal.within (_start note) (_start note + _duration note)
            (_pitch note)
    lowest <- tryJust "no pitch" $ fmap Pitch.nn $
        Seq.minimum $ map snd $ Signal.unsignal pitches
    str <- tryJust "below strings" $ lastLessEqual fst (lowest + eta) strings
    return (str, note)
    where
    -- It's ok to be this far below the string.
    eta = 0.005

collectFingers :: [Note] -> Either Error ([Guitar.Note], [Guitar.Finger])
collectFingers = collect . Seq.keyed_group_stable _string
    where
    collect stringNotes = do
        (notes, fingers) <- unzip <$> mapM oneString stringNotes
        -- TODO merge by start?
        return (concat notes, fingers)
    -- All the notes on a single string get a single finger.
    oneString :: (Guitar.String, [Note])
        -> Either Error ([Guitar.Note], Guitar.Finger)
    oneString (string, notes) = do
        let overlaps = findOverlaps notes
        let startDur n = (_start n, _duration n)
        unless (null overlaps) $
            Left $ "overlaps: " <> pretty (map (startDur *** startDur) overlaps)
        let pitch = Signal.merge_right_extend
                [Signal.clip_to (_start n) (_pitch n) | n <- notes]
            finger = Signal.merge_right_extend
                [Signal.clip_to (_start n) (_finger n) | n <- notes]
        return
            ( filter ((>0) . Guitar.nAmplitude) $ map (makeNote string) notes
            , makeFinger string pitch finger
            )
    makeNote string note = Guitar.Note
        { nStrike = Guitar.Pluck
        , nString = string
        , nStart = RealTime.to_seconds (_start note)
        , nDuration = 0.0013
        , nLocation = _location note
        , nAmplitude = _dynamic note * maxAmp
        }
    makeFinger string pitch finger = Guitar.Finger
        { fString = string
        , fInitial = (0, 0)
        , fMovement = movement string (Signal.unsignal pitch) finger
        }
    movement _ [] _ = []
    movement string ((x0, y0) : pitch) finger =
        position nn (x0-offset) y0 0
        : position nn (x0-offset) y0 (fingerAt x0)
        : [position nn x y (fingerAt x) | (x, y) <- pitch]
        where
        nn = Guitar.sNn string
        fingerAt x = fromMaybe 0 $ Signal.at x finger
    offset = 0.01
    position stringNn x y amp =
        ( RealTime.to_seconds x
        , Guitar.pitchLocation stringNn (Pitch.nn y)
        , if Pitch.nns_equal (Pitch.nn y) stringNn then 0 else amp * maxAmp
        )

findOverlaps :: [Note] -> [(Note, Note)]
findOverlaps [] = []
findOverlaps [_] = []
findOverlaps (n1 : n2 : ns)
    | _start n1 + _duration n1 - eta > _start n2 =
        (n1, n2) : findOverlaps (n2 : ns)
    | otherwise = findOverlaps (n2 : ns)
    where eta = 0.001


-- * util

lastLessEqual :: Ord k => (a -> k) -> k -> [a] -> Maybe a
lastLessEqual key k = go
    where
    go [] = Nothing
    go [x]
        | key x <= k = Just x
        | otherwise = Nothing
    go (x1:x2:xs)
        | key x2 <= k = go (x2:xs)
        | otherwise = go [x1]

instance Pretty Note where
    format (Note inst _str start dur pitch finger dyn loc) =
        Pretty.record "Note"
            [ ("instrument", Pretty.format (Guitar.iName inst))
            , ("start", Pretty.format start)
            -- , ("string", Pretty.format str)
            , ("duration", Pretty.format dur)
            , ("pitch", Pretty.format pitch)
            , ("finger", Pretty.format finger)
            , ("dynamic", Pretty.format dyn)
            , ("location", Pretty.format loc)
            ]
