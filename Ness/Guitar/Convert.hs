module Ness.Guitar.Convert where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO

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
import qualified Ness.Guitar.Bali as Bali
import qualified Ness.Util as Util


-- | Scale dynamic=1 to this.
maxAmp :: Newtons
maxAmp = 0.65

type Error = Text

run :: String -> IO ()
run block = do
    scores <- either errorIO return =<< loadConvert block
    Util.submitOne "guitar-bali" (Guitar.renderAll (head scores)) False

loadConvert :: String -> IO (Either Error [(Guitar.Instrument, Guitar.Score)])
loadConvert b = convert Bali.instruments <$> load (blockFile b)

blockFile :: String -> FilePath
blockFile b = "im/ness-notes/untitled-" ++ b

printScore block = mapM_ (Text.IO.putStrLn . snd . Guitar.renderAll)
    =<< either errorIO return =<< loadConvert block

load :: FilePath -> IO [Note.Note]
load fname = either (errorIO . pretty) return =<< Note.unserialize fname

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
convert :: Map Text Guitar.Instrument -> [Note.Note]
    -> Either Error [(Guitar.Instrument, Guitar.Score)]
convert instruments =
    fmap (map (second (uncurry makeScore))) . collectFingers
        <=< mapM (convertNote instruments)

makeScore :: [Guitar.Note] -> [Guitar.Finger] -> Guitar.Score
makeScore notes fingers = Guitar.Score
    { sDecay = 4
    , sHighpass = True
    , sNotes = notes
    , sFingers = fingers
    }

convertNote :: Map Text Guitar.Instrument -> Note.Note -> Either Error Note
convertNote instruments note = first ((pretty note <> ": ")<>) $ do
    inst <- tryJust "no instrument" $
        Map.lookup (Note.instrument note) instruments
    pitch <- tryJust "no pitch" $ Map.lookup Control.pitch $ Note.controls note
    finger <- tryJust "no finger" $ Map.lookup "finger" $ Note.controls note
    dyn <- tryJust "no amp" $ Note.initialControl Control.dynamic note
    loc <- tryJust "no location" $ Note.initialControl "location" note
    string <- tryJust ("no string: " <> Note.element note) $
        List.find ((== Note.element note) . pretty . Guitar.sNn)
            (Guitar.iStrings inst)
    let muted = Attrs.contain (Note.attributes note) Attrs.mute
    -- Mute by touching lightly higher up on the string.
    return $ if muted
        then Note
            { _instrument = inst
            , _instrumentName = Note.instrument note
            , _string = string
            , _start = Note.start note
            , _duration = Note.duration note
            , _pitch = Signal.constant $
                fromMaybe 0 (Signal.at (Note.start note) pitch)
                    + Pitch.nn_to_double muteOffset
            , _finger = Signal.constant 0.01
            , _dynamic = 0
            , _location = 0
            }
        else Note
            { _instrument = inst
            , _instrumentName = Note.instrument note
            , _string = string
            , _start = Note.start note
            , _duration = Note.duration note
            , _pitch = pitch
            , _finger = finger
            , _dynamic = dyn
            , _location = loc
            }

muteOffset :: Pitch.NoteNumber
muteOffset = 0.5

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

collectFingers :: [Note]
    -> Either Error [(Guitar.Instrument, ([Guitar.Note], [Guitar.Finger]))]
collectFingers =
    mapM collect . map (second (Seq.keyed_group_stable _string))
        . Seq.keyed_group_stable _instrument
    where
    collect (inst, stringNotes) = do
        (notes, fingers) <- unzip <$> mapM oneString stringNotes
        -- TODO merge by start?
        return (inst, (concat notes, fingers))

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
        , fInitial = (0.01, 0)
        , fMovement =
            [ ( RealTime.to_seconds x
              , Guitar.pitchLocation nn (Pitch.nn y)
              , if Pitch.nns_equal (Pitch.nn y) nn then 0
                else fromMaybe 0 (Signal.at x finger) * maxAmp
              )
            | (x, y) <- Signal.unsignal pitch
            ]
        }
        where nn = Guitar.sNn string

findOverlaps :: [Note] -> [(Note, Note)]
findOverlaps [] = []
findOverlaps [_] = []
findOverlaps (n1 : n2 : ns)
    | _start n1 + _duration n1 > _start n2 = (n1, n2) : findOverlaps (n2 : ns)
    | otherwise = findOverlaps (n2 : ns)


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
    format (Note _inst _str start dur pitch finger dyn loc) =
        Pretty.record "Note"
            [ ("start", Pretty.format start)
            -- , ("string", Pretty.format str)
            , ("duration", Pretty.format dur)
            , ("pitch", Pretty.format pitch)
            , ("finger", Pretty.format finger)
            , ("dynamic", Pretty.format dyn)
            , ("location", Pretty.format loc)
            ]
