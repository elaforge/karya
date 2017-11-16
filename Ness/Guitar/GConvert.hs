-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Guitar.GConvert where
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
    let get c = tryJust ("no " <> pretty c) $ Map.lookup c (Note.controls note)
    let getStart c = tryJust ("no value: " <> pretty c)
            . Signal.at (Note.start note) =<< get c
    pitch <- get Control.pitch
    finger <- get Patch.c_finger
    dyn <- getStart Control.dynamic
    loc <- getStart Patch.c_location
    string <- tryJust ("no string: " <> showt (Note.element note)) $
        List.find ((== Note.element note) . Guitar.sName)
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

collectFingers :: [Note] -> Either Error ([Guitar.Note], [Guitar.Finger])
collectFingers = collect . Seq.keyed_group_stable _string
    where
    collect stringNotes = do
        (notes, fingers) <- unzip <$> mapM oneString stringNotes
        return (Seq.merge_lists Guitar.nStart notes, fingers)
    -- All the notes on a single string get a single finger.
    oneString :: (Guitar.String, [Note])
        -> Either Error ([Guitar.Note], Guitar.Finger)
    oneString (string, notes) = do
        let overlaps = findOverlaps notes
        let startDur n = (_start n, _duration n)
        unless (null overlaps) $
            Left $ "overlaps: " <> pretty (map (startDur *** startDur) overlaps)
        let pitch = Signal.merge_segments [(_start n, _pitch n) | n <- notes]
            fingerWeight =
                Signal.merge_segments [(_start n, _finger n) | n <- notes]
        return
            ( filter ((>0) . Guitar.nAmplitude) $ map (makeNote string) notes
            , makeFinger string notes pitch fingerWeight
            )
    makeNote string note = Guitar.Note
        { nStrike = Guitar.Pluck
        , nString = string
        , nStart = RealTime.to_seconds (_start note)
        , nDuration = 0.0013
        , nLocation = _location note
        , nAmplitude = _dynamic note * maxAmp
        }
    makeFinger string notes pitch fingerWeight = Guitar.Finger
        { fString = string
        , fInitial = (0, 0)
        , fMovement = fingerMovement2 string notes pitch fingerWeight
        }

fingerMovement2 :: Guitar.String -> [Note]
    -> Signal.Signal -> Signal.Signal -> [(Seconds, Location, Newtons)]
fingerMovement2 string notes pitch fingerWeight =
    concat $ snd $ List.mapAccumL note (Signal.unsignal pitch)
        (Seq.zip_next notes)
    where
    note pitches (note, nextNote) =
        (here, movement (_start note) nextAttack
            (takeWhile ((<nextAttack) . fst) here))
        where
        here = dropWhile ((<= _start note) . fst) pitches
        nextAttack = maybe 1e20 (subtract prepare . _start) nextNote
    -- For each note attack, move into position and push down the finger over
    -- the 'prepare' time.
    movement attack nextAttack pitches =
        position attack initial
        : [position t nn | (t, nn) <- here]
        where
        initial = pitchAt attack
        -- Extend the last sample since karya assumes it's constant after the
        -- end, but the pitch of multiple notes have been joined.
        -- TODO shouldn't Signal.merge_segments have fixed it?  I guess not,
        -- if I am filtering samples here.  Signal segment refactor can't come
        -- soon enough!
        here = case Seq.viewr pitches of
            Just (ps, (t, p))
                | t < nextAttack -> ps ++ [(t, p), (nextAttack, p)]
                | otherwise -> pitches
            Nothing -> [(nextAttack, initial)]
    position t nn =
        ( max 0 $ RealTime.to_seconds t
        , max 0 $ Guitar.pitchLocation stringNn (Pitch.nn nn)
        , if Pitch.nns_equal (Pitch.nn nn) stringNn
            then 0 else weightAt t * maxAmp
        )
        where stringNn = Guitar.sNn string
    weightAt t = fromMaybe 0 $ Signal.at t fingerWeight
    pitchAt t = fromMaybe 0 $ Signal.at t pitch
    -- Time before the note to move the finger.
    prepare = 0.05

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

-- * unused

-- | This is the old slightly broken version.
fingerMovement1 :: Guitar.String -> [RealTime.RealTime]
    -> Signal.Signal -> Signal.Signal
    -> [(Seconds, Location, Newtons)]
fingerMovement1 string _attacks pitch fingerWeight =
    case Signal.unsignal pitch of
        [] -> []
        ((x0, y0) : pitch) ->
            position x0 y0 0
            : position x0 y0 (weightAt x0)
            : [position t nn (weightAt t) | (t, nn) <- pitch]
    where
    weightAt t = fromMaybe 0 $ Signal.at t fingerWeight
    position t nn amp =
        ( max 0 $ RealTime.to_seconds t - offset
        , max 0 $ Guitar.pitchLocation stringNn (Pitch.nn nn)
        , if Pitch.nns_equal (Pitch.nn nn) stringNn then 0 else amp * maxAmp
        )
        where
        offset = 0.01
        stringNn = Guitar.sNn string

-- | Unused, I rely on karya doing this now.
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
