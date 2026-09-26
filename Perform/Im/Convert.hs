-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert 'Score.Event's to the low-level event format, 'Note.Note'.
module Perform.Im.Convert (write, convert) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Perform.ConvertUtil as ConvertUtil
import qualified Perform.Im.Patch as Patch
import qualified Perform.Signal

import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Types


-- | Serialize the events to the given path.  This is done atomically because
-- this is run from the derive thread, which can be killed at any time.
write :: RealTime -> RealTime -> BlockId
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument) -> FilePath
    -> Vector.Vector Score.Event -> IO ()
write adjust0 playMultiplier blockId lookupInst filename events = do
    notes <- LEvent.writeLogs $ convert blockId lookupInst $
        -- TODO fix sorted Derive.Stream: so I can remove this.
        Lists.sortOn Score.eventStart $
        Vector.toList events
    -- The play multiplier is a speed multiplier, so it's a note time divider.
    Note.serialize filename $ multiplyTime adjust0 (1/playMultiplier) $
        map (uncurry trimControls) $ Lists.zipNexts notes
    pure ()

multiplyTime :: RealTime -> RealTime -> [Note.Note] -> [Note.Note]
multiplyTime adjust0 n
    | n == 1 && adjust0 == 0 = id
    | otherwise = map $ \note -> note
        { Note.start = n * (Note.start note + adjust0)
        , Note.duration = n * Note.duration note
        , Note.controls = Signal.mapX (*n) <$> Note.controls note
        }

-- | Trim signals down to the note ranges.  It's more efficient to serialize
-- fewer samples, and leads to less rerendering since note hashes are based
-- on all their controls, whether or not they're in range.
--
-- Also normalize constant signals to a single sample, fewer bogus breakpoints
-- can make the synthesizer more efficient.
trimControls :: Note.Note -> [Note.Note] -> Note.Note
trimControls note nexts =
    note { Note.controls = trimControl note nexts <$> Note.controls note }

trimControl :: Note.Note -> [Note.Note] -> Signal.Signal -> Signal.Signal
trimControl note nexts sig =
    case Signal.constantValFrom (Note.start note) sig of
        Nothing -> case List.find ((== key note) . key) nexts of
            Nothing -> Signal.dropBefore (Note.start note) sig
            Just next -> Signal.dropAfter (Note.start next) $
                Signal.dropBefore (Note.start note) sig
        Just y -> Signal.constant y
    where
    key n = (Note.instrument n, Note.element n)

convert :: BlockId -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> [LEvent.LEvent Note.Note]
convert blockId = ConvertUtil.convert $ \event resolved ->
    case Cmd.instBackend resolved of
        Cmd.Im patch -> convertEvent blockId event patch patchName
            where InstT.Qualified _ patchName = Cmd.instQualified resolved
        _ -> []

convertEvent :: BlockId -> Score.Event -> Patch.Patch -> InstT.Name
    -> [LEvent.LEvent Note.Note]
convertEvent blockId event patch patchName = run $ do
    let supported = Patch.patchControls patch
    let controls = Score.eventControls event
    pitch <- if Map.member Control.pitch supported
        then Just . convertSignal <$> convertPitch event
        else pure Nothing
    pure $ Note.Note
        { patch = patchName
        , instrument = Score.eventInstrument event
        , trackId = eventTrackId blockId event
        -- To make it easier to set element by hand, I support some types which
        -- are likely to be used for element names.
        , element =
            case Env.lookup EnvKey.element (Score.eventEnviron event) of
                Just (DeriveT.VStr (Expr.Str a)) -> a
                Just val | Just num <- DeriveT.constantVal val ->
                    ShowVal.showVal num
                _ -> ""
        , start = Score.eventStart event
        , duration = Score.eventDuration event
        , controls = maybe id (Map.insert Control.pitch) pitch $
            convertControls supported controls
        -- Restrict attributes to the ones it says it accepts, to protect
        -- against false advertising.
        , attributes = maybe mempty fst $
            Common.lookupAttributes (Score.eventAttributes event)
                (Patch.patchAttributeMap patch)
        , stack = Score.eventStack event
        }

-- | The event TrackId is later used to display render progress.  Progress is
-- only displayed on the root block, so find its tracks.  The innermost track
-- is most likely to be the one with the notes on it.
eventTrackId :: BlockId -> Score.Event -> Maybe TrackId
eventTrackId blockId = Lists.last <=< lookup blockId . Stack.blockTracksOf
    . Score.eventStack

run :: Log.LogId a -> [LEvent.LEvent a]
run action = LEvent.Event note : map LEvent.Log logs
    where (note, logs) = Log.runId action

convertSignal :: Perform.Signal.Signal a -> Signal.Signal
convertSignal = Perform.Signal.coerce

-- TODO trim controls?
convertControls :: Map Control.Control a -> ScoreT.ControlMap
    -> Map Control.Control Signal.Signal
convertControls supported controls = Map.fromList
    [ (toControl c, convertSignal sig)
    | (c, ScoreT.Typed _ sig) <- Map.toList controls
    , Map.member (toControl c) supported
    ]

toControl :: ScoreT.Control -> Control.Control
toControl = Control.Control . ScoreT.controlName

convertPitch :: Log.LogMonad m => Score.Event -> m Perform.Signal.NoteNumber
convertPitch event = do
    let (sig, warns) = Score.nnSignal event
    unless (null warns) $ Log.warn $
        "convert pitch: " <> Text.intercalate ", " (map pretty warns)
    pure sig
