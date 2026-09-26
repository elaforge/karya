-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.PerformTest where
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified App.LoadInstruments as LoadInstruments
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Simple as Simple
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Types

import           Global
import           Types



-- | Dump perform events to a Readable format so they can be pasted into
-- a test.  This is analogous to 'Ui.UiTest.to_spec'.
dumpPerfEvents :: FilePath -> [Types.Event] -> IO ()
dumpPerfEvents fname events =
    IO.writeFile fname $ PPrint.pshow (map Simple.dumpExactPerfEvent events)

readPerfEvents :: [Simple.ExactPerfEvent] -> IO [Types.Event]
readPerfEvents events = do
    db <- LoadInstruments.load =<< Path.getAppDir
    pure $ mapMaybe (Simple.loadExactPerfEvent (lookupPatch db)) events

lookupPatch :: Cmd.InstrumentDb -> InstT.Qualified -> Maybe Types.Patch
lookupPatch db qualified = do
    patch <- Inst.instMidi =<< Inst.lookup qualified db
    let scoreInst = ScoreT.Instrument (InstT.showQualified qualified)
    pure $ Types.patchFromSettings scoreInst mempty patch

emptyEvent :: Types.Event
emptyEvent = Types.Event
    { eventStart = 0
    , eventDuration = 0
    , eventPatch = patch1
    , eventControls = mempty
    , eventPitch = mempty
    , eventStartVelocity = Perform.defaultVelocity
    , eventEndVelocity = Perform.defaultVelocity
    , eventStack = DeriveTest.fakeStack
    }

patch1, patch2 :: Types.Patch
patch1 = mkpatch "patch1"
patch2 = mkpatch "patch2"

mkpatch :: Text -> Types.Patch
mkpatch name =
    Types.patchFromSettings (ScoreT.Instrument name)
        (Patch.patchDefaults patch) patch
    where patch = Patch.patch (-1, 1) name


-- * extract

type Extracted a = (Text, RealTime, a)

extractMsg :: (Midi.Message -> Maybe a) -> [Midi.WriteMessage] -> [a]
extractMsg f = map (\(_, _, a) -> a) . extract f

extractMsgTs :: (Midi.Message -> Maybe a) -> [Midi.WriteMessage]
    -> [(RealTime, a)]
extractMsgTs f = map (\(_, ts, a) -> (ts, a)) . extract f

extract :: (Midi.Message -> Maybe a) -> [Midi.WriteMessage]
    -> [Extracted a]
extract e wmsgs =
    [ (dev, ts, a)
    | (dev, ts, msg) <- extractMidi wmsgs, Just a <- [e msg]
    ]

extractMidi :: [Midi.WriteMessage] -> [Extracted Midi.Message]
extractMidi wmsgs =
    [ (Midi.writeDeviceText dev, ts, msg)
    | Midi.WriteMessage dev ts msg <- wmsgs
    ]

eNoteOnOff :: Midi.Message -> Maybe (Midi.Channel, Bool, Midi.Key)
eNoteOnOff msg = case msg of
    Midi.ChannelMessage chan (Midi.NoteOn key _) -> Just (chan, True, key)
    Midi.ChannelMessage chan (Midi.NoteOff key _) -> Just (chan, False, key)
    _ -> Nothing

eChanMsg :: Midi.Message -> Maybe (Midi.Channel, Midi.ChannelMessage)
eChanMsg (Midi.ChannelMessage chan msg) = Just (chan, msg)
eChanMsg _ = Nothing

eCmsg :: Midi.Message -> Maybe Midi.ChannelMessage
eCmsg = fmap snd . eChanMsg

eCc :: Midi.Control -> Midi.Message -> Maybe Midi.ControlValue
eCc cc (Midi.ChannelMessage _ (Midi.ControlChange msgCc val))
    | cc == msgCc = Just val
eCc _ _ = Nothing

ePitchbend :: Midi.Message -> Maybe Midi.PitchBendValue
ePitchbend (Midi.ChannelMessage _ (Midi.PitchBend n)) = Just n
ePitchbend _ = Nothing
