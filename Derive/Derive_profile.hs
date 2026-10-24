-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Derive_profile where
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Lists as Lists
import qualified Util.Test.Testing as Testing
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Simple as Simple

import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


-- q=90 means 1.5 quarters / sec
-- So 3 8ths, 6 16ths, 12 32nds
-- 32 * 60 * 60 * 2 = 230400 for 2 hours
-- two_hours = 230400
-- time_minutes events = events / 32 / 60
profileTransform32 :: Profile
profileTransform32 = runProfileTransform 32 20000

profileTransform8 :: Profile
profileTransform8 = runProfileTransform 8 20000

profileTransform0 :: Profile
profileTransform0 = runProfileTransform 0 20000

runProfileTransform :: Int -> Int -> Profile
runProfileTransform transformers size =
    runProfileSetup withId "transform" Nothing $ do
        UiTest.exec Ui.empty $ mkblock $
            uninvertedNoteTrack (Texts.join2 " | " inst1 transform) 0 size
    where
    withId = CallTest.withNoteTransformer "id" $ CallTest.transformer $
        \_args deriver -> fmap noop <$> deriver
    noop e = e { Score.eventStart = Score.eventStart e + 1 }
    transform = Text.intercalate " | " (replicate transformers "id")

-- | Make a giant block with simple non-overlapping controls and tempo changes.
profileBigBlock :: Profile
profileBigBlock = deriveProfile "big block" $ mkblock $
    makeTempo size
        : note inst1 0 size ++ note inst1 2 size ++ note inst2 4 size
    where
    note = uninvertedNoteTrack
    size = 2000

uninvertedNoteTrack :: Title -> Int -> Int -> [UiTest.TrackSpec]
uninvertedNoteTrack name offset size =
    map (trackTake size . trackDrop offset)
        [simplePitchTrack, velTrack, noteTrack name]


-- | Block with a control controlling multiple note tracks.  Intended to
-- profile channelize control sharing.
profileShared = deriveProfile "shared" $ makeSharedControl 2000

makeSharedControl size =
    mkblock $ makeTempo size
        : trackTake size modTrack
        : trackSet 0 ++ trackSet 2 -- ++ track_set 4
    where
    trackSet offset = map (trackTake size . trackDrop offset)
        [ simplePitchTrack
        , velTrack
        , noteTrack inst1
        ]

-- | Block with non-tempered scale so pitches can't share.  Intended to profile
-- channelize pitch sharing.
profileNontempered = deriveProfile "nontempered" $ makeNontempered 1000
    where
    makeNontempered size = mkblock $ map (trackUntil size)
        [ makeTempo (floor (ScoreTime.toDouble size))
        , nontemperedPitchTrack
        , velTrack
        , noteTrack inst1
        ]

-- | Giant control track with lots of samples.  Intended to profile control
-- track derivation.
profileControl = deriveProfile "control" $ makeBigControl 15000

makeBigControl size = mkblock $ map (trackUntil size)
    [ modTrack
    , (inst1, [(0, size, "")])
    ]

profileNestedSimple =
    deriveProfile "nested simple" $ makeNestedNotes 10 3 60
profileNestedControls =
    deriveProfile "nested controls" $ makeNestedControls 10 3 60
-- Just plain notes, no controls, so laziness works.
profileNestedNocontrol = deriveProfile "nested nocontrol" $
    makeNested [noteTrack inst1] 10 3 60

-- Profile with some bogus busy waits in there to make things clearer on the
-- heap profile.
profileSize :: Profile
profileSize = deriveSize $ makeNestedControls 10 3 60

-- profile_bloom_derive = derive_saved False "data/bloom"
-- profile_bloom_perform = derive_saved True "data/bloom"
-- profile_pnovla_derive = derive_saved False "data/pnovla"
-- profile_pnovla_perform = derive_saved True "data/pnovla"
-- profile_viola_sonata_perform = derive_saved True "data/viola-sonata"

profileBloom = profileFile "data/bloom"
profilePnovla = profileFile "data/pnovla"
profileViolaSonata = profileFile "data/viola-sonata"

profileFile :: FilePath -> Profile
profileFile fname = do
    cmdConfig <- DeriveSaved.loadCmdConfig
    DeriveSaved.performFile cmdConfig fname
    pure ()


-- * make states

makeNestedNotes :: Ui.M m => Int -> Int -> Int -> m ()
makeNestedNotes = makeNested [simplePitchTrack, noteTrack inst1]

makeNestedControls :: Ui.M m => Int -> Int -> Int -> m ()
makeNestedControls = makeNested
    [ simplePitchTrack
    , modTrack
    , noteTrack inst1
    ]

-- | Try to generate a "normal" score.  This means a lightly nested structure,
-- with the terminal blocks having a more events than the intermediate ones.
--
-- This doesn't produce intermediate tempo or control curves.
makeNested :: Ui.M m => [UiTest.TrackSpec] -> Int -> Int -> Int -> m ()
makeNested bottomTracks size depth bottomSize = do
    rulerId <- Create.ruler "ruler" UiTest.defaultRuler
    go rulerId UiTest.defaultBlockName depth
    Ui.modify setAllocations
    where
    go rulerId blockName 1 = do
        UiTest.mkblockRulerId rulerId (UiTest.bid blockName) ""
            (map (trackTake bottomSize) bottomTracks)
        pure ()
    go rulerId blockName depth = do
        let subBids = [blockName <> "." <> showt sub | sub <- [0 .. size-1]]
            step = bottomSize * size^(depth-2)
        UiTest.mkblockRulerId rulerId (UiTest.bid blockName) ""
            (map (trackTake size) [ctrack (fromIntegral step) inst1 subBids])
        forM_ subBids $ \sub -> go rulerId sub (depth-1)

mkblock :: Ui.M m => [UiTest.TrackSpec] -> m ()
mkblock tracks = do
    UiTest.mkblock tracks
    tracks <- TrackTree.tracksOf UiTest.defaultBlockId
    -- Track slicing makes things much slower.  I should profile that too, but
    -- let's profile without it first.
    Ui.setSkeleton UiTest.defaultBlockId $ ParseSkeleton.noteBottomParser
        [ ParseSkeleton.Track (Ui.trackTracknum t) (Ui.trackTitle t)
        | t <- tracks
        ]
    Ui.modify setAllocations

-- * implementation

deriveSaved :: Bool -> FilePath -> Profile
deriveSaved withPerform fname = do
    cmdConfig <- DeriveSaved.loadCmdConfig
    (uiState, cmdState) <- either errorIO pure
        =<< DeriveSaved.loadScore cmdConfig fname
    blockId <- maybe (errorIO $ txt fname <> ": no root block") pure $
        Ui.config#UiConfig.root #$ uiState
    ((events, logs), _cpu) <-
        DeriveSaved.timedDerive fname uiState cmdState blockId
    mapM_ Log.write logs
    when withPerform $ do
        ((_msgs, logs), _cpu) <- DeriveSaved.timedPerform cmdState
            ("perform " ++ fname) uiState events
        mapM_ Log.write logs

deriveSize :: Ui.StateId a -> Profile
deriveSize create = do
    Thread.printTimer "force mmsgs" (const "done") (Thread.force mmsgs)
    Thread.printTimer "gc" (const "") Mem.performGC
    -- This puts a gap in the heap graph so I can tell which each phase begins.
    -- Surely there is a less ridiculous way to do this.
    -- TODO yes, since 8.2 ghc puts heap profile in the eventlog, but then
    -- of course h2ps doesn't work.
    Thread.printTimer "busy" id $ pure $ show
        (busyWait 100000000)
    Thread.printTimer "length" id $ pure $ show (length mmsgs)
    pure ()
    where
    uiState = UiTest.exec Ui.empty create
    result = DeriveTest.deriveBlock uiState UiTest.defaultBlockId
    mmsgs = snd $ DeriveTest.performStream DeriveTest.defaultConvertLookup
        (Ui.config#UiConfig.allocations #$ uiState)
        (Derive.rEvents result)

busyWait :: Int -> Double
busyWait ops = go ops 2
    where
    go 0 accum = accum
    go n accum
        | even n = go (n-1) (accum**2)
        | otherwise = go (n-1) (sqrt accum)

-- | This runs the derivation several times to minimize the creation cost.
deriveProfile :: String -> Ui.StateId a -> Profile
deriveProfile name create = replicateM_ 6 $
    runProfile name Nothing (UiTest.exec Ui.empty create)

runProfile :: String
    -> Maybe DeriveTest.Lookup -- ^ If given, also run a perform.
    -> Ui.State -> Profile
runProfile = runProfileSetup mempty

runProfileSetup :: DeriveTest.Setup -> String
    -> Maybe DeriveTest.Lookup -- ^ If given, also run a perform.
    -> Ui.State -> Profile
runProfileSetup setup fname maybeLookup uiState = do
    blockId <- maybe (errorIO $ txt fname <> ": no root block") pure $
        Ui.config#UiConfig.root #$ uiState
    startCpu <- Thread.currentCpu
    let section = timeSection startCpu
    let result = DeriveTest.deriveBlockSetup setup uiState blockId
    let events = Stream.toList $ Derive.rEvents result
    section "derive" $ do
        Thread.force events
        Testing.prettyp (take 4 events)
        pure events
    whenJust maybeLookup $ \lookup -> section "midi" $ do
        let mmsgs = snd $ DeriveTest.performStream lookup
                (Ui.config#UiConfig.allocations #$ uiState)
                (Stream.fromSortedList events)
        Thread.force mmsgs
        pure mmsgs

timeSection :: Thread.Seconds -> String -> IO [a] -> IO ()
timeSection startCpu title op = do
    putStr $ "--> " ++ title ++ ": "
    IO.hFlush IO.stdout
    (vals, metric) <- Thread.timeAction op
    curCpu <- Thread.currentCpu
    let len = length vals
    let cpuSecs = Thread.metricCpu metric
    Printf.printf "%d vals -> %.2f (%.2f / sec) (running: %.2f)\n"
        len (secs cpuSecs) (fromIntegral len / secs cpuSecs)
        (secs (curCpu - startCpu))
    where
    secs :: Thread.Seconds -> Double
    secs = realToFrac


-- * state building

type Title = Text

inst1, inst2 :: Title
inst1 = ">i1"
inst2 = ">i2"

setAllocations :: Ui.State -> Ui.State
setAllocations = Ui.config#UiConfig.allocations #= allocs
    where
    allocs = Simple.allocations
        [ ("i1", ("s/1", dev [0, 1]))
        , ("i2", ("s/2", dev [2]))
        ]
        where dev = Simple.Midi . map ("wdev",)

noteTrack :: Title -> UiTest.TrackSpec
noteTrack inst = ctrack 1 inst [""]

makeTempo :: Int -> UiTest.TrackSpec
makeTempo n = trackTake (ceiling (fromIntegral n / 10)) $
    ctrack0 10 "tempo" ["1", "2", "3", "i 1"]

modTrack :: UiTest.TrackSpec
modTrack = ctrack0 1 "cc1 | srate = 50" ["1", "i 0", "e 1", "i .5", "i 0"]

simplePitchTrack :: UiTest.TrackSpec
simplePitchTrack =
    ctrack0 1 "*twelve" ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]

nontemperedPitchTrack :: UiTest.TrackSpec
nontemperedPitchTrack =
    ctrack0 1 "*legong" ["4i", "4o", "4e", "4u", "4a", "5i", "5o"]

velTrack :: UiTest.TrackSpec
velTrack = ctrack0 1 "vel" ["1", ".2", ".4", ".6"]

ctrack0 :: ScoreTime -> Title -> [Text] -> UiTest.TrackSpec
ctrack0 step title ts =
    (title, [(p, 0, t) | (p, t) <- zip (Lists.range_ 0 step) (cycle ts)])

ctrack :: ScoreTime -> Title -> [Text] -> UiTest.TrackSpec
ctrack step title ts =
    (title, [(p, step, t) | (p, t) <- zip (Lists.range_ 0 step) (cycle ts)])

trackUntil :: ScoreTime -> UiTest.TrackSpec -> UiTest.TrackSpec
trackUntil until = second (takeWhile (\(p, _, _) -> p < until))

trackDrop n = second (shift . drop n)
    where
    shift [] = []
    shift ((p, d, t) : rest) =
        (0, d, t) : map (\(p', d', t') -> (p' - p, d', t')) rest

trackTake :: Int -> UiTest.TrackSpec -> UiTest.TrackSpec
trackTake = second . take
