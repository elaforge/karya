-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Instances to serialize and unserialize data types used by Ui.Ui.State.

    Types that I think might change have versions.  If the type changes,
    increment the put_version and add a new branch to the get_version case.

    Generally, the various parts of ADTs are unpacked with explicit type
    signatures.  That way, if one of the types is changed, there will be
    a type error over here pointing at the get/put code that needs to be
    updated.
-}
module Cmd.Serialize (
    allocationsMagic, scoreMagic, viewsMagic
    , isOldSettings
) where
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Util.Lists as Lists
import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import           Util.Serialize
    (Serialize, badEnum, badTag, badVersion, get, getEnum, getTag,
     getVersion, put, putEnum, putTag, putVersion)

import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.Parse.Ky as Ky
import qualified Derive.REnv as REnv
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import           Midi.Instances ()
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Meter.Make as Meter.Make
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import           Global
import           Types


allocationsMagic :: Serialize.Magic UiConfig.Allocations
allocationsMagic = Serialize.Magic 'a' 'l' 'l' 'o'

scoreMagic :: Serialize.Magic Ui.State
scoreMagic = Serialize.Magic 's' 'c' 'o' 'r'

viewsMagic :: Serialize.Magic (Map ViewId Block.View)
viewsMagic = Serialize.Magic 'v' 'i' 'e' 'w'

-- * Serialize instances

instance Serialize Ui.State where
    put (Ui.State a b c d e) = putVersion 6
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- getVersion
        case v of
            6 -> do
                views :: Map Types.ViewId Block.View <- get
                blocks :: Map Types.BlockId Block.Block <- get
                tracks :: Map Types.TrackId Track.Track <- get
                rulers :: Map Types.RulerId Ruler.Ruler <- get
                config :: UiConfig.Config <- get
                pure $ Ui.State views blocks tracks rulers config
            _ -> badVersion "Ui.State" v

instance Serialize UiConfig.Config where
    put (UiConfig.Config ns meta root _insts lilypond defaults
            savedViews ky tscore)
        =  putVersion 15
            >> put ns >> put meta >> put root -- >> put insts
            >> put lilypond
            >> put defaults >> put savedViews >> put ky >> put tscore
    get = getVersion >>= \v -> case v of
        11 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            savedViews :: UiConfig.SavedViews <- get
            kyFile :: Maybe FilePath <- get
            let ky = upgradeTransform transform
                    (maybe "" (\fn -> "import '" <> txt fn <> "'\n") kyFile)
            ky <- either (fail . untxt) pure $ upgradeKy insts ky
            pure $ UiConfig.Config ns meta root insts lilypond defaults
                savedViews ky ""
        12 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            savedViews :: UiConfig.SavedViews <- get
            ky :: Text <- get
            ky <- either (fail . untxt) pure $ upgradeKy insts ky
            pure $ UiConfig.Config ns meta root insts lilypond
                defaults savedViews (upgradeTransform transform ky)
                ""
        13 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            savedViews :: UiConfig.SavedViews <- get
            ky :: Text <- get
            ky <- either (fail . untxt) pure $ upgradeKy insts ky
            pure $ UiConfig.Config ns meta root insts lilypond defaults
                savedViews ky ""
        14 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            savedViews :: UiConfig.SavedViews <- get
            ky :: Text <- get
            tscore :: Text <- get
            ky <- either (fail . untxt) pure $ upgradeKy insts ky
            pure $ UiConfig.Config ns meta root insts lilypond defaults
                savedViews ky tscore
        15 -> do
            -- No more UiConfig.Allocations, stored in ky now.
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            -- _insts :: UiConfig.Allocations <- get
            let insts = mempty
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            savedViews :: UiConfig.SavedViews <- get
            ky :: Text <- get
            tscore :: Text <- get
            pure $ UiConfig.Config ns meta root insts lilypond defaults
                savedViews ky tscore
        _ -> badVersion "UiConfig.Config" v
        where
        upgradeTransform globalTransform ky
            | Text.null (Text.strip globalTransform) = ky
            | otherwise = ky <> "\n\nnote transformer:\nGLOBAL = "
                <> globalTransform

upgradeKy :: UiConfig.Allocations -> Text -> Either Text Text
upgradeKy insts ky = do
    section <- Instruments.unInstruments (const Nothing) insts
    pure $ mconcat
        [ Text.stripEnd $
            Ky.replaceSection Instruments.instrumentSection ("-- "<>) ky
        , "\n\n", Instruments.instrumentSection, ":\n", section
        ]

instance Serialize UiConfig.Allocations where
    put (UiConfig.Allocations a) = putVersion 1 >> put a
    get = do
        v <- getVersion
        case v of
            1 -> do
                configs :: Map ScoreT.Instrument UiConfig.Allocation <- get
                pure $ UiConfig.Allocations configs
            _ -> badVersion "UiConfig.Allocations" v

instance Serialize UiConfig.Allocation where
    put (UiConfig.Allocation a b c) = putVersion 0 >> put a >> put b >> put c
    get = getVersion >>= \case
        0 -> do
            qualified :: InstT.Qualified <- get
            config :: Common.Config <- get
            backend :: UiConfig.Backend <- get
            pure $ UiConfig.Allocation qualified config backend
        v -> badVersion "UiConfig.Allocation" v

instance Serialize UiConfig.Backend where
    put = \case
        UiConfig.Midi a -> putTag 0 >> put a
        UiConfig.Im -> putTag 1
        UiConfig.Dummy a -> putTag 4 >> put a
        UiConfig.Sc -> putTag 3
    get = getTag >>= \case
        0 -> do
            config :: Patch.Config <- get
            pure $ UiConfig.Midi config
        1 -> pure UiConfig.Im
        2 -> pure $ UiConfig.Dummy ""
        3 -> pure UiConfig.Sc
        4 -> UiConfig.Dummy <$> (get :: Serialize.Get Text)
        tag -> badTag "UiConfig.Backend" tag

-- | For backward compatibility.
newtype MidiConfigs = MidiConfigs (Map ScoreT.Instrument Patch.Config)
    deriving (Show)

instance Serialize MidiConfigs where
    put (MidiConfigs a) = putVersion 5 >> put a
    get = getVersion >>= \case
        5 -> do
            insts :: Map ScoreT.Instrument Patch.Config <- get
            pure $ MidiConfigs insts
        v -> badVersion "Patch.MidiConfigs" v

instance Serialize UiConfig.Meta where
    put (UiConfig.Meta a b c d e) = putVersion 5
        >> put a >> put b >> put c >> put d >> put e
    get = getVersion >>= \case
        3 -> do
            creation :: Time.UTCTime <- get
            lastSave :: Time.UTCTime <- get
            _notes :: Text <- get
            midi :: Map BlockId UiConfig.MidiPerformance <- get
            lily :: Map BlockId UiConfig.LilypondPerformance <- get
            pure $ UiConfig.Meta creation lastSave midi lily mempty
        4 -> do
            creation :: Time.UTCTime <- get
            lastSave :: Time.UTCTime <- get
            _notes :: Text <- get
            midi :: Map BlockId UiConfig.MidiPerformance <- get
            lily :: Map BlockId UiConfig.LilypondPerformance <- get
            im :: Map BlockId UiConfig.ImPerformance <- get
            pure $ UiConfig.Meta creation lastSave midi lily im
        -- I dropped metaNotes, I don't think I ever put anything in there.
        5 -> do
            creation :: Time.UTCTime <- get
            lastSave :: Time.UTCTime <- get
            midi :: Map BlockId UiConfig.MidiPerformance <- get
            lily :: Map BlockId UiConfig.LilypondPerformance <- get
            im :: Map BlockId UiConfig.ImPerformance <- get
            pure $ UiConfig.Meta creation lastSave midi lily im
        v -> badVersion "UiConfig.Meta" v

instance Serialize a => Serialize (UiConfig.Performance a) where
    put (UiConfig.Performance a b c) = putVersion 0 >> put a >> put b >> put c
    get = getVersion >>= \case
        0 -> do
            perf :: a <- get
            creation :: Time.UTCTime <- get
            patch :: Text <- get
            pure $ UiConfig.Performance perf creation patch
        v -> badVersion "UiConfig.Performance" v

instance Serialize UiConfig.Default where
    put (UiConfig.Default a) = putVersion 4 >> put a
    get = getVersion >>= \case
        4 -> do
            tempo :: Signal.Y <- get
            pure $ UiConfig.Default tempo
        v -> badVersion "UiConfig.Default" v

-- ** Block

instance Serialize Block.Block where
    -- Config is not serialized because everything in the block config is
    -- either derived from the Cmd.State or is hardcoded.
    -- Except Block.config_skeleton breaks this rule :/
    put (Block.Block a config b c d e f g) = putVersion 14
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
        >> put (Block.configSkeleton config)
    get = getVersion >>= \case
        11 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            meta :: Map Text Text <- get
            pure $ Block.Block title config tracks skel iblock itracks mempty
                meta
        12 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            manual :: Map Block.SourceKey [OldNoteDestination] <- get
            meta :: Map Text Text <- get
            pure $ Block.Block title config tracks skel iblock itracks
                (map upgradeNoteDestination <$> manual) meta
        13 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            dtracks :: Block.ManualDestinations <- get
            meta :: Map Text Text <- get
            pure $ Block.Block title config tracks skel iblock itracks
                dtracks meta
        14 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            dtracks :: Block.ManualDestinations <- get
            meta :: Map Text Text <- get
            skelConfig :: Block.Skeleton <- get
            pure $ Block.Block title
                (config { Block.configSkeleton = skelConfig })
                tracks skel iblock itracks dtracks meta
        v -> badVersion "Block.Block" v
        where
        config = Block.defaultConfig { Block.configSkeleton = Block.Explicit }

instance Serialize Block.Skeleton where
    put = \case
        Block.Explicit -> putTag 0
        Block.Implicit -> putTag 1
    get = getTag >>= \case
        0 -> pure Block.Explicit
        1 -> pure Block.Implicit
        tag -> badTag "Block.Skeleton" tag

instance Serialize Block.TrackDestinations where
    put (Block.DeriveDestinations a) = putTag 2 >> put a
    put (Block.ScoreDestinations a) = putTag 1 >> put a
    get = getTag >>= \case
        0 -> Block.DeriveDestinations . map upgradeNoteDestination <$> get
        1 -> Block.ScoreDestinations <$> get
        2 -> Block.DeriveDestinations <$> get
        tag -> badTag "Block.TrackDestinations" tag

-- | Oops, I forgot to put a version on NoteDestination so of course this
-- happens...
data OldNoteDestination = OldNoteDestination
    (TrackId, Block.EventIndex)
    (Map Text (TrackId, Block.EventIndex))

upgradeNoteDestination :: OldNoteDestination -> Block.NoteDestination
upgradeNoteDestination (OldNoteDestination a b) = Block.NoteDestination "" a b

instance Serialize OldNoteDestination where
    put (OldNoteDestination a b) = put a >> put b
    get = OldNoteDestination <$> get <*> get

instance Serialize Block.NoteDestination where
    put (Block.NoteDestination a b c) = putVersion 0 >> put a >> put b >> put c
    get = getVersion >>= \case
        0 -> do
            key :: Text <- get
            note :: (TrackId, Block.EventIndex) <- get
            controls :: (Map Text (TrackId, Block.EventIndex)) <- get
            pure $ Block.NoteDestination key note controls
        v -> badVersion "Block.Block" v

instance Serialize Block.Track where
    -- The suggested_width is actually derived from fltk, so don't save it.
    -- It's awkward to have it in Block.Track, I have another derived
    -- field like this in Ruler.Ruler's Marklist when there's a Meter.
    put (Block.Track id width _suggested flags merged) = putVersion 3
        >> put id >> put width>> put flags >> put merged
    get = getVersion >>= \case
        3 -> do
            id :: Block.TracklikeId <- get
            width :: Types.Width <- get
            flags :: Set Block.TrackFlag <- get
            merged :: Set Types.TrackId <- get
            pure $ Block.Track id width width flags merged
        v -> badVersion "Block.Track" v

instance Serialize Block.TrackFlag where
    put = putTag . \case
        Block.Collapse -> 0
        Block.Solo -> 1
        Block.Mute -> 2
        Block.Disable -> 3
        Block.Merge -> 4
    get = getTag >>= \case
        0 -> pure Block.Collapse
        1 -> pure Block.Solo
        2 -> pure Block.Mute
        3 -> pure Block.Disable
        4 -> pure Block.Merge
        tag -> badTag "Block.TrackFlag" tag

instance Serialize Block.TracklikeId where
    put (Block.TId a b) = putTag 0 >> put a >> put b
    put (Block.RId a) = putTag 1 >> put a
    put (Block.DId a) = putTag 2 >> put a
    get = do
        tag <- getTag
        case tag of
            0 -> do
                tid :: TrackId <- get
                rid :: RulerId <- get
                pure $ Block.TId tid rid
            1 -> do
                rid :: RulerId <- get
                pure $ Block.RId rid
            2 -> do
                div :: Block.Divider <- get
                pure $ Block.DId div
            _ -> badTag "Block.TracklikeId" tag

instance Serialize Block.Divider where
    put (Block.Divider a) = put a
    get = Block.Divider <$> get

instance Serialize Block.View where
    put (Block.View a b c d e f g) = putVersion 7
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
    get = getVersion >>= \case
        5 -> do
            block :: Types.BlockId <- get
            rect :: Rect.Rect <- get
            trackPadding :: Int <- get
            timePadding :: Int <- get
            status :: Map (Int, Text) Text <- get
            trackScroll :: Types.Width <- get
            zoom :: Zoom.Zoom <- get
            selections :: Map Sel.Num OldSelection <- get
            let padding = Block.Padding trackPadding timePadding 0
            pure $ Block.View block rect padding status trackScroll zoom
                (upgrade <$> selections)
        6 -> do
            block :: Types.BlockId <- get
            rect :: Rect.Rect <- get
            padding :: Block.Padding <- get
            status :: Map (Int, Text) Text <- get
            trackScroll :: Types.Width <- get
            zoom :: Zoom.Zoom <- get
            selections :: Map Sel.Num OldSelection <- get
            pure $ Block.View block rect padding status trackScroll zoom
                (upgrade <$> selections)
        7 -> do
            block :: Types.BlockId <- get
            rect :: Rect.Rect <- get
            padding :: Block.Padding <- get
            status :: Map (Int, Text) Text <- get
            trackScroll :: Types.Width <- get
            zoom :: Zoom.Zoom <- get
            selections :: Map Sel.Num Sel.Selection <- get
            pure $ Block.View block rect padding status trackScroll zoom
                selections
        v -> badVersion "Block.View" v
        where
        upgrade (OldSelection a b c d) = Sel.Selection a b c d Sel.Positive

instance Serialize Block.Padding where
    put (Block.Padding a b c) = putVersion 0
        >> put a >> put b >> put c
    get = getVersion >>= \case
        0 -> do
            left :: Int <- get
            top :: Int <- get
            bottom :: Int <- get
            pure $ Block.Padding left top bottom
        v -> badVersion "Block.Padding" v

instance Serialize Rect.Rect where
    put r = put (Rect.x r) >> put (Rect.y r) >> put (Rect.w r) >> put (Rect.h r)
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        pure (Rect.xywh a b c d)

instance Serialize Zoom.Zoom where
    put (Zoom.Zoom a b) = put a >> put b
    get = do
        offset :: ScoreTime <- get
        factor :: Double <- get
        pure $ Zoom.Zoom offset factor

data OldSelection = OldSelection TrackNum TrackTime TrackNum TrackTime

instance Serialize OldSelection where
    put (OldSelection a b c d) = put a >> put b >> put c >> put d
    get = OldSelection <$> get <*> get <*> get <*> get

instance Serialize Sel.Selection where
    put (Sel.Selection a b c d e) = putVersion 0
        >> put a >> put b >> put c >> put d >> put e
    get = getVersion >>= \case
        0 -> do
            strack :: Int <- get
            stime :: ScoreTime <- get
            ctrack :: Int <- get
            ctime :: ScoreTime <- get
            orient :: Sel.Orientation <- get
            pure $ Sel.Selection strack stime ctrack ctime orient
        v -> badVersion "Sel.Selection" v

instance Serialize Sel.Orientation where
    put = Serialize.putEnumUnsafe
    get = Serialize.getEnumUnsafe

-- ** Types, Color, Font

instance Serialize Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        pure (Color.Color a b c d)

-- ** Ruler

instance Serialize Ruler.Ruler where
    put (Ruler.Ruler marklists b c d) = do
        putVersion 8
        put $ strip <$> marklists
        put b >> put c >> put d
        where
        -- I don't actually need to store the Marklist if Meter is set, becasue
        -- I can regenerate it.
        strip (Just meter, _mlist) = (Just meter, Mark.empty)
        strip (Nothing, mlist) = (Nothing, mlist)
    get = getVersion >>= \case
        6 -> do
            marklists :: Map Ruler.Name (Maybe Text, OldMarklist) <- get
            bg :: Color.Color <- get
            showNames :: Bool <- get
            alignToBottom :: Bool <- get
            pure $ Ruler.Ruler (upgrade <$> marklists) bg showNames
                alignToBottom
            where
            upgrade (_name, OldMarklist mlist) =
                (Nothing, Mark.marklistFromVector mlist)
        7 -> do
            marklists :: OldMarklists <- get
            bg :: Color.Color <- get
            showNames :: Bool <- get
            alignToBottom :: Bool <- get
            pure $ Ruler.Ruler (upgrade marklists) bg showNames
                alignToBottom
            where
            upgrade :: OldMarklists -> Ruler.Marklists
            upgrade = fmap $ \(_config, OldMarklist mlist) ->
                (Nothing, Mark.marklistFromVector mlist)
            -- Upgrade marklists by throwing out the OldMeterConfig.
            -- Alternately, I could try to automatically figure out what the
            -- Meter should be...
        8 -> do
            marklists :: Map Ruler.Name (Maybe Meter.Meter, Mark.Marklist)
                <- get
            bg :: Color.Color <- get
            showNames :: Bool <- get
            alignToBottom :: Bool <- get
            pure $ Ruler.Ruler (add <$> marklists) bg showNames
                alignToBottom
            where
            add (Just meter, _) = (Just meter, Meter.Make.makeMarklist meter)
            add (Nothing, mlist) = (Nothing, mlist)
        v -> badVersion "Ruler.Ruler" v

type OldMarklists = Map Text (Maybe OldMeterConfig, OldMarklist)

instance Serialize OldMeterConfig where
    put (OldMeterConfig a b) = putVersion 0 >> put a >> put b
    get = getVersion >>= \case
        0 -> OldMeterConfig <$> get <*> get
        v -> badVersion "OldMeterConfig" v

-- | Configuration specific to the 'meter' marklist.
data OldMeterConfig = OldMeterConfig
    -- | The type of meter that this marklist represents.  This is looked up in
    -- a table of meter types to figure out how to do transformations on the
    -- meter, since different meters follow different rules.
    { configName :: !Text
    , configStartMeasure :: !Int
    } deriving (Eq, Show)

instance Serialize Meter.Meter where
    put (Meter.Meter a b) = putVersion 0 >> put a >> put b
    get = getVersion >>= \case
        0 -> do
            config :: Meter.Config <- get
            sections :: [Meter.MSection] <- get
            pure $ Meter.Meter config sections
        v -> badVersion "Meter.Meter" v

instance Serialize Meter.MSection where
    put (Meter.MSection a b c) = putVersion 0 >> put a >> put b >> put c
    get = getVersion >>= \case
        0 -> do
            count :: Meter.Measures <- get
            measureDuration :: Meter.Duration <- get
            measure :: Meter.AbstractMeter <- get
            pure $ Meter.MSection count measureDuration measure
        v -> badVersion "Meter.MSection" v

instance Serialize Meter.Config where
    put (Meter.Config a b c d e) = putVersion 0
        >> put a >> put b >> put c >> put d >> put e
    get = getVersion >>= \case
        0 -> do
            labeledRanks :: Set Meter.Rank <- get
            label :: Meter.LabelConfig <- get
            startMeasure :: Meter.Measures <- get
            minDepth :: Int <- get
            stripDepth :: Int <- get
            pure $ Meter.Config labeledRanks label startMeasure minDepth
                stripDepth
        v -> badVersion "Meter.Config" v

instance Serialize Meter.Rank where
    put a = (putVersion 1 >>) $ putEnum $ case a of
        Meter.Section -> 0
        Meter.W -> 1
        Meter.H -> 2
        Meter.Q -> 3
        Meter.E -> 4
        Meter.S -> 5
        Meter.T32 -> 6
        Meter.T64 -> 7
        Meter.T128 -> 8
        Meter.T256 -> 9
    get = getVersion >>= \case
        0 -> Serialize.getEnumUnsafe
        1 -> getEnum >>= \case
            0 -> pure Meter.Section
            1 -> pure Meter.W
            2 -> pure Meter.H
            3 -> pure Meter.Q
            4 -> pure Meter.E
            5 -> pure Meter.S
            6 -> pure Meter.T32
            7 -> pure Meter.T64
            8 -> pure Meter.T128
            9 -> pure Meter.T256
            n -> badEnum "Meter.Rank" n
        v -> badVersion "Meter.Rank" v

instance Serialize Meter.LabelConfig where
    put a = putVersion 0 >> case a of
        Meter.BigNumber a -> putTag 0 >> put a
        Meter.Cycle a -> putTag 1 >> put a
    get = getVersion >>= \case
        0 -> getTag >>= \case
            0 -> Meter.BigNumber <$> get
            1 -> Meter.Cycle <$> get
            t -> badTag "Meter.LabelConfig" t
        v -> badVersion "Meter.LabelConfig" v

instance Serialize Meter.AbstractMeter where
    put = \case
        Meter.T -> putTag 0
        Meter.D ts -> putTag 1 >> put ts
    get = getTag >>= \case
        0 -> pure Meter.T
        1 -> Meter.D <$> get
        tag -> badTag "Meter.AbstractMeter" tag

-- The old version is unversioned
newtype OldMarklist = OldMarklist Mark.MarklistVector
    deriving (Serialize)

instance Serialize Mark.Marklist where
    put mlist = putVersion 0 >> put (Mark.marklistVec mlist)
    get = getVersion >>= \case
        0 -> do
            vec :: Mark.MarklistVector <- get
            pure $ Mark.marklistFromVector vec
        v -> badVersion "Mark.Marklist" v

-- TODO I thought to make a new Mark with typed Mark.Rank, but it's
-- easier to just fromEnum it.
instance Serialize Mark.Mark where
    put (Mark.Mark a b c d e f) =
        put (fromEnum a) >> put b >> put c >> put d >> put e >> put f
    get = do
        rank :: Int <- get
        width :: Int <- get
        color :: Color.Color <- get
        name :: Text <- get
        nameZoom :: Double <- get
        zoom :: Double <- get
        pure $ Mark.Mark (toEnum rank) width color name nameZoom zoom

-- ** Track

instance Serialize Track.Track where
    put (Track.Track a b c d e) = putVersion 5
        >> put a >> put b >> put c >> put d >> put e
    get = getVersion >>= \case
        4 -> do
            title :: Text <- get
            events :: Events.Events <- get
            color :: Color.Color <- get
            render :: Track.RenderConfig <- get
            pure $ Track.Track title events color render True
        5 -> do
            title :: Text <- get
            events :: Events.Events <- get
            color :: Color.Color <- get
            render :: Track.RenderConfig <- get
            waveform :: Bool <- get
            pure $ Track.Track title events color render waveform
        v -> badVersion "Track.Track" v

instance Serialize Track.RenderConfig where
    put (Track.RenderConfig a b) = putVersion 1 >> put a >> put b
    get = getVersion >>= \case
        1 -> do
            style :: Track.RenderStyle <- get
            color :: Color.Color <- get
            pure $ Track.RenderConfig style color
        v -> badVersion "Track.RenderConfig" v

instance Serialize Track.RenderStyle where
    put Track.NoRender = putTag 0
    put (Track.Line a) = putTag 1 >> put a
    put (Track.Filled a) = putTag 2 >> put a
    get = getTag >>= \case
        0 -> pure Track.NoRender
        1 -> do
            source :: Maybe Track.RenderSource <- get
            pure $ Track.Line source
        2 -> do
            source :: Maybe Track.RenderSource <- get
            pure $ Track.Filled source
        tag -> badTag "Track.RenderStyle" tag

instance Serialize Track.RenderSource where
    put (Track.Control a) = putTag 0 >> put a
    put (Track.Pitch a) = do
        putTag 1
        -- It used to be @Maybe ScoreT.Control@ but changed to ScoreT.PControl.
        -- RenderSource isn't versioned so adjust here.
        let c = if a == ScoreT.defaultPitch then Nothing else Just a
        put c
    get = getTag >>= \case
        0 -> do
            control :: ScoreT.Control <- get
            pure $ Track.Control control
        1 -> do
            control :: Maybe ScoreT.PControl <- get
            pure $ Track.Pitch (fromMaybe ScoreT.defaultPitch control)
        tag -> badTag "Track.RenderSource" tag

-- ** Perform.Midi.Patch

instance Serialize Patch.Config where
    put (Patch.Config a b c) = putVersion 11 >> put a >> put b >> put c
    get = getVersion >>= \case
        7 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            scale :: Maybe Patch.Scale <- get
            controlDefaults :: ScoreT.ControlValMap <- get
            let settings = oldSettings
                    { Patch.configScale = scale
                    , Patch.configControlDefaults = nonempty controlDefaults
                    }
            pure $ Patch.Config alloc Nothing settings
        8 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            scale :: Maybe Patch.Scale <- get
            controlDefaults :: ScoreT.ControlValMap <- get
            initialization :: Set Patch.Initialization <- get
            let settings = oldSettings
                    { Patch.configScale = scale
                    , Patch.configControlDefaults = nonempty controlDefaults
                    }
            pure $ Patch.Config alloc (upgradeInitialization initialization)
                settings
        9 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            controlDefaults :: ScoreT.ControlValMap <- get
            initialization :: Set Patch.Initialization <- get
            settings :: Patch.Settings <- get
            pure $ Patch.Config alloc
                (upgradeInitialization initialization)
                (settings
                    { Patch.configControlDefaults = nonempty controlDefaults
                    })
        10 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            initialization :: Set Patch.Initialization <- get
            settings :: Patch.Settings <- get
            pure $ Patch.Config alloc (upgradeInitialization initialization)
                settings
        11 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            initialization :: Maybe Patch.Initialization <- get
            settings :: Patch.Settings <- get
            pure $ Patch.Config alloc initialization settings
        v -> badVersion "Patch.Config" v
        where
        nonempty x = if x == mempty then Nothing else Just x
        upgradeInitialization = Lists.head . Set.toList

-- | This tags Settings which will have to be upgraded by merging with patch
-- defaults.
oldSettings :: Patch.Settings
oldSettings = mempty

isOldSettings :: Patch.Settings -> Bool
isOldSettings =
    (== Patch.configPitchBendRange mempty) . Patch.configPitchBendRange

instance Serialize Patch.Scale where
    put (Patch.Scale a b) = put a >> put b
    get = Patch.Scale <$> get <*> get

instance Serialize Patch.Initialization where
    put a = (putVersion 1 >>) $ putEnum $ case a of
        Patch.Tuning -> 0
        Patch.NrpnTuning -> 1
    get = getVersion >>= \case
        0 -> get >>= \n -> case n :: Int of
            0 -> pure Patch.Tuning
            1 -> pure Patch.NrpnTuning
            n -> fail $ "unknown enum val for Patch.Initialization " <> show n
        1 -> getEnum >>= \case
            0 -> pure Patch.Tuning
            1 -> pure Patch.NrpnTuning
            n -> badEnum "Patch.Initialization" n
        v -> badVersion "Patch.Initialization" v

instance Serialize Patch.Settings where
    put (Patch.Settings a b c d e) = putVersion 2
        >> put a >> put b >> put c >> put d >> put e
    get = getVersion >>= \case
        0 -> do
            flags :: Set OldFlag <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            _pitchBendRange :: Midi.Control.PbRange <- get
            pure $ Patch.Settings
                (if Set.null flags then Nothing else Just (upgrade flags))
                scale decay Nothing Nothing
        1 -> do
            flags :: Maybe (Set OldFlag) <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            pitchBendRange :: Maybe Midi.Control.PbRange <- get
            pure $ Patch.Settings (upgrade <$> flags) scale decay
                pitchBendRange Nothing
        2 -> do
            flags :: Maybe (Set OldFlag) <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            pitchBendRange :: Maybe Midi.Control.PbRange <- get
            controlDefaults :: Maybe ScoreT.ControlValMap <- get
            pure $ Patch.Settings (upgrade <$> flags) scale decay
                pitchBendRange controlDefaults
        v -> badVersion "Patch.Settings" v
        where
        upgrade = Set.fromList . map (\(OldFlag a) -> a) . Set.toList

instance Serialize Patch.Flag where
    put a = (putVersion 0 >>) $ putEnum $ case a of
        Patch.Pressure -> 0
        Patch.HoldKeyswitch -> 1
        Patch.ResumePlay -> 2
        Patch.UseFinalNoteOff -> 3
    get = getVersion >>= \case
        0 -> get >>= \n -> case n :: Int of
            0 -> pure Patch.Pressure
            1 -> pure Patch.HoldKeyswitch
            2 -> pure Patch.ResumePlay
            3 -> pure Patch.UseFinalNoteOff
            n -> fail $ "unknown enum val for Patch.Flag " <> show n
        v -> badVersion "Patch.Initialization" v

newtype OldFlag = OldFlag Patch.Flag
    deriving (Show, Eq, Ord)

instance Serialize OldFlag where
    put _ = error "unimplemented"
    get = get >>= \(tag :: Int) -> OldFlag <$> case tag of
        1 -> pure Patch.Pressure
        2 -> pure Patch.HoldKeyswitch
        3 -> pure Patch.ResumePlay
        4 -> pure Patch.UseFinalNoteOff
        _ -> badTag "Flag" (fromIntegral tag)

-- ** Instrument.Common

instance Serialize Common.Config where
    -- This went from version 1 to 0 because I reverted the Maybe Environ.
    put (Common.Config a b c d) = putVersion 0
        >> put a >> put b >> put c >> put d
    get = getVersion >>= \case
        0 -> do
            environ :: REnv.Environ <- get
            controls :: ScoreT.ControlValMap <- get
            mute :: Bool <- get
            solo :: Bool <- get
            pure $ Common.Config environ controls mute solo
        1 -> do
            environ :: Maybe REnv.Environ <- get
            controls :: ScoreT.ControlValMap <- get
            mute :: Bool <- get
            solo :: Bool <- get
            pure $ Common.Config (fromMaybe mempty environ) controls mute solo
        v -> badVersion "Common.Config" v

instance Serialize Common.Flag where
    put a = (putVersion 1 >>) $ putEnum $ case a of
        Common.Triggered -> 0
    get = getVersion >>= \case
        0 -> get >>= \(t :: Int) -> case t of
            0 -> pure Common.Triggered
            _ -> badTag name (fromIntegral t)
        1 -> getEnum >>= \case
            0 -> pure Common.Triggered
            n -> badEnum name n
        v -> badVersion name v
        where name = "Common.Flag"

-- ** lilypond

instance Serialize Lilypond.Config where
    put (Lilypond.Config a b c d) = putVersion 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- getVersion
        case v of
            3 -> do
                quarter :: RealTime <- get
                quantize :: Lilypond.Duration <- get
                dottedRests :: Bool <- get
                staves :: [(ScoreT.Instrument, Lilypond.StaffConfig)] <- get
                pure $ Lilypond.Config quarter quantize dottedRests staves
            _ -> badVersion "Lilypond.Config" v

instance Serialize Lilypond.StaffConfig where
    put (Lilypond.StaffConfig a b c d e) = putVersion 2
        >> put a >> put b >> put c >> put d >> put e
    get = getVersion >>= \case
        2 -> do
            long :: Lilypond.Instrument <- get
            short :: Lilypond.Instrument <- get
            code :: [Text] <- get
            display :: Bool <- get
            addBass :: Bool <- get
            pure $ Lilypond.StaffConfig long short code display addBass
        v -> badVersion "Lilypond.StaffConfig" v

instance Serialize Lilypond.Duration where
    put = Serialize.putEnumUnsafe
    get = Serialize.getEnumUnsafe
