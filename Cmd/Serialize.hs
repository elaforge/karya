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
    allocations_magic, score_magic, views_magic
    , is_old_settings
) where
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Util.Lists as Lists
import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import           Util.Serialize
    (Serialize, bad_enum, bad_tag, bad_version, get, get_enum, get_tag,
     get_version, put, put_enum, put_tag, put_version)

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


allocations_magic :: Serialize.Magic UiConfig.Allocations
allocations_magic = Serialize.Magic 'a' 'l' 'l' 'o'

score_magic :: Serialize.Magic Ui.State
score_magic = Serialize.Magic 's' 'c' 'o' 'r'

views_magic :: Serialize.Magic (Map ViewId Block.View)
views_magic = Serialize.Magic 'v' 'i' 'e' 'w'

-- * Serialize instances

instance Serialize Ui.State where
    put (Ui.State a b c d e) = put_version 6
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            6 -> do
                views :: Map Types.ViewId Block.View <- get
                blocks :: Map Types.BlockId Block.Block <- get
                tracks :: Map Types.TrackId Track.Track <- get
                rulers :: Map Types.RulerId Ruler.Ruler <- get
                config :: UiConfig.Config <- get
                return $ Ui.State views blocks tracks rulers config
            _ -> bad_version "Ui.State" v

instance Serialize UiConfig.Config where
    put (UiConfig.Config ns meta root allocs lilypond defaults
            saved_views ky tscore)
        =  put_version 14
            >> put ns >> put meta >> put root >> put allocs >> put lilypond
            >> put defaults >> put saved_views >> put ky >> put tscore
    get = get_version >>= \v -> case v of
        11 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            saved_views :: UiConfig.SavedViews <- get
            ky_file :: Maybe FilePath <- get
            return $ UiConfig.Config ns meta root insts lilypond defaults
                saved_views
                (upgrade_transform transform
                    (maybe "" (\fn -> "import '" <> txt fn <> "'\n") ky_file))
                ""
        12 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            saved_views :: UiConfig.SavedViews <- get
            ky :: Text <- get
            return $ UiConfig.Config ns meta root insts lilypond
                defaults saved_views (upgrade_transform transform ky)
                ""
        13 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            saved_views :: UiConfig.SavedViews <- get
            ky :: Text <- get
            return $ UiConfig.Config ns meta root insts lilypond defaults
                saved_views ky ""
        14 -> do
            ns :: Id.Namespace <- get
            meta :: UiConfig.Meta <- get
            root :: Maybe BlockId <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: UiConfig.Default <- get
            saved_views :: UiConfig.SavedViews <- get
            ky :: Text <- get
            tscore :: Text <- get
            return $ UiConfig.Config ns meta root insts lilypond defaults
                saved_views ky tscore
        _ -> bad_version "UiConfig.Config" v
        where
        upgrade_transform global_transform ky
            | Text.null (Text.strip global_transform) = ky
            | otherwise = ky <> "\n\nnote transformer:\nGLOBAL = "
                <> global_transform

instance Serialize UiConfig.Allocations where
    put (UiConfig.Allocations a) = put_version 1 >> put a
    get = do
        v <- get_version
        case v of
            1 -> do
                configs :: Map ScoreT.Instrument UiConfig.Allocation <- get
                return $ UiConfig.Allocations configs
            _ -> bad_version "UiConfig.Allocations" v

instance Serialize UiConfig.Allocation where
    put (UiConfig.Allocation a b c) = put_version 0 >> put a >> put b >> put c
    get = get_version >>= \case
        0 -> do
            qualified :: InstT.Qualified <- get
            config :: Common.Config <- get
            backend :: UiConfig.Backend <- get
            return $ UiConfig.Allocation qualified config backend
        v -> bad_version "UiConfig.Allocation" v

instance Serialize UiConfig.Backend where
    put = \case
        UiConfig.Midi a -> put_tag 0 >> put a
        UiConfig.Im -> put_tag 1
        UiConfig.Dummy a -> put_tag 4 >> put a
        UiConfig.Sc -> put_tag 3
    get = get_tag >>= \case
        0 -> do
            config :: Patch.Config <- get
            return $ UiConfig.Midi config
        1 -> return UiConfig.Im
        2 -> return $ UiConfig.Dummy ""
        3 -> return UiConfig.Sc
        4 -> UiConfig.Dummy <$> (get :: Serialize.Get Text)
        tag -> bad_tag "UiConfig.Backend" tag

-- | For backward compatibility.
newtype MidiConfigs = MidiConfigs (Map ScoreT.Instrument Patch.Config)
    deriving (Show)

instance Serialize MidiConfigs where
    put (MidiConfigs a) = put_version 5 >> put a
    get = get_version >>= \case
        5 -> do
            insts :: Map ScoreT.Instrument Patch.Config <- get
            return $ MidiConfigs insts
        v -> bad_version "Patch.MidiConfigs" v

instance Serialize UiConfig.Meta where
    put (UiConfig.Meta a b c d e f) = put_version 4
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = get_version >>= \case
        3 -> do
            creation :: Time.UTCTime <- get
            last_save :: Time.UTCTime <- get
            notes :: Text <- get
            midi :: Map BlockId UiConfig.MidiPerformance <- get
            lily :: Map BlockId UiConfig.LilypondPerformance <- get
            return $ UiConfig.Meta creation last_save notes midi lily mempty
        4 -> do
            creation :: Time.UTCTime <- get
            last_save :: Time.UTCTime <- get
            notes :: Text <- get
            midi :: Map BlockId UiConfig.MidiPerformance <- get
            lily :: Map BlockId UiConfig.LilypondPerformance <- get
            im :: Map BlockId UiConfig.ImPerformance <- get
            return $ UiConfig.Meta creation last_save notes midi lily im
        v -> bad_version "UiConfig.Meta" v

instance Serialize a => Serialize (UiConfig.Performance a) where
    put (UiConfig.Performance a b c) = put_version 0 >> put a >> put b >> put c
    get = get_version >>= \case
        0 -> do
            perf :: a <- get
            creation :: Time.UTCTime <- get
            patch :: Text <- get
            return $ UiConfig.Performance perf creation patch
        v -> bad_version "UiConfig.Performance" v

instance Serialize UiConfig.Default where
    put (UiConfig.Default a) = put_version 4 >> put a
    get = get_version >>= \case
        4 -> do
            tempo :: Signal.Y <- get
            return $ UiConfig.Default tempo
        v -> bad_version "UiConfig.Default" v

-- ** Block

instance Serialize Block.Block where
    -- Config is not serialized because everything in the block config is
    -- either derived from the Cmd.State or is hardcoded.
    -- Except Block.config_skeleton breaks this rule :/
    put (Block.Block a config b c d e f g) = put_version 14
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
        >> put (Block.config_skeleton config)
    get = get_version >>= \case
        11 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            meta :: Map Text Text <- get
            return $ Block.Block title config tracks skel iblock itracks mempty
                meta
        12 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            manual :: Map Block.SourceKey [OldNoteDestination] <- get
            meta :: Map Text Text <- get
            return $ Block.Block title config tracks skel iblock itracks
                (map upgrade_note_destination <$> manual) meta
        13 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            dtracks :: Block.ManualDestinations <- get
            meta :: Map Text Text <- get
            return $ Block.Block title config tracks skel iblock itracks
                dtracks meta
        14 -> do
            title :: Text <- get
            tracks :: [Block.Track] <- get
            skel :: Skeleton.Skeleton <- get
            iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
            itracks :: [(TrackId, Block.TrackDestinations)] <- get
            dtracks :: Block.ManualDestinations <- get
            meta :: Map Text Text <- get
            skel_config :: Block.Skeleton <- get
            return $ Block.Block title
                (config { Block.config_skeleton = skel_config })
                tracks skel iblock itracks dtracks meta
        v -> bad_version "Block.Block" v
        where
        config = Block.default_config { Block.config_skeleton = Block.Explicit }

instance Serialize Block.Skeleton where
    put = \case
        Block.Explicit -> put_tag 0
        Block.Implicit -> put_tag 1
    get = get_tag >>= \case
        0 -> pure Block.Explicit
        1 -> pure Block.Implicit
        tag -> bad_tag "Block.Skeleton" tag

instance Serialize Block.TrackDestinations where
    put (Block.DeriveDestinations a) = put_tag 2 >> put a
    put (Block.ScoreDestinations a) = put_tag 1 >> put a
    get = get_tag >>= \case
        0 -> Block.DeriveDestinations . map upgrade_note_destination <$> get
        1 -> Block.ScoreDestinations <$> get
        2 -> Block.DeriveDestinations <$> get
        tag -> bad_tag "Block.TrackDestinations" tag

-- | Oops, I forgot to put a version on NoteDestination so of course this
-- happens...
data OldNoteDestination = OldNoteDestination
    (TrackId, Block.EventIndex)
    (Map Text (TrackId, Block.EventIndex))

upgrade_note_destination :: OldNoteDestination -> Block.NoteDestination
upgrade_note_destination (OldNoteDestination a b) = Block.NoteDestination "" a b

instance Serialize OldNoteDestination where
    put (OldNoteDestination a b) = put a >> put b
    get = OldNoteDestination <$> get <*> get

instance Serialize Block.NoteDestination where
    put (Block.NoteDestination a b c) = put_version 0 >> put a >> put b >> put c
    get = get_version >>= \case
        0 -> do
            key :: Text <- get
            note :: (TrackId, Block.EventIndex) <- get
            controls :: (Map Text (TrackId, Block.EventIndex)) <- get
            return $ Block.NoteDestination key note controls
        v -> bad_version "Block.Block" v

instance Serialize Block.Track where
    -- The suggested_width is actually derived from fltk, so don't save it.
    -- It's awkward to have it in Block.Track, I have another derived
    -- field like this in Ruler.Ruler's Marklist when there's a Meter.
    put (Block.Track id width _suggested flags merged) = put_version 3
        >> put id >> put width>> put flags >> put merged
    get = get_version >>= \case
        3 -> do
            id :: Block.TracklikeId <- get
            width :: Types.Width <- get
            flags :: Set Block.TrackFlag <- get
            merged :: Set Types.TrackId <- get
            return $ Block.Track id width width flags merged
        v -> bad_version "Block.Track" v

instance Serialize Block.TrackFlag where
    put = put_tag . \case
        Block.Collapse -> 0
        Block.Solo -> 1
        Block.Mute -> 2
        Block.Disable -> 3
        Block.Merge -> 4
    get = get_tag >>= \case
        0 -> return Block.Collapse
        1 -> return Block.Solo
        2 -> return Block.Mute
        3 -> return Block.Disable
        4 -> return Block.Merge
        tag -> bad_tag "Block.TrackFlag" tag

instance Serialize Block.TracklikeId where
    put (Block.TId a b) = put_tag 0 >> put a >> put b
    put (Block.RId a) = put_tag 1 >> put a
    put (Block.DId a) = put_tag 2 >> put a
    get = do
        tag <- get_tag
        case tag of
            0 -> do
                tid :: TrackId <- get
                rid :: RulerId <- get
                return $ Block.TId tid rid
            1 -> do
                rid :: RulerId <- get
                return $ Block.RId rid
            2 -> do
                div :: Block.Divider <- get
                return $ Block.DId div
            _ -> bad_tag "Block.TracklikeId" tag

instance Serialize Block.Divider where
    put (Block.Divider a) = put a
    get = Block.Divider <$> get

instance Serialize Block.View where
    put (Block.View a b c d e f g) = put_version 7
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
    get = get_version >>= \case
        5 -> do
            block :: Types.BlockId <- get
            rect :: Rect.Rect <- get
            track_padding :: Int <- get
            time_padding :: Int <- get
            status :: Map (Int, Text) Text <- get
            track_scroll :: Types.Width <- get
            zoom :: Zoom.Zoom <- get
            selections :: Map Sel.Num OldSelection <- get
            let padding = Block.Padding track_padding time_padding 0
            return $ Block.View block rect padding status track_scroll zoom
                (upgrade <$> selections)
        6 -> do
            block :: Types.BlockId <- get
            rect :: Rect.Rect <- get
            padding :: Block.Padding <- get
            status :: Map (Int, Text) Text <- get
            track_scroll :: Types.Width <- get
            zoom :: Zoom.Zoom <- get
            selections :: Map Sel.Num OldSelection <- get
            return $ Block.View block rect padding status track_scroll zoom
                (upgrade <$> selections)
        7 -> do
            block :: Types.BlockId <- get
            rect :: Rect.Rect <- get
            padding :: Block.Padding <- get
            status :: Map (Int, Text) Text <- get
            track_scroll :: Types.Width <- get
            zoom :: Zoom.Zoom <- get
            selections :: Map Sel.Num Sel.Selection <- get
            return $ Block.View block rect padding status track_scroll zoom
                selections
        v -> bad_version "Block.View" v
        where
        upgrade (OldSelection a b c d) = Sel.Selection a b c d Sel.Positive

instance Serialize Block.Padding where
    put (Block.Padding a b c) = put_version 0
        >> put a >> put b >> put c
    get = get_version >>= \case
        0 -> do
            left :: Int <- get
            top :: Int <- get
            bottom :: Int <- get
            return $ Block.Padding left top bottom
        v -> bad_version "Block.Padding" v

instance Serialize Rect.Rect where
    put r = put (Rect.x r) >> put (Rect.y r) >> put (Rect.w r) >> put (Rect.h r)
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Rect.xywh a b c d)

instance Serialize Zoom.Zoom where
    put (Zoom.Zoom a b) = put a >> put b
    get = do
        offset :: ScoreTime <- get
        factor :: Double <- get
        return $ Zoom.Zoom offset factor

data OldSelection = OldSelection TrackNum TrackTime TrackNum TrackTime

instance Serialize OldSelection where
    put (OldSelection a b c d) = put a >> put b >> put c >> put d
    get = OldSelection <$> get <*> get <*> get <*> get

instance Serialize Sel.Selection where
    put (Sel.Selection a b c d e) = put_version 0
        >> put a >> put b >> put c >> put d >> put e
    get = get_version >>= \case
        0 -> do
            strack :: Int <- get
            stime :: ScoreTime <- get
            ctrack :: Int <- get
            ctime :: ScoreTime <- get
            orient :: Sel.Orientation <- get
            return $ Sel.Selection strack stime ctrack ctime orient
        v -> bad_version "Sel.Selection" v

instance Serialize Sel.Orientation where
    put = Serialize.put_enum_unsafe
    get = Serialize.get_enum_unsafe

-- ** Types, Color, Font

instance Serialize Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

-- ** Ruler

instance Serialize Ruler.Ruler where
    put (Ruler.Ruler marklists b c d) = do
        put_version 8
        put $ strip <$> marklists
        put b >> put c >> put d
        where
        -- I don't actually need to store the Marklist if Meter is set, becasue
        -- I can regenerate it.
        strip (Just meter, _mlist) = (Just meter, Mark.empty)
        strip (Nothing, mlist) = (Nothing, mlist)
    get = get_version >>= \case
        6 -> do
            marklists :: Map Ruler.Name (Maybe Text, OldMarklist) <- get
            bg :: Color.Color <- get
            show_names :: Bool <- get
            align_to_bottom :: Bool <- get
            return $ Ruler.Ruler (upgrade <$> marklists) bg show_names
                align_to_bottom
            where
            upgrade (_name, OldMarklist mlist) =
                (Nothing, Mark.marklist_from_vector mlist)
        7 -> do
            marklists :: OldMarklists <- get
            bg :: Color.Color <- get
            show_names :: Bool <- get
            align_to_bottom :: Bool <- get
            return $ Ruler.Ruler (upgrade marklists) bg show_names
                align_to_bottom
            where
            upgrade :: OldMarklists -> Ruler.Marklists
            upgrade = fmap $ \(_config, OldMarklist mlist) ->
                (Nothing, Mark.marklist_from_vector mlist)
            -- Upgrade marklists by throwing out the OldMeterConfig.
            -- Alternately, I could try to automatically figure out what the
            -- Meter should be...
        8 -> do
            marklists :: Map Ruler.Name (Maybe Meter.Meter, Mark.Marklist)
                <- get
            bg :: Color.Color <- get
            show_names :: Bool <- get
            align_to_bottom :: Bool <- get
            return $ Ruler.Ruler (add <$> marklists) bg show_names
                align_to_bottom
            where
            add (Just meter, _) = (Just meter, Meter.Make.make_marklist meter)
            add (Nothing, mlist) = (Nothing, mlist)
        v -> bad_version "Ruler.Ruler" v

type OldMarklists = Map Text (Maybe OldMeterConfig, OldMarklist)

instance Serialize OldMeterConfig where
    put (OldMeterConfig a b) = put_version 0 >> put a >> put b
    get = get_version >>= \case
        0 -> OldMeterConfig <$> get <*> get
        v -> bad_version "OldMeterConfig" v

-- | Configuration specific to the 'meter' marklist.
data OldMeterConfig = OldMeterConfig {
    -- | The type of meter that this marklist represents.  This is looked up in
    -- a table of meter types to figure out how to do transformations on the
    -- meter, since different meters follow different rules.
    config_name :: !Text
    , config_start_measure :: !Int
    } deriving (Eq, Show)

instance Serialize Meter.Meter where
    put (Meter.Meter a b) = put_version 0 >> put a >> put b
    get = get_version >>= \case
        0 -> do
            config :: Meter.Config <- get
            sections :: [Meter.MSection] <- get
            return $ Meter.Meter config sections
        v -> bad_version "Meter.Meter" v

instance Serialize Meter.MSection where
    put (Meter.MSection a b c) = put_version 0 >> put a >> put b >> put c
    get = get_version >>= \case
        0 -> do
            count :: Meter.Measures <- get
            measure_duration :: Meter.Duration <- get
            measure :: Meter.AbstractMeter <- get
            return $ Meter.MSection count measure_duration measure
        v -> bad_version "Meter.MSection" v

instance Serialize Meter.Config where
    put (Meter.Config a b c d e) = put_version 0
        >> put a >> put b >> put c >> put d >> put e
    get = get_version >>= \case
        0 -> do
            labeled_ranks :: Set Meter.Rank <- get
            label :: Meter.LabelConfig <- get
            start_measure :: Meter.Measures <- get
            min_depth :: Int <- get
            strip_depth :: Int <- get
            return $ Meter.Config labeled_ranks label start_measure min_depth
                strip_depth
        v -> bad_version "Meter.Config" v

instance Serialize Meter.Rank where
    put a = (put_version 1 >>) $ put_enum $ case a of
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
    get = get_version >>= \case
        0 -> Serialize.get_enum_unsafe
        1 -> get_enum >>= \case
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
            n -> bad_enum "Meter.Rank" n
        v -> bad_version "Meter.Rank" v

instance Serialize Meter.LabelConfig where
    put a = put_version 0 >> case a of
        Meter.BigNumber a -> put_tag 0 >> put a
        Meter.Cycle a -> put_tag 1 >> put a
    get = get_version >>= \case
        0 -> get_tag >>= \case
            0 -> Meter.BigNumber <$> get
            1 -> Meter.Cycle <$> get
            t -> bad_tag "Meter.LabelConfig" t
        v -> bad_version "Meter.LabelConfig" v

instance Serialize Meter.AbstractMeter where
    put = \case
        Meter.T -> put_tag 0
        Meter.D ts -> put_tag 1 >> put ts
    get = get_tag >>= \case
        0 -> pure Meter.T
        1 -> Meter.D <$> get
        tag -> bad_tag "Meter.AbstractMeter" tag

-- The old version is unversioned
newtype OldMarklist = OldMarklist Mark.MarklistVector
    deriving (Serialize)

instance Serialize Mark.Marklist where
    put mlist = put_version 0 >> put (Mark.marklist_vec mlist)
    get = get_version >>= \case
        0 -> do
            vec :: Mark.MarklistVector <- get
            return $ Mark.marklist_from_vector vec
        v -> bad_version "Mark.Marklist" v

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
        name_zoom :: Double <- get
        zoom :: Double <- get
        return $ Mark.Mark (toEnum rank) width color name name_zoom zoom

-- ** Track

instance Serialize Track.Track where
    put (Track.Track a b c d e) = put_version 5
        >> put a >> put b >> put c >> put d >> put e
    get = get_version >>= \case
        4 -> do
            title :: Text <- get
            events :: Events.Events <- get
            color :: Color.Color <- get
            render :: Track.RenderConfig <- get
            return $ Track.Track title events color render True
        5 -> do
            title :: Text <- get
            events :: Events.Events <- get
            color :: Color.Color <- get
            render :: Track.RenderConfig <- get
            waveform :: Bool <- get
            return $ Track.Track title events color render waveform
        v -> bad_version "Track.Track" v

instance Serialize Track.RenderConfig where
    put (Track.RenderConfig a b) = put_version 1 >> put a >> put b
    get = get_version >>= \case
        1 -> do
            style :: Track.RenderStyle <- get
            color :: Color.Color <- get
            return $ Track.RenderConfig style color
        v -> bad_version "Track.RenderConfig" v

instance Serialize Track.RenderStyle where
    put Track.NoRender = put_tag 0
    put (Track.Line a) = put_tag 1 >> put a
    put (Track.Filled a) = put_tag 2 >> put a
    get = get_tag >>= \case
        0 -> return Track.NoRender
        1 -> do
            source :: Maybe Track.RenderSource <- get
            return $ Track.Line source
        2 -> do
            source :: Maybe Track.RenderSource <- get
            return $ Track.Filled source
        tag -> bad_tag "Track.RenderStyle" tag

instance Serialize Track.RenderSource where
    put (Track.Control a) = put_tag 0 >> put a
    put (Track.Pitch a) = do
        put_tag 1
        -- It used to be @Maybe ScoreT.Control@ but changed to ScoreT.PControl.
        -- RenderSource isn't versioned so adjust here.
        let c = if a == ScoreT.default_pitch then Nothing else Just a
        put c
    get = get_tag >>= \case
        0 -> do
            control :: ScoreT.Control <- get
            return $ Track.Control control
        1 -> do
            control :: Maybe ScoreT.PControl <- get
            return $ Track.Pitch (fromMaybe ScoreT.default_pitch control)
        tag -> bad_tag "Track.RenderSource" tag

-- ** Perform.Midi.Patch

instance Serialize Patch.Config where
    put (Patch.Config a b c) = put_version 11 >> put a >> put b >> put c
    get = get_version >>= \case
        7 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            scale :: Maybe Patch.Scale <- get
            control_defaults :: ScoreT.ControlValMap <- get
            let settings = old_settings
                    { Patch.config_scale = scale
                    , Patch.config_control_defaults = nonempty control_defaults
                    }
            return $ Patch.Config alloc Nothing settings
        8 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            scale :: Maybe Patch.Scale <- get
            control_defaults :: ScoreT.ControlValMap <- get
            initialization :: Set Patch.Initialization <- get
            let settings = old_settings
                    { Patch.config_scale = scale
                    , Patch.config_control_defaults = nonempty control_defaults
                    }
            return $ Patch.Config alloc (upgrade_initialization initialization)
                settings
        9 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            control_defaults :: ScoreT.ControlValMap <- get
            initialization :: Set Patch.Initialization <- get
            settings :: Patch.Settings <- get
            return $ Patch.Config alloc
                (upgrade_initialization initialization)
                (settings
                    { Patch.config_control_defaults = nonempty control_defaults
                    })
        10 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            initialization :: Set Patch.Initialization <- get
            settings :: Patch.Settings <- get
            return $ Patch.Config alloc (upgrade_initialization initialization)
                settings
        11 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            initialization :: Maybe Patch.Initialization <- get
            settings :: Patch.Settings <- get
            return $ Patch.Config alloc initialization settings
        v -> bad_version "Patch.Config" v
        where
        nonempty x = if x == mempty then Nothing else Just x
        upgrade_initialization = Lists.head . Set.toList

-- | This tags Settings which will have to be upgraded by merging with patch
-- defaults.
old_settings :: Patch.Settings
old_settings = mempty

is_old_settings :: Patch.Settings -> Bool
is_old_settings =
    (== Patch.config_pitch_bend_range mempty) . Patch.config_pitch_bend_range

instance Serialize Patch.Scale where
    put (Patch.Scale a b) = put a >> put b
    get = Patch.Scale <$> get <*> get

instance Serialize Patch.Initialization where
    put a = (put_version 1 >>) $ put_enum $ case a of
        Patch.Tuning -> 0
        Patch.NrpnTuning -> 1
    get = get_version >>= \case
        0 -> get >>= \n -> case n :: Int of
            0 -> pure Patch.Tuning
            1 -> pure Patch.NrpnTuning
            n -> fail $ "unknown enum val for Patch.Initialization " <> show n
        1 -> get_enum >>= \case
            0 -> pure Patch.Tuning
            1 -> pure Patch.NrpnTuning
            n -> bad_enum "Patch.Initialization" n
        v -> bad_version "Patch.Initialization" v

instance Serialize Patch.Settings where
    put (Patch.Settings a b c d e) = put_version 2
        >> put a >> put b >> put c >> put d >> put e
    get = get_version >>= \case
        0 -> do
            flags :: Set Patch.Flag <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            _pitch_bend_range :: Midi.Control.PbRange <- get
            return $ Patch.Settings
                (if Set.null flags then Nothing else Just flags)
                scale decay Nothing Nothing
        1 -> do
            flags :: Maybe (Set Patch.Flag) <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            pitch_bend_range :: Maybe Midi.Control.PbRange <- get
            return $ Patch.Settings flags scale decay pitch_bend_range Nothing
        2 -> do
            flags :: Maybe (Set Patch.Flag) <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            pitch_bend_range :: Maybe Midi.Control.PbRange <- get
            control_defaults :: Maybe ScoreT.ControlValMap <- get
            return $ Patch.Settings flags scale decay pitch_bend_range
                control_defaults
        v -> bad_version "Patch.Settings" v

-- TODO this should have had a version.  Add one when I next do an incompatible
-- update, and remove Old_Triggered.
instance Serialize Patch.Flag where
    -- The tag is Int rather than Word8, because this originally used
    -- Serialize.put_enum_unsafe and get_enum_unsafe.  Those are dangerous for
    -- compatibility though, because when I deleted a Flag it silently broke
    -- saves.
    put = \case
        Patch.Old_Triggered -> tag 0
        Patch.Pressure -> tag 1
        Patch.HoldKeyswitch -> tag 2
        Patch.ResumePlay -> tag 3
        Patch.UseFinalNoteOff -> tag 4
        where
        tag n = put (n :: Int)
    get = get >>= \(tag :: Int) -> case tag of
        0 -> return Patch.Old_Triggered
        1 -> return Patch.Pressure
        2 -> return Patch.HoldKeyswitch
        3 -> return Patch.ResumePlay
        4 -> return Patch.UseFinalNoteOff
        5 -> return Patch.Old_Triggered
        _ -> bad_tag "Flag" (fromIntegral tag)

-- ** Instrument.Common

instance Serialize Common.Config where
    -- This went from version 1 to 0 because I reverted the Maybe Environ.
    put (Common.Config a b c d) = put_version 0
        >> put a >> put b >> put c >> put d
    get = get_version >>= \case
        0 -> do
            environ :: REnv.Environ <- get
            controls :: ScoreT.ControlValMap <- get
            mute :: Bool <- get
            solo :: Bool <- get
            return $ Common.Config environ controls mute solo
        1 -> do
            environ :: Maybe REnv.Environ <- get
            controls :: ScoreT.ControlValMap <- get
            mute :: Bool <- get
            solo :: Bool <- get
            return $ Common.Config (fromMaybe mempty environ) controls mute solo
        v -> bad_version "Common.Config" v

instance Serialize Common.Flag where
    put a = (put_version 1 >>) $ put_enum $ case a of
        Common.Triggered -> 0
    get = get_version >>= \case
        0 -> get >>= \(t :: Int) -> case t of
            0 -> return Common.Triggered
            _ -> bad_tag name (fromIntegral t)
        1 -> get_enum >>= \case
            0 -> pure Common.Triggered
            n -> bad_enum name n
        v -> bad_version name v
        where name = "Common.Flag"

-- ** lilypond

instance Serialize Lilypond.Config where
    put (Lilypond.Config a b c d) = put_version 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            3 -> do
                quarter :: RealTime <- get
                quantize :: Lilypond.Duration <- get
                dotted_rests :: Bool <- get
                staves :: [(ScoreT.Instrument, Lilypond.StaffConfig)] <- get
                return $ Lilypond.Config quarter quantize dotted_rests staves
            _ -> bad_version "Lilypond.Config" v

instance Serialize Lilypond.StaffConfig where
    put (Lilypond.StaffConfig a b c d e) = put_version 2
        >> put a >> put b >> put c >> put d >> put e
    get = get_version >>= \case
        2 -> do
            long :: Lilypond.Instrument <- get
            short :: Lilypond.Instrument <- get
            code :: [Text] <- get
            display :: Bool <- get
            add_bass :: Bool <- get
            return $ Lilypond.StaffConfig long short code display add_bass
        v -> bad_version "Lilypond.StaffConfig" v

instance Serialize Lilypond.Duration where
    put = Serialize.put_enum_unsafe
    get = Serialize.get_enum_unsafe
