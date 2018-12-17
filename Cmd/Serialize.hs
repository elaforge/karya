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
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import Util.Serialize (Serialize, get, put, get_tag, put_tag, bad_tag)

import Midi.Instances ()
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score
import qualified Derive.ScoreTypes as ScoreTypes

import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import Global
import Types


allocations_magic :: Serialize.Magic UiConfig.Allocations
allocations_magic = Serialize.Magic 'a' 'l' 'l' 'o'

score_magic :: Serialize.Magic Ui.State
score_magic = Serialize.Magic 's' 'c' 'o' 'r'

views_magic :: Serialize.Magic (Map ViewId Block.View)
views_magic = Serialize.Magic 'v' 'i' 'e' 'w'

-- * Serialize instances

instance Serialize Ui.State where
    put (Ui.State a b c d e) = Serialize.put_version 6
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- Serialize.get_version
        case v of
            6 -> do
                views :: Map Types.ViewId Block.View <- get
                blocks :: Map Types.BlockId Block.Block <- get
                tracks :: Map Types.TrackId Track.Track <- get
                rulers :: Map Types.RulerId Ruler.Ruler <- get
                config :: Ui.Config <- get
                return $ Ui.State views blocks tracks rulers config
            _ -> Serialize.bad_version "Ui.State" v

instance Serialize Ui.Config where
    put (Ui.Config ns meta root allocs lilypond defaults
            saved_views ky)
        =  Serialize.put_version 13
            >> put ns >> put meta >> put root >> put allocs >> put lilypond
            >> put defaults >> put saved_views >> put ky
    get = Serialize.get_version >>= \v -> case v of
        11 -> do
            ns :: Id.Namespace <- get
            meta :: Ui.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: Ui.Default <- get
            saved_views :: Ui.SavedViews <- get
            ky_file :: Maybe FilePath <- get
            return $ Ui.Config ns meta root insts lilypond defaults saved_views
                (upgrade_transform transform
                    (maybe "" (\fn -> "import '" <> txt fn <> "'\n") ky_file))
        12 -> do
            ns :: Id.Namespace <- get
            meta :: Ui.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: Ui.Default <- get
            saved_views :: Ui.SavedViews <- get
            ky :: Text <- get
            return $ Ui.Config ns meta root insts lilypond
                defaults saved_views (upgrade_transform transform ky)
        13 -> do
            ns :: Id.Namespace <- get
            meta :: Ui.Meta <- get
            root :: Maybe BlockId <- get
            insts :: UiConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: Ui.Default <- get
            saved_views :: Ui.SavedViews <- get
            ky :: Text <- get
            return $ Ui.Config ns meta root insts lilypond defaults saved_views
                ky
        _ -> Serialize.bad_version "Ui.Config" v
        where
        upgrade_transform global_transform ky
            | Text.null (Text.strip global_transform) = ky
            | otherwise = ky <> "\n\nnote transformer:\nGLOBAL = "
                <> global_transform

instance Serialize.Serialize UiConfig.Allocations where
    put (UiConfig.Allocations a) = Serialize.put_version 1 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            1 -> do
                configs :: Map Score.Instrument UiConfig.Allocation <- get
                return $ UiConfig.Allocations configs
            _ -> Serialize.bad_version "UiConfig.Allocations" v

instance Serialize UiConfig.Allocation where
    put (UiConfig.Allocation a b c) =
        Serialize.put_version 0 >> put a >> put b >> put c
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            qualified :: InstTypes.Qualified <- get
            config :: Common.Config <- get
            backend :: UiConfig.Backend <- get
            return $ UiConfig.Allocation qualified config backend
        _ -> Serialize.bad_version "UiConfig.Allocation" v

instance Serialize UiConfig.Backend where
    put (UiConfig.Midi a) = put_tag 0 >> put a
    put UiConfig.Im = put_tag 1
    put UiConfig.Dummy = put_tag 2
    get = get_tag >>= \tag -> case tag of
        0 -> do
            config :: Patch.Config <- get
            return $ UiConfig.Midi config
        1 -> return UiConfig.Im
        2 -> return UiConfig.Dummy
        _ -> bad_tag "UiConfig.Backend" tag

-- | For backward compatibility.
newtype MidiConfigs = MidiConfigs (Map Score.Instrument Patch.Config)
    deriving (Show)

instance Serialize MidiConfigs where
    put (MidiConfigs a) = Serialize.put_version 5 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            5 -> do
                insts :: Map Score.Instrument Patch.Config <- get
                return $ MidiConfigs insts
            _ -> Serialize.bad_version "Patch.MidiConfigs" v

instance Serialize Ui.Meta where
    put (Ui.Meta a b c d e) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d >> put e
    get = Serialize.get_version >>= \v -> case v of
        3 -> do
            creation :: Time.UTCTime <- get
            last_save :: Time.UTCTime <- get
            notes :: Text <- get
            midi :: Map BlockId Ui.MidiPerformance <- get
            lily :: Map BlockId Ui.LilypondPerformance <- get
            return $ Ui.Meta creation last_save notes midi lily
        _ -> Serialize.bad_version "Ui.Meta" v

instance Serialize a => Serialize (Ui.Performance a) where
    put (Ui.Performance a b c) = Serialize.put_version 0 >> put a >> put b
        >> put c
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            perf :: a <- get
            creation :: Time.UTCTime <- get
            patch :: Text <- get
            return $ Ui.Performance perf creation patch
        _ -> Serialize.bad_version "Ui.Performance" v

instance Serialize Ui.Default where
    put (Ui.Default a) = Serialize.put_version 4 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            4 -> do
                tempo :: Signal.Y <- get
                return $ Ui.Default tempo
            _ -> Serialize.bad_version "Ui.Default" v

-- ** Block

instance Serialize Block.Block where
    -- Config is not serialized because everything in the block config is
    -- either derived from the Cmd.State or is hardcoded.
    put (Block.Block a _config b c d e f g) = Serialize.put_version 12
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
    get = do
        v <- Serialize.get_version
        case v of
            11 -> do
                title :: Text <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
                itracks :: [(TrackId, Block.TrackDestinations)] <- get
                meta :: Map Text Text <- get
                return $ Block.Block title Block.default_config tracks skel
                    iblock itracks mempty meta
            12 -> do
                title :: Text <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
                itracks :: [(TrackId, Block.TrackDestinations)] <- get
                dtracks :: Block.ManualDestinations <- get
                meta :: Map Text Text <- get
                return $ Block.Block title Block.default_config tracks skel
                    iblock itracks dtracks meta
            _ -> Serialize.bad_version "Block.Block" v

instance Serialize Block.TrackDestinations where
    put (Block.DeriveDestinations a) = put_tag 0 >> put a
    put (Block.ScoreDestinations a) = put_tag 1 >> put a
    get = do
        tag <- get_tag
        case tag of
            0 -> Block.DeriveDestinations <$> get
            1 -> Block.ScoreDestinations <$> get
            _ -> bad_tag "Block.TrackDestinations" tag

instance Serialize Block.NoteDestination where
    put (Block.NoteDestination a b) = put a >> put b
    get = do
        note :: (TrackId, Block.EventIndex) <- get
        controls :: (Map Text (TrackId, Block.EventIndex)) <- get
        return $ Block.NoteDestination note controls

instance Serialize Block.Track where
    put (Block.Track a b c d) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                id :: Block.TracklikeId <- get
                width :: Types.Width <- get
                flags :: Set Block.TrackFlag <- get
                merged :: Set Types.TrackId <- get
                return $ Block.Track id width flags merged
            _ -> Serialize.bad_version "Block.Track" v

instance Serialize Block.TrackFlag where
    put Block.Collapse = put_tag 0
    put Block.Solo = put_tag 1
    put Block.Mute = put_tag 2
    put Block.Disable = put_tag 3
    get = do
        tag <- get_tag
        case tag of
            0 -> return Block.Collapse
            1 -> return Block.Solo
            2 -> return Block.Mute
            3 -> return Block.Disable
            _ -> bad_tag "Block.TrackFlag" tag

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
    put (Block.View a b c d e f g) = Serialize.put_version 7
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
    get = do
        v <- Serialize.get_version
        case v of
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
            _ -> Serialize.bad_version "Block.View" v
        where
        upgrade (OldSelection a b c d) = Sel.Selection a b c d Sel.Positive

instance Serialize Block.Padding where
    put (Block.Padding a b c) = Serialize.put_version 0
        >> put a >> put b >> put c
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                left :: Int <- get
                top :: Int <- get
                bottom :: Int <- get
                return $ Block.Padding left top bottom
            _ -> Serialize.bad_version "Block.Padding" v

instance Serialize Rect.Rect where
    put r = put (Rect.rx r) >> put (Rect.ry r) >> put (Rect.rw r)
        >> put (Rect.rh r)
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
    put (Sel.Selection a b c d e) = Serialize.put_version 0
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                strack :: Int <- get
                stime :: ScoreTime <- get
                ctrack :: Int <- get
                ctime :: ScoreTime <- get
                orient :: Sel.Orientation <- get
                return $ Sel.Selection strack stime ctrack ctime orient
            _ -> Serialize.bad_version "Sel.Selection" v

instance Serialize Sel.Orientation where
    put = Serialize.put_enum
    get = Serialize.get_enum

-- ** Types, Color, Font

instance Serialize Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

-- ** Ruler

instance Serialize Ruler.Ruler where
    put (Ruler.Ruler a b c d) = Serialize.put_version 7
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            6 -> do
                marklists :: Map Ruler.Name (Maybe Text, Ruler.Marklist) <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler (upgrade <$> marklists) bg show_names
                    align_to_bottom
                where
                upgrade (name, mlist) =
                    ((\n -> Ruler.MeterConfig n 1) <$> name, mlist)
            7 -> do
                marklists :: Map Ruler.Name
                    (Maybe Ruler.MeterConfig, Ruler.Marklist) <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler marklists bg show_names align_to_bottom
            _ -> Serialize.bad_version "Ruler.Ruler" v

instance Serialize Ruler.MeterConfig where
    put (Ruler.MeterConfig a b) = Serialize.put_version 0 >> put a >> put b
    get = Serialize.get_version >>= \case
        0 -> do
            name :: Text <- get
            start_measure :: Int <- get
            return $ Ruler.MeterConfig name start_measure
        v -> Serialize.bad_version "Ruler.MeterConfig" v

instance Serialize Ruler.Marklist where
    put mlist = put (Ruler.marklist_vec mlist)
    get = do
        vec :: Ruler.MarklistVector <- get
        return $ Ruler.marklist_from_vector vec

instance Serialize Ruler.Mark where
    put (Ruler.Mark a b c d e f) = put a >> put b >> put c >> put d >> put e
        >> put f
    get = do
        rank :: Int <- get
        width :: Int <- get
        color :: Color.Color <- get
        name :: Text <- get
        name_zoom :: Double <- get
        zoom :: Double <- get
        return $ Ruler.Mark rank width color name name_zoom zoom

-- ** Track

instance Serialize Track.Track where
    put (Track.Track a b c d) = Serialize.put_version 4
        >> put a >> put b >> put c >> put d
    get = Serialize.get_version >>= \case
        4 -> do
            title :: Text <- get
            events :: Events.Events <- get
            color :: Color.Color <- get
            render :: Track.RenderConfig <- get
            return $ Track.Track title events color render
        v -> Serialize.bad_version "Track.Track" v

instance Serialize Track.RenderConfig where
    put (Track.RenderConfig a b) = Serialize.put_version 1 >> put a >> put b
    get = Serialize.get_version >>= \case
        1 -> do
            style :: Track.RenderStyle <- get
            color :: Color.Color <- get
            return $ Track.RenderConfig style color
        v -> Serialize.bad_version "Track.RenderConfig" v

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
        -- It used to be @Maybe Score.Control@ but changed to Score.PControl.
        -- RenderSource isn't versioned so adjust here.
        let c = if a == Score.default_pitch then Nothing else Just a
        put c
    get = get_tag >>= \case
        0 -> do
            control :: Score.Control <- get
            return $ Track.Control control
        1 -> do
            control :: Maybe Score.PControl <- get
            return $ Track.Pitch (fromMaybe Score.default_pitch control)
        tag -> bad_tag "Track.RenderSource" tag

-- ** Perform.Midi.Patch

instance Serialize Patch.Config where
    put (Patch.Config a b c d) = Serialize.put_version 9
        >> put a >> put b >> put c >> put d
    get = Serialize.get_version >>= \case
        7 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            scale :: Maybe Patch.Scale <- get
            control_defaults :: Score.ControlValMap <- get
            let settings = old_settings { Patch.config_scale = scale }
            return $ Patch.Config alloc control_defaults mempty settings
        8 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            scale :: Maybe Patch.Scale <- get
            control_defaults :: Score.ControlValMap <- get
            initialization :: Set Patch.Initialization <- get
            let settings = old_settings { Patch.config_scale = scale }
            return $ Patch.Config alloc control_defaults initialization
                settings
        9 -> do
            alloc :: [(Patch.Addr, Maybe Patch.Voices)] <- get
            control_defaults :: Score.ControlValMap <- get
            initialization :: Set Patch.Initialization <- get
            settings :: Patch.Settings <- get
            return $ Patch.Config alloc control_defaults initialization
                settings
        v -> Serialize.bad_version "Patch.Config" v

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
    put a = Serialize.put_version 0 >> Serialize.put_enum a
    get = Serialize.get_version >>= \case
        0 -> Serialize.get_enum
        v -> Serialize.bad_version "Patch.Initialization" v

instance Serialize Patch.Settings where
    put (Patch.Settings a b c d) = Serialize.put_version 0
        >> put a >> put b >> put c >> put d
    get = Serialize.get_version >>= \case
        0 -> do
            flags :: Set Patch.Flag <- get
            scale :: Maybe Patch.Scale <- get
            decay :: Maybe RealTime <- get
            pitch_bend_range :: Midi.Control.PbRange <- get
            return $ Patch.Settings flags scale decay pitch_bend_range
        v -> Serialize.bad_version "Patch.Settings" v

-- TODO this should have had a version.  Add one when I next do an incompatible
-- update, and remove Old_Triggered.
instance Serialize Patch.Flag where
    -- The tag is Int rather than Word8, because this originally used
    -- Serialize.put_enum and get_enum.  Those are dangerous for compatibility
    -- though, because when I deleted a Flag it silently broke saves.
    put = \case
        Patch.Old_Triggered -> tag 0
        Patch.Pressure -> tag 1
        Patch.HoldKeyswitch -> tag 2
        Patch.ResumePlay -> tag 3
        Patch.UseFinalNoteOff -> tag 4
        where
        tag n = Serialize.put (n :: Int)
    get = Serialize.get >>= \(tag :: Int) -> case tag of
        0 -> return Patch.Old_Triggered
        1 -> return Patch.Pressure
        2 -> return Patch.HoldKeyswitch
        3 -> return Patch.ResumePlay
        4 -> return Patch.UseFinalNoteOff
        _ -> Serialize.bad_tag "Flag" (fromIntegral tag)

-- ** Instrument.Common

instance Serialize Common.Config where
    put (Common.Config a b c d) = Serialize.put_version 0
        >> put a >> put b >> put c >> put d
    get = Serialize.get_version >>= \case
        0 -> do
            environ :: RestrictedEnviron.Environ <- get
            controls :: ScoreTypes.ControlValMap <- get
            mute :: Bool <- get
            solo :: Bool <- get
            return $ Common.Config environ controls mute solo
        v -> Serialize.bad_version "Common.Config" v

instance Serialize Common.Flag where
    put a = Serialize.put_version 0 >> Serialize.put_enum a
    get = Serialize.get_version >>= \case
        0 -> Serialize.get_enum
        v -> Serialize.bad_version "Common.Flag" v

-- ** lilypond

instance Serialize Lilypond.Config where
    put (Lilypond.Config a b c d) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                quarter :: RealTime <- get
                quantize :: Lilypond.Duration <- get
                dotted_rests :: Bool <- get
                staves :: [(Score.Instrument, Lilypond.StaffConfig)] <- get
                return $ Lilypond.Config quarter quantize dotted_rests staves
            _ -> Serialize.bad_version "Lilypond.Config" v

instance Serialize Lilypond.StaffConfig where
    put (Lilypond.StaffConfig a b c d e) = Serialize.put_version 2
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- Serialize.get_version
        case v of
            2 -> do
                long :: Lilypond.Instrument <- get
                short :: Lilypond.Instrument <- get
                code :: [Text] <- get
                display :: Bool <- get
                add_bass :: Bool <- get
                return $ Lilypond.StaffConfig long short code display add_bass
            _ -> Serialize.bad_version "Lilypond.StaffConfig" v

instance Serialize Lilypond.Duration where
    put a = Serialize.put_enum a
    get = Serialize.get_enum
