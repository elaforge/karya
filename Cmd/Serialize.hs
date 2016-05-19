-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{- | Instances to serialize and unserialize data types used by Ui.State.State.

    Types that I think might change have versions.  If the type changes,
    increment the put_version and add a new branch to the get_version case.

    Generally, the various parts of ADTs are unpacked with explicit type
    signatures.  That way, if one of the types is changed, there will be
    a type error over here pointing at the get/put code that needs to be
    updated.
-}
module Cmd.Serialize (
    allocations_magic, score_magic, views_magic
) where
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score
import qualified Derive.ScoreTypes as ScoreTypes

import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import Global
import Types


allocations_magic :: Serialize.Magic StateConfig.Allocations
allocations_magic = Serialize.Magic 'a' 'l' 'l' 'o'

score_magic :: Serialize.Magic State.State
score_magic = Serialize.Magic 's' 'c' 'o' 'r'

views_magic :: Serialize.Magic (Map.Map ViewId Block.View)
views_magic = Serialize.Magic 'v' 'i' 'e' 'w'

-- * Serialize instances

instance Serialize State.State where
    put (State.State a b c d e) = Serialize.put_version 6
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- Serialize.get_version
        case v of
            6 -> do
                views :: Map.Map Types.ViewId Block.View <- get
                blocks :: Map.Map Types.BlockId Block.Block <- get
                tracks :: Map.Map Types.TrackId Track.Track <- get
                rulers :: Map.Map Types.RulerId Ruler.Ruler <- get
                config :: State.Config <- get
                return $ State.State views blocks tracks rulers config
            _ -> Serialize.bad_version "State.State" v

instance Serialize State.Config where
    put (State.Config ns meta root transform allocs lilypond defaults
            saved_views defs)
        =  Serialize.put_version 11
            >> put ns >> put meta >> put root >> put transform
            >> put allocs >> put lilypond >> put defaults >> put saved_views
            >> put defs
    get = Serialize.get_version >>= \v -> case v of
        11 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            transform :: Text <- get
            insts :: StateConfig.Allocations <- get
            lilypond :: Lilypond.Config <- get
            defaults :: State.Default <- get
            saved_views :: State.SavedViews <- get
            defs :: Maybe FilePath <- get
            return $ State.Config ns meta root transform insts lilypond
                defaults saved_views defs
        _ -> Serialize.bad_version "State.Config" v

instance Serialize.Serialize StateConfig.Allocations where
    put (StateConfig.Allocations a) = Serialize.put_version 1 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            1 -> do
                configs :: Map.Map Score.Instrument StateConfig.Allocation
                    <- get
                return $ StateConfig.Allocations configs
            _ -> Serialize.bad_version "StateConfig.Allocations" v

instance Serialize StateConfig.Allocation where
    put (StateConfig.Allocation a b c) =
        Serialize.put_version 0 >> put a >> put b >> put c
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            qualified :: InstTypes.Qualified <- get
            config :: Common.Config <- get
            backend :: StateConfig.Backend <- get
            return $ StateConfig.Allocation qualified config backend
        _ -> Serialize.bad_version "StateConfig.Allocation" v

instance Serialize StateConfig.Backend where
    put (StateConfig.Midi a) = put_tag 0 >> put a
    put StateConfig.Im = put_tag 1
    put StateConfig.Dummy = put_tag 2
    get = get_tag >>= \tag -> case tag of
        0 -> do
            config :: Patch.Config <- get
            return $ StateConfig.Midi config
        1 -> return StateConfig.Im
        2 -> return StateConfig.Dummy
        _ -> bad_tag "StateConfig.Backend" tag

instance Serialize Common.Config where
    put (Common.Config a b c d) = Serialize.put_version 0
        >> put a >> put b >> put c >> put d
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            environ :: RestrictedEnviron.Environ <- get
            controls :: ScoreTypes.ControlValMap <- get
            mute :: Bool <- get
            solo :: Bool <- get
            return $ Common.Config environ controls mute solo
        _ -> Serialize.bad_version "Common.Config" v

-- | For backward compatibility.
newtype MidiConfigs = MidiConfigs (Map.Map Score.Instrument Patch.Config)
    deriving (Show)

instance Serialize MidiConfigs where
    put (MidiConfigs a) = Serialize.put_version 5 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            5 -> do
                insts :: Map.Map Score.Instrument Patch.Config <- get
                return $ MidiConfigs insts
            _ -> Serialize.bad_version "Patch.MidiConfigs" v

instance Serialize State.Meta where
    put (State.Meta a b c d e) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d >> put e
    get = Serialize.get_version >>= \v -> case v of
        3 -> do
            creation :: Time.UTCTime <- get
            last_save :: Time.UTCTime <- get
            notes :: Text <- get
            midi :: Map.Map BlockId State.MidiPerformance <- get
            lily :: Map.Map BlockId State.LilypondPerformance <- get
            return $ State.Meta creation last_save notes midi lily
        _ -> Serialize.bad_version "State.Meta" v

instance Serialize a => Serialize (State.Performance a) where
    put (State.Performance a b c) = Serialize.put_version 0 >> put a >> put b
        >> put c
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            perf :: a <- get
            creation :: Time.UTCTime <- get
            patch :: Text <- get
            return $ State.Performance perf creation patch
        _ -> Serialize.bad_version "State.Performance" v

instance Serialize State.Default where
    put (State.Default a) = Serialize.put_version 4 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            4 -> do
                tempo :: Signal.Y <- get
                return $ State.Default tempo
            _ -> Serialize.bad_version "State.Default" v

-- ** Block

instance Serialize Block.Block where
    -- Config is not serialized because everything in the block config is
    -- either derived from the Cmd.State or is hardcoded.
    put (Block.Block a _config b c d e f) = Serialize.put_version 11
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = do
        v <- Serialize.get_version
        case v of
            11 -> do
                title :: Text <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                iblock :: Maybe (BlockId, Block.TrackDestinations) <- get
                itracks :: [(TrackId, Block.TrackDestinations)] <- get
                meta :: Map.Map Text Text <- get
                return $ Block.Block title Block.default_config tracks skel
                    iblock itracks meta
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

instance Serialize Block.DeriveDestination where
    put (Block.DeriveDestination a b) = put a >> put b
    get = do
        note :: (TrackId, Block.EventIndex) <- get
        controls :: (Map.Map Text (TrackId, Block.EventIndex)) <- get
        return $ Block.DeriveDestination note controls

instance Serialize Skeleton.Skeleton where
    put (Skeleton.Skeleton a) = put a
    get = get >>= \a -> return (Skeleton.Skeleton a)

instance Serialize Block.Track where
    put (Block.Track a b c d) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                id :: Block.TracklikeId <- get
                width :: Types.Width <- get
                flags :: Set.Set Block.TrackFlag <- get
                merged :: Set.Set Types.TrackId <- get
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
    put (Block.View a b c d e f g h) = Serialize.put_version 5
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
    get = do
        v <- Serialize.get_version
        case v of
            5 -> do
                block :: Types.BlockId <- get
                rect :: Rect.Rect <- get
                visible_track :: Int <- get
                visible_time :: Int <- get
                status :: Map.Map (Int, Text) Text <- get
                track_scroll :: Types.Width <- get
                zoom :: Types.Zoom <- get
                selections :: Map.Map Sel.Num Sel.Selection <- get
                return $ Block.View block rect visible_track visible_time
                    status track_scroll zoom selections
            _ -> Serialize.bad_version "Block.View" v

instance Serialize Rect.Rect where
    put r = put (Rect.rx r) >> put (Rect.ry r) >> put (Rect.rw r)
        >> put (Rect.rh r)
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Rect.xywh a b c d)

instance Serialize Types.Zoom where
    put (Types.Zoom a b) = put a >> put b
    get = do
        offset :: ScoreTime <- get
        factor :: Double <- get
        return $ Types.Zoom offset factor

instance Serialize Sel.Selection where
    put (Sel.Selection a b c d) = put a >> put b >> put c >> put d
    get = do
        strack :: Int <- get
        stime :: ScoreTime <- get
        ctrack :: Int <- get
        ctime :: ScoreTime <- get
        return $ Sel.Selection strack stime ctrack ctime

-- ** Types, Color, Font

instance Serialize Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

-- ** Ruler

instance Serialize Ruler.Ruler where
    put (Ruler.Ruler a b c d) = Serialize.put_version 6
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            6 -> do
                marklists :: Map.Map Ruler.Name (Maybe Ruler.MeterType,
                    Ruler.Marklist) <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler marklists bg show_names align_to_bottom
            _ -> Serialize.bad_version "Ruler.Ruler" v

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
    put (Track.Track a b c d) = Serialize.put_version 4 >>
        put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            4 -> do
                title :: Text <- get
                events :: Events.Events <- get
                color :: Color.Color <- get
                render :: Track.RenderConfig <- get
                return $ Track.Track title events color render
            _ -> Serialize.bad_version "Track.Track" v

instance Serialize Track.RenderConfig where
    put (Track.RenderConfig a b) = Serialize.put_version 1 >> put a >> put b
    get = Serialize.get_version >>= \v -> case v of
        1 -> do
            style :: Track.RenderStyle <- get
            color :: Color.Color <- get
            return $ Track.RenderConfig style color
        _ -> Serialize.bad_version "Track.RenderConfig" v

instance Serialize Track.RenderStyle where
    put Track.NoRender = put_tag 0
    put (Track.Line a) = put_tag 1 >> put a
    put (Track.Filled a) = put_tag 2 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> return Track.NoRender
        1 -> do
            source :: Maybe Track.RenderSource <- get
            return $ Track.Line source
        2 -> do
            source :: Maybe Track.RenderSource <- get
            return $ Track.Filled source
        _ -> bad_tag "Track.RenderStyle" tag

instance Serialize Track.RenderSource where
    put (Track.Control a) = put_tag 0 >> put a
    put (Track.Pitch a) = do
        put_tag 1
        -- It used to be @Maybe Score.Control@ but changed to Score.PControl.
        -- RenderSource isn't versioned so adjust here.
        let c = if a == Score.default_pitch then Nothing else Just a
        put c
    get = get_tag >>= \tag -> case tag of
        0 -> do
            control :: Score.Control <- get
            return $ Track.Control control
        1 -> do
            control :: Maybe Score.PControl <- get
            return $ Track.Pitch (fromMaybe Score.default_pitch control)
        _ -> bad_tag "Track.RenderSource" tag

-- ** Perform.Midi.Patch

instance Serialize Patch.Config where
    put (Patch.Config a b c) = Serialize.put_version 7
        >> put a >> put b >> put c
    get = do
        v <- Serialize.get_version
        case v of
            7 -> do
                addrs :: [(Patch.Addr, Maybe Patch.Voices)] <- get
                scale :: Maybe Patch.Scale <- get
                control_defaults :: Score.ControlValMap <- get
                return $ Patch.Config addrs scale control_defaults
            _ -> Serialize.bad_version "Patch.Config" v

instance Serialize Patch.Scale where
    put (Patch.Scale a b) = put a >> put b
    get = Patch.Scale <$> get <*> get

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
    put = put . fromEnum
    get = toEnum <$> get
