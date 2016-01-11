-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- needed for SomeException
{- | Serialize and unserialize all the data types used by Ui.State.State.

    Types that I think might change have versions.  If the type changes,
    increment the put_version and add a new branch to the get_version case.

    Generally, the various parts of ADTs are unpacked with explicit type
    signatures.  That way, if one of the types is changed, there will be
    a type error over here pointing at the get/put code that needs to be
    updated.
-}
module Cmd.Serialize (
    serialize, unserialize
    -- * Magic
    , Magic, midi_magic, midi_config_magic, score_magic, views_magic
    , InstrumentDb(..), instrument_db_magic
) where
import qualified Control.Exception as Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Util.File as File
import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import Util.Serialize (Serialize, get, put, get_tag, put_tag, bad_tag)

import Midi.Instances ()
import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Instrument.MidiConfig as MidiConfig
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Signal as Signal

import Global
import Types


serialize :: Serialize a => Magic a -> FilePath -> a -> IO ()
serialize magic fname state = do
    backup_file fname
    make_dir fname
    File.writeGz fname $ magic_bytes magic <> Serialize.encode state

-- | Returns Left if there was a parsing error, and Right Nothing if the file
-- didn't exist.  This can throw IO errors.
unserialize :: Serialize a => Magic a -> FilePath -> IO (Either Text (Maybe a))
unserialize magic fname = do
    maybe_bytes <- File.ignoreEnoent $ File.readGz fname
    case maybe_bytes of
        Nothing -> return (Right Nothing)
        Just bytes
            | not (magic_bytes magic `B.isPrefixOf` bytes) ->
                return $ Left $ "expected magic code "
                    <> showt (magic_bytes magic) <> " but got "
                    <> showt (B.take magic_length bytes)
            | otherwise -> do
                -- This is subtle.  Apparently Serialize.decode can still throw
                -- an exception unless the contents of the Either is forced to
                -- whnf.
                val <- Exception.evaluate
                        (Serialize.decode (B.drop magic_length bytes))
                    `Exception.catch` \(exc :: Exception.SomeException) ->
                        return $ Left $ "exception: " <> show exc
                return $ either (Left . txt) (Right . Just) val

-- | Move @file@ to @file.last@.  Do this before writing a new one that may
-- fail.
backup_file :: FilePath -> IO ()
backup_file fname = do
    File.requireWritable fname
    File.requireWritable (fname ++ ".last")
    void $ File.ignoreEnoent $ Directory.renameFile fname (fname ++ ".last")

make_dir :: FilePath -> IO ()
make_dir = Directory.createDirectoryIfMissing True . FilePath.takeDirectory


-- * magic

-- | This is a four byte prefix to identify a particular file type, tagged with
-- the serialized type.  The Chars are just for syntactic convenience only, and
-- must be ASCII.
--
-- The constructor is not exported, so all magics have to be defined here,
-- which should make it easy to avoid collisions.
data Magic a = Magic !Char !Char !Char !Char deriving (Show)

magic_bytes :: Magic a -> B.ByteString
magic_bytes (Magic c1 c2 c3 c4) = Char8.pack [c1, c2, c3, c4]

magic_length :: Int
magic_length = 4

-- | Saved MIDI performance.
midi_magic :: Magic (Vector.Vector Midi.WriteMessage)
midi_magic = Magic 'm' 'i' 'd' 'i'

midi_config_magic :: Magic MidiConfig.Config
midi_config_magic = Magic 'm' 'c' 'o' 'n'

score_magic :: Magic State.State
score_magic = Magic 's' 'c' 'o' 'r'

views_magic :: Magic (Map.Map ViewId Block.View)
views_magic = Magic 'v' 'i' 'e' 'w'

newtype InstrumentDb = InstrumentDb (Time.UTCTime, Instrument.Synth ())

instrument_db_magic :: Magic InstrumentDb
instrument_db_magic = Magic 'i' 'n' 's' 't'

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
    put (State.Config ns meta root midi transform instruments lilypond defaults
            saved_views defs)
        =  Serialize.put_version 9
            >> put ns >> put meta >> put root >> put (Configs midi)
            >> put transform >> put instruments >> put lilypond >> put defaults
            >> put saved_views >> put defs
    get = Serialize.get_version >>= \v -> case v of
        8 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: Text <- get
            instruments :: Map.Map Score.Instrument Score.Instrument <- get
            lilypond :: Lilypond.Config <- get
            defaults :: State.Default <- get
            saved_views :: State.SavedViews <- get
            return $ State.Config ns meta root midi transform instruments
                lilypond defaults saved_views Nothing
        9 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: Text <- get
            instruments :: Map.Map Score.Instrument Score.Instrument <- get
            lilypond :: Lilypond.Config <- get
            defaults :: State.Default <- get
            saved_views :: State.SavedViews <- get
            defs :: Maybe FilePath <- get
            return $ State.Config ns meta root midi transform instruments
                lilypond defaults saved_views defs
        _ -> Serialize.bad_version "State.Config" v

instance Serialize State.Meta where
    put (State.Meta a b c d e) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d >> put e
    get = Serialize.get_version >>= \v -> case v of
        2 -> do
            creation :: Time.UTCTime <- get
            notes :: Text <- get
            midi :: Map.Map BlockId State.MidiPerformance <- get
            lily :: Map.Map BlockId State.LilypondPerformance <- get
            return $ State.Meta creation creation notes midi lily
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
            2 -> do
                id :: Block.TracklikeId <- get
                width :: Types.Width <- get
                flags :: Set.Set Block.TrackFlag <- get
                merged :: [Types.TrackId] <- get
                return $ Block.Track id width flags (Set.fromList merged)
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
            5 -> do
                marklists :: Map.Map Ruler.Name Ruler.Marklist <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler (Map.mapWithKey upgrade marklists) bg
                    show_names align_to_bottom
                where
                upgrade name mlist
                    | name == Ruler.meter = (Just "meter", mlist)
                    | otherwise = (Nothing, mlist)
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

-- ** Midi.Instrument

instance Serialize.Serialize MidiConfig.Config where
    put (MidiConfig.Config a b) = Serialize.put_version 0 >> put a >> put b
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                midi :: Instrument.Configs <- get
                aliases :: Map.Map Score.Instrument Score.Instrument <- get
                return $ MidiConfig.Config midi aliases
            _ -> Serialize.bad_version "MidiConfig.Config" v

-- | It's a type synonym, but Serialize needs a newtype.
newtype Configs = Configs Instrument.Configs

instance Serialize Configs where
    put (Configs a) = Serialize.put_version 5 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            5 -> do
                insts :: Map.Map Score.Instrument Instrument.Config <- get
                return $ Configs insts
            _ -> Serialize.bad_version "Instrument.Configs" v

instance Serialize Instrument.Config where
    put (Instrument.Config a b c d e f g) = Serialize.put_version 6
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
    get = do
        v <- Serialize.get_version
        case v of
            4 -> do
                addrs :: [(Instrument.Addr, Maybe Instrument.Voices)] <- get
                environ :: RestrictedEnviron.Environ <- get
                control_defaults :: Score.ControlValMap <- get
                mute :: Bool <- get
                solo :: Bool <- get
                let controls = mempty
                return $ Instrument.Config addrs environ controls Nothing
                    control_defaults mute solo
            5 -> do
                addrs :: [(Instrument.Addr, Maybe Instrument.Voices)] <- get
                environ :: RestrictedEnviron.Environ <- get
                scale :: Maybe Instrument.PatchScale <- get
                control_defaults :: Score.ControlValMap <- get
                mute :: Bool <- get
                solo :: Bool <- get
                let controls = mempty
                return $ Instrument.Config addrs environ controls scale
                    control_defaults mute solo
            6 -> do
                addrs :: [(Instrument.Addr, Maybe Instrument.Voices)] <- get
                environ :: RestrictedEnviron.Environ <- get
                controls :: Score.ControlValMap <- get
                scale :: Maybe Instrument.PatchScale <- get
                control_defaults :: Score.ControlValMap <- get
                mute :: Bool <- get
                solo :: Bool <- get
                return $ Instrument.Config addrs environ controls scale
                    control_defaults mute solo
            _ -> Serialize.bad_version "Instrument.Config" v

instance Serialize Instrument.PatchScale where
    put (Instrument.PatchScale a b) = put a >> put b
    get = Instrument.PatchScale <$> get <*> get

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
            1 -> do
                long :: Lilypond.Instrument <- get
                short :: Lilypond.Instrument <- get
                code :: [Text] <- get
                display :: Bool <- get
                return $ Lilypond.StaffConfig long short code display False
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

-- ** misc

instance Serialize Time.UTCTime where
    put time = put (show time)
    get = get >>= return . read
