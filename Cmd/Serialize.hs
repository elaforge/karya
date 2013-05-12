{-# LANGUAGE ScopedTypeVariables #-} -- needed for SomeException
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Serialize and unserialize all the data types used by Ui.State.State.

    Types that I think might change have versions.  If the type changes,
    increment the put_version and add a new branch to the get_version case.

    Generally, the various parts of ADTs are unpacked with explicit type
    signatures.  That way, if one of the types is changed, there will be
    a type error over here pointing at the get/put code that needs to be
    updated.
-}
module Cmd.Serialize where
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Time as Time

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import Util.Control
import qualified Util.File as File
import qualified Util.PPrint as PPrint
import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import Util.Serialize (Serialize, get, put, get_tag, put_tag, bad_tag)

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Score as Score
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Types


serialize :: (Serialize a) => FilePath -> a -> IO ()
serialize fname state = do
    backup_file fname
    make_dir fname
    ByteString.writeFile fname $ Serialize.encode state

serialize_text :: (Show a) => FilePath -> a -> IO ()
serialize_text fname state = do
    backup_file fname
    make_dir fname
    writeFile fname (show state ++ "\n")

-- | Like 'serialize_text' but pretty-print it.  Probably not suitable for
-- giant things.
serialize_pretty_text :: (Show a) => FilePath -> a -> IO ()
serialize_pretty_text fname state = do
    backup_file fname
    make_dir fname
    writeFile fname (PPrint.pshow state)

-- | Returns Left if there was a parsing error, and Right Nothing if the file
-- didn't exist.
unserialize :: (Serialize a) => FilePath -> IO (Either String (Maybe a))
unserialize fname = do
    maybe_bytes <- File.ignore_enoent $ ByteString.readFile fname
    case maybe_bytes of
        Nothing -> return (Right Nothing)
        Just bytes -> do
            -- This is subtle.  Apparently Serialize.decode can still throw an
            -- exception unless the contents of the Either is forced to whnf.
            val <- Exception.evaluate (Serialize.decode bytes)
                `Exception.catch` \(exc :: Exception.SomeException) ->
                    return $ Left $ "exception: " ++ show exc
            return $ Just <$> val

unserialize_text :: (Read a) => FilePath -> IO (Either String a)
unserialize_text fname = do
    ui_str <- IO.readFile fname
    result <- Exception.try $ readIO ui_str
    return $ case result of
        Left (exc :: Exception.SomeException) -> Left (show exc)
        Right val -> Right val


-- | Move the file to file.last.  Do this before writing a new one that may
-- fail.
backup_file :: FilePath -> IO ()
backup_file fname =
    void $ File.ignore_enoent $ Directory.renameFile fname (fname ++ ".last")

make_dir :: FilePath -> IO ()
make_dir = Directory.createDirectoryIfMissing True . FilePath.takeDirectory

-- * data types

data SaveState = SaveState {
    save_ui_state :: State.State
    , save_date :: Time.UTCTime
    } deriving (Show)

make_save_state :: State.State -> IO SaveState
make_save_state ui_state = do
    utc <- Time.getCurrentTime
    return (SaveState ui_state utc)

-- * binary instances

instance Serialize SaveState where
    put (SaveState a b) = Serialize.put_version 0
        >> put a >> put b
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                ui_state :: State.State <- get
                date :: Time.UTCTime <- get
                return (SaveState ui_state date)
            _ -> Serialize.bad_version "SaveState" v

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
    put (State.Config ns meta root midi transform instruments lilypond defaults)
        =  Serialize.put_version 6
            >> put ns >> put meta >> put root >> put (Configs midi)
            >> put transform >> put instruments >> put lilypond >> put defaults
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            ns :: Id.Namespace <- get
            _dir :: String <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            defaults :: State.Default <- get
            return $ State.Config ns State.empty_meta root midi "" mempty
                Lilypond.default_config defaults
        1 -> do
            ns :: Id.Namespace <- get
            _dir :: String <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: String <- get
            defaults :: State.Default <- get
            return $ State.Config ns State.empty_meta root midi
                (txt transform) mempty Lilypond.default_config defaults
        2 -> do
            ns :: Id.Namespace <- get
            _dir :: String <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: String <- get
            defaults :: State.Default <- get
            return $ State.Config ns meta root midi (txt transform) mempty
                Lilypond.default_config defaults
        3 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: String <- get
            defaults :: State.Default <- get
            return $ State.Config ns meta root midi (txt transform) mempty
                Lilypond.default_config defaults
        4 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: String <- get
            instruments :: Map.Map Score.Instrument Score.Instrument <- get
            defaults :: State.Default <- get
            return $ State.Config ns meta root midi (txt transform) instruments
                Lilypond.default_config defaults
        5 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: String <- get
            instruments :: Map.Map Score.Instrument Score.Instrument <- get
            lilypond :: Lilypond.Config <- get
            defaults :: State.Default <- get
            return $ State.Config ns meta root midi (txt transform) instruments
                lilypond defaults
        6 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: Text <- get
            instruments :: Map.Map Score.Instrument Score.Instrument <- get
            lilypond :: Lilypond.Config <- get
            defaults :: State.Default <- get
            return $ State.Config ns meta root midi transform instruments
                lilypond defaults
        _ -> Serialize.bad_version "State.Config" v

instance Serialize State.Meta where
    put (State.Meta a b) = Serialize.put_version 0 >> put a >> put b
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            creation :: Time.UTCTime <- get
            notes :: String <- get
            return $ State.Meta creation notes
        _ -> Serialize.bad_version "State.Meta" v

instance Serialize State.Default where
    put (State.Default a b c d) =
        Serialize.put_version 1 >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            1 -> do
                scale :: Pitch.ScaleId <- get
                key :: Maybe Pitch.Key <- get
                inst :: Maybe Score.Instrument <- get
                tempo :: Signal.Y <- get
                return $ State.Default scale key inst tempo
            _ -> Serialize.bad_version "State.Default" v

-- ** Block

instance Serialize Block.Block where
    -- Config is not serialized because everything in the block config is
    -- either derived from the Cmd.State or is hardcoded.
    put (Block.Block a _config b c d e f) = Serialize.put_version 9
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = do
        v <- Serialize.get_version
        case v of
            7 -> do
                title :: String <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                _integrated ::
                    Maybe (BlockId, NonEmpty OldTrackDestination) <- get
                _itracks :: [(TrackId, NonEmpty OldTrackDestination)] <- get
                _meta :: Map.Map String String <- get
                return $ Block.Block (txt title) Block.default_config tracks
                    skel Nothing [] mempty
            8 -> do
                title :: String <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                integrated ::
                    Maybe (BlockId, NonEmpty Block.TrackDestination) <- get
                itracks :: [(TrackId, NonEmpty Block.TrackDestination)] <- get
                _meta :: Map.Map String String <- get
                return $ Block.Block (txt title) Block.default_config tracks
                    skel integrated itracks mempty
            9 -> do
                title :: Text <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                integrated ::
                    Maybe (BlockId, NonEmpty Block.TrackDestination) <- get
                itracks :: [(TrackId, NonEmpty Block.TrackDestination)] <- get
                meta :: Map.Map Text Text <- get
                return $ Block.Block title Block.default_config tracks skel
                    integrated itracks meta
            _ -> Serialize.bad_version "Block.Block" v

instance Serialize OldTrackDestination where
    put (OldTrackDestination a b) = put a >> put b
    get = do
        note :: (TrackId, Block.EventIndex) <- get
        controls :: (Map.Map String (TrackId, Block.EventIndex)) <- get
        return $ OldTrackDestination note controls

-- | This holds the 'EventIndex' for one track or block.
data OldTrackDestination = OldTrackDestination
    (TrackId, Block.EventIndex) (Map.Map String (TrackId, Block.EventIndex))

instance Serialize Block.TrackDestination where
    put (Block.TrackDestination a b) = put a >> put b
    get = do
        note :: (TrackId, Block.EventIndex) <- get
        controls :: (Map.Map Text (TrackId, Block.EventIndex)) <- get
        return $ Block.TrackDestination note controls

instance Serialize Skeleton.Skeleton where
    put (Skeleton.Skeleton a) = put a
    get = get >>= \a -> return (Skeleton.Skeleton a)

instance Serialize Block.Track where
    put (Block.Track a b c d) = Serialize.put_version 2
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            1 -> do
                id :: Block.TracklikeId <- get
                width :: Types.Width <- get
                flags :: [Block.TrackFlag] <- get
                merged :: [Types.TrackId] <- get
                return $ Block.Track id width (Set.fromList flags) merged
            2 -> do
                id :: Block.TracklikeId <- get
                width :: Types.Width <- get
                flags :: Set.Set Block.TrackFlag <- get
                merged :: [Types.TrackId] <- get
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
    get = do
        color :: Color.Color <- get
        return $ Block.Divider color

instance Serialize Block.View where
    put (Block.View a b c d e f g h) = Serialize.put_version 5
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                block :: Types.BlockId <- get
                rect :: Rect.Rect <- get
                visible_track :: Int <- get
                visible_time :: Int <- get
                _status :: Map.Map String String <- get
                track_scroll :: Types.Width <- get
                zoom :: Types.Zoom <- get
                selections :: Map.Map Types.SelNum Types.Selection <- get
                return $ Block.View block rect visible_track visible_time
                    mempty track_scroll zoom selections
            4 -> do
                block :: Types.BlockId <- get
                rect :: Rect.Rect <- get
                visible_track :: Int <- get
                visible_time :: Int <- get
                _status :: Map.Map (Int, String) String <- get
                track_scroll :: Types.Width <- get
                zoom :: Types.Zoom <- get
                selections :: Map.Map Types.SelNum Types.Selection <- get
                return $ Block.View block rect visible_track visible_time
                    mempty track_scroll zoom selections
            5 -> do
                block :: Types.BlockId <- get
                rect :: Rect.Rect <- get
                visible_track :: Int <- get
                visible_time :: Int <- get
                status :: Map.Map (Int, Text) Text <- get
                track_scroll :: Types.Width <- get
                zoom :: Types.Zoom <- get
                selections :: Map.Map Types.SelNum Types.Selection <- get
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

instance Serialize Types.Selection where
    put (Types.Selection a b c d) = put a >> put b >> put c >> put d
    get = do
        strack :: Int <- get
        stime :: ScoreTime <- get
        ctrack :: Int <- get
        ctime :: ScoreTime <- get
        return $ Types.Selection strack stime ctrack ctime

-- ** Types, Color, Font

instance Serialize Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

-- ** Ruler

instance Serialize Ruler.Ruler where
    put (Ruler.Ruler a b c d) = Serialize.put_version 5
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            2 -> do
                marklists :: Map.Map String Marklist0 <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                _use_alpha :: Bool <- get
                align_to_bottom :: Bool <- get
                _full_width :: Bool <- get
                return $ Ruler.Ruler (convert marklists)
                    bg show_names align_to_bottom
            3 -> do
                marklists :: Map.Map String Marklist0 <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler (convert marklists)
                    bg show_names align_to_bottom
            4 -> do
                marklists :: Map.Map Ruler.Name Marklist1 <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler (Map.map convert_marklist1 marklists)
                    bg show_names align_to_bottom
            5 -> do
                marklists :: Map.Map Ruler.Name Ruler.Marklist <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler marklists bg show_names align_to_bottom
            _ -> Serialize.bad_version "Ruler.Ruler" v
        where
        convert = Map.fromList . map (txt *** convert_marklist0) . Map.toList

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

convert_marklist1 :: Marklist1 -> Ruler.Marklist
convert_marklist1 (Marklist1 marks) = Ruler.marklist $ Map.toAscList marks

newtype Marklist1 = Marklist1 (Map.Map ScoreTime Ruler.Mark)
    deriving (Serialize)

convert_marklist0 :: Marklist0 -> Ruler.Marklist
convert_marklist0 (Marklist0 marks) =
    Ruler.marklist $ Map.toAscList $ Map.map convert marks
    where
    convert (Mark0 rank width color name name_zoom zoom) = Ruler.Mark
        rank width color (txt name) name_zoom zoom

data Marklist0 = Marklist0 (Map.Map ScoreTime Mark0)
data Mark0 = Mark0 Int Int Color.Color String Double Double

instance Serialize Marklist0 where
    put _ = error "can't serialize Marklist0"
    get = do
        marks :: Map.Map ScoreTime Mark0 <- get
        return $ Marklist0 marks

instance Serialize Mark0 where
    put _ = error "can't serialize Mark0"
    get = do
        rank :: Int <- get
        width :: Int <- get
        color :: Color.Color <- get
        name :: String <- get
        name_zoom :: Double <- get
        zoom :: Double <- get
        return $ Mark0 rank width color name name_zoom zoom

-- ** Track

instance Serialize Track.Track where
    put (Track.Track a b c d) = Serialize.put_version 4 >>
        put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                title :: String <- get
                events :: Events.Events <- get
                color :: Color.Color <- get
                render :: Track.RenderConfig <- get
                return $ Track.Track (txt title) events color render
            4 -> do
                title :: Text <- get
                events :: Events.Events <- get
                color :: Color.Color <- get
                render :: Track.RenderConfig <- get
                return $ Track.Track title events color render
            _ -> Serialize.bad_version "Track.Track" v

instance Serialize Track.RenderConfig where
    put (Track.RenderConfig a b) = Serialize.put_version 0 >> put a >> put b
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                style :: Track.RenderStyle <- get
                color :: Color.Color <- get
                return $ Track.RenderConfig style color
            _ -> Serialize.bad_version "Track.RenderConfig" v

instance Serialize Track.RenderStyle where
    put Track.NoRender = put_tag 0
    put Track.Line = put_tag 1
    put Track.Filled = put_tag 2
    get = do
        tag <- get_tag
        case tag of
            0 -> return Track.NoRender
            1 -> return Track.Line
            2 -> return Track.Filled
            _ -> bad_tag "Track.RenderStyle" tag

-- ** Midi.Instrument

-- | It's a type synonym, but Serialize needs a newtype.
newtype Configs = Configs Instrument.Configs

instance Serialize Configs where
    put (Configs a) = Serialize.put_version 4 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                alloc :: Map.Map Score.Instrument [Instrument.Addr] <- get
                return $ Configs $ Instrument.configs $ Map.toList alloc
            4 -> do
                insts :: Map.Map Score.Instrument Instrument.Config <- get
                return $ Configs insts
            _ -> Serialize.bad_version "Instrument.Configs" v

instance Serialize Instrument.Config where
    put (Instrument.Config a b c) = Serialize.put_version 0
        >> put a >> put b >> put c
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                addrs :: [Instrument.Addr] <- get
                mute :: Bool <- get
                solo :: Bool <- get
                return $ Instrument.Config addrs mute solo
            _ -> Serialize.bad_version "Instrument.Config" v

instance Serialize Score.Instrument where
    put (Score.Instrument a) = put a
    get = fmap Score.Instrument get

-- ** Midi

instance Serialize Midi.ReadDevice where
    put = put . Midi.read_device_string
    get = get >>= \a -> return (Midi.read_device a)

instance Serialize Midi.WriteDevice where
    put = put . Midi.write_device_string
    get = get >>= \a -> return (Midi.write_device a)

instance Serialize Midi.Message where
    put (Midi.ChannelMessage a b) = put_tag 0 >> put a >> put b
    put (Midi.CommonMessage a) = put_tag 1 >> put a
    put (Midi.RealtimeMessage a) = put_tag 2 >> put a
    put (Midi.UnknownMessage a b c) = put_tag 3 >> put a >> put b >> put c
    get = do
        tag <- get_tag
        case tag of
            0 -> get >>= \a -> get >>= \b -> return (Midi.ChannelMessage a b)
            1 -> get >>= \a -> return (Midi.CommonMessage a)
            2 -> get >>= \a -> return (Midi.RealtimeMessage a)
            3 -> get >>= \a -> get >>= \b -> get >>= \c ->
                return (Midi.UnknownMessage a b c)
            _ -> bad_tag "Midi.Message" tag

instance Serialize Midi.ChannelMessage where
    put (Midi.NoteOff a b) = put_tag 0 >> put a >> put b
    put (Midi.NoteOn a b) = put_tag 1 >> put a >> put b
    put (Midi.Aftertouch a b) = put_tag 2 >> put a >> put b
    put (Midi.ControlChange a b) = put_tag 3 >> put a >> put b
    put (Midi.ProgramChange a) = put_tag 4 >> put a
    put (Midi.ChannelPressure a) = put_tag 5 >> put a
    put (Midi.PitchBend a) = put_tag 6 >> put a
    put Midi.AllSoundOff = put_tag 7
    put Midi.ResetAllControls = put_tag 8
    put (Midi.LocalControl a) = put_tag 9 >> put a
    put Midi.AllNotesOff = put_tag 10
    put (Midi.UndefinedChannelMode a b) = put_tag 11 >> put a >> put b
    get = do
        tag <- get_tag
        case tag of
            0 -> get >>= \a -> get >>= \b -> return (Midi.NoteOff a b)
            1 -> get >>= \a -> get >>= \b -> return (Midi.NoteOn a b)
            2 -> get >>= \a -> get >>= \b -> return (Midi.Aftertouch a b)
            3 -> get >>= \a -> get >>= \b -> return (Midi.ControlChange a b)
            4 -> get >>= \a -> return (Midi.ProgramChange a)
            5 -> get >>= \a -> return (Midi.ChannelPressure a)
            6 -> get >>= \a -> return (Midi.PitchBend a)
            7 -> return Midi.AllSoundOff
            8 -> return Midi.ResetAllControls
            9 -> get >>= \a -> return (Midi.LocalControl a)
            10 -> return Midi.AllNotesOff
            11 -> get >>= \a -> get >>= \b ->
                return (Midi.UndefinedChannelMode a b)
            _ -> bad_tag "Midi.ChannelMessage" tag

instance Serialize Midi.CommonMessage where
    put (Midi.SystemExclusive a b) = put_tag 0 >> put a >> put b
    put (Midi.SongPositionPointer a) = put_tag 1 >> put a
    put (Midi.SongSelect a) = put_tag 2 >> put a
    put Midi.TuneRequest = put_tag 3
    put Midi.EOX = put_tag 4
    put (Midi.UndefinedCommon a) = put_tag 5 >> put a
    get = do
        tag <- get_tag
        case tag of
            0 -> get >>= \a -> get >>= \b -> return (Midi.SystemExclusive a b)
            1 -> get >>= \a -> return (Midi.SongPositionPointer a)
            2 -> get >>= \a -> return (Midi.SongSelect a)
            3 -> return Midi.TuneRequest
            4 -> return Midi.EOX
            5 -> get >>= \a -> return (Midi.UndefinedCommon a)
            _ -> bad_tag "Midi.CommonMessage" tag

instance Serialize Midi.RealtimeMessage where
    put Midi.TimingClock = put_tag 0
    put Midi.Start = put_tag 1
    put Midi.Continue = put_tag 2
    put Midi.Stop = put_tag 3
    put Midi.ActiveSense = put_tag 4
    put Midi.Reset = put_tag 5
    put (Midi.UndefinedRealtime a) = put_tag 6 >> put a
    get = do
        tag <- get_tag
        case tag of
            0 -> return Midi.TimingClock
            1 -> return Midi.Start
            2 -> return Midi.Continue
            3 -> return Midi.Stop
            4 -> return Midi.ActiveSense
            5 -> return Midi.Reset
            6 -> get >>= \a -> return (Midi.UndefinedRealtime a)
            _ -> bad_tag "Midi.RealtimeMessage" tag

instance Serialize Midi.Key where
    put (Midi.Key a) = put a
    get = Midi.Key <$> get

-- ** lilypond

instance Serialize Lilypond.Config where
    put (Lilypond.Config a b c d) = Serialize.put_version 2
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                quarter :: RealTime <- get
                quantize :: Lilypond.Duration <- get
                dotted_rests :: Bool <- get
                staves :: [(Score.Instrument, String, String)] <- get
                let configs =
                        [staff inst (txt a) (txt b) | (inst, a, b) <- staves]
                return $ Lilypond.Config quarter quantize dotted_rests configs
            1 -> do
                quarter :: RealTime <- get
                quantize :: Lilypond.Duration <- get
                dotted_rests :: Bool <- get
                staves :: [(Score.Instrument, Lilypond.Instrument,
                    Lilypond.Instrument)] <- get
                return $ Lilypond.Config quarter quantize dotted_rests
                    [staff inst a b | (inst, a, b) <- staves]
            2 -> do
                quarter :: RealTime <- get
                quantize :: Lilypond.Duration <- get
                dotted_rests :: Bool <- get
                staves :: [(Score.Instrument, Lilypond.StaffConfig)] <- get
                return $ Lilypond.Config quarter quantize dotted_rests staves
            _ -> Serialize.bad_version "Lilypond.Config" v
        where
        staff inst a b = (inst, Lilypond.empty_staff_config
            { Lilypond.staff_long = a
            , Lilypond.staff_short = b
            })

instance Serialize Lilypond.StaffConfig where
    put (Lilypond.StaffConfig a b c d) = Serialize.put_version 1
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            0 -> do
                long :: Lilypond.Instrument <- get
                short :: Lilypond.Instrument <- get
                code :: [Text] <- get
                return $ Lilypond.StaffConfig long short code True
            1 -> do
                long :: Lilypond.Instrument <- get
                short :: Lilypond.Instrument <- get
                code :: [Text] <- get
                display :: Bool <- get
                return $ Lilypond.StaffConfig long short code display
            _ -> Serialize.bad_version "Lilypond.StaffConfig" v

instance Serialize Lilypond.Duration where
    put = put . fromEnum
    get = toEnum <$> get

-- ** misc

instance Serialize Time.UTCTime where
    put time = put (show time)
    get = get >>= return . read
