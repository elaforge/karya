{-# LANGUAGE ScopedTypeVariables #-} -- needed for SomeException
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
import qualified Data.Time as Time

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import Util.Control
import qualified Util.File as File
import qualified Util.PPrint as PPrint
import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import Util.Serialize
       (Serialize, get, put, get_tag, put_tag, bad_tag)

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

unserialize :: (Serialize a) => FilePath -> IO (Either String a)
unserialize fname = do
    result <- Serialize.decode <$> ByteString.readFile fname
    -- This is subtle.  Apparently Serialize.decode can still throw an
    -- exception unless the contents of the Either is forced to whnf.
    case result of
        Left e -> return (Left e)
        Right e -> return (Right e)
    `Exception.catch` \(exc :: Exception.SomeException) ->
        return $ Left $ "exception: " ++ show exc

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
    }

save_state :: State.State -> IO SaveState
save_state ui_state = do
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
    put (State.Config a b c d e f g) = Serialize.put_version 2
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            ns :: Id.Namespace <- get
            dir :: String <- get
            root :: Maybe BlockId <- get
            midi :: Instrument.Config <- get
            defaults :: State.Default <- get
            return $ State.Config ns dir State.empty_meta root midi
                [] defaults
        1 -> do
            ns :: Id.Namespace <- get
            dir :: String <- get
            root :: Maybe BlockId <- get
            midi :: Instrument.Config <- get
            transform :: String <- get
            defaults :: State.Default <- get
            return $ State.Config ns dir State.empty_meta root midi
                transform defaults
        2 -> do
            ns :: Id.Namespace <- get
            dir :: String <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            midi :: Instrument.Config <- get
            transform :: String <- get
            defaults :: State.Default <- get
            return $ State.Config ns dir meta root midi transform defaults
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
    put (Block.Block a _config b c d e f) = Serialize.put_version 7
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = do
        v <- Serialize.get_version
        case v of
            7 -> do
                title :: String <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                integrated ::
                    Maybe (BlockId, NonEmpty Block.TrackDestination) <- get
                itracks :: [(TrackId, NonEmpty Block.TrackDestination)] <- get
                meta :: Map.Map String String <- get
                return $ Block.Block title Block.default_config tracks skel
                    integrated itracks meta
            _ -> Serialize.bad_version "Block.Block" v

instance Serialize Block.TrackDestination where
    put (Block.TrackDestination a b) = put a >> put b
    get = do
        note :: (TrackId, Block.EventIndex) <- get
        controls :: (Map.Map String (TrackId, Block.EventIndex)) <- get
        return $ Block.TrackDestination note controls

instance Serialize Skeleton.Skeleton where
    put (Skeleton.Skeleton a) = put a
    get = get >>= \a -> return (Skeleton.Skeleton a)

instance Serialize Block.Track where
    put (Block.Track a b c d) = Serialize.put_version 1
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            1 -> do
                id :: Block.TracklikeId <- get
                width :: Types.Width <- get
                flags :: [Block.TrackFlag] <- get
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
    put (Block.View a b c d e f g h) = Serialize.put_version 4
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
                status :: Map.Map (Int, String) String <- get
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
    put (Ruler.Ruler a b c d) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            2 -> do
                marklists :: Map.Map Ruler.Name Ruler.Marklist <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                _use_alpha :: Bool <- get
                align_to_bottom :: Bool <- get
                _full_width :: Bool <- get
                return $ Ruler.Ruler marklists bg show_names align_to_bottom
            3 -> do
                marklists :: Map.Map Ruler.Name Ruler.Marklist <- get
                bg :: Color.Color <- get
                show_names :: Bool <- get
                align_to_bottom :: Bool <- get
                return $ Ruler.Ruler marklists bg show_names align_to_bottom
            _ -> Serialize.bad_version "Ruler.Ruler" v

instance Serialize Ruler.Marklist where
    put mlist = put (Ruler.marklist_map mlist)
    get = do
        marks :: Map.Map ScoreTime Ruler.Mark <- get
        return $ Ruler.marklist marks

instance Serialize Ruler.Mark where
    put (Ruler.Mark a b c d e f) = put a >> put b >> put c >> put d >> put e
        >> put f
    get = do
        rank :: Int <- get
        width :: Int <- get
        color :: Color.Color <- get
        name :: String <- get
        name_zoom :: Double <- get
        zoom :: Double <- get
        return $ Ruler.Mark rank width color name name_zoom zoom

-- ** Track

instance Serialize Track.Track where
    put (Track.Track a b c d) = Serialize.put_version 3 >>
        put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                title :: String <- get
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

instance Serialize Instrument.Config where
    put (Instrument.Config a) = Serialize.put_version 3 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                alloc :: Map.Map Score.Instrument [Instrument.Addr] <- get
                return $ Instrument.Config alloc
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

-- ** misc

instance Serialize Time.UTCTime where
    put time = put (show time)
    get = get >>= return . read
