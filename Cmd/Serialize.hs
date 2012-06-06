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
import Data.Serialize (Get, Put, getWord8, putWord8)
import qualified Data.Time as Time
import qualified Data.Word as Word

import qualified System.IO as IO

import Util.Control
import qualified Util.File as File
import qualified Util.PPrint as PPrint
import qualified Util.Rect as Rect
import qualified Util.Serialize as Serialize
import Util.Serialize (Serialize, get, put)

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Style as Style
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Types


serialize :: (Serialize a) => FilePath -> a -> IO ()
serialize fname state = do
    File.backup_file fname
    ByteString.writeFile fname $ Serialize.encode state

serialize_text :: (Show a) => FilePath -> a -> IO ()
serialize_text fname state = do
    File.backup_file fname
    IO.writeFile fname (show state ++ "\n")

-- | Like 'serialize_text' but pretty-print it.  Probably not suitable for
-- giant things.
serialize_pretty_text :: (Show a) => FilePath -> a -> IO ()
serialize_pretty_text fname state = do
    File.backup_file fname
    IO.writeFile fname (PPrint.pshow state)

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


-- * data types

data SaveState = SaveState {
    save_ui_state :: State.State
    , save_date :: Time.UTCTime
    -- undo-related metadata?
    } deriving (Read, Show)
save_state ui_state = do
    utc <- Time.getCurrentTime
    return (SaveState ui_state utc)

put_version :: Word.Word8 -> Put
put_version n = putWord8 n

get_version :: Get Word.Word8
get_version = getWord8

version_error :: String -> Word.Word8 -> a
version_error typ ver = error $
    "unknown version " ++ show ver ++ " for " ++ show typ

-- * binary instances

instance Serialize SaveState where
    put (SaveState a b) = put_version 0
        >> put a >> put b
    get = do
        v <- get_version
        case v of
            0 -> do
                ui_state <- get :: Get State.State
                date <- get :: Get Time.UTCTime
                return (SaveState ui_state date)
            _ -> version_error "SaveState" v

instance Serialize State.State where
    put (State.State a b c d e) = put_version 6
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            6 -> do
                views <- get :: Get (Map.Map Types.ViewId Block.View)
                blocks <- get :: Get (Map.Map Types.BlockId Block.Block)
                tracks <- get :: Get (Map.Map Types.TrackId Track.Track)
                rulers <- get :: Get (Map.Map Types.RulerId Ruler.Ruler)
                config <- get :: Get State.Config
                return $ State.State views blocks tracks rulers config
            _ -> version_error "State.State" v

instance Serialize State.Config where
    put (State.Config a b c d e) = put_version 0
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            0 -> do
                ns <- get :: Get Id.Namespace
                dir <- get :: Get String
                root <- get :: Get (Maybe BlockId)
                midi <- get :: Get Instrument.Config
                defaults <- get :: Get State.Default
                return $ State.Config ns dir root midi defaults
            _ -> version_error "State.Config" v

instance Serialize State.Default where
    put (State.Default a b c d) =
        put_version 1 >> put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            0 -> do
                scale <- get :: Get Pitch.ScaleId
                inst <- get :: Get (Maybe Score.Instrument)
                tempo <- get :: Get Signal.Y
                return $ State.Default scale Nothing inst tempo
            1 -> do
                scale <- get :: Get Pitch.ScaleId
                key <- get :: Get (Maybe Pitch.Key)
                inst <- get :: Get (Maybe Score.Instrument)
                tempo <- get :: Get Signal.Y
                return $ State.Default scale key inst tempo
            _ -> version_error "State.Default" v

instance Serialize Id.Id where
    put = put . Id.un_id
    get = get >>= \(a, b) -> return (Id.unsafe_id a b)

instance Serialize Id.Namespace where
    put = put . Id.un_namespace
    get = get >>= \a -> return (Id.unsafe_namespace a)

-- ** Block

instance Serialize Types.BlockId where
    put (Types.BlockId a) = put a
    get = get >>= \a -> return (Types.BlockId a)

instance Serialize Types.ViewId where
    put (Types.ViewId a) = put a
    get = get >>= \a -> return (Types.ViewId a)

instance Serialize Block.Block where
    put (Block.Block a _config b c d) = put_version 5
        >> put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            4 -> do
                title <- get :: Get String
                tracks <- get :: Get [Block.Track]
                skel <- get :: Get Skeleton.Skeleton
                -- Everything in the block config is either derived from the
                -- Cmd.State or is hardcoded.
                return $ Block.Block title Block.default_config tracks skel
                    Map.empty
            5 -> do
                title <- get :: Get String
                tracks <- get :: Get [Block.Track]
                skel <- get :: Get Skeleton.Skeleton
                -- Everything in the block config is either derived from the
                -- Cmd.State or is hardcoded.
                meta <- get :: Get (Map.Map String String)
                return $ Block.Block title Block.default_config tracks skel
                    meta
            _ -> version_error "Block.Block" v

instance Serialize Skeleton.Skeleton where
    put (Skeleton.Skeleton a) = put a
    get = get >>= \a -> return (Skeleton.Skeleton a)

instance Serialize Block.Track where
    put (Block.Track a b c d) = put_version 1
        >> put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            1 -> do
                id <- get :: Get Block.TracklikeId
                width <- get :: Get Types.Width
                flags <- get :: Get [Block.TrackFlag]
                merged <- get :: Get [Types.TrackId]
                return $ Block.Track id width flags merged
            _ -> version_error "Block.Track" v

instance Serialize Block.TrackFlag where
    put (Block.Collapse) = putWord8 0
    put (Block.Solo) = putWord8 1
    put (Block.Mute) = putWord8 2
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Block.Collapse
            1 -> return Block.Solo
            2 -> return Block.Mute
            _ -> fail "no parse for Block.TrackFlag"

instance Serialize Block.TracklikeId where
    put (Block.TId a b) = putWord8 0 >> put a >> put b
    put (Block.RId a) = putWord8 1 >> put a
    put (Block.DId a) = putWord8 2 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> do
                tid <- get :: Get TrackId
                rid <- get :: Get RulerId
                return $ Block.TId tid rid
            1 -> do
                rid <- get :: Get RulerId
                return $ Block.RId rid
            2 -> do
                div <- get :: Get Block.Divider
                return $ Block.DId div
            _ -> fail "no parse for Block.TracklikeId"

instance Serialize Block.Divider where
    put (Block.Divider a) = put a
    get = do
        color <- get :: Get Color.Color
        return $ Block.Divider color

instance Serialize Block.View where
    put (Block.View a b c d e f g h) = put_version 3
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
    get = do
        v <- get_version
        case v of
            3 -> do
                block <- get :: Get Types.BlockId
                rect <- get :: Get Rect.Rect
                visible_track <- get :: Get Int
                visible_time <- get :: Get Int
                status <- get :: Get (Map.Map String String)
                track_scroll <- get :: Get Types.Width
                zoom <- get :: Get Types.Zoom
                selections <- get :: Get (Map.Map Types.SelNum Types.Selection)
                return $ Block.View block rect visible_track visible_time
                    status track_scroll zoom selections
            _ -> version_error "Block.View" v

instance Serialize Rect.Rect where
    put r = put (Rect.rx r) >> put (Rect.ry r) >> put (Rect.rw r)
        >> put (Rect.rh r)
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Rect.xywh a b c d)

instance Serialize Types.Zoom where
    put (Types.Zoom a b) = put a >> put b
    get = do
        offset <- get :: Get ScoreTime
        factor <- get :: Get Double
        return $ Types.Zoom offset factor

instance Serialize Types.Selection where
    put (Types.Selection a b c d) = put a >> put b >> put c >> put d
    get = do
        strack <- get :: Get Int
        stime <- get :: Get ScoreTime
        ctrack <- get :: Get Int
        ctime <- get :: Get ScoreTime
        return $ Types.Selection strack stime ctrack ctime

-- ** Types, Color, Font

instance Serialize Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

-- ** Ruler

instance Serialize Types.RulerId where
    put (Types.RulerId a) = put a
    get = get >>= \a -> return (Types.RulerId a)

instance Serialize Ruler.Ruler where
    put (Ruler.Ruler a b c d e f) = put_version 2
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = do
        v <- get_version
        case v of
            2 -> do
                marklists <- get :: Get (Map.Map Ruler.Name Ruler.Marklist)
                bg <- get :: Get Color.Color
                show_names <- get :: Get Bool
                use_alpha <- get :: Get Bool
                align_to_bottom <- get :: Get Bool
                full_width <- get :: Get Bool
                return $ Ruler.Ruler marklists bg show_names use_alpha
                    align_to_bottom full_width
            _ -> version_error "Ruler.Ruler" v

instance Serialize Ruler.Marklist where
    put (Ruler.Marklist a) = put a
    get = do
        m <- get :: Get (Map.Map ScoreTime Ruler.Mark)
        return $ Ruler.Marklist m

instance Serialize Ruler.Mark where
    put (Ruler.Mark a b c d e f) = put a >> put b >> put c >> put d >> put e
        >> put f
    get = do
        rank <- get :: Get Int
        width <- get :: Get Int
        color <- get :: Get Color.Color
        name <- get :: Get String
        name_zoom <- get :: Get Double
        zoom <- get :: Get Double
        return $ Ruler.Mark rank width color name name_zoom zoom

-- ** Track

instance Serialize Types.TrackId where
    put (Types.TrackId a) = put a
    get = get >>= \a -> return (Types.TrackId a)

instance Serialize Track.Track where
    put (Track.Track a b c d) = put_version 1 >>
        put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            1 -> do
                title <- get :: Get String
                events <- get :: Get Events.Events
                color <- get :: Get Color.Color
                render <- get :: Get Track.RenderConfig
                return $ Track.Track title events color render
            _ -> version_error "Track.Track" v

instance Serialize Track.RenderConfig where
    put (Track.RenderConfig a b) = put_version 0 >> put a >> put b
    get = do
        v <- get_version
        case v of
            0 -> do
                style <- get :: Get Track.RenderStyle
                color <- get :: Get Color.Color
                return $ Track.RenderConfig style color
            _ -> version_error "Track.RenderConfig" v

instance Serialize Track.RenderStyle where
    put Track.NoRender = putWord8 0
    put Track.Line = putWord8 1
    put Track.Filled = putWord8 2
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Track.NoRender
            1 -> return Track.Line
            2 -> return Track.Filled
            _ -> fail "no parse for Track.RenderStyle"

instance Serialize Events.Events where
    put (Events.Events a) = put_version 0 >> put a
    get = do
        v <- get_version
        case v of
            0 -> do
                events <- get :: Get (Map.Map ScoreTime Event.Event)
                return $ Events.Events events
            _ -> version_error "Events.Events" v

-- ** Event

instance Serialize Event.Event where
    put (Event.Event text dur style) = do
        put text
        put dur
        put style
    get = do
        text <- get :: Get ByteString.ByteString
        dur <- get :: Get ScoreTime
        style <- get :: Get Style.StyleId
        return $ Event.Event text dur style

-- ** Midi.Instrument

instance Serialize Instrument.Config where
    put (Instrument.Config a) = put_version 3 >> put a
    get = do
        v <- get_version
        case v of
            3 -> do
                alloc <- get :: Get
                    (Map.Map Score.Instrument [Instrument.Addr])
                return $ Instrument.Config alloc
            4 -> do
                alloc <- get :: Get
                    (Map.Map Score.Instrument [Instrument.Addr])
                _ <- get :: Get (Map.Map Midi.WriteDevice Midi.WriteDevice)
                return $ Instrument.Config alloc
            _ -> version_error "Instrument.Config" v

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
    put (Midi.ChannelMessage a b) = putWord8 0 >> put a >> put b
    put (Midi.CommonMessage a) = putWord8 1 >> put a
    put (Midi.RealtimeMessage a) = putWord8 2 >> put a
    put (Midi.UnknownMessage a b c) = putWord8 3 >> put a >> put b >> put c
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> get >>= \a -> get >>= \b -> return (Midi.ChannelMessage a b)
            1 -> get >>= \a -> return (Midi.CommonMessage a)
            2 -> get >>= \a -> return (Midi.RealtimeMessage a)
            3 -> get >>= \a -> get >>= \b -> get >>= \c ->
                return (Midi.UnknownMessage a b c)
            _ -> fail "no parse for Midi.Message"

instance Serialize Midi.ChannelMessage where
    put (Midi.NoteOff a b) = putWord8 0 >> put a >> put b
    put (Midi.NoteOn a b) = putWord8 1 >> put a >> put b
    put (Midi.Aftertouch a b) = putWord8 2 >> put a >> put b
    put (Midi.ControlChange a b) = putWord8 3 >> put a >> put b
    put (Midi.ProgramChange a) = putWord8 4 >> put a
    put (Midi.ChannelPressure a) = putWord8 5 >> put a
    put (Midi.PitchBend a) = putWord8 6 >> put a
    put Midi.AllSoundOff = putWord8 7
    put Midi.ResetAllControls = putWord8 8
    put (Midi.LocalControl a) = putWord8 9 >> put a
    put Midi.AllNotesOff = putWord8 10
    put (Midi.UndefinedChannelMode a b) = putWord8 11 >> put a >> put b
    get = do
        tag_ <- getWord8
        case tag_ of
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
            _ -> fail "no parse for Midi.ChannelMessage"

instance Serialize Midi.CommonMessage where
    put (Midi.SystemExclusive a b) = putWord8 0 >> put a >> put b
    put (Midi.SongPositionPointer a) = putWord8 1 >> put a
    put (Midi.SongSelect a) = putWord8 2 >> put a
    put Midi.TuneRequest = putWord8 3
    put Midi.EOX = putWord8 4
    put (Midi.UndefinedCommon a) = putWord8 5 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> get >>= \a -> get >>= \b -> return (Midi.SystemExclusive a b)
            1 -> get >>= \a -> return (Midi.SongPositionPointer a)
            2 -> get >>= \a -> return (Midi.SongSelect a)
            3 -> return Midi.TuneRequest
            4 -> return Midi.EOX
            5 -> get >>= \a -> return (Midi.UndefinedCommon a)
            _ -> fail "no parse for Midi.CommonMessage"

instance Serialize Midi.RealtimeMessage where
    put Midi.TimingClock = putWord8 0
    put Midi.Start = putWord8 1
    put Midi.Continue = putWord8 2
    put Midi.Stop = putWord8 3
    put Midi.ActiveSense = putWord8 4
    put Midi.Reset = putWord8 5
    put (Midi.UndefinedRealtime a) = putWord8 6 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Midi.TimingClock
            1 -> return Midi.Start
            2 -> return Midi.Continue
            3 -> return Midi.Stop
            4 -> return Midi.ActiveSense
            5 -> return Midi.Reset
            6 -> get >>= \a -> return (Midi.UndefinedRealtime a)
            _ -> fail "no parse for Midi.RealtimeMessage"

-- ** misc

instance Serialize Time.UTCTime where
    put time = put (show time)
    get = get >>= return . read
