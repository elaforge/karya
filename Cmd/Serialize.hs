{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Extensions needed for the Binary Block.Block instance.
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

import qualified Data.Array.IArray as IArray
import qualified Data.Binary as Binary
import Data.Binary (Binary, Get, get, put, getWord8, putWord8)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified System.IO as IO

import qualified Util.File as File
import qualified Util.PPrint as PPrint
import qualified Util.Binary

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Font as Font
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument

import qualified App.Config as Config


serialize :: (Binary a) => FilePath -> a -> IO ()
serialize fname state = do
    File.backup_file fname
    Binary.encodeFile fname state

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

unserialize :: (Show a, Binary a) =>
    FilePath -> IO (Either Exception.SomeException a)
unserialize fname = Exception.try $ do
    st <- Binary.decodeFile fname
    -- Data.Binary is lazy, but I want errors parsing to get caught right here.
    -- The correct thing to do would be to use the binary-strict package, but
    -- it's not drop-in and this is expedient.
    -- Come to think of it, this probably kills off most of Data.Binary's
    -- vaunted blazing speed.
    length (show st) `seq` return st

unserialize_text :: (Read a) =>
    FilePath -> IO (Either Exception.SomeException a)
unserialize_text fname = do
    ui_str <- IO.readFile fname
    Exception.try $ readIO ui_str


-- * data types

data SaveState = SaveState {
    save_ui_state :: State.State
    , save_date :: Time.UTCTime
    -- undo-related metadata?
    } deriving (Read, Show)
save_state ui_state = do
    utc <- Time.getCurrentTime
    return (SaveState ui_state utc)

put_version n = Binary.putWord8 n
get_version = Binary.getWord8

throw = error
version_error typ ver = throw $
    "unknown version " ++ show ver ++ " for " ++ show typ

-- * binary instances

instance Binary SaveState where
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

instance Binary State.State where
    put (State.State a b c d e f g h i j) = put_version 4
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
        >> put i >> put j
    get = do
        v <- get_version
        case v of
            3 -> do
                proj <- get :: Get String
                dir <- get :: Get String
                views <- get :: Get (Map.Map Types.ViewId Block.View)
                blocks <- get :: Get (Map.Map Types.BlockId Block.Block)
                tracks <- get :: Get (Map.Map Types.TrackId Track.Track)
                rulers <- get :: Get (Map.Map Types.RulerId Ruler.Ruler)
                midi_config <- get :: Get Instrument.Config
                default_scale <- get :: Get Pitch.ScaleId
                default_inst <- get :: Get (Maybe Score.Instrument)
                return $ State.State proj dir Nothing views blocks tracks
                    rulers midi_config default_scale default_inst
            4 -> do
                proj <- get :: Get String
                dir <- get :: Get String
                root <- get :: Get (Maybe BlockId)
                views <- get :: Get (Map.Map Types.ViewId Block.View)
                blocks <- get :: Get (Map.Map Types.BlockId Block.Block)
                tracks <- get :: Get (Map.Map Types.TrackId Track.Track)
                rulers <- get :: Get (Map.Map Types.RulerId Ruler.Ruler)
                midi_config <- get :: Get Instrument.Config
                default_scale <- get :: Get Pitch.ScaleId
                default_inst <- get :: Get (Maybe Score.Instrument)
                return $ State.State proj dir root views blocks tracks rulers
                    midi_config default_scale default_inst
            _ -> version_error "State.State" v

instance Binary Pitch.ScaleId where
    put (Pitch.ScaleId a) = put a
    get = get >>= \a -> return (Pitch.ScaleId a)

instance Binary Id.Id where
    put ident = put (Id.un_id ident)
    get = get >>= \(a, b) -> return (Id.id a b)

-- ** Block

instance Binary Types.BlockId where
    put (Types.BlockId a) = put a
    get = get >>= \a -> return (Types.BlockId a)

instance Binary Types.ViewId where
    put (Types.ViewId a) = put a
    get = get >>= \a -> return (Types.ViewId a)

instance Binary Types.SchemaId where
    put (Types.SchemaId a) = put a
    get = get >>= \a -> return (Types.SchemaId a)

instance Binary Block.Block where
    put (Block.Block a b c d e) = put_version 3
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            2 -> do
                title <- get :: Get String
                config <- get :: Get Block.Config
                track_widths <- get :: Get [(Block.TracklikeId, Types.Width)]
                schema_id <- get :: Get Types.SchemaId
                let tracks = map (uncurry Block.block_track) track_widths
                return $ Block.Block title config tracks Skeleton.empty
                    schema_id
            3 -> do
                title <- get :: Get String
                config <- get :: Get Block.Config
                tracks <- get :: Get [Block.BlockTrack]
                skel <- get :: Get Skeleton.Skeleton
                schema_id <- get :: Get Types.SchemaId
                return $ Block.Block title config tracks skel schema_id
            _ -> version_error "Block.Block" v

-- Everything in the block config is either derived from the Cmd.State or is
-- hardcoded.
instance Binary Block.Config where
    put _ = put ()
    get = do
        _ <- get :: Get ()
        return Block.default_config

instance Binary Skeleton.Skeleton where
    put (Skeleton.Skeleton a) = put a
    get = get >>= \a -> return (Skeleton.Skeleton a)

instance Binary Block.BlockTrack where
    put (Block.BlockTrack a b c d) = put_version 1
        >> put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            0 -> do
                id <- get :: Get Block.TracklikeId
                width <- get :: Get Types.Width
                _ <- get :: Get Bool
                _ <- get :: Get (Maybe TrackNum)
                _ <- get :: Get Bool
                _ <- get :: Get Bool
                return $ Block.BlockTrack id width [] []
            1 -> do
                id <- get :: Get Block.TracklikeId
                width <- get :: Get Types.Width
                flags <- get :: Get [Block.TrackFlag]
                merged <- get :: Get [Types.TrackId]
                return $ Block.BlockTrack id width flags merged
            _ -> version_error "Block.BlockTrack" v

instance Binary Block.TrackFlag where
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

tid = Block.TId :: Types.TrackId -> Types.RulerId -> Block.TracklikeId
rid = Block.RId :: Types.RulerId -> Block.TracklikeId
did = Block.DId :: Block.Divider -> Block.TracklikeId
instance Binary Block.TracklikeId where
    put (Block.TId a b) = putWord8 0 >> put a >> put b
    put (Block.RId a) = putWord8 1 >> put a
    put (Block.DId a) = putWord8 2 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> get >>= \a -> get >>= \b -> return (tid a b)
            1 -> get >>= \a -> return (rid a)
            2 -> get >>= \a -> return (did a)
            _ -> fail "no parse for Block.TracklikeId"

divider = Block.Divider :: Color.Color -> Block.Divider
instance Binary Block.Divider where
    put (Block.Divider a) = put a
    get = get >>= \a -> return (divider a)

instance Binary Block.View where
    put (Block.View a b c d e f g h i j) = put_version 1
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
        >> put i >> put j
    get = do
        v <- get_version
        case v of
            0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
                get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h ->
                return (Block.View a b 0 0 c d e f g h)
            1 -> do
                block <- get :: Get Types.BlockId
                rect <- get :: Get Types.Rect
                visible_track <- get :: Get Int
                visible_time <- get :: Get Int
                config <- get :: Get Block.ViewConfig
                status <- get :: Get (Map.Map String String)
                track_scroll <- get :: Get Types.Width
                zoom <- get :: Get Types.Zoom
                selections <- get :: Get (Map.Map Types.SelNum Types.Selection)
                tracks <- get :: Get [Block.TrackView]
                return $ Block.View block rect visible_track visible_time
                    config status track_scroll zoom selections tracks
            _ -> version_error "Block.View" v

track_view = Block.TrackView :: Types.Width -> Block.TrackView
instance Binary Block.TrackView where
    put (Block.TrackView a) = put a
    get = get >>= \a -> return (track_view a)

instance Binary Types.Rect where
    put (Types.Rect a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Types.Rect a b c d)

instance Binary Block.ViewConfig where
    put (Block.ViewConfig a b c d e) = put_version 2
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            2 -> do
                block_title <- get :: Get Int
                track_title <- get :: Get Int
                skel_height <- get :: Get Int
                sb_size <- get :: Get Int
                status_size <- get :: Get Int
                return $ Block.ViewConfig block_title track_title skel_height
                    sb_size status_size
            _ -> version_error "Block.ViewConfig" v

zoom = Types.Zoom :: ScoreTime -> Double -> Types.Zoom
instance Binary Types.Zoom where
    put (Types.Zoom a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (zoom a b)

selection = Types.Selection :: TrackNum -> ScoreTime -> TrackNum
    -> ScoreTime -> Types.Selection
instance Binary Types.Selection where
    put (Types.Selection a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (selection a b c d)

-- ** Types, Color, Font

instance Binary ScoreTime where
    put (Types.ScoreTime a) = put (Util.Binary.NDouble a)
    get = get >>= \(Util.Binary.NDouble a) -> return (Types.ScoreTime a)

instance Binary Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

text_style = Font.EventStyle :: Font.Font -> [Font.FontFace] -> Int
    -> Color.Color -> Font.EventStyle
instance Binary Font.EventStyle where
    put (Font.EventStyle a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (text_style a b c d)

-- TODO store as strings?
instance Binary Font.Font where
    put Font.Helvetica = putWord8 0
    put Font.Times = putWord8 1
    put Font.Courier = putWord8 2
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Font.Helvetica
            1 -> return Font.Times
            2 -> return Font.Courier
            _ -> fail "no parse for Font.Font"

instance Binary Font.FontFace where
    put Font.Bold = putWord8 0
    put Font.Italic = putWord8 1
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Font.Bold
            1 -> return Font.Italic
            _ -> fail "no parse for Font.FontFace"

-- ** Ruler

instance Binary Types.RulerId where
    put (Types.RulerId a) = put a
    get = get >>= \a -> return (Types.RulerId a)

instance Binary Ruler.Ruler where
    put (Ruler.Ruler a b c d e f) = put_version 1
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = do
        v <- get_version
        case v of
            0 -> do
                marklists <- get :: Get [Ruler.NameMarklist]
                bg <- get :: Get Color.Color
                show_names <- get :: Get Bool
                use_alpha <- get :: Get Bool
                full_width <- get :: Get Bool
                return $ Ruler.Ruler marklists bg show_names use_alpha False
                    full_width
            1 -> do
                marklists <- get :: Get [Ruler.NameMarklist]
                bg <- get :: Get Color.Color
                show_names <- get :: Get Bool
                use_alpha <- get :: Get Bool
                align_to_bottom <- get :: Get Bool
                full_width <- get :: Get Bool
                return $ Ruler.Ruler marklists bg show_names use_alpha
                    align_to_bottom full_width
            _ -> version_error "Ruler.Ruler" v

marklist = Ruler.Marklist :: IArray.Array Int Ruler.PosMark -> Ruler.Marklist
instance Binary Ruler.Marklist where
    put (Ruler.Marklist a) = put a
    get = get >>= \a -> return (marklist a)

mark = Ruler.Mark :: Int -> Int -> Color.Color -> String -> Double -> Double
    -> Ruler.Mark
instance Binary Ruler.Mark where
    put (Ruler.Mark a b c d e f) = put a >> put b >> put c >> put d >> put e
        >> put f
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e ->
        get >>= \f -> return (mark a b c d e f)

-- ** Track

instance Binary Types.TrackId where
    put (Types.TrackId a) = put a
    get = get >>= \a -> return (Types.TrackId a)

instance Binary Track.Track where
    put (Track.Track a b c d) = put_version 1 >>
        put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            0 -> do
                title <- get :: Get String
                events <- get :: Get Track.TrackEvents
                bg <- get :: Get Color.Color
                return $ Track.Track title events bg Config.render_config
            1 -> do
                title <- get :: Get String
                events <- get :: Get Track.TrackEvents
                bg <- get :: Get Color.Color
                render <- get :: Get Track.RenderConfig
                return $ Track.Track title events bg render
            _ -> version_error "Track.Track" v

instance Binary Track.RenderConfig where
    put (Track.RenderConfig a b) = put_version 0 >> put a >> put b
    get = do
        v <- get_version
        case v of
            0 -> do
                style <- get :: Get Track.RenderStyle
                color <- get :: Get Color.Color
                return $ Track.RenderConfig style color
            _ -> version_error "Track.RenderConfig" v

instance Binary Track.RenderStyle where
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

track_events = Track.TrackEvents :: Map.Map ScoreTime Event.Event
    -> Track.TrackEvents
instance Binary Track.TrackEvents where
    put (Track.TrackEvents a) = put_version 0 >> put a
    get = do
        v <- get_version
        case v of
            0 -> get >>= \a -> return (track_events a)
            _ -> version_error "Track.TrackEvents" v

-- ** Event

instance Binary Event.Event where
    put (Event.Event text dur style) = do
        put text
        put dur
        put style
    get = do
        text <- get :: Get ByteString.ByteString
        dur <- get :: Get ScoreTime
        style <- get :: Get Event.StyleId
        return $ Event.Event text dur style

instance Binary Event.StyleId where
    put (Event.StyleId a) = put a
    get = fmap Event.StyleId get

-- ** Midi.Instrument

instance Binary Instrument.Config where
    put (Instrument.Config a) = put_version 3 >> put a
    get = do
        v <- get_version
        case v of
            2 -> do
                alloc <- get :: Get (Map.Map Score.Instrument [Instrument.Addr])
                _ <- get :: Get (Maybe Score.Instrument)
                return $ Instrument.Config alloc
            3 -> do
                alloc <- get :: Get (Map.Map Score.Instrument [Instrument.Addr])
                return $ Instrument.Config alloc
            _ -> version_error "Instrument.Config" v

instance Binary Score.Instrument where
    put (Score.Instrument a) = put a
    get = fmap Score.Instrument get

-- ** Midi

instance Binary Midi.ReadDevice where
    put (Midi.ReadDevice a) = put a
    get = get >>= \a -> return (Midi.ReadDevice a)

instance Binary Midi.WriteDevice where
    put (Midi.WriteDevice a) = put a
    get = get >>= \a -> return (Midi.WriteDevice a)

instance Binary Midi.Message where
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

instance Binary Midi.ChannelMessage where
    put (Midi.NoteOff a b) = putWord8 0 >> put a >> put b
    put (Midi.NoteOn a b) = putWord8 1 >> put a >> put b
    put (Midi.Aftertouch a b) = putWord8 2 >> put a >> put b
    put (Midi.ControlChange a b) = putWord8 3 >> put a >> put b
    put (Midi.ProgramChange a) = putWord8 4 >> put a
    put (Midi.ChannelPressure a) = putWord8 5 >> put a
    put (Midi.PitchBend a) = putWord8 6 >> put a
    put Midi.AllSoundOff = putWord8 7
    put Midi.ResetAllCcontrols = putWord8 8
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
            8 -> return Midi.ResetAllCcontrols
            9 -> get >>= \a -> return (Midi.LocalControl a)
            10 -> return Midi.AllNotesOff
            11 -> get >>= \a -> get >>= \b ->
                return (Midi.UndefinedChannelMode a b)
            _ -> fail "no parse for Midi.ChannelMessage"

instance Binary Midi.CommonMessage where
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

instance Binary Midi.RealtimeMessage where
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

instance Binary Time.UTCTime where
    put time = put (show time)
    get = get >>= return . read
