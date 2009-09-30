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
import Control.Monad

import qualified Data.Array.IArray as IArray
import qualified Data.Binary as Binary
import Data.Binary (Binary, Get, get, put, getWord8, putWord8)
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified System.IO as IO

import qualified Util.File as File

import Ui.Types
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Color as Color
import qualified Ui.Font as Font
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ruler as Ruler
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument

import qualified App.Config as Config


serialize :: FilePath -> SaveState -> IO ()
serialize fname state = do
    File.backup_file fname
    Binary.encodeFile fname state

serialize_text :: FilePath -> SaveState -> IO ()
serialize_text fname state = do
    File.backup_file fname
    IO.writeFile fname (show state ++ "\n")

unserialize :: FilePath -> IO (Either Exception.SomeException SaveState)
unserialize fname = Exception.try $ do
    st <- Binary.decodeFile fname
    -- Data.Binary is lazy, but I want errors parsing to get caught right here.
    -- The correct thing to do would be to use the binary-strict package, but
    -- it's not drop-in and this is expedient.
    -- Come to think of it, this probably kills off most of Data.Binary's
    -- vaunted blazing speed.
    length (show st) `seq` return st

unserialize_text :: FilePath -> IO (Either Exception.SomeException SaveState)
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

-- TODO This is provided by Data.Binary, but it's compiled against the stdlib
-- Data.Map and I have my own hacked version for the moment.  Remove this
-- later.
instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e) where
    put = put . Map.toAscList
    get = liftM Map.fromDistinctAscList get

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
    put (State.State a b c d e f g h) = put_version 2
        >> put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
    get = do
        v <- get_version
        case v of
            1 -> do
                proj <- get :: Get String
                dir <- get :: Get String
                views <- get :: Get (Map.Map Block.ViewId Block.View)
                blocks <- get :: Get (Map.Map Block.BlockId Block.Block)
                tracks <- get :: Get (Map.Map Track.TrackId Track.Track)
                rulers <- get :: Get (Map.Map Ruler.RulerId Ruler.Ruler)
                midi_config <- get :: Get Instrument.Config
                return $ State.State proj dir views blocks tracks rulers
                    midi_config (Pitch.ScaleId Config.project_scale_id)
            2 -> do
                proj <- get :: Get String
                dir <- get :: Get String
                views <- get :: Get (Map.Map Block.ViewId Block.View)
                blocks <- get :: Get (Map.Map Block.BlockId Block.Block)
                tracks <- get :: Get (Map.Map Track.TrackId Track.Track)
                rulers <- get :: Get (Map.Map Ruler.RulerId Ruler.Ruler)
                midi_config <- get :: Get Instrument.Config
                project_scale <- get :: Get Pitch.ScaleId
                return $ State.State proj dir views blocks tracks rulers
                    midi_config project_scale
            _ -> version_error "State.State" v

instance Binary Pitch.ScaleId where
    put (Pitch.ScaleId a) = put a
    get = get >>= \a -> return (Pitch.ScaleId a)

instance Binary Id.Id where
    put ident = put (Id.un_id ident)
    get = get >>= \(a, b) -> return (Id.id a b)

-- ** Block

instance Binary Block.BlockId where
    put (Block.BlockId a) = put a
    get = get >>= \a -> return (Block.BlockId a)

instance Binary Block.ViewId where
    put (Block.ViewId a) = put a
    get = get >>= \a -> return (Block.ViewId a)

instance Binary Block.SchemaId where
    put (Block.SchemaId a) = put a
    get = get >>= \a -> return (Block.SchemaId a)

instance Binary Block.Block where
    put (Block.Block a b c d e) = put_version 3
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            2 -> do
                title <- get :: Get String
                config <- get :: Get Block.Config
                track_widths <- get :: Get [(Block.TracklikeId, Block.Width)]
                schema_id <- get :: Get Block.SchemaId
                let tracks = map (uncurry Block.block_track) track_widths
                return $ Block.Block title config tracks Skeleton.empty
                    schema_id
            3 -> do
                title <- get :: Get String
                config <- get :: Get Block.Config
                tracks <- get :: Get [Block.BlockTrack]
                skel <- get :: Get Skeleton.Skeleton
                schema_id <- get :: Get Block.SchemaId
                return $ Block.Block title config tracks skel schema_id
            _ -> version_error "Block.Block" v

-- Everything in the block config is either derived from the Cmd.State or is
-- hardcoded.
instance Binary Block.Config where
    put _ = put ()
    get = do
        _ <- get :: Get ()
        return Config.block_config

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
                width <- get :: Get Block.Width
                _ <- get :: Get Bool
                _ <- get :: Get (Maybe Block.TrackNum)
                _ <- get :: Get Bool
                _ <- get :: Get Bool
                return $ Block.BlockTrack id width [] []
            1 -> do
                id <- get :: Get Block.TracklikeId
                width <- get :: Get Block.Width
                flags <- get :: Get [Block.TrackFlag]
                merged <- get :: Get [Track.TrackId]
                return $ Block.BlockTrack id width flags merged
            _ -> version_error "Block.BlockTrack" v

instance Binary Block.TrackFlag where
    put (Block.Hide) = putWord8 0
    put (Block.Solo) = putWord8 1
    put (Block.Mute) = putWord8 2
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Block.Hide
            1 -> return Block.Solo
            2 -> return Block.Mute
            _ -> fail "no parse for Block.TrackFlag"

tid = Block.TId :: Track.TrackId -> Ruler.RulerId -> Block.TracklikeId
rid = Block.RId :: Ruler.RulerId -> Block.TracklikeId
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

divider = Block.Divider :: Color -> Block.Divider
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
                block <- get :: Get Block.BlockId
                rect <- get :: Get Block.Rect
                visible_track <- get :: Get Int
                visible_time <- get :: Get Int
                config <- get :: Get Block.ViewConfig
                status <- get :: Get (Map.Map String String)
                track_scroll <- get :: Get Block.Width
                zoom <- get :: Get Block.Zoom
                selections <- get :: Get (Map.Map Block.SelNum Block.Selection)
                tracks <- get :: Get [Block.TrackView]
                return $ Block.View block rect visible_track visible_time
                    config status track_scroll zoom selections tracks
            _ -> version_error "Block.View" v

track_view = Block.TrackView :: Block.Width -> Block.TrackView
instance Binary Block.TrackView where
    put (Block.TrackView a) = put a
    get = get >>= \a -> return (track_view a)

instance Binary Block.Rect where
    put (Block.Rect a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Block.Rect a b c d)

instance Binary Block.ViewConfig where
    put (Block.ViewConfig a b c d e) = put_version 2
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            1 -> do
                block_title <- get :: Get Int
                track_title <- get :: Get Int
                sb_size <- get :: Get Int
                status_size <- get :: Get Int
                let skel_height = Block.vconfig_skel_height Config.view_config
                return $ Block.ViewConfig block_title track_title skel_height
                    sb_size status_size
            2 -> do
                block_title <- get :: Get Int
                track_title <- get :: Get Int
                skel_height <- get :: Get Int
                sb_size <- get :: Get Int
                status_size <- get :: Get Int
                return $ Block.ViewConfig block_title track_title skel_height
                    sb_size status_size
            _ -> version_error "Block.ViewConfig" v

zoom = Block.Zoom :: TrackPos -> Double -> Block.Zoom
instance Binary Block.Zoom where
    put (Block.Zoom a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (zoom a b)

selection = Block.Selection :: Block.TrackNum -> TrackPos -> Block.TrackNum
    -> TrackPos -> Block.Selection
instance Binary Block.Selection where
    put (Block.Selection a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (selection a b c d)

-- ** Types, Color, Font

instance Binary TrackPos where
    put (TrackPos a) = put a
    get = get >>= \a -> return (TrackPos a)

instance Binary Color.Color where
    put (Color.Color a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Color.Color a b c d)

text_style = Font.TextStyle :: Font.Font -> [Font.FontFace] -> Int -> Color
    -> Font.TextStyle
instance Binary Font.TextStyle where
    put (Font.TextStyle a b c d) = put a >> put b >> put c >> put d
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

instance Binary Ruler.RulerId where
    put (Ruler.RulerId a) = put a
    get = get >>= \a -> return (Ruler.RulerId a)

ruler = Ruler.Ruler :: [Ruler.NameMarklist] -> Color -> Bool -> Bool -> Bool
    -> Ruler.Ruler
instance Binary Ruler.Ruler where
    put (Ruler.Ruler a b c d e) = put_version 0
        >> put a >> put b >> put c >> put d >> put e
    get = do
        v <- get_version
        case v of
            0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
                get >>= \e -> return (ruler a b c d e)
            _ -> version_error "Ruler.Ruler" v

marklist = Ruler.Marklist :: IArray.Array Int Ruler.PosMark -> Ruler.Marklist
instance Binary Ruler.Marklist where
    put (Ruler.Marklist a) = put a
    get = get >>= \a -> return (marklist a)

mark = Ruler.Mark :: Int -> Int -> Color -> String -> Double -> Double
    -> Ruler.Mark
instance Binary Ruler.Mark where
    put (Ruler.Mark a b c d e f) = put a >> put b >> put c >> put d >> put e
        >> put f
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e ->
        get >>= \f -> return (mark a b c d e f)

-- ** Track

instance Binary Track.TrackId where
    put (Track.TrackId a) = put a
    get = get >>= \a -> return (Track.TrackId a)

instance Binary Track.Track where
    put (Track.Track a b c d) = put_version 1 >>
        put a >> put b >> put c >> put d
    get = do
        v <- get_version
        case v of
            0 -> do
                title <- get :: Get String
                events <- get :: Get Track.TrackEvents
                bg <- get :: Get Color
                return $ Track.Track title events bg Config.render_config
            1 -> do
                title <- get :: Get String
                events <- get :: Get Track.TrackEvents
                bg <- get :: Get Color
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
                color <- get :: Get Color
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

track_events = Track.TrackEvents :: Map.Map TrackPos Event.Event
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
    put (Event.Event a b c) = put a >> put b >> put c
    get = do
        text <- get :: Get String
        dur <- get :: Get TrackPos
        style <- get :: Get Event.StyleId
        return $ Event.Event text dur style

instance Binary Event.StyleId where
    put (Event.StyleId a) = put a
    get = liftM Event.StyleId get

-- ** Midi.Instrument

instance Binary Instrument.Config where
    put (Instrument.Config a b) = put_version 2 >> put a >> put b
    get = do
        v <- get_version
        case v of
            1 -> do
                alloc <- get :: Get (Map.Map Score.Instrument [Instrument.Addr])
                _ <- get :: Get (Maybe Instrument.Addr)
                return $ Instrument.Config alloc Nothing
            2 -> do
                alloc <- get :: Get (Map.Map Score.Instrument [Instrument.Addr])
                default_inst <- get :: Get (Maybe Score.Instrument)
                return $ Instrument.Config alloc default_inst
            _ -> version_error "Instrument.Config" v

instance Binary Score.Instrument where
    put (Score.Instrument a) = put a
    get = liftM Score.Instrument get

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
    put Midi.ResetAllControllers = putWord8 8
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
            8 -> return Midi.ResetAllControllers
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
