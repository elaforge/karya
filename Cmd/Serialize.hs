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
module Cmd.Serialize where
import qualified Control.Exception as Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Vector as Vector

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
import Midi.Instances ()
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


-- | This is a four byte prefix to identify a particular file type.  The Chars
-- are just for syntactic convenience only, and must be ASCII.
data Magic = Magic !Char !Char !Char !Char deriving (Show)

magic_bytes :: Magic -> B.ByteString
magic_bytes (Magic c1 c2 c3 c4) = Char8.pack [c1, c2, c3, c4]

magic_length :: Int
magic_length = 4

serialize :: (Serialize a) => Magic -> FilePath -> a -> IO ()
serialize magic fname state = do
    backup_file fname
    make_dir fname
    File.writeGz fname $ magic_bytes magic <> Serialize.encode state

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
unserialize :: (Serialize a) => Magic -> FilePath
    -> IO (Either String (Maybe a))
unserialize magic fname = do
    maybe_bytes <- File.ignoreEnoent $ File.readGz fname
    case maybe_bytes of
        Nothing -> return (Right Nothing)
        Just bytes
            | not (magic_bytes magic `B.isPrefixOf` bytes) ->
                return $ Left $ "expected magic code "
                    <> show (magic_bytes magic) <> " but got "
                    <> show (B.take magic_length bytes)
            | otherwise -> do
                -- This is subtle.  Apparently Serialize.decode can still throw
                -- an exception unless the contents of the Either is forced to
                -- whnf.
                val <- Exception.evaluate
                        (Serialize.decode (B.drop magic_length bytes))
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
backup_file fname = do
    moved <- File.ignoreEnoent $
        Directory.renameFile (fname ++ ".gz") (fname ++ ".last.gz")
    case moved of
        Nothing -> void $ File.ignoreEnoent $
            Directory.renameFile fname (fname ++ ".last")
        _ -> return ()

make_dir :: FilePath -> IO ()
make_dir = Directory.createDirectoryIfMissing True . FilePath.takeDirectory


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
            saved_views)
        =  Serialize.put_version 8
            >> put ns >> put meta >> put root >> put (Configs midi)
            >> put transform >> put instruments >> put lilypond >> put defaults
            >> put saved_views
    get = Serialize.get_version >>= \v -> case v of
        7 -> do
            ns :: Id.Namespace <- get
            meta :: State.Meta <- get
            root :: Maybe BlockId <- get
            Configs midi :: Configs <- get
            transform :: Text <- get
            instruments :: Map.Map Score.Instrument Score.Instrument <- get
            lilypond :: Lilypond.Config <- get
            defaults :: State.Default <- get
            return $ State.Config ns meta root midi transform instruments
                lilypond defaults mempty
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
                lilypond defaults saved_views
        _ -> Serialize.bad_version "State.Config" v

instance Serialize State.Meta where
    put (State.Meta a b c) = Serialize.put_version 1 >> put a >> put b >> put c
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            creation :: Time.UTCTime <- get
            notes :: String <- get
            return $ State.Meta creation (txt notes) mempty
        1 -> do
            creation :: Time.UTCTime <- get
            notes :: Text <- get
            performances :: Map.Map BlockId State.Performance <- get
            return $ State.Meta creation notes performances
        _ -> Serialize.bad_version "State.Meta" v

instance Serialize State.Performance where
    put (State.Performance a b c) = Serialize.put_version 0 >> put a >> put b
        >> put c
    get = Serialize.get_version >>= \v -> case v of
        0 -> do
            midi :: Vector.Vector Midi.WriteMessage <- get
            creation :: Time.UTCTime <- get
            patch :: Text <- get
            return $ State.Performance midi creation patch
        _ -> Serialize.bad_version "State.Performance" v

instance Serialize State.Default where
    put (State.Default a) = Serialize.put_version 4 >> put a
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                _scale :: Pitch.ScaleId <- get
                _key :: Maybe Pitch.Key <- get
                _inst :: Maybe Score.Instrument <- get
                tempo :: Signal.Y <- get
                return $ State.Default tempo
            4 -> do
                tempo :: Signal.Y <- get
                return $ State.Default tempo
            _ -> Serialize.bad_version "State.Default" v

-- ** Block

instance Serialize Block.Block where
    -- Config is not serialized because everything in the block config is
    -- either derived from the Cmd.State or is hardcoded.
    put (Block.Block a _config b c d e f) = Serialize.put_version 10
        >> put a >> put b >> put c >> put d >> put e >> put f
    get = do
        v <- Serialize.get_version
        case v of
            9 -> do
                title :: Text <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                integrated ::
                    Maybe (BlockId, NonEmpty Block.TrackDestination) <- get
                itracks :: [(TrackId, NonEmpty Block.TrackDestination)] <- get
                meta :: Map.Map Text Text <- get
                return $ Block.Block title Block.default_config tracks skel
                    (second NonEmpty.toList <$> integrated)
                    (map (second NonEmpty.toList) itracks) meta
            10 -> do
                title :: Text <- get
                tracks :: [Block.Track] <- get
                skel :: Skeleton.Skeleton <- get
                integrated :: Maybe (BlockId, [Block.TrackDestination]) <- get
                itracks :: [(TrackId, [Block.TrackDestination])] <- get
                meta :: Map.Map Text Text <- get
                return $ Block.Block title Block.default_config tracks skel
                    integrated itracks meta
            _ -> Serialize.bad_version "Block.Block" v

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
            5 -> do
                marklists :: Map.Map Ruler.Name Ruler.Marklist <- get
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
        0 -> do
            style :: RenderStyle0 <- get
            color :: Color.Color <- get
            return $ Track.RenderConfig (convert style) color
        1 -> do
            style :: Track.RenderStyle <- get
            color :: Color.Color <- get
            return $ Track.RenderConfig style color
        _ -> Serialize.bad_version "Track.RenderConfig" v
        where
        convert NoRender = Track.NoRender
        convert Line = Track.Line Nothing
        convert Filled = Track.Filled Nothing

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
    put (Track.Pitch a) = put_tag 1 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> do
            control :: Score.Control <- get
            return $ Track.Control control
        1 -> do
            control :: Maybe Score.Control <- get
            return $ Track.Pitch control
        _ -> bad_tag "Track.RenderSource" tag

instance Serialize RenderStyle0 where
    put NoRender = put_tag 0
    put Line = put_tag 1
    put Filled = put_tag 2
    get = do
        tag <- get_tag
        case tag of
            0 -> return NoRender
            1 -> return Line
            2 -> return Filled
            _ -> bad_tag "RenderStyle0" tag

data RenderStyle0 = NoRender | Line | Filled

-- ** Midi.Instrument

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
    put (Instrument.Config a b c d) = Serialize.put_version 3
        >> put a >> put b >> put c >> put d
    get = do
        v <- Serialize.get_version
        case v of
            1 -> do
                addrs :: [Instrument.Addr] <- get
                mute :: Bool <- get
                solo :: Bool <- get
                return $ Instrument.Config (map (flip (,) Nothing) addrs)
                    mempty mute solo
            2 -> do
                addrs :: [Instrument.Addr] <- get
                controls :: Score.ControlValMap <- get
                mute :: Bool <- get
                solo :: Bool <- get
                return $ Instrument.Config (map (flip (,) Nothing) addrs)
                    controls mute solo
            3 -> do
                addrs :: [(Instrument.Addr, Maybe Instrument.Voices)] <- get
                controls :: Score.ControlValMap <- get
                mute :: Bool <- get
                solo :: Bool <- get
                return $ Instrument.Config addrs controls mute solo
            _ -> Serialize.bad_version "Instrument.Config" v

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
