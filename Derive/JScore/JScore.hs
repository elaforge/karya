-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE StrictData #-}
{- | Similar to "Derive.TScore.Parse", but specialized for javanese notation.

    - Use numbers for pitches instead of letters.  Infer octave based on
    instrument, and ' ,
    - Parse notes without spaces.  Possible because pitch is a single digit.
    - Trailing / for damped notes.
    - Use . for continued pitch, _ for rest.  Usually duration is implicit.
    But inferring gender style from pipilan seems complicated.
    - Can't use . for low if I use it for rest!  Or , and ' like lilypond?
    - Infer left and right hand for instruments.
    - Associate tags (irama, name, balungan, prev seleh, seleh)
    - Switch laras and pathet, e.g. slendro manyura to pelog barang.
    - "|" is the same.  I could use || for gatra if I have multiple gatra things
        (puthut gelut?  or just have two separate calls?)
    - Bar | is optional in TS, but I could make it required to infer rhythm.
    - Biggest difference is to infer durations.  This limits | ability to catch
    errors, but since I only allow 4 and 8, not really.  With explicit dots
    it's easy, parse all notes in the measure, check for 4 or 8.  With implicit
    trailing dots I have to remember spaces and require space separation.

    How to do tempo?  Hard to draw overbars.
    In tanggung, one bar is 2 notes.
    In dadi, one bar is 1 note.
    In wiled, one bar is 1 note.

    TODO it would be nicer if this were simply a subset of normal tscore.
    Not sure if possible.  At the least it should reuse as much as possible
    of Check, and all of TScore.
-}
module Derive.JScore.JScore (
    integrate_file
    , integrate
    -- TESTING
    , ConvertedBlock(..)
    , collect_columns
    , convert_source
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Maps as Maps

import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Manual as Manual
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import qualified Derive.JScore.Check as Check
import qualified Derive.JScore.Format as Format
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * integrate

data ConvertedBlock = ConvertedBlock {
    block_name :: Text
    , block_tracks :: Convert.Tracks
    } deriving (Show, Eq)

type Error = Text

source_key :: Block.SourceKey
source_key = "jscore"

integrate_file :: (MonadIO m, Ui.M m) => FilePath
    -> m (Either [Text] (Maybe BlockId, [BlockId]))
integrate_file = integrate <=< liftIO . Text.IO.readFile

integrate :: Ui.M m => Text -> m (Either [Text] (Maybe BlockId, [BlockId]))
integrate source = case convert_source source of
    Left errors -> pure $ Left errors
    Right (toplevel, blocks) -> do
        ns <- Ui.get_namespace
        let toplevel_id = Id.make_unchecked $ Id.id ns toplevel_name
        toplevel_rid <- toplevel_ruler toplevel_id (length blocks)
        toplevel_bid <- integrate_block toplevel_rid toplevel
        -- These blocks should all be 1 gatra.
        gatra_ruler <- get_ruler (Id.id ns "gatra") Gong.gatra
        gatra_bids <- mapMaybeM (integrate_block gatra_ruler) blocks
        pure $ Right (toplevel_bid, gatra_bids)

get_ruler :: Ui.M m => Id.Id -> Meter.Meter -> m RulerId
get_ruler ruler_id meter = Ui.lookup_ruler (Id.RulerId ruler_id) >>= \case
    Just _ -> pure (Id.RulerId ruler_id)
    Nothing -> Ui.create_ruler ruler_id (Ruler.meter_ruler meter)

toplevel_ruler :: Ui.M m => BlockId -> Int -> m RulerId
toplevel_ruler block_id gatras = RulerUtil.replace block_id $ const $ Right $
    Ruler.meter_ruler $ Gong.java lines
    where
    lines = ceiling (fromIntegral gatras / 4)
    -- TODO hardcoded 4 gatras / kenong, but it depends on form
    -- I guess there should be %form=ketawang

integrate_block :: Ui.M m => RulerId -> ConvertedBlock -> m (Maybe BlockId)
integrate_block ruler_id (ConvertedBlock { block_name, block_tracks }) = do
    ns <- Ui.get_namespace
    let block_id = Id.make_unchecked $ Id.id ns block_name
    Manual.block source_key block_id ruler_id block_title block_tracks
    where
    block_title = ""

t0 = _print_integrate "short.jscore"
t1 = _print_integrate "Example/jscore/pangkur-ngrenas.jscore"
t2 = _print_integrate "Example/jscore/pangkur.jscore"

-- | Show results of integration from ghci.
_print_integrate :: FilePath -> IO ()
_print_integrate fname = do
    source <- Text.IO.readFile fname
    case convert_source source of
        Left errors -> mapM_ Text.IO.putStrLn errors
        Right (toplevel, blocks) -> do
            mapM_ Text.IO.putStrLn $ List.intercalate [""] $ map pp_block blocks
            mapM_ Text.IO.putStrLn $ pp_block toplevel
    where
    pp_block (ConvertedBlock name tracks) =
        name <> ":" : concatMap pp_tracks tracks
    pp_tracks (note, controls) = pp_track note : map pp_track controls
    pp_track (Convert.Track title events) = title <> ": "
        <> Text.unwords (map pp_event events)
    pp_event e = pretty (Event.start e)
        <> (if Event.duration e == 0 then "" else "~" <> pretty (Event.end e))
        <> (if Event.text e == "" then "" else "(" <> Event.text e <> ")")

type Block = T.Block T.Pitch [[(T.Time, T.Note T.Pitch T.Time)]]

convert_source :: Text -> Either [Error] (ConvertedBlock, [ConvertedBlock])
convert_source source = do
    score <- first (:[]) $ Parse.parse_score source
    let (blocks, errors) = Logger.runId $
            -- TODO gender barung <= tanggung uses BiasStart.
            -- >= Wiled uses BiasEnd, but also spells out every note, so not
            -- needed.
            Check.for_integrate Check.BiasStart score
    unless (null errors) $ Left $ map (T.show_error source) errors
    blocks <- first (map mkerror) $ resolve_blocks blocks
    toplevel <- first ((:[]) . mkerror) $ generate_toplevel blocks
    pure (toplevel, map convert_block blocks)
    where
    mkerror (msg, pos) = msg <> if null pos then ""
        else "\n" <> mconcat (mapMaybe (T.show_pos source) pos)

generate_toplevel :: [(Check.Meta, Block)]
    -> Either (Error, [T.Pos]) ConvertedBlock
generate_toplevel blocks = do
    (gatras, blocks) <- unzip <$> collect_balungan blocks
    -- Blocks are in rows, must rotate back to per-instrument columns.
    let by_inst = map snd <$> collect_columns (Check.m_instrument . fst) blocks
    pure $ ConvertedBlock
        { block_name = toplevel_name
        , block_tracks =
            (balungan_track gatras, []) :
            [ (inst_track inst blocks, [])
            | (inst, blocks) <- Map.toList by_inst
            ]
        }
    where
    inst_track inst blocks = Convert.Track
        { track_title = ">" <> instrument_name inst
        , track_events =
            [ Event.event t 4 (to_name (T.block_names block))
            | (t, block) <- zip (Lists.range_ 0 4) blocks
            ]
        }

toplevel_name :: Text
toplevel_name = "score"

balungan_track :: [T.Gatra T.Pitch] -> Convert.Track
balungan_track gatras = Convert.Track
    { track_title = ">"
    , track_events =
        [ Event.event t 0 ("-- " <> Format.format_balungan n)
        | (t, n) <- zip (Lists.range_ 0 1)
            [n | g <- gatras, n <- T.gatra_notes g]
        , has_pitch n
        ]
    }
    where has_pitch (T.Balungan n _) = Maybe.isJust n

-- | Extract balungan from each instrument, and match it up vertically to put
-- the instruments together.
collect_balungan :: [(Check.Meta, Block)]
    -> Either (Error, [T.Pos]) [(T.Gatra T.Pitch, [(Check.Meta, Block)])]
collect_balungan blocks = mapM check_gatra $ List.transpose by_inst
    where
    by_inst :: [[(Check.Meta, Block)]]
    by_inst = Lists.groupSort (Check.m_instrument . fst) blocks
    check_gatra blocks = case map (T.block_gatra . snd) blocks of
        gatra : gatras
            | not $ all (==gatra) gatras -> Left
                ( "same position in score for different instruments has\
                    \ inconsistent gatra:\n"
                    <> Text.unwords (map Format.format_gatra (gatra : gatras))
                , map (T.block_pos . snd) blocks
                )
            | otherwise -> Right (gatra, blocks)
        [] -> Left ("unreached", []) -- TODO why?

resolve_blocks :: [(Check.Meta, Block)]
    -> Either [(Error, [T.Pos])] [(Check.Meta, Block)]
resolve_blocks blocks
    | null errors = Right resolved
    | otherwise = Left errors
    where
    resolved = disambiguate_names $ normalize_names blocks
    errors = check_empty_no_match (map snd resolved)
        ++ check_duplicates (map snd resolved)

-- | Check for blocks given the same name but have different tracks.
-- 'disambiguate_names' should have prevented this!
check_duplicates :: [Block] -> [(Error, [T.Pos])]
check_duplicates blocks =
    map make $ filter ((>1) . length . snd) $ Map.toList by_name
    where
    by_name = Maps.multimap $ Lists.keyOn T.block_names $
        filter (not . null . T.block_tracks) blocks
    make (name, blocks) =
        ( "multiple differing blocks with name " <> to_name name
            <> ":\n" <> Text.unlines (map (pretty . T.block_tracks) blocks)
        , map T.block_pos blocks
        )

check_empty_no_match :: [Block] -> [(Error, [T.Pos])]
check_empty_no_match blocks = mapMaybe check blocks
    where
    check block
        | Set.member (T.block_names block) names = Nothing
        | otherwise = Just
            ( "empty block " <> to_name (T.block_names block)
                <> " has no matches"
            , [T.block_pos block]
            )
    names = Set.fromList $ ["gp", "t", "mbalung"]
        : (map T.block_names $ filter (not . null . T.block_tracks) blocks)

normalize_names :: [(Check.Meta, Block)] -> [(Check.Meta, Block)]
normalize_names = map set
    where
    set (meta, block) = (meta,) $ block
        { T.block_names = normalize_name meta (T.block_gatra block)
            (T.block_names block)
        }

normalize_name :: Check.Meta -> T.Gatra T.Pitch -> [Text] -> [Text]
normalize_name (Check.Meta { m_irama, m_instrument }) gatra =
    (prefix++) . \case
        [] | Just c <- seleh -> ["seleh", c]
        ["gantung"] | Just c <- seleh -> ["gantung", c]
        names -> map abbr names
    where
    prefix = [instrument_name m_instrument, irama_prefix m_irama]
    abbr n = fromMaybe n $ fromMaybe Nothing $
        Map.lookup n Check.standard_names
    seleh = Text.singleton . T.pc_char <$> T.seleh gatra

-- | If two blocks have the same name, it's ok if they have the same notes.
-- If they have different notes, then disambiguate with the gatra and then
-- with an index number.
disambiguate_names :: [(meta, Block)] -> [(meta, Block)]
disambiguate_names blocks = map get $ zip [0..] blocks
    where
    get (i, (meta, block)) = (meta, Map.findWithDefault block i by_index)
    -- I have to keep the Block order the same, so do imperative update by
    -- index.
    by_index :: Map Int Block
    by_index = Map.fromList $
        disambiguate_with with_index $ disambiguate_with (map with_gatra) $
        zip [0..] $ map snd blocks
    -- Disambiguate a group of blocks that all have the same name, if there
    -- is >1 with different tracks.
    disambiguate_with with blocks =
        -- Empty blocks have either already been resolved, or were empty to
        -- begin with.
        (empty ++) $ concatMap (disambiguate with) $
            Lists.groupSort (T.block_names . snd) nonempty
        where
        (empty, nonempty) = List.partition (null . T.block_tracks . snd) blocks
    -- All blocks with matching tracks get the same name, and all but the first
    -- are stripped of their notes, so they wind up calling the same block.
    disambiguate with blocks =
        concatMap (Lists.mapTail (second strip)) $
            if length groups <= 1 then groups else with groups
        where
        groups = Lists.groupSort (T.block_tracks . snd) blocks
    with_index = zipWith (\i -> map (second (append (showt i)))) [1 :: Int ..]
    -- Each group of blocks with the same
    with_gatra blocks@(block : _) = map (second (append gatra)) blocks
        where gatra = simple_gatra $ T.block_gatra (snd block)
    with_gatra blocks = blocks
    append p block = block { T.block_names = T.block_names block ++ [p] }
    strip block = block { T.block_tracks = [] }

simple_gatra :: T.Gatra T.Pitch -> Text
simple_gatra (T.Gatra n1 n2 n3 n4) = mconcatMap fmt [n1, n2, n3, n4]
    where
    fmt (T.Balungan p _) = case p of
        Just (T.Pitch _ pc) -> Text.singleton (T.pc_char pc)
        Nothing -> "x" -- "." is already the block separator

convert_block :: (Check.Meta, Block) -> ConvertedBlock
convert_block (meta, block) = ConvertedBlock
    { block_name = to_name (T.block_names block)
    , block_tracks = map (convert_track (Check.m_instrument meta)) $
        zip hands (reverse (T.block_tracks block))
        -- Reverse tracks for same reason as tscore: when writing left to
        -- right, right hand goes above.  When top to bottom, right hand goes
        -- to the right.
    }
    where
    hands
        | length (T.block_tracks block) == 2 = [Just "l", Just "r"]
        | otherwise = repeat Nothing

to_name :: [Text] -> Text
to_name = Text.intercalate "-"

convert_track :: T.Instrument -> (Maybe Text, [Check.Event])
    -> (Convert.Track, [Convert.Track])
convert_track inst (mb_hand, events) =
    ( Convert.Track
        { track_title = ">" <> instrument_name inst
            <> maybe "" (" | hand="<>)  mb_hand
        , track_events = notes
        }
    , (:[]) $ Convert.Track
        { track_title = "*" -- pitch
        , track_events = pitches
        }
    )
    where (notes, pitches) = unzip $ map to_events events

to_events :: (T.Time, T.Note T.Pitch T.Time) -> (Event.Event, Event.Event)
to_events (start, note) =
    ( Event.event (track_time start) (track_time (T.note_duration note))
        (if T.note_zero_duration note then "/" else "")
    , Event.event (track_time start) 0 (convert_pitch (T.note_pitch note))
    )

convert_pitch :: T.Pitch -> Text
convert_pitch (T.Pitch oct pc) = showt oct <> Text.singleton (T.pc_char pc)

track_time :: T.Time -> TrackTime
track_time = realToFrac

irama_prefix :: T.Irama -> Text
irama_prefix = \case
    T.Lancar -> "l"
    T.Tanggung -> "t"
    T.Dadi -> "d"
    T.Wiled -> "w"
    T.Rangkep -> "r"

instrument_name :: T.Instrument -> Text
instrument_name = \case
    T.GenderBarung -> "gb"
    T.GenderPanerus -> "gp"
    T.Siter -> "si"

-- * util

collect_columns :: Ord k => (a -> k) -> [[a]] -> Map k [a]
collect_columns key = fmap reverse . List.foldl' go Map.empty
    where go cols row = Map.unionWith (++) (keyedGroupMap key row) cols
    -- If there is one column value per row, then I'm prepending singleton [x]
    -- on the map values, which is efficient, but meant I have to reverse
    -- later.  TODO didn't I have a partitionMany or something that would
    -- collect by key without reversing?

-- Lists.keyedGroupSort but leaves it as Map
keyedGroupMap :: Ord key => (a -> key) -> [a] -> Map key [a]
keyedGroupMap key = foldr go Map.empty
    where go x = Map.alter (Just . maybe [x] (x:)) (key x)

-- insertCons :: Ord k => k -> a -> Map k [a] -> Map k [a]
-- insertCons k a = Map.alter (Just . maybe [a] (a:)) k

-- insertCons :: Ord k => k -> a -> Map k [a] -> Map k [a]
-- insertCons k v m = case Map.lookup k m of
--     Nothing -> Map.insert k [v] m
--     Just vs -> Map.insert k (v:vs) m
