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
    integrateFile
    , integrate
    -- TESTING
    , ConvertedBlock(..)
    , collectColumns
    , convertSource
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

data ConvertedBlock = ConvertedBlock
    { blockName :: Text
    , blockTracks :: Convert.Tracks
    } deriving (Show, Eq)

type Error = Text

sourceKey :: Block.SourceKey
sourceKey = "jscore"

integrateFile :: (MonadIO m, Ui.M m) => FilePath
    -> m (Either [Text] (Maybe BlockId, [BlockId]))
integrateFile fname = integrate fname =<< liftIO (Text.IO.readFile fname)

integrate :: Ui.M m => FilePath -> Text
    -> m (Either [Text] (Maybe BlockId, [BlockId]))
integrate fname source = case convertSource fname source of
    Left errors -> pure $ Left errors
    Right (toplevel, blocks) -> do
        ns <- Ui.getNamespace
        let toplevelId = Id.makeUnchecked $ Id.id ns toplevelName
        toplevelRid <- toplevelRuler toplevelId (length blocks)
        toplevelBid <- integrateBlock toplevelRid toplevel
        -- These blocks should all be 1 gatra.
        gatraRuler <- getRuler (Id.id ns "gatra") Gong.gatra
        gatraBids <- mapMaybeM (integrateBlock gatraRuler) blocks
        pure $ Right (toplevelBid, gatraBids)

getRuler :: Ui.M m => Id.Id -> Meter.Meter -> m RulerId
getRuler rulerId meter = Ui.lookupRuler (Id.RulerId rulerId) >>= \case
    Just _ -> pure (Id.RulerId rulerId)
    Nothing -> Ui.createRuler rulerId (Ruler.meterRuler meter)

toplevelRuler :: Ui.M m => BlockId -> Int -> m RulerId
toplevelRuler blockId gatras = RulerUtil.replace blockId $ const $ Right $
    Ruler.meterRuler $ Gong.java lines
    where
    lines = ceiling (fromIntegral gatras / 2)
    -- TODO hardcoded 2 gatras / kenong, but it depends on form
    -- I guess there should be %form=ketawang

integrateBlock :: Ui.M m => RulerId -> ConvertedBlock -> m (Maybe BlockId)
integrateBlock rulerId (ConvertedBlock { blockName, blockTracks }) = do
    ns <- Ui.getNamespace
    let blockId = Id.makeUnchecked $ Id.id ns blockName
    Manual.block sourceKey blockId rulerId blockTitle blockTracks
    where
    blockTitle = ""

t0 = _printIntegrate "short.jscore"
t1 = _printIntegrate "Example/jscore/pangkur-ngrenas.jscore"
t2 = _printIntegrate "Example/jscore/pangkur.jscore"

-- | Show results of integration from ghci.
_printIntegrate :: FilePath -> IO ()
_printIntegrate fname = do
    source <- Text.IO.readFile fname
    case convertSource fname source of
        Left errors -> mapM_ Text.IO.putStrLn errors
        Right (toplevel, blocks) -> do
            mapM_ Text.IO.putStrLn $ List.intercalate [""] $ map ppBlock blocks
            mapM_ Text.IO.putStrLn $ ppBlock toplevel
    where
    ppBlock (ConvertedBlock name tracks) =
        name <> ":" : concatMap ppTracks tracks
    ppTracks (note, controls) = ppTrack note : map ppTrack controls
    ppTrack (Convert.Track title events) = title <> ": "
        <> Text.unwords (map ppEvent events)
    ppEvent e = pretty (Event.start e)
        <> (if Event.duration e == 0 then "" else "~" <> pretty (Event.end e))
        <> (if Event.text e == "" then "" else "(" <> Event.text e <> ")")

type Block = T.Block T.Pitch [[(T.Time, T.Note T.Pitch T.Time)]]

convertSource :: FilePath -> Text
    -> Either [Error] (ConvertedBlock, [ConvertedBlock])
convertSource fname code = do
    score <- first (:[]) $ Parse.parseScore code
    let (blocks, errors) = Logger.runId $
            -- TODO gender barung <= tanggung uses BiasStart.
            -- >= Wiled uses BiasEnd, but also spells out every note, so not
            -- needed.
            Check.forIntegrate Check.BiasStart score
    unless (null errors) $ Left $ map (T.showError fname code) errors
    blocks <- first (map mkError) $ resolveBlocks blocks
    toplevel <- first ((:[]) . mkError) $ generateToplevel blocks
    pure (toplevel, map convertBlock blocks)
    where
    mkError (msg, []) = T.showError fname code $ T.Error Nothing msg
    -- Yes this will format weird, and ParseM does support multiple positions,
    -- but not worth it right now.  Why do I have multiple positions?
    mkError (msg, p:ps) = T.showErrors fname code $
        T.Error (Just p) msg : [T.Error (Just p) "" | p <- ps]

generateToplevel :: [(Check.Meta, Block)]
    -> Either (Error, [T.Pos]) ConvertedBlock
generateToplevel blocks = do
    (gatras, blocks) <- unzip <$> collectBalungan blocks
    -- Blocks are in rows, must rotate back to per-instrument columns.
    let byInst = map snd <$> collectColumns (Check.mInstrument . fst) blocks
    pure $ ConvertedBlock
        { blockName = toplevelName
        , blockTracks =
            (balunganTrack gatras, []) :
            [ (instTrack inst blocks, [])
            | (inst, blocks) <- Map.toList byInst
            ]
        }
    where
    instTrack inst blocks = Convert.Track
        { trackTitle = ">" <> instrumentName inst
        , trackEvents =
            [ Event.event t 4 (toName (T.blockNames block))
            | (t, block) <- zip (Lists.range_ 0 4) blocks
            , T.blockNames block `Set.notMember` specialNames
            ]
        }

toplevelName :: Text
toplevelName = "score"

balunganTrack :: [T.Gatra T.Pitch] -> Convert.Track
balunganTrack gatras = Convert.Track
    { trackTitle = ">"
    , trackEvents =
        [ Event.event t (-0) ("-- " <> Format.formatBalungan n)
        | (t, n) <- zip (drop 1 (Lists.range_ 0 1))
            [n | g <- gatras, n <- T.gatraNotes g]
        , hasPitch n
        ]
    }
    where hasPitch (T.Balungan n _) = Maybe.isJust n

-- | Extract balungan from each instrument, and match it up vertically to put
-- the instruments together.
collectBalungan :: [(Check.Meta, Block)]
    -> Either (Error, [T.Pos]) [(T.Gatra T.Pitch, [(Check.Meta, Block)])]
collectBalungan blocks = mapM checkGatra $ List.transpose byInst
    where
    byInst :: [[(Check.Meta, Block)]]
    byInst = Lists.groupSort (Check.mInstrument . fst) blocks
    checkGatra blocks = case map (T.blockGatra . snd) blocks of
        gatra : gatras
            | not $ all (==gatra) gatras -> Left
                ( "same position in score for different instruments has\
                    \ inconsistent gatra:\n"
                    <> Text.unwords (map Format.formatGatra (gatra : gatras))
                , map (T.blockPos . snd) blocks
                )
            | otherwise -> Right (gatra, blocks)
        [] -> Left ("unreached", []) -- TODO why?

resolveBlocks :: [(Check.Meta, Block)]
    -> Either [(Error, [T.Pos])] [(Check.Meta, Block)]
resolveBlocks blocks
    | null errors = Right resolved
    | otherwise = Left errors
    where
    resolved = disambiguateNames $ normalizeNames blocks
    errors = checkEmptyNoMatch (map snd resolved)
        ++ checkDuplicates (map snd resolved)

-- | Check for blocks given the same name but have different tracks.
-- 'disambiguate_names' should have prevented this!
checkDuplicates :: [Block] -> [(Error, [T.Pos])]
checkDuplicates blocks =
    map make $ filter ((>1) . length . snd) $ Map.toList byName
    where
    byName = Maps.multimap $ Lists.keyOn T.blockNames $
        filter (not . null . T.blockTracks) blocks
    make (name, blocks) =
        ( "multiple differing blocks with name " <> toName name
            <> ":\n" <> Text.unlines (map (pretty . T.blockTracks) blocks)
        , map T.blockPos blocks
        )

checkEmptyNoMatch :: [Block] -> [(Error, [T.Pos])]
checkEmptyNoMatch blocks = mapMaybe check blocks
    where
    check block
        | Set.member (T.blockNames block) names = Nothing
        | otherwise = Just
            ( "empty block " <> toName (T.blockNames block)
                <> " has no matches"
            , [T.blockPos block]
            )
    names = specialNames <> Set.fromList
        (map T.blockNames $ filter (not . null . T.blockTracks) blocks)

normalizeNames :: [(Check.Meta, Block)] -> [(Check.Meta, Block)]
normalizeNames = map set
    where
    set (meta, block) = (meta,) $ block
        { T.blockNames = normalizeName meta (T.blockGatra block)
            (T.blockNames block)
        }

normalizeName :: Check.Meta -> T.Gatra T.Pitch -> [Text] -> [Text]
normalizeName (Check.Meta { mIrama, mInstrument }) gatra names
    | names `Set.member` specialNames = names
    | otherwise = prefix ++ case names of
        [] | Just c <- seleh -> ["seleh", c]
        ["gantung"] | Just c <- seleh -> ["gantung", c]
        _ -> map abbr names
    where
    prefix = [instrumentName mInstrument, iramaPrefix mIrama]
    abbr n = fromMaybe n $ fromMaybe Nothing $
        Map.lookup n Check.standardNames
    seleh = Text.singleton . T.pcChar <$> T.seleh gatra

-- | Let these remain undefined.
-- TODO mbalung could copy from balungan, missing could omit the event.
specialNames :: Set [Text]
specialNames = Set.fromList $ map (:[]) ["mbalung", "missing"]

-- | If two blocks have the same name, it's ok if they have the same notes.
-- If they have different notes, then disambiguate with the gatra and then
-- with an index number.
disambiguateNames :: [(meta, Block)] -> [(meta, Block)]
disambiguateNames blocks = map get $ zip [0..] blocks
    where
    get (i, (meta, block)) = (meta, Map.findWithDefault block i byIndex)
    -- I have to keep the Block order the same, so do imperative update by
    -- index.
    byIndex :: Map Int Block
    byIndex = Map.fromList $
        disambiguateWith withIndex $ disambiguateWith (map withGatra) $
        zip [0..] $ map snd blocks
    -- Disambiguate a group of blocks that all have the same name, if there
    -- is >1 with different tracks.
    disambiguateWith with blocks =
        -- Empty blocks have either already been resolved, or were empty to
        -- begin with.
        (empty ++) $ concatMap (disambiguate with) $
            Lists.groupSort (T.blockNames . snd) nonempty
        where
        (empty, nonempty) = List.partition (null . T.blockTracks . snd) blocks
    -- All blocks with matching tracks get the same name, and all but the first
    -- are stripped of their notes, so they wind up calling the same block.
    disambiguate with blocks =
        concatMap (Lists.mapTail (second strip)) $
            if length groups <= 1 then groups else with groups
        where
        groups = Lists.groupSort (T.blockTracks . snd) blocks
    withIndex = zipWith (\i -> map (second (append (showt i)))) [1 :: Int ..]
    -- Each group of blocks with the same
    withGatra blocks@(block : _) = map (second (append gatra)) blocks
        where gatra = simpleGatra $ T.blockGatra (snd block)
    withGatra blocks = blocks
    append p block = block { T.blockNames = T.blockNames block ++ [p] }
    strip block = block { T.blockTracks = [] }

simpleGatra :: T.Gatra T.Pitch -> Text
simpleGatra (T.Gatra n1 n2 n3 n4) = mconcatMap fmt [n1, n2, n3, n4]
    where
    fmt (T.Balungan p _) = case p of
        Just (T.Pitch _ pc) -> Text.singleton (T.pcChar pc)
        Nothing -> "x" -- "." is already the block separator

convertBlock :: (Check.Meta, Block) -> ConvertedBlock
convertBlock (meta, block) = ConvertedBlock
    { blockName = toName (T.blockNames block)
    , blockTracks = map (convertTrack (Check.mInstrument meta)) $
        zip hands (reverse (T.blockTracks block))
        -- Reverse tracks for same reason as tscore: when writing left to
        -- right, right hand goes above.  When top to bottom, right hand goes
        -- to the right.
    }
    where
    hands
        | length (T.blockTracks block) == 2 = [Just "l", Just "r"]
        | otherwise = repeat Nothing

toName :: [Text] -> Text
toName = Text.intercalate "-"

convertTrack :: T.Instrument -> (Maybe Text, [Check.Event])
    -> (Convert.Track, [Convert.Track])
convertTrack inst (mbHand, events) =
    ( Convert.Track
        { trackTitle = ">" <> instrumentName inst
            <> maybe "" (" | hand="<>)  mbHand
        , trackEvents = notes
        }
    , (:[]) $ Convert.Track
        { trackTitle = "*" -- pitch
        , trackEvents = pitches
        }
    )
    where (notes, pitches) = unzip $ map toEvents events

toEvents :: (T.Time, T.Note T.Pitch T.Time) -> (Event.Event, Event.Event)
toEvents (start, note) =
    ( Event.event (trackTime start) (trackTime (T.noteDuration note))
        (if T.noteZeroDuration note then "/" else "")
    , Event.event (trackTime start) 0 (convertPitch (T.notePitch note))
    )

convertPitch :: T.Pitch -> Text
convertPitch (T.Pitch oct pc) = showt oct <> Text.singleton (T.pcChar pc)

trackTime :: T.Time -> TrackTime
trackTime = realToFrac

iramaPrefix :: T.Irama -> Text
iramaPrefix = \case
    T.Lancar -> "l"
    T.Tanggung -> "t"
    T.Dadi -> "d"
    T.Wiled -> "w"
    T.Rangkep -> "r"

instrumentName :: T.Instrument -> Text
instrumentName = \case
    T.GenderBarung -> "gb"
    T.GenderPanerus -> "gp"
    T.Siter -> "si"

-- * util

collectColumns :: Ord k => (a -> k) -> [[a]] -> Map k [a]
collectColumns key = fmap reverse . List.foldl' go Map.empty
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
