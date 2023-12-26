-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java.Check where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Num as Num

import qualified Derive.TScore.Java.T as T
import           Derive.TScore.Java.T (Octave, Pitch(..))

import           Global


type CheckM a = Logger.Logger T.Error a

warn :: T.Pos -> Text -> CheckM ()
warn pos msg = Logger.log (T.Error pos msg)

{-
    Checks:
    - barlines correct
    - directives understood
    - within instrument range
    - left hand below right hand
-}

data Bias = BiasStart | BiasEnd
    deriving (Show, Eq)

resolve_tokens :: Bias -> [T.ParsedToken]
    -> CheckM [(T.Time, T.Note (Pitch Octave) T.Time)]
resolve_tokens bias =
    fmap (resolve_durations bias) . normalize_barlines bias . resolve_pitch

type FormatToken = T.Token (T.Note (Pitch Octave) ()) T.Rest

-- | Score-to-score, this takes the parsed score to a somewhat more normalized
-- version.  So not the same as converting to tracklang, because I don't need
-- actual times and durations.  Also I have to 'normalize_hands' to make
-- sure they are at the same "zoom", but it's only since I don't want to
-- convert from times back to notes.
format_score :: T.ParsedScore -> CheckM (T.Score (T.Block [[FormatToken]]))
format_score (T.Score toplevels) =
    T.Score . snd <$> Lists.mapAccumLM format [] toplevels
    where
    format metas (pos, T.ToplevelMeta a) =
        pure (a : metas, (pos, T.ToplevelMeta a))
    format metas (pos, T.BlockDefinition b) = do
        -- Must 'resolve_pitch' before 'infer_chord'.
        b <- format_block b
        laras <- case Lists.head [laras | T.Laras laras <- metas] of
            Nothing -> do
                warn pos "no laras, defaulting to pelog-lima"
                pure T.PelogLima
            Just laras -> pure laras
        b <- normalize_name laras pos b
        pure (metas, (pos, T.BlockDefinition b))

format_block :: T.ParsedBlock -> CheckM (T.Block [[FormatToken]])
format_block block = do
    tracks <- case T.block_tracks block of
        -- TODO or I could do resolve_names first?  Still may fail to resolve.
        Nothing -> pure []
        Just tracks -> format_tracks tracks
    pure $ block { T.block_tracks = tracks }

format_tracks :: T.Tracks -> CheckM [[FormatToken]]
format_tracks (T.Tracks tracks) = case map T.track_tokens tracks of
    [lefts, rights] -> do
        lefts <- format_tokens bias lefts
        rights <- format_tokens bias rights
        (lefts, rights) <- normalize_hands bias lefts rights
        pure [lefts, rights]
    -- TODO error for two handed instruments?
    tracks -> mapM (format_tokens bias) tracks
    where
    bias = BiasStart

format_tokens :: Bias -> [T.ParsedToken]
    -> CheckM [T.Token (T.Note (Pitch Octave) ()) T.Rest]
format_tokens bias = -- fmap lima_to_barang .
    normalize_barlines bias . resolve_pitch
    -- This doesn't do 'resolve_durations', because I want the original rests.

-- * normalize names

-- | Un-abbreviate standard cengkok names.
normalize_name :: T.Laras -> T.Pos -> T.Block [[FormatToken]]
    -> CheckM (T.Block [[FormatToken]])
normalize_name laras pos block = do
    names <- case T.block_names block of
        [] -> pure ["none"]
        name : names -> case Map.lookup name from_abbr of
            Just name -> pure $ name : names
            Nothing -> do
                unless (name `elem` map fst standard_names) $
                    warn pos $ "unknown name: " <> name
                pure $ name : names
    -- TODO is this always accurate, or should I get it from the tracks?
    let seleh = Text.singleton . T.pc_char <$> T.seleh (T.block_gatra block)
    let inferred = Maybe.catMaybes [chord, seleh]
    pure $ block { T.block_names = names, T.block_inferred = inferred }
    where
    chord = fmap (Text.toLower . showt) $
        infer_chord laras (T.block_tracks block)
    from_abbr = Map.fromList $ map Tuple.swap $
        filter (not . Text.null . snd) standard_names

data Chord = Kempyung | Gembyang
    deriving (Eq, Show)

infer_chord :: T.Laras -> [[T.Token (T.Note (Pitch Octave) dur) rest]]
    -> Maybe Chord
infer_chord laras tracks = case map final_pitch tracks of
    [Just high, Just low]
        | pitch_pc high == pitch_pc low
        && pitch_octave high == pitch_octave low + 1 -> Just Gembyang
        | T.add_pc laras 3 low == high -> Just Kempyung
    _ -> Nothing

final_pitch :: [T.Token (T.Note pitch dur) rest] -> Maybe pitch
final_pitch =
    msum . map pitch_of . take 3 . filter (not . is_barline) . reverse
    -- Only consider a few final notes, sometimes a final chord is offset,
    -- but not by much.
    where
    is_barline (T.TBarline {}) = True
    is_barline _ = False
    pitch_of (T.TNote _ note) = Just $ T.note_pitch note
    pitch_of _ = Nothing

standard_names :: [(Text, Text)]
standard_names =
    [ ("ayu-kuning", "ak")
    , ("buko", "")
    , ("debyang-debyung", "dd")
    , ("dualolo", "dll")
    , ("duduk", "")
    , ("gantung", "")
    , ("gelut", "g")
    , ("jarik-kawung", "jk")
    , ("kacaryan", "kc")
    , ("kutuk-kuning", "kk")
    , ("puthut", "p") -- 2 part pattern puthut gelut
    , ("puthut-semedi", "ps")
    , ("rambatan", "")
    , ("tumurun", "tm")
    ]

-- ** resolve_pitch

resolve_pitch
    :: [T.Token (T.Note (Pitch T.RelativeOctave) dur) rest]
    -> [T.Token (T.Note (Pitch Octave) dur) rest]
resolve_pitch = snd . List.mapAccumL resolve (0, Nothing)
    where
    resolve prev = \case
        T.TBarline pos -> (prev, T.TBarline pos)
        T.TRest pos rest -> (prev, T.TRest pos rest)
        T.TNote pos note ->
            ( (oct, Just pc)
            , T.TNote pos $ note { T.note_pitch = pitch }
            )
            where
            pitch@(Pitch oct pc) = infer_octave prev (T.note_pitch note)

-- Initial prev_oct is based on the instrument.
-- Similar to Check.infer_octave, but uses simpler 'Pitch' instead of
-- 'Perform.Pitch.Pitch'.
infer_octave :: (Octave, Maybe T.PitchClass)
    -> Pitch T.RelativeOctave -> Pitch Octave
infer_octave (prev_oct, Nothing) (Pitch (T.RelativeOctave rel_oct) pc) =
    Pitch (prev_oct + rel_oct) pc
infer_octave (prev_oct, Just prev_pc) (Pitch (T.RelativeOctave rel_oct) pc) =
    case compare rel_oct 0 of
        -- If distances are equal, favor downward motion.  But since PitchClass
        -- has an odd number, this never happens.  If I omit P4, it could
        -- though, but then I should also omit P1 or P7.
        EQ
            | prev_pc == pc -> Pitch prev_oct pc
            | otherwise -> Lists.minOn (abs . T.pitch_diff prev) above below
        GT -> T.add_oct (rel_oct-1) above
        LT -> T.add_oct (rel_oct+1) below
    where
    prev = Pitch prev_oct prev_pc
    -- If I'm moving up, then I don't need to increment the octave to be above.
    above = Pitch (if pc > prev_pc then prev_oct else prev_oct+1) pc
    below = Pitch (if pc < prev_pc then prev_oct else prev_oct-1) pc

-- ** resolve_durations

resolve_durations :: Bias -> [T.Token (T.Note pitch dur) T.Rest]
    -> [(T.Time, T.Note pitch T.Time)]
resolve_durations bias =
    Maybe.catMaybes . snd . List.mapAccumL resolve_rests 0 . Lists.zipNexts
        . concatMap resolve . Lists.split is_barline
    where
    resolve bar = mapMaybe (add_dur len) bar
        where len = length bar
    add_dur len = \case
        T.TBarline {} -> Nothing
        T.TRest _ rest -> Just (dur, Left rest)
        T.TNote _ note -> Just (dur, Right note)
        where dur = T.Time (1 / fromIntegral len)
    is_barline (T.TBarline {}) = True
    is_barline _ = False
    -- If BiasEnd, each note goes from the *end* of its duration until the
    -- start of the next note, or non-sustaining rest.  This gives them the
    -- end-bias.
    resolve_rests time ((dur, Left _), _) = (time + dur, Nothing)
    resolve_rests time ((dur, Right n), nexts) =
        ( time + dur
        , Just $ case bias of
            BiasEnd -> (time + dur, n { T.note_duration = sustain })
            BiasStart -> (time, n { T.note_duration = dur + sustain })
        )
        where
        -- The final note will have 0 dur, which I can look for and set.
        -- If the final note is a sustain rest, then use its duration, not 0.
        -- TODO Not sure if this is useful, maybe it should be sustain duration
        -- plus the default end dur?  Maybe I should take a default end-note
        -- duration?
        sustain = Num.sum $ case bias of
            BiasEnd -> until_note nexts
            BiasStart -> map fst $ takeWhile is_sustain nexts
    is_sustain (_, Left (T.Rest { rest_sustain })) = rest_sustain
    is_sustain _ = False
    until_note [] = []
    until_note ((dur, n) : ns) = case n of
        Left (T.Rest { rest_sustain }) ->
            dur : if rest_sustain then until_note ns else []
        Right (T.Note {}) -> [dur]

-- | Verify bar durations, infer rests if necessary.  After this, all bars
-- should be a power of 2.
normalize_barlines :: Bias -> [T.Token (T.Note pitch T.HasSpace) T.Rest]
    -> CheckM [T.Token (T.Note pitch ()) T.Rest]
normalize_barlines bias = map_bars $ \case
    [] -> pure []
    bar@(t : _)
        | power_of_2 (length bar) -> pure $ map strip bar
        | power_of_2 (length inferred) -> pure $ map strip inferred
        | otherwise -> do
            warn (T.token_pos t) $
                "bar not a power of 2: " <> showt (length bar)
                <> ", with inferred rests: " <> showt (length inferred)
            pure $ map strip bar
        where
        inferred = infer_rests bias bar
        strip = T.map_note (\n -> n { T.note_duration = () })

-- | The T.Tokens given to the function will not contain T.TBarline.
-- I could put it in the type, but it seems too much bother.
map_bars :: Monad m => ([T.Token n1 r1] -> m [T.Token n2 r2])
    -> [T.Token n1 r1] -> m [T.Token n2 r2]
map_bars f tokens = concat . add <$> mapM f (pre : map snd posts)
    where
    add [] = []
    add (t:ts) = t : zipWith (:) bars ts
    bars = map (T.TBarline . fst) posts
    (pre, posts) = Lists.splitWith is_barline tokens
    is_barline (T.TBarline pos) = Just pos
    is_barline _ = Nothing

power_of_2 :: Int -> Bool
power_of_2 n = snd (properFraction (logBase 2 (fromIntegral n))) == 0

-- | If there aren't enough notes in the bar, try inferring a rest before
-- every odd group of notes followed by space.
infer_rests :: Bias -> [T.Token (T.Note pitch T.HasSpace) T.Rest]
    -> [T.Token (T.Note pitch T.HasSpace) T.Rest]
infer_rests bias = concatMap infer . Lists.splitAfter has_space
    where
    infer tokens
        | even (length tokens) = tokens
        | bias == BiasEnd = extra_rest : tokens
        | otherwise = tokens ++ [extra_rest]
    extra_rest = T.TRest T.fake_pos $ T.Rest True T.HasSpace
    has_space = \case
        T.TBarline {} -> True
        T.TNote _ note -> T.note_duration note == T.HasSpace
        T.TRest _ rest -> T.rest_space rest == T.HasSpace

-- | Split on barlines, zipPadded, map across them.
normalize_hands :: Bias -> [T.Token note T.Rest] -> [T.Token note T.Rest]
    -> CheckM ([T.Token note T.Rest], [T.Token note T.Rest])
normalize_hands bias lefts rights =
    fmap (bimap join_bars join_bars . unzip) $ mapM normalize $
    Lists.zipPadded (split_bars lefts) (split_bars rights)
    where
    normalize = \case
        Lists.First (pos, lefts) -> do
            warn pos "left hand with no right hand"
            pure ((pos, lefts), (pos, []))
        Lists.Second (pos, rights) -> do
            warn pos "right hand with no left hand"
            pure ((pos, []), (pos, rights))
        Lists.Both (pos0, lefts) (pos1, rights) ->
            case (log2 (length lefts), log2 (length rights)) of
                (Nothing, _) -> do
                    warn pos0 "not power of 2"
                    pure ((pos0, lefts), (pos1, rights))
                (_, Nothing) -> do
                    warn pos1 "not power of 2"
                    pure ((pos0, lefts), (pos1, rights))
                (Just d1, Just d2)
                    | delta >= 0 ->
                        pure ((pos0, lefts), (pos1, expand pos1 delta rights))
                    | otherwise ->
                        pure ((pos0, expand pos0 delta lefts), (pos1, rights))
                    where delta = d1 - d2
    expand pos delta = case bias of
        BiasEnd -> concatMap (\n -> replicate (delta^2) (rest pos) ++ [n])
        BiasStart -> concatMap (: replicate (delta^2) (rest pos))
    rest pos = T.TRest pos (T.Rest True T.NoSpace)

log2 :: Int -> Maybe Int
log2 n
    | frac == 0 = Just i
    | otherwise = Nothing
    where (i, frac) = properFraction $ logBase 2 (fromIntegral n)

split_bars :: [T.Token note rest] -> [(T.Pos, [T.Token note rest])]
split_bars [] = []
split_bars tokens@(t0 : _) = (T.token_pos t0, group0) : groups
    where
    (group0, groups) = Lists.splitWith is_barline tokens
    is_barline (T.TBarline pos) = Just pos
    is_barline _ = Nothing

join_bars :: [(T.Pos, [T.Token note rest])] -> [T.Token note rest]
join_bars [] = []
join_bars ((_, g0) : gs) = concat $ g0 : map join gs
    where join (pos, tokens) = T.TBarline pos : tokens
