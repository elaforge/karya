-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Pattern based derivation.
--
-- TODO This is merely a proof of concept.  Sekaran could be implemented a lot
-- of ways, but I'll have to wait to see which ones are most compositionally
-- useful.  It would definitely be more natural, though, if the notation marked
-- the seleh instead of the first note.  But that would require either
-- a preproc pass or deriving the notes backwards.
module Derive.Call.Bali.Sekar where
import qualified Data.Char as Char
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Flags as Flags
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map [("sekar", c_sekar)]

module_ :: Module.Module
module_ = "bali" <> "sekar"

c_sekar :: Derive.Generator Derive.Note
c_sekar = Derive.make_call module_ "sekar" (Tags.inst <> Tags.subs)
    "Arrange sub-notes according to a pattern." $ Sig.call ((,)
    <$> Sig.required "pattern"
        "Apply this pattern to the encompassed notes. The pattern is\
        \ letters from a-z, where `a` is the first note and `z` is the 26th.\
        \ Capital letters replace that note with a rest. Gaps in the input\
        \ notes count as rest notes."
    <*> Sig.environ "arrive" Sig.Prefixed True ("If true, the last note of the\
        \ pattern is aligned to the end of the event, and given "
        <> ShowVal.doc_val Flags.infer_duration <> ".")
    ) $ \(pattern, arrive) args -> do
        pattern <- make_pattern pattern
        let derive = (if arrive then sekar_arrive else sekar)
                (Args.range args) pattern
        mconcatMap derive =<< Sub.sub_rest_events arrive True args

sekar_arrive :: (ScoreTime, ScoreTime) -> Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekar_arrive range pattern events_ =
    Sub.derive $ add_flags $ align $ map (Sub.stretch factor) $
        Sub.strip_rests realized
    where
    -- The first event should be a rest, since I passed end_bias=True to
    -- Sub.sub_rest_events.  The stretch factor assumes the event durations add
    -- up to 'range'.  For that to be true, I have to give the initial rest
    -- duration to the destination note.
    events = case events_ of
        [] -> []
        rest : events -> case Seq.viewr events of
            Nothing -> events
            Just (initial, final)
                | Sub.event_start final >= snd range ->
                    initial ++ [final { Sub.event_duration = dur }]
                | otherwise -> events ++ [Sub.Event (snd range) dur Nothing]
                where dur = Sub.event_duration rest

    realized = realize_groups pattern events
    factor = sum_duration events / sum_duration realized
    -- Align notes to the end of the range.
    align es = case Seq.last es of
        Nothing -> []
        Just e -> map (Sub.at (snd range - Sub.event_start e)) es
    add_flags = Seq.map_last $ fmap $ fmap $ Post.emap1_ $ Score.add_flags $
        Flags.infer_duration <> Flags.cancel_next

sum_duration :: [Sub.GenericEvent a] -> ScoreTime
sum_duration = sum . map Sub.event_duration

sekar :: (ScoreTime, ScoreTime) -> Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekar range pattern events =
    Sub.derive $ map (Sub.place (fst range) factor) $ Sub.strip_rests realized
    where
    realized = realize_groups pattern events
    factor = sum_duration events / sum_duration realized

realize_groups :: Pattern -> [Sub.RestEvent] -> [Sub.RestEvent]
realize_groups _ [] = []
realize_groups pattern events@(event:_) =
    go (Sub.event_start event) $ split_groups (pattern_length pattern) events
    where
    go _ [] = []
    go start (group : groups) =
        notes ++ go (start + sum_duration notes) groups
        where notes = realize start group pattern

split_groups :: Int -> [a] -> [[a]]
split_groups n notes = case splitAt n notes of
    ([], _) -> []
    (pre, post) -> pre : split_groups n post

type Pattern = [(Index, Element)]
-- | Index into melody notes.
type Index = Int

data Element = Note | Rest deriving (Show)

pattern_length :: Pattern -> Int
pattern_length = maximum . (0:) . map ((+1) . fst)

make_pattern :: Text -> Derive.Deriver Pattern
make_pattern pattern = do
    when (Text.null pattern || Text.any (not . a_to_z . Char.toLower) pattern) $
        Derive.throw $ "pattern chars must be a-z: " ++ show pattern
    return
        [ (fromEnum (Char.toLower c) - fromEnum 'a',
            if Char.isUpper c then Rest else Note)
        | c <- Text.unpack pattern
        ]
    where a_to_z c = 'a' <= c && c <= 'z'

-- | Apply the pattern to the events.
realize :: ScoreTime -> [Sub.RestEvent] -> Pattern -> [Sub.RestEvent]
realize start events = map place . add_starts . mapMaybe resolve
    where
    resolve (i, element) = resolve1 element <$> Seq.at events i
    -- Rests have a duration, but no deriver.
    resolve1 element (Sub.Event _ dur d) = case d of
        Nothing -> (dur, Nothing)
        Just deriver -> (,) dur $ case element of
            Note -> Just deriver
            Rest -> Nothing
    add_starts events = zip (scanl (+) start (map fst events)) events
    place (start, (dur, maybe_deriver)) = Sub.Event start dur maybe_deriver
