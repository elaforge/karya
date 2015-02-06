-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of standard mridangam patterns.
module Derive.Call.India.Mridangam where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import Global
import Types


-- * calls

module_ :: Module.Module
module_ = "india" <> "mridangam"

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map $ faran_calls <> kandam_calls

faran_calls :: [(TrackLang.CallId, Derive.Generator Derive.Note)]
faran_calls = map make farans
    where
    make faran =
        ( TrackLang.Symbol (faran_name faran)
        , c_faran_nadai $ concat $ realize_faran faran
        )

kandam_calls :: [(TrackLang.CallId, Derive.Generator Derive.Note)]
kandam_calls = map make kandam_patterns
    where make (name, notes) = (TrackLang.Symbol name, c_kandam notes)

c_kandam :: [Note] -> Derive.Generator Derive.Note
c_kandam notes = Derive.make_call (module_ <> "kandam") "kandam" Tags.inst
    "Emit a kandam pattern."
    $ Sig.call0 $ \args -> do
        let dur = Args.duration args / fromIntegral (length notes)
        realize (Args.info args) $ zip (Seq.range_ (Args.start args) dur) notes

-- | TODO not bound, but seems like it may be useful
c_faran_stretch :: [Note] -> Derive.Generator Derive.Note
c_faran_stretch notes = Derive.make_call (module_ <> "faran") "faran" Tags.inst
    "Emit one of the standard mridangam farans. Each complete pattern is\
    \ 4 avartanams long."
    $ Sig.call (Sig.defaulted "aksharas" 0
        "Emit this many aksharas of adi talam, possibly fractional. There are 4\
        \ matras in each akshara, and 8 aksharas per avartanam. \
        \ A positive number takes from the beginning, a negative number from\
        \ the end. 0 plays all of them."
    ) $ \aksharas args ->
        stretch_call notes aksharas (Args.range args) (Args.info args)

-- | Emit the Notes, stretched to the given range.
stretch_call :: [Note] -> Double -> (ScoreTime, ScoreTime)
    -> Derive.CallInfo Score.Event -> Derive.NoteDeriver
stretch_call notes aksharas (start, end) cinfo =
    realize cinfo $ zip (Seq.range_ start dur) clipped
    where
    dur = (end - start) / fromIntegral (length clipped)
    clipped
        | aksharas == 0 = notes
        | aksharas > 0 = take wanted notes
        | otherwise = Seq.rtake wanted notes
    wanted = floor (abs aksharas * 4 :: Double) -- 4 matras per akshara.

c_faran_nadai :: [Note] -> Derive.Generator Derive.Note
c_faran_nadai notes = Derive.make_call (module_ <> "faran") "faran" Tags.inst
    "Emit one of the standard mridangam farans. Each complete pattern is\
    \ 4 avartanams long. It will be clipped to fit into the event."
    $ Sig.call ((,)
    <$> Sig.defaulted "nadai" 4 "Fit this many notes into one akshara."
    <*> Sig.defaulted "offset" 0 "Drop offset*32 notes from the beginning."
    ) $ \(TrackLang.Positive nadai, TrackLang.Positive offset) args -> do
        -- TODO assume 1 akshara is 1 ScoreTime, but I can also ask the ruler
        let akshara = 1
        nadai_call notes akshara nadai offset (Args.range args)
            (Args.info args)

-- | Emit the notes at the given akshara tempo, clipped to the range.
nadai_call :: [Note] -> ScoreTime -> Int -> Double -> (ScoreTime, ScoreTime)
    -> Derive.CallInfo Score.Event -> Derive.NoteDeriver
nadai_call notes akshara nadai offset (start, end) cinfo =
    realize cinfo $ zip (Seq.range_ start note_dur) $
        take n_notes (drop (floor (offset * 32)) notes)
    where
    note_dur = akshara / fromIntegral nadai
    n_notes = floor $ (end - start) / note_dur

realize :: Derive.CallInfo Score.Event -> [(ScoreTime, Note)]
    -> Derive.NoteDeriver
realize cinfo = mconcat . map apply . filter ((/=x) . snd)
    where
    apply (start, note) =
        Derive.place start 0 $ Eval.apply_generator cinfo (note_to_call note) []

-- * make track

type Track = [(ScoreTime, ScoreTime, TrackLang.CallId)]

make_faran_track :: ScoreTime -> Text -> Double -> Maybe Track
make_faran_track duration name aksharas =
    make <$> List.find ((==name) . faran_name) farans
    where
    make faran = make_track duration (concat (realize_faran faran)) aksharas

make_track :: ScoreTime -> [Note] -> Double -> Track
make_track duration notes aksharas =
    [ (fromIntegral i * dur, 0, note_to_call note)
    | (i, note) <- zip [0..] clipped
    , note /= x
    ]
    where
    dur = duration / fromIntegral (length clipped)
    -- TODO copy paste from stretch_call
    clipped
        | aksharas == 0 = notes
        | aksharas > 0 = take wanted notes
        | otherwise = Seq.rtake wanted notes
    wanted = floor (abs aksharas * 4 :: Double) -- 4 matras per akshara.

note_to_call :: Note -> TrackLang.CallId
note_to_call (Note t v) =
    TrackLang.Symbol $ maybe "" prettyt v <> maybe "" prettyt t

-- * implementation

data Thoppi = Tha | Thom deriving (Eq, Show)
data Valantalai = Ki | Ta | Nam | Din | Chapu | Dim deriving (Eq, Show)

-- | 'note_to_call' uses these, so they should match up with the calls in the
-- instrument.
instance Pretty.Pretty Thoppi where
    prettyt x = case x of
        Tha -> "+"
        Thom -> "o"
instance Pretty.Pretty Valantalai where
    prettyt x = case x of
        Ki -> "k"
        Ta -> "t"
        Nam -> "n"
        Din -> "d"
        Chapu -> "u"
        Dim -> "i"

data Note = Note (Maybe Thoppi) (Maybe Valantalai)
    deriving (Eq, Show)

instance Pretty.Pretty Note where
    prettyt (Note Nothing Nothing) = "-"
    prettyt (Note thoppi val) = maybe "" prettyt thoppi <> maybe "" prettyt val

type Pattern = [Note]

right :: Valantalai -> Note
right = Note Nothing . Just

left :: Thoppi -> Note
left t = Note (Just t) Nothing

both :: Thoppi -> Valantalai -> Note
both t v = Note (Just t) (Just v)

-- | The convention is that closed strokes use consonants, ringing strokes use
-- vowels.  Pitched closed notes use voiced consonants.
k, t, n, d, u, i :: Note
k = right Ki -- or Ti
t = right Ta
n = right Nam
d = right Din
u = right Chapu
i = right Dim

o, p :: Note
o = left Thom
p = left Tha

od, ok, ot :: Note
od = both Thom Din
ok = both Thom Ki
ot = both Thom Ta

x :: Note
x = Note Nothing Nothing

-- | Return one Pattern per avartanam.
realize_faran :: Faran -> [Pattern]
realize_faran faran = map concat
    [ [pattern, pattern]
    , [start, start, pattern]
    , [short, short, faran_fill faran, pattern]
    , [short, short, short, fill2, drop (length pattern - rest) pattern]
    ]
    where
    rest = 32 - length short * 3 - length fill2
    fill2 = if null (faran_fill2 faran) then faran_fill faran
        else faran_fill2 faran
    pattern = faran_start faran ++ faran_end faran
    start = faran_start faran
    short = take 6 start

data Faran = Faran {
    faran_name :: !Text
    -- | 8 Notes.
    , faran_start :: !Pattern
    -- | 4 Notes.
    , faran_fill :: !Pattern
    -- | 4 or more Notes, used for the last avartanam.
    , faran_fill2 :: !Pattern
    , faran_end :: !Pattern
    -- | Just for reference.
    , faran_solkattu :: ![Text]
    } deriving (Show)

farans :: [Faran]
farans = concat
    [ map (fill [p, n, p, k])
        [ f "ktkn" [k,  t,  k,  n,  p,  k,  t,  k] "ki ta ki na ta ki ta ki"
        , f "ookn" [o,  o,  k,  n,  p,  k,  t,  k] ""
        , f "oonn" [o,  o,  n,  n,  p,  k,  t,  k] ""
        , f "o-on" [od, x,  od, n,  p,  k,  t,  k] ""
        , f "odon" [o,  d,  o,  n,  p,  k,  t,  k] ""
        , f "tkon" [ot, k,  o,  n,  p,  k,  t,  k] ""
        , f "ou-n" [o,  u,  x,  n,  p,  k,  t,  k] ""
        , f "+u-n" [p,  u,  x,  n,  p,  k,  t,  k] ""
        ]
    , map (fill [o, u, x, k])
        [ f "ou-k" [o,  u,  x,  k,  k,  o,  o,  k] ""
        , f "ou+k" [o,  u,  p,  k,  k,  o,  o,  k] ""
        , f "okou" [o,  k,  o,  u,  x,  k,  t,  k] ""
        , f "okou+" [o,  k,  o,  u,  p,  k,  t,  k] ""
        ]
    , map (fill [o, x, k, x])
        [ f "okoo" [o,  k,  o,  o,  k,  o,  o,  k] ""
        , f "o-ko" [o,  x,  k,  o,  k,  o,  ot, k] ""
        , f "ooko" [o,  o,  k,  o,  k,  o,  ot, k] ""
        , f "o-kt" [o,  x,  k,  t,  k,  o,  ot, k] ""
        , f "ookt" [o,  o,  k,  t,  k,  o,  ot, k] ""
        , f "k-kt" [k,  x,  k,  t,  k,  o,  ot, k] ""
        , f "k+kt" [k,  p,  k,  t,  k,  o,  ot, k] ""
        ]
    ,   [ fill [o, o, k, t] $ f "+koo" [p,  k,  o,  o,  k,  t,  p,  k] ""
        , fill [o, x, k, x] $ fill2 [o, k, p, k, t] $
            f "nk++" [n,  k,  p,  p,  k,  o,  o,  k] ""
        , fill [n, o, ok, x] $ fill2 [ok, x, u, x] $
            f "nok-" [n,  o,  ok, x,  u,  x,  p,  k] ""
        ]
    ]
    where
    f name start sol = Faran
        { faran_name = name
        , faran_start = start
        , faran_fill = []
        , faran_fill2 = []
        , faran_end = end
        , faran_solkattu = Text.words sol
        }
    fill p faran = faran { faran_fill = p }
    fill2 p faran = faran { faran_fill2 = p }
    end = [n, p, u, p, k, t, p, k] -- na ka tha ri ki ta tha ka


-- * kandam

kandam_patterns :: [(Text, Pattern)]
kandam_patterns = map (first (<>"5"))
    [ ("ktkn", [k, x, t, x, k, x, n, x, o, x])
    , ("ktkk", [k, x, t, x, k, x, k, n, o, x])
    , ("ktkkt", [k, x, t, x, k, x, k, t, o, x])
    , ("ktkt", [k, t, k, t, p, k, p, t, o, x])
    , ("n-kt", [n, x, k, t, p, k, p, t, o, x])
    , ("u+kt", [u, p, k, t, p, k, p, t, o, x])
    , ("kt+k", [k, t, p, k, p, k, t, k, n, o])
    , ("k-t-", [k, x, t, x, k, t, x, k, n, o])
    , ("+ko-", [p, k, od,x, k, t, k, n, o, k])
    ]
