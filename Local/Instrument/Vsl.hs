-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TupleSections #-}
-- | Vienna Symphonic Library.
module Local.Instrument.Vsl where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Map as Map
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Trill as Trill
import qualified Derive.Derive as Derive
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Tag as Tag
import qualified Local.Instrument.VslInst as VslInst
import Global


-- * util

-- | For interactive use, find keyswitches with the given attributes.
find_attrs :: Instrument.InstrumentName -> String -> [Text]
find_attrs inst with_attrs =
    map ShowVal.show_val $ filter (`Score.attrs_contain` search)
        (patch_attrs patch)
    where
    search = either (error . untxt) id (Parse.parse_attrs with_attrs)
    patch = fromMaybe (error $ "patch not found: " ++ show inst) $
        List.find ((==inst) . Instrument.patch_name) (map fst patches)

-- | Write matrices to a file for visual reference.
write_matrices :: IO ()
write_matrices = Text.IO.writeFile "matrices.txt" $ Text.unlines $
    map show_matrix (map (fst . fst) instruments)

show_matrix :: VslInst.Instrument -> Text
show_matrix (name, _, attrs) =
    name <> ":\n" <> Text.unlines (map format matrices)
    where
    matrices = Seq.chunked 12 $ concatMap (Seq.chunked 12)
        (map_shape strip attrs)
    format = Text.unlines . TextUtil.formatColumns 1
        -- . (header:) . map (map ShowVal.show_val)
        . zipWith (:) col_header . (header:) . map (map ShowVal.show_val)
    header = take 12 $ map showt [1..]
    col_header = take 13 $ map Text.singleton "-abcdefghijkl"
    strip = strip_attrs . map (`Score.attrs_diff` variants)
    variants = VslInst.updown <> VslInst.crescdim <> VslInst.highlow

-- | Transform elements but retain the matrix's shape.
map_shape :: ([a] -> [b]) -> [[a]] -> [[b]]
map_shape f rows = split (map length rows) $ f (concat rows)
    where
    split (len:lens) xs = pre : split lens post
        where (pre, post) = splitAt len xs
    split [] _ = []

-- * instrument definition

load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return synth_descs

synth_descs :: [MidiInst.SynthDesc]
synth_descs = MidiInst.make $
    (MidiInst.softsynth synth "Vienna Symphonic Library" (-2, 2) [])
    { MidiInst.extra_patches = patches }

synth :: Instrument.SynthName
synth = "vsl"

patches :: [MidiInst.Patch]
patches =
    [add_code hmap (make_patch inst category)
        | ((inst, hmap), category) <- instruments]
    where
    add_code hmap patch = (patch, code)
        where code = MidiInst.note_calls (note_calls hmap patch)

instruments :: [((VslInst.Instrument, Maybe HarmonicMap), Text)]
instruments = concatMap tag $
    (solo_string_instruments, Tag.c_strings)
    : no_hmap
    [ (VslInst.strings, Tag.c_strings)
    , (VslInst.harps, Tag.c_strings)
    , (VslInst.woodwinds1, Tag.c_woodwinds)
    , (VslInst.woodwinds2, Tag.c_woodwinds)
    , (VslInst.brass1, Tag.c_brass)
    ]
    where
    tag (inst, t) = map (, t) inst
    no_hmap = map (first (map (, Nothing)))

solo_string_instruments :: [(VslInst.Instrument, Maybe HarmonicMap)]
solo_string_instruments = map (second Just)
    [ (VslInst.solo_violin, violin_harmonics)
    , (VslInst.solo_viola, viola_harmonics)
    , (VslInst.solo_cello, cello_harmonics)
    , (VslInst.solo_bass, bass_harmonics)
    ]

-- | Add various note calls, depending on the attributes that the patch
-- understands.
note_calls :: Maybe HarmonicMap -> Instrument.Patch
    -> [MidiInst.Call Derive.Note]
note_calls maybe_hmap patch =
    with_attr Attrs.trill [g "tr" Trill.c_attr_trill]
    <> with_attr Attrs.trem [MidiInst.both "trem" Trill.c_attr_tremolo]
    <> with_attr VslInst.grace [g "g" (grace_call (patch_attrs patch))]
    <> with_attr VslInst.legato [g "(" Articulation.c_attr_legato]
    <> MidiInst.null_call (note_call patch)
    where
    g = MidiInst.generator
    with_attr attr calls = if has_attr attr patch then calls else []

    -- Like the standard note call, but ignore attrs that are already handled
    -- with keyswitches.
    note_call patch = Note.note_call "" "" mempty $
        maybe (Note.default_note config) (natural_harmonic config) maybe_hmap
        where config = note_config patch
    note_config patch = Note.use_attributes
        { Note.config_staccato = not $ has_attr Attrs.staccato patch }

patch_attrs :: Instrument.Patch -> [Score.Attributes]
patch_attrs = Instrument.mapped_attributes . Instrument.patch_attribute_map

has_attr :: Score.Attributes -> Instrument.Patch -> Bool
has_attr attr = any (`Score.attrs_contain` attr) . patch_attrs

grace_call :: [Score.Attributes] -> Derive.Generator Derive.Note
grace_call attrs =
    Grace.c_grace_attr (Map.filter (`elem` attrs) grace_intervals)

grace_intervals :: Map.Map Int Score.Attributes
grace_intervals = Map.fromList $
    [(n, VslInst.grace <> VslInst.up <> attrs) | (n, attrs) <- ints]
    ++ [(-n, VslInst.grace <> VslInst.down <> attrs) | (n, attrs) <- ints]
    where ints = zip [1..] VslInst.intervals_to_oct

-- | If +harm+nat (and optionally a string) attributes are present, try to
-- play this pitch as a natural harmonic.  That means replacing the pitch and
-- reapplying the default note call.
natural_harmonic :: Note.Config -> HarmonicMap -> Note.GenerateNote
natural_harmonic config (strings, hmap) args = do
    attrs <- Call.get_attrs
    with_pitch <- if Score.attrs_contain attrs (Attrs.harm <> VslInst.nat)
        then harmonic_pitch $ List.find (Score.attrs_contain attrs) strings
        else return id
    with_pitch $ Note.default_note config args
    where
    harmonic_pitch maybe_string = do
        nn <- Derive.require "note pitch"
            =<< Derive.nn_at =<< Args.real_start args
        let pitch = Midi.to_key (round nn)
        case find_harmonic hmap pitch maybe_string of
            Nothing -> Derive.throw $ pretty pitch <> " unplayable on "
                <> maybe (pretty strings) pretty maybe_string
            Just key -> return $
                Call.with_pitch (Pitches.nn_pitch (Midi.from_key key))

-- * keyswitches

type Instrument = (Instrument.InstrumentName, [Keyswitch])
type Keyswitch = (Score.Attributes, [Instrument.Keyswitch])

make_patch :: VslInst.Instrument -> Text -> Instrument.Patch
make_patch inst category =
    instrument_patch category (second strip (make_instrument inst))
    where strip = uncurry zip . first strip_attrs . unzip

instrument_patch :: Text -> Instrument -> Instrument.Patch
instrument_patch category (name, keyswitches) =
    Instrument.add_tag (Tag.category, category) $
    (Instrument.attribute_map #= keyswitch_map keyswitches) $
        MidiInst.patch (-2, 2) name []

make_instrument :: VslInst.Instrument -> Instrument
make_instrument (name, keys, attrs) = (name, matrix keys attrs)

keyswitch_map :: [Keyswitch] -> Instrument.AttributeMap
keyswitch_map = Instrument.keyswitches . Seq.sort_on (attr_key . fst) . process
    where
    process keyswitches = zip attrs ks
        where (attrs, ks) = unzip (drop_dups keyswitches)
    drop_dups = Seq.unique_on fst
    attr_key = negate . Set.size . Score.attrs_set

-- | Since the VSL matrix is only 12x12, a row of articulations greater than
-- that overflows to the next row.  Given that I'm definitely going to overflow
-- 12 rows, I wind up overflowing to the next matrix, and when counting tha AB
-- switch, each articulation has 3 or 4 keyswitches.
--
-- I could probably cram most instruments into 144 cells and only require 2-3
-- keyswitches, but it would be hard to read and easy to mess up, wouldn't let
-- me disable and enable cells by row, and with custom patches I'll probably
-- wind up with more than 144 anyway.
matrix :: VslInst.Keys -> [[Score.Attributes]] -> [Keyswitch]
matrix keys = add . Seq.chunked 12 . concatMap (Seq.chunked 12)
    where
    add matrices = do
        (matrix_ks, rows) <- zip select_matrix matrices
        (row_ks, row) <- zip yaxis rows
        (col_ks, cell) <- zip xaxis row
        (ab_ks, attrs) <- maybe [([], cell)] (zip (map (:[]) ab)) $
            expand_ab cell
        return (attrs, matrix_ks : row_ks : col_ks : ab_ks)
    xaxis = keys_from (VslInst.key_xaxis keys)
    yaxis = keys_from (VslInst.key_yaxis keys)
    ab = keys_from (VslInst.key_ab keys)
    select_matrix = keys_from (VslInst.key_matrix keys)

keys_from :: Midi.Key -> [Instrument.Keyswitch]
keys_from low_key = map Instrument.Keyswitch [low_key ..]

-- * attrs

strip_attrs :: [Score.Attributes] -> [Score.Attributes]
strip_attrs attrs = snd $ foldr strip_attr (Set.fromList attrs, attrs) strip
    where
    strip = reverse
        [ VslInst.sus, VslInst.vib, VslInst.perf, VslInst.fast, VslInst.fa
        , VslInst.norm, VslInst.na, VslInst.legato, VslInst.v1, VslInst.art
        , VslInst.med, VslInst.short
        ]

-- | Strip the given attr, but only if it wouldn't cause clashes.
strip_attr :: Score.Attributes -> (Set.Set Score.Attributes, [Score.Attributes])
    -> (Set.Set Score.Attributes, [Score.Attributes])
strip_attr attr (all_attrs_set, all_attrs)
    | any (`Score.attrs_contain` attr) all_attrs =
        List.mapAccumL strip_redundant all_attrs_set all_attrs
    | otherwise = (all_attrs_set, all_attrs)
    where
    -- Initially I had a naive version that search for clashes with a linear
    -- search in 'all_attrs'.  But it turns out to be slow since there are
    -- around 41 instruments * 12 attrs to strip * 285 attrs * 285 for linear
    -- search.  Or something.  Anyway, previously forcing all the patches took
    -- 0.39 CPU seconds, now it's down to 0.19.
    --
    -- This whole calculation winds up in 'patches' as a CAF, so it should be
    -- possible at compile time.
    strip_redundant attrs_set attrs
        | Set.member stripped attrs_set = (attrs_set, attrs)
        | otherwise = (Set.insert stripped attrs_set, stripped)
        where stripped = Score.attrs_diff attrs attr

expand_ab :: Score.Attributes -> Maybe [Score.Attributes]
expand_ab attrs
    | Just stripped <- extract VslInst.updown =
        Just [stripped <> VslInst.up, stripped <> VslInst.down]
    | Just stripped <- extract VslInst.crescdim =
        Just $ map (`strip` VslInst.dyn)
            [stripped <> VslInst.cresc, stripped <> VslInst.dim]
    | Just stripped <- extract VslInst.highlow =
        Just [stripped <> VslInst.high, stripped <> VslInst.low]
    | otherwise = Nothing
    where
    extract attr
        | Score.attrs_contain attrs attr = Just $ strip attrs attr
        | otherwise = Nothing
    strip = Score.attrs_diff


-- * natural harmonics

type HarmonicMap = ([OpenString], Map.Map Midi.Key [(OpenString, Midi.Key)])
type OpenString = Score.Attributes

find_harmonic :: Map.Map Midi.Key [(OpenString, Midi.Key)] -> Midi.Key
    -> Maybe OpenString -> Maybe Midi.Key
find_harmonic hmap pitch maybe_str =
    maybe (fmap snd . Seq.head) lookup maybe_str =<< Map.lookup pitch hmap

harmonic_map :: [(OpenString, Midi.Key)] -> HarmonicMap
harmonic_map strings = (map fst strings ,) $ Map.multimap $ do
    (oct, (str, base)) <- zip [0..] strings
    (key, interval) <- natural_harmonics
    return (add base interval, (str, add key (oct * 12)))
    where add key n = Midi.to_key (Midi.from_key key + n)

violin_harmonics, viola_harmonics, cello_harmonics, bass_harmonics
    :: HarmonicMap
violin_harmonics = harmonic_map $ map (first Score.attr)
    [("g", Key.g3), ("d", Key.d4), ("a", Key.a4), ("e", Key.e4)]
viola_harmonics = harmonic_map $ map (first Score.attr)
    [("c", Key.c3), ("g", Key.g3), ("d", Key.d4), ("a", Key.a4)]
cello_harmonics = harmonic_map $ map (first Score.attr)
    [("c", Key.c2), ("g", Key.g2), ("d", Key.d3), ("a", Key.a3)]
bass_harmonics = harmonic_map $ map (first Score.attr)
    [("e", Key.e1), ("a", Key.a1), ("d", Key.d2), ("g", Key.g2)]

natural_harmonics :: [(Midi.Key, Int)]
natural_harmonics = absolute
    [ (Key.c3, 0)
    , (Key.d3, 12)
    , (Key.e3, 7)
    , (Key.f3, 5)
    , (Key.g3, 4)
    , (Key.gs3, 3)
    , (Key.a3, 3)
    , (Key.as3, 2)
    ]
    where absolute = uncurry zip . second (drop 1 . scanl (+) 0) . unzip
