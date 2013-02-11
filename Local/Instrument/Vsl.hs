-- | Vienna Symphonic Library.
module Local.Instrument.Vsl where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Ornament as Ornament
import qualified Derive.Call.Trill as Trill
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Tag as Tag
import qualified Local.Instrument.VslInst as VslInst
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return synth_descs

synth_descs :: [MidiInst.SynthDesc]
synth_descs = MidiInst.make $
    (MidiInst.softsynth synth "Vienna Symphonic Library" (-2, 2) [])
    { MidiInst.extra_patches = patches }

synth :: Instrument.SynthName
synth = "vsl"

patches :: [MidiInst.Patch]
patches = map (add_code . make_patch) instruments
    where
    add_code patch = (patch, code)
        where code = MidiInst.note_calls (note_calls patch)

instruments :: [(VslInst.Instrument, String)]
instruments = concat
    [ tag Tag.c_strings VslInst.solo_strings
    , tag Tag.c_strings VslInst.strings
    , tag Tag.c_strings VslInst.harps
    , tag Tag.c_woodwinds VslInst.woodwinds1
    , tag Tag.c_woodwinds VslInst.woodwinds2
    , tag Tag.c_brass VslInst.brass1
    ]
    where tag t = map (flip (,) t)

-- | Add various note calls, depending on the attributes that the patch
-- understands.
note_calls :: Instrument.Patch -> [(String, Derive.NoteCall)]
note_calls patch =
    with_attr Attrs.trill
        [("tr", Trill.c_attr_trill), ("`tr`", Trill.c_attr_trill)]
    <> with_attr Attrs.trem [("trem", Trill.c_attr_tremolo)]
    <> with_attr Attrs.staccato (MidiInst.null_call staccato_keyswitch)
    <> with_attr VslInst.grace [("g", grace_call (patch_attrs patch))]
    where
    with_attr attr calls = if has_attr attr patch then calls else []

    -- Like the standard note call, but ignore +staccato, because it has its
    -- own sample.
    staccato_keyswitch = Note.note_call ""
        "Staccato doesn't change note duration, since the sample already has\
        \ that built-in."
        (Note.default_note False)

patch_attrs :: Instrument.Patch -> [Score.Attributes]
patch_attrs = Instrument.keyswitch_attributes . Instrument.patch_keyswitches

has_attr :: Score.Attributes -> Instrument.Patch -> Bool
has_attr attr = any (`Score.attrs_contain` attr) . patch_attrs

grace_call :: [Score.Attributes] -> Derive.NoteCall
grace_call attrs =
    Ornament.c_grace_attr (Map.filter (`elem` attrs) grace_intervals)

grace_intervals :: Map.Map Int Score.Attributes
grace_intervals = Map.fromList $
    [(n, VslInst.grace <> VslInst.up <> attrs) | (n, attrs) <- ints]
    ++ [(-n, VslInst.grace <> VslInst.down <> attrs) | (n, attrs) <- ints]
    where ints = zip [1..] VslInst.intervals_to_oct

-- * keyswitches

type Instrument = (Instrument.InstrumentName, [Keyswitch])
type Keyswitch = (Score.Attributes, [Instrument.Keyswitch])

make_patch :: (VslInst.Instrument, String) -> Instrument.Patch
make_patch (inst, category) =
    instrument_patch category (second strip (make_instrument inst))
    where strip = uncurry zip . first strip_attrs . unzip

instrument_patch :: String -> Instrument -> Instrument.Patch
instrument_patch category (name, keyswitches) =
    Instrument.add_tag (Tag.category, category) $
    (Instrument.keyswitches #= keyswitch_map keyswitches) $
        MidiInst.patch (-2, 2) name []

make_instrument :: VslInst.Instrument -> Instrument
make_instrument (name, keys, attrs) = (name, matrix keys attrs)

keyswitch_map :: [Keyswitch] -> Instrument.KeyswitchMap
keyswitch_map =
    Instrument.KeyswitchMap . Seq.sort_on (attr_key . fst) . process
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

-- | Write matrices to a file for visual reference.
write_matrices :: IO ()
write_matrices = writeFile "matrices.txt" $ unlines $
    map show_matrix (map fst instruments)

show_matrix :: VslInst.Instrument -> String
show_matrix (name, _, attrs) =
    name ++ ":\n" ++ unlines (map format matrices)
    where
    matrices = Seq.chunked 12 $ concatMap (Seq.chunked 12)
        (map_shape strip attrs)
    format = unlines . Seq.format_columns 1
        . zipWith (:) col_header . (header:)
        . map (map (abbr . ShowVal.show_val))
    header = take 12 $ map show [1..]
    col_header = take 13 $ map (:"") "-abcdefghijkl"
    abbr = Seq.replace "staccato" "stac" . Seq.replace "harmonic" "harm"
    strip = strip_attrs . map (`Score.attrs_diff` variants)
    variants = VslInst.updown <> VslInst.crescdim <> VslInst.highlow

-- | Transform elements but retain the matrix's shape.
map_shape :: ([a] -> [b]) -> [[a]] -> [[b]]
map_shape f rows = split (map length rows) $ f (concat rows)
    where
    split (len:lens) xs = pre : split lens post
        where (pre, post) = splitAt len xs
    split [] _ = []

-- * attrs

strip_attrs :: [Score.Attributes] -> [Score.Attributes]
strip_attrs attrs = foldr strip_attr attrs strip
    where
    strip = reverse [VslInst.sus, VslInst.vib, VslInst.perf, VslInst.fast,
        VslInst.norm, VslInst.na, VslInst.legato, VslInst.v1, VslInst.art]

-- | Strip the given attr, but only if it wouldn't cause clashes.
strip_attr :: Score.Attributes -> [Score.Attributes] -> [Score.Attributes]
strip_attr attr all_attrs = map (strip_redundant attr) all_attrs
    where
    strip = flip Score.attrs_diff
    strip_redundant attr attrs
        | stripped `elem` all_attrs = attrs
        | otherwise = stripped
        where stripped = strip attr attrs

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
