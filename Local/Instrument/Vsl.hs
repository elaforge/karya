-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE MultiWayIf #-}
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
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Prelude.Articulation as Articulation
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Prelude.Trill as Trill
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Tag as Tag

import qualified Local.Instrument.VslInst as VslInst
import Global
import Types


synth :: MidiInst.Synth
synth = MidiInst.synth "vsl" "Vienna Symphonic Library" $
    MidiInst.synth_controls controls patches
    where
    controls =
        [ (11, "expression") -- apparently like cc7 volume?
        , (22, "attack") -- attack time
        , (23, "release") -- release time
        , (24, "filter") -- low pass filter
        , (20, "slot-xf") -- xfade between a and b slot positions
        , (2,  "velocity-xf") -- xfade between vel layers, set breath dyn
        , (28, "velocity-xf-on") -- velocity xfade on/off
        , (29, "rsamp-on") -- release samples on/off
        , (25, "delay") -- scale per patch delay
        , (26, "tuning") -- scale out of tune curve
        , (27, "humanize") -- scale both 'delay' and 'tuning'
        , (30, "dyn-range") -- scale effect of velocity
        , (21, "start-scaler") -- scale start offset
        , (14, "reverb") -- reverb wet
        , (15, "reverb-on") -- reverb on/off
        ]

-- * util

-- | For interactive use, find keyswitches with the given attributes.
find_attrs :: InstTypes.Name -> String -> [Text]
find_attrs inst with_attrs =
    map ShowVal.show_val $ filter (`Attrs.contain` search)
        (patch_attributes (MidiInst.patch_patch patch))
    where
    search = either (error . untxt) id (Parse.parse_attrs with_attrs)
    patch = fromMaybe (error $ "patch not found: " ++ show inst) $
        List.find ((==inst) . Patch.patch_name . MidiInst.patch_patch) patches

-- | Write matrices to a file for visual reference.
write_matrices :: IO ()
write_matrices = Text.IO.writeFile "matrices.txt" $ Text.unlines $
    map (show_matrix . fst . fst) instruments

show_matrix :: VslInst.Instrument -> Text
show_matrix (name, _, attrs) =
    name <> ":\n" <> Text.unlines (map format matrices)
    where
    matrices = Seq.chunked cols $ concatMap (Seq.chunked cols)
        (map_shape strip attrs)
    format = Text.unlines . TextUtil.formatColumns 1
        . zipWith (:) col_header . (header:) . map (map ShowVal.show_val)
    header = take cols $ map showt [1..]
    col_header = take (cols+1) $ map Text.singleton $ '-' : ['a'..]
    strip = strip_attrs . map (`Attrs.difference` variants)
    variants = VslInst.updown <> VslInst.crescdim <> VslInst.highlow
    cols = 12

-- | Transform elements but retain the matrix's shape.
map_shape :: ([a] -> [b]) -> [[a]] -> [[b]]
map_shape f rows = split (map length rows) $ f (concat rows)
    where
    split (len:lens) xs = pre : split lens post
        where (pre, post) = splitAt len xs
    split [] _ = []

-- * instrument definition

patches :: [MidiInst.Patch]
patches =
    [ open_strings hmap $ add_code hmap (make_patch inst category)
    | ((inst, hmap), category) <- instruments
    ]
    where
    add_code hmap patch = MidiInst.code
        #= MidiInst.note_calls (note_calls hmap (MidiInst.patch_patch patch)) $
            patch
    open_strings Nothing = id
    open_strings (Just hmap) =
        MidiInst.environ EnvKey.open_strings (hmap_string_nns hmap)

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
note_calls :: Maybe HarmonicMap -> Patch.Patch -> [MidiInst.Call Derive.Note]
note_calls maybe_hmap patch =
    with_attr Attrs.trill [g "tr" (Trill.c_note_trill True Nothing Nothing)]
    <> with_attr Attrs.trem [MidiInst.both "trem" Trill.c_attr_tremolo]
    <> with_attr VslInst.grace [g "g" (grace_call (patch_attributes patch))]
    <> with_attr VslInst.legato [g "(" Articulation.c_attr_slur]
    <> MidiInst.null_call (note_call patch)
    <> [MidiInst.both "sec" c_infer_seconds]
    where
    g = MidiInst.generator
    with_attr attr calls = if has_attr attr patch then calls else []
    note_call patch = Note.note_call ""
        "This is like the standard note call, but ignores attrs that are\
            \ already handled with keyswitches."
        mempty (maybe (Note.default_note config) (harmonic config) maybe_hmap)
        where config = note_config patch
    note_config patch = Note.use_attributes
        { Note.config_staccato = not $ has_attr Attrs.staccato patch }

patch_attributes :: Patch.Patch -> [Attrs.Attributes]
patch_attributes = Common.mapped_attributes . Patch.patch_attribute_map

has_attr :: Attrs.Attributes -> Patch.Patch -> Bool
has_attr attr = any (`Attrs.contain` attr) . patch_attributes

grace_call :: [Attrs.Attributes] -> Derive.Generator Derive.Note
grace_call attrs =
    Grace.c_attr_grace (Map.filter (`elem` attrs) grace_intervals)

grace_intervals :: Map Int Attrs.Attributes
grace_intervals = Map.fromList $
    [(n, VslInst.grace <> VslInst.up <> attrs) | (n, attrs) <- ints]
    ++ [(-n, VslInst.grace <> VslInst.down <> attrs) | (n, attrs) <- ints]
    where ints = zip [1..] VslInst.intervals_to_oct

-- | If +harm+nat (and optionally a string) attributes are present, try to
-- play this pitch as a natural harmonic.  That means replacing the pitch and
-- reapplying the default note call.
harmonic :: Note.Config -> HarmonicMap -> Note.GenerateNote
harmonic config hmap args = do
    attrs <- Call.get_attributes
    let has = Attrs.contain attrs
    with_pitch <- if
        | has (Attrs.harm <> Attrs.natural) ->
            natural_harmonic (has Attrs.gliss) $
                List.find has (hmap_strings hmap)
        -- VSL has its artificial harmonics pitched one octave too high.
        | has Attrs.harm -> return $ Call.add_constant Controls.octave (-1)
        | otherwise -> return id
    with_pitch $ Note.default_note config args
    where
    natural_harmonic gliss maybe_string = do
        nn <- Derive.require "note pitch"
            =<< Derive.nn_at =<< Args.real_start args
        let pitch = Midi.to_key (round nn)
        case find_harmonic hmap gliss pitch maybe_string of
            Nothing -> Derive.throw $ pretty pitch <> " unplayable on "
                <> maybe (pretty (hmap_strings hmap)) pretty maybe_string
            Just key -> return $
                Call.with_pitch (PSignal.nn_pitch (Midi.from_key key))

-- * keyswitches

type Instrument = (InstTypes.Name, [Keyswitch])
type Keyswitch = (Attrs.Attributes, [Patch.Keyswitch])

make_patch :: VslInst.Instrument -> Text -> MidiInst.Patch
make_patch inst category =
    instrument_patch category (second strip (make_instrument inst))
    where strip = uncurry zip . first strip_attrs . unzip

instrument_patch :: Text -> Instrument -> MidiInst.Patch
instrument_patch category (name, keyswitches) =
    -- MidiInst.pressure means I expect to have velocity-xf enabled and
    -- assigned to cc2.
    MidiInst.pressure $
    MidiInst.common#Common.tags %= ((Tag.category, category) :) $
    MidiInst.attribute_map #= keyswitch_map keyswitches $
        MidiInst.named_patch (-2, 2) name []

make_instrument :: VslInst.Instrument -> Instrument
make_instrument (name, keys, attrs) = (name, matrix keys attrs)

keyswitch_map :: [Keyswitch] -> Patch.AttributeMap
keyswitch_map = Patch.keyswitches . Seq.sort_on (priority . fst) . process
    where
    process keyswitches = zip attrs ks
        where (attrs, ks) = unzip (drop_dups keyswitches)
    drop_dups = Seq.unique_on fst
    priority attrs = Map.findWithDefault 0 attrs attribute_priority

-- | Order attributes by priority.  This should correspond to specificity, or
-- to perceptual importance, as documented in 'Patch.AttributeMap'.
attribute_priority :: Map Attrs.Attributes Int
attribute_priority = Map.fromList ((`zip` [-1, -2 ..]) (reverse high)) <> low
    where
    high =
        [ VslInst.pizz
        , VslInst.spiccato
        , VslInst.harsh
        , VslInst.staccato
        , VslInst.detache
        , VslInst.detache <> VslInst.long
        ]
    low = Map.fromList [(VslInst.nv, 100)]

-- | Since the VSL matrix is only 12x12, a row of articulations greater than
-- that overflows to the next row.  Given that I'm definitely going to overflow
-- 12 rows, I wind up overflowing to the next matrix, and when counting tha AB
-- switch, each articulation has 3 or 4 keyswitches.
--
-- I could probably cram most instruments into 144 cells and only require 2-3
-- keyswitches, but it would be hard to read and easy to mess up, wouldn't let
-- me disable and enable cells by row, and with custom patches I'll probably
-- wind up with more than 144 anyway.
matrix :: VslInst.Keys -> [[Attrs.Attributes]] -> [Keyswitch]
matrix keys = add . Seq.chunked 12 . concatMap (Seq.chunked 12)
    where
    add matrices = do
        (matrix_ks, rows) <- zip select_matrix matrices
        (row_ks, row) <- zip y_axis rows
        (col_ks, cell) <- zip x_axis row
        (ab_ks, attrs) <- maybe [([], cell)] (zip (map (:[]) ab)) $
            expand_ab cell
        return (attrs, matrix_ks : row_ks : col_ks : ab_ks)
    x_axis = keys_from (VslInst.key_x_axis keys)
    y_axis = keys_from (VslInst.key_y_axis keys)
    ab = keys_from (VslInst.key_ab keys)
    select_matrix = keys_from (VslInst.key_matrix keys)

keys_from :: Midi.Key -> [Patch.Keyswitch]
keys_from low_key = map Patch.Keyswitch [low_key ..]

-- * attrs

-- | Remove attrs which can be assumed as a default.  The idea is to make it
-- easier to address an articulation while still remaining non-ambiguous.
--
-- Attrs are removed in order unless removal would create a conflict with
-- another articulation.  The result is that the attrs early in the strip list
-- are removed first, so if you have both 'VslInst.med' and 'VslInst.short',
-- 'VslInst.med' will become the default, while 'VslInst.short' retains its
-- attribute.
strip_attrs :: [Attrs.Attributes] -> [Attrs.Attributes]
strip_attrs attrs = snd $ foldr strip_attr (Set.fromList attrs, attrs) strip
    where
    strip = reverse
        [ VslInst.sus, VslInst.vib, VslInst.perf, VslInst.fast, VslInst.fa
        , VslInst.norm, VslInst.na, VslInst.legato, VslInst.v1, Attrs.artificial
        , VslInst.med, VslInst.short
        ]

-- | Strip the given attr, but only if it wouldn't cause clashes.
strip_attr :: Attrs.Attributes -> (Set Attrs.Attributes, [Attrs.Attributes])
    -> (Set Attrs.Attributes, [Attrs.Attributes])
strip_attr attr (all_attrs_set, all_attrs)
    | any (`Attrs.contain` attr) all_attrs =
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
    -- possible to run at compile time, presumably via TH.  In any case, even
    -- as a normal CAF, it only happens once per run.
    strip_redundant attrs_set attrs
        | Set.member stripped attrs_set = (attrs_set, attrs)
        | otherwise = (Set.insert stripped attrs_set, stripped)
        where stripped = Attrs.difference attrs attr

expand_ab :: Attrs.Attributes -> Maybe [Attrs.Attributes]
expand_ab attrs
    | Just stripped <- extract VslInst.updown =
        Just [stripped <> VslInst.up, stripped <> VslInst.down]
    | Just stripped <- extract VslInst.crescdim =
        Just $ map (`Attrs.difference` VslInst.dyn)
            [stripped <> VslInst.cresc, stripped <> VslInst.dim]
    | Just stripped <- extract VslInst.highlow =
        Just [stripped <> VslInst.high, stripped <> VslInst.low]
    | otherwise = Nothing
    where
    extract attr
        | Attrs.contain attrs attr = Just $ Attrs.difference attrs attr
        | otherwise = Nothing


-- * natural harmonics

data HarmonicMap = HarmonicMap {
    hmap_strings :: [OpenString]
    , hmap_string_nns :: [Pitch.NoteNumber]
    -- | Map sounding pitch to possible strings and the key to play to get that
    -- pitch on that string.
    , hmap_key_to_natural :: Map Midi.Key [(OpenString, Midi.Key)]
    -- | Same as 'hmap_key_to_natural' except map from the gliss destination.
    , hmap_key_to_gliss_destination :: Map Midi.Key [(OpenString, Midi.Key)]
    } deriving (Show)
type OpenString = Attrs.Attributes

find_harmonic :: HarmonicMap -> Bool -> Midi.Key -> Maybe OpenString
    -> Maybe Midi.Key
find_harmonic hmap gliss pitch maybe_str =
    maybe (fmap snd . Seq.head) lookup maybe_str =<< Map.lookup pitch m
    where
    m = (if gliss then hmap_key_to_gliss_destination else hmap_key_to_natural)
        hmap

harmonic_map :: [(OpenString, Midi.Key)] -> HarmonicMap
harmonic_map strings = HarmonicMap
    { hmap_strings = map fst strings
    , hmap_string_nns = map (Pitch.key_to_nn . snd) strings
    , hmap_key_to_natural = make natural_harmonics
    , hmap_key_to_gliss_destination = make gliss_natural_harmonics
    }
    where
    make key_to_interval = Map.multimap $ do
        (oct, (str, base)) <- zip [0..] strings
        (key, interval) <- key_to_interval
        return (add base interval, (str, add key (oct * 12)))
        where add key n = Midi.to_key (Midi.from_key key + n)

violin_harmonics, viola_harmonics, cello_harmonics, bass_harmonics
    :: HarmonicMap
violin_harmonics = harmonic_map $ map (first Attrs.attr)
    [("g", Key.g3), ("d", Key.d4), ("a", Key.a4), ("e", Key.e4)]
viola_harmonics = harmonic_map $ map (first Attrs.attr)
    [("c", Key.c3), ("g", Key.g3), ("d", Key.d4), ("a", Key.a4)]
cello_harmonics = harmonic_map $ map (first Attrs.attr)
    [("c", Key.c2), ("g", Key.g2), ("d", Key.d3), ("a", Key.a3)]
bass_harmonics = harmonic_map $ map (first Attrs.attr)
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

gliss_natural_harmonics :: [(Midi.Key, Int)]
gliss_natural_harmonics = absolute
    [ (Key.c3, 12)
    , (Key.d3, 7)
    , (Key.e3, 5)
    , (Key.f3, 4)
    , (Key.g3, 3)
    , (Key.a3, 3)
    , (Key.as3, 2)
    ]
    where absolute = uncurry zip . second (drop 1 . scanl (+) 0) . unzip

-- * infer seconds

c_infer_seconds :: Make.Calls Derive.Note
c_infer_seconds = Make.transform_notes Module.instrument "infer-seconds"
    Tags.attr
    "Infer a `+sec#` attr based on the duration of the event and the available\
    \ keymaps for the current instrument and attribute set."
    ((,)
    <$> Sig.defaulted "attrs" mempty "Add these attributes."
    <*> Sig.defaulted "round" Nothing "Round up to a longer `+sec#` attribute,\
        \ down to a shorter one, or to the closest."
    ) $ \(attrs, round) deriver -> do
        (_, inst) <- Derive.get_instrument =<< Call.get_instrument
        Post.emap1_ (infer_seconds round (Derive.inst_attributes inst)) <$>
            Call.add_attributes attrs deriver

infer_seconds :: Maybe Call.UpDown -> [Attrs.Attributes] -> Score.Event
    -> Score.Event
infer_seconds round inst_attrs event = case closest of
    Nothing -> event
    Just (secs, _) -> Score.add_attributes (VslInst.sec secs) event
    where
    dur = Score.event_duration event
    closest = Seq.minimum_on (abs . subtract dur . fst) $ case round of
        Nothing -> relevant_attrs
        Just Call.Up -> filter ((>=dur) . fst) relevant_attrs
        Just Call.Down -> filter ((<=dur) . fst) relevant_attrs
    relevant_attrs :: [(RealTime, Attrs.Attributes)]
    relevant_attrs = filter (Attrs.contain event_attrs . snd) $
        mapMaybe VslInst.parse_sec inst_attrs
    event_attrs = Score.event_attributes event
