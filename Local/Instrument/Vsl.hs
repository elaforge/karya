-- | Vienna Symphonic Library.
module Local.Instrument.Vsl where
import qualified Data.List as List
import qualified Data.Set as Set

import Util.Control
-- import Util.Pretty (pprint)
import qualified Util.Seq as Seq

import qualified Midi.CC as CC
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi

import Derive.Attrs
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return synth_descs

synth_descs :: [MidiInst.SynthDesc]
synth_descs = MidiInst.make $ (MidiInst.softsynth synth (-2, 2) [])
    { MidiInst.extra_patches = patches }

synth :: Instrument.SynthName
synth = "vsl"

patches :: [MidiInst.Patch]
patches = MidiInst.with_empty_code $ concat
    [ insts solo_strings, insts string_sections
    , insts woodwinds1, insts woodwinds2, insts special_woodwinds
    , insts brass1, insts brass2, insts special_brass
    , insts harps
    ]
    where
    insts = map (uncurry inst)
    inst name ks = (Instrument.keyswitches #= keyswitch_map ks) $
        MidiInst.patch (-2, 2) name []

type Keyswitch = (Attributes, [Instrument.Keyswitch])
type Instrument = (Instrument.InstrumentName, [Keyswitch])

keyswitch_map :: [Keyswitch] -> Instrument.KeyswitchMap
keyswitch_map =
    Instrument.KeyswitchMap . Seq.sort_on (attr_key . fst) . process
    where
    process keyswitches = zip (strip_attributes attrs) ks
        where (attrs, ks) = unzip (drop_dups keyswitches)
    drop_dups = Seq.unique_on fst
    attr_key = negate . Set.size . Score.attrs_set

-- | The instrument keyswitch definitions are copied directly from the VSL
-- documentation and define unnecessarily specific attributes.  This strips
-- off the redundant attributes if the keyswitch is unambiguous without them.
strip_attributes :: [Attributes] -> [Attributes]
strip_attributes attrs = reverse $ foldr strip [] $ reverse attrs
    where
    strip attr stripped =
        List.foldl' (try_strip stripped) attr redundants : stripped
    try_strip stripped attr redundant
        | clean `elem` attrs || clean `elem` stripped = attr
        | otherwise = clean
        where clean = Score.attrs_remove redundant attr
    redundants =
        [ sustain, vibrato
        , dyn -- modes should already be expanded
        , slow, medium, fast
        , light, heavy
        , short, long
        , v1, v2, v3
        ]

-- * solo strings

solo_strings :: [Instrument]
solo_strings = concat
    [ inst keys "violin", inst keys "viola"
    -- Actually it's c6, but that collides with the playable range.
    , inst (keys { key_preset = Key.c7 }) "cello"
    , inst bass_keys "bass"
    ]
    where
    bass_keys = keys
        { key_a = Key.a0, key_b = Key.as0
        , key_art = Key.c6, key_preset = Key.c7
        }
    inst keys name = preset3 keys
        (add_mod $ map keyswitches [[legato], [porta]])
        (add_mod $ map keyswitches [[legato], [staccato], [spiccato]])
        name (solo_string_art keys)

solo_string_art :: Keys -> [Keyswitch]
solo_string_art keys = matrix keys
    [staccato, sustain, fp, trem, trill <> half, pont <> staccato,
        pont <> trem, harmonic <> staccato, medium <> gliss, pizz]
    [detache <> short, marcato, sfz, trem <> auto, trill <> whole,
        pont <> sustain, pont <> trem <> auto, harmonic <> sustain,
        fast <> gliss, snap]

-- * string sections

string_sections :: [Instrument]
string_sections = concat
    [ inst keys "violins", inst keys "violas"
    , inst (keys { key_preset = Key.c6 }) "cellos"
    , inst (keys { key_art = Key.c6, key_preset = Key.c7 }) "basses"
    , [("strings", strings)]
    ]
    where
    inst keys name = preset3 keys
        (add_mod $ map keyswitches [[legato], [porta]])
        (add_mod $ map keyswitches [[legato], [staccato], [spiccato]])
        name (string_section keys)

strings :: [Keyswitch]
strings = keyswitches_from Key.e7 $ keyswitches
    [staccato <> long, detache <> short, detache <> long, sustain, fp, sfz,
        trem, pizz]

string_section :: Keys -> [Keyswitch]
string_section keys = matrix keys
    [staccato <> long, sustain, tasto, fp, trem, trill <> half,
        pont <> staccato, pont <> trem, harmonic <> staccato, pizz]
    [detache <> short, sustain <> auto, tasto <> auto, trill <> whole,
        pont <> sustain, pont <> trem <> auto, harmonic <> sustain, snap]

-- * woodwinds 1

woodwinds1 :: [Instrument]
woodwinds1 = concat
    [ inst keys "flute1" flute1
    , inst keys "oboe2" oboe2
    , inst keys "clarinet" clarinet
    , inst (keys { key_art = Key.c6, key_preset = Key.c7 }) "bassoon" bassoon
    , inst keys "flutes" flutes
    , inst keys "oboes" oboes
    , inst keys "clarinets" clarinets
    , inst (keys { key_art = Key.c6, key_preset = Key.c7 })
        "bassoons" bassoons
    ]
    where inst = preset3_standard

flute1 = matrix keys
    [staccato, sustain <> vibrato, pfp <> sec3, fp, flutter, trill <> half]
    [portato, sustain <> nv, pfp <> sec6, sfz, flutter <> dyn, trill <> whole]

oboe2 = matrix keys
    [staccato, sustain <> vibrato, pfp <> sec2, fp, flutter, trill <> half]
    [portato, sustain <> nv, pfp <> sec4, sfz, flutter <> dyn, trill <> whole]

clarinet = matrix keys
    [staccato, sustain <> vibrato, pfp <> sec2, fp, flutter, trill <> half]
    [portato <> short, portato <> long, pfp <> sec4, sfz, flutter <> dyn,
        trill <> whole]

bassoon = matrix_low
    [staccato, sustain <> vibrato, pfp <> vibrato <> sec3, fp <> nv, flutter,
        trill <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec5, sfz <> nv,
        flutter, trill <> whole]

flutes = matrix keys
    [staccato, sustain <> vibrato, fp <> vibrato, trill <> half, cluster]
    [portato <> short, sustain <> vibrato, sfz <> vibrato, trill <> whole,
        cluster <> sfz]

oboes = matrix keys
    [staccato, sustain, fp, trill <> half, cluster]
    [portato <> short, sustain, sfz, trill <> whole, cluster <> sfz]

clarinets = matrix keys
    [staccato, sustain, fp, trill <> half, cluster]
    [portato <> short, sustain, sfz, trill <> whole, cluster <> sfz]

bassoons = matrix_low
    [staccato, sustain, fp, trill <> half, cluster]
    [portato <> short, sustain, sfz, trill <> whole, cluster <> sfz]

-- * woodwinds 2

woodwinds2 :: [Instrument]
woodwinds2 = concat
    [ inst keys "piccolo" piccolo
    , inst keys "flute2" flute2
    , inst keys "alto-flute" alto_flute
    , inst keys "english-horn1" english_horn1
    , inst keys "english-horn2" english_horn2
    , inst keys "clarinet-eb" clarinet_eb
    , inst (keys { key_preset = Key.c6 }) "bass-clarinet" bass_clarinet
    , inst (keys { key_art = Key.c6, key_preset = Key.c7 })
        "contra-bassoon" contra_bassoon
    ]
    where inst = preset3_standard

piccolo = matrix keys
    [staccato, sustain <> vibrato <> v1, pfp <> vibrato <> sec6, fp <> vibrato,
        flutter, trill <> half]
    [portato <> short, sustain <> progressive <> vibrato,
        fpf <> vibrato <> sec5, sfz <> vibrato <> v1, flutter, trill <> whole]

flute2 = matrix keys
    [staccato, sustain <> progressive <> vibrato, pfp <> vibrato <> sec2,
        fp <> vibrato, flutter, trill <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec4, sfz <> vibrato,
        flutter <> dyn, trill <> whole]

alto_flute = matrix keys
    [staccato, sustain <> vibrato, pfp <> vibrato <> sec3, fp <> vibrato,
        flutter, trill <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec5, sfz <> vibrato,
        flutter <> cresc, trill <> whole]

oboe1 = matrix keys
    [staccato, sustain <> progressive <> vibrato, pfp <> sec2, fp, flutter,
        trill <> half]
    [portato <> short, sustain <> nv, pfp <> sec6, sfz, flutter <> cresc,
        trill <> whole]

english_horn1 = matrix keys
    [staccato, sustain <> vibrato, pfp <> vibrato <> sec2, fp, flutter,
        trill <> half]
    [portato <> short, sustain <> nv, pfp <> nv <> sec6, sfz, flutter <> cresc,
        trill <> whole]

english_horn2 = matrix keys
    [staccato, sustain <> vibrato, pfp <> vibrato <> sec2, fp, flutter,
        trill <> half]
    [portato <> short, sustain <> progressive <> vibrato,
        pfp <> vibrato <> sec4, sfz, flutter <> dyn, trill <> whole]

clarinet_eb = matrix keys
    [staccato, sustain <> nv, pfp <> sec2, fp, flutter, trill <> half]
    [portato <> short, portato <> long <> attack, pfp <> sec4, sfz,
        flutter <> cresc, trill <> whole]

bass_clarinet = matrix keys
    [staccato, sustain <> nv, pfp <> sec2, fp, flutter, trill <> half]
    [portato <> short, portato <> long <> nv, pfp <> sec4, sfz, flutter,
        trill <> whole]

contra_bassoon = matrix_low
    [staccato, sustain <> vibrato, pfp <> sec2, fp, flutter]
    [portato <> short, sustain <> nv, pfp <> sec4, sfz, flutter <> cresc]

-- * special woodwinds

special_woodwinds :: [Instrument]
special_woodwinds = concat
    [ inst Key.c2 "bass-flute" bass_flute
    , inst Key.c2 "oboe-damore" oboe_damore
    , inst Key.c2 "heckelphone" heckelphone
    , inst Key.c7 "contrabass-clarinet" contrabass_clarinet
    , inst Key.c2 "basset-horn" basset_horn
    ]
    where
    inst preset_key name ks = preset [preset_key ..] name
        [("art", ks), ("leg", keyswitches [legato])]

bass_flute = matrix keys
    [staccato, sustain <> vibrato, fp <> vibrato, flutter]
    [portato <> short, sustain <> vibrato, sfz <> vibrato, flutter <> cresc]

oboe_damore = matrix keys
    [staccato, sustain <> vibrato, fp <> vibrato, flutter]
    [portato <> short, sustain <> vibrato, sfz <> vibrato, flutter <> cresc]

heckelphone = matrix keys
    [staccato, sustain <> vibrato, fp <> vibrato, flutter]
    [portato <> short, sustain <> vibrato, sfz <> vibrato, flutter]

contrabass_clarinet = matrix_low
    [staccato, sustain, fp, flutter]
    [portato, sustain, sfz, flutter <> cresc]

basset_horn = matrix keys
    [staccato, sustain, fp, flutter]
    [portato <> short, sustain, sfz, flutter <> cresc]

-- * brass 1

brass1 :: [Instrument]
brass1 = concat
    [ inst keys "trumpet" trumpet_c, inst keys "trumpet-mute" trumpet_c_mute
    , inst keys "horn" horn, inst keys "trombone" trombone
    , inst keys "trombone-mute-a" trombone_mute_a
    , inst keys "trombone-mute-b" trombone_mute_b
    , inst (keys { key_art = Key.c6, key_preset = Key.c7 }) "tuba" tuba
    , inst keys "trumpets" trumpets
    , inst keys "trumpets-mute" trumpets_mute
    , inst keys "horns" horns
    , inst keys "horns-stopped" horns_stopped
    , inst keys "trombones" trombones
    , inst keys "trombones-mute" trombones_mute
    ]
    where inst = preset3_standard

trumpet_c = matrix keys
    [staccato, sustain <> light <> vibrato, pfp <> vibrato <> sec4, fp,
        flutter, trill <> fast <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec8, sfz,
        flutter <> cresc, trill <> fast <> whole]

trumpet_c_mute = matrix keys
    [staccato, sustain <> vibrato, pfp <> vibrato <> sec2, fp <> flutter,
        trill <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec5, sfz,
        flutter <> cresc, trill <> whole]

horn = matrix keys
    [staccato, sustain <> vibrato, pfp <> nv <> sec6, fp, flutter,
        trill <> half]
    [portato <> short, sustain <> nv, pfp <> nv <> sec8, sfz, flutter <> cresc,
        trill <> whole]

trombone = matrix keys
    [staccato, sustain <> light <> vibrato, pfp <> nv <> sec6, fp, flutter]
    [portato <> short, sustain <> nv, pfp <> nv <> sec6, sfz, flutter <> cresc]

trombone_mute_a = matrix keys
    [staccato, sustain <> progressive <> vibrato, pfp <> nv <> sec2, fp,
        flutter]
    [portato <> short, sustain <> nv, pfp <> nv <> sec4, sfz, flutter]

trombone_mute_b = matrix keys
    [staccato, sustain <> progressive <> vibrato, pfp <> nv <> sec2, fp,
        flutter]
    [portato <> short, sustain <> nv, pfp <> nv <> sec4, sfz, flutter <> cresc]

tuba = matrix_low
    [staccato, sustain <> vibrato <> v1, pfp <> nv <> sec4, fp, flutter,
        trill <> half]
    [portato <> medium, sustain <> nv, pfp <> nv <> sec4, sfz,
        flutter <> cresc, trill <> whole]

trumpets = matrix keys
    [staccato, sustain <> vibrato, pfp <> sec2, fp, flutter, trill <> half]
    [portato <> short, sustain <> nv, pfp <> sec4, sfz, flutter <> cresc,
        trill <> whole]

trumpets_mute = matrix keys
    [staccato, sustain, pfp <> sec2, fp, flutter]
    [portato <> short, sustain, pfp <> sec4, sfz, flutter <> cresc]

horns = matrix keys
    [staccato, sustain, pfp <> sec4, fp, flutter]
    [portato <> short, marcato, pfp <> sec6, sfz, flutter <> cresc]

-- doesn't have any articulations
horns_stopped = matrix keys [] []

trombones = matrix keys
    [staccato, sustain, pfp <> sec4, fp, flutter]
    [portato <> short, marcato, pfp <> sec6, sfz, flutter <> cresc]

trombones_mute = matrix keys
    [staccato, sustain, pfp <> sec2, fp, flutter]
    [portato <> short, sustain, pfp <> sec4, sfz, flutter <> cresc]

-- * brass 2

brass2 :: [Instrument]
brass2 =
    [ ("trumpet-piccolo", trumpet_piccolo), ("bass-trumpet", bass_trumpet)
    , ("horn-triple", horn_triple), ("bass-trombone", bass_trombone)
    , ("contrabass-trombone", contrabass_trombone)
    , ("contrabass-tuba", contrabass_tuba), ("wagner-tuba", wagner_tuba)
    , ("horns-a8", horns_a8)
    ]

trumpet_piccolo = matrix keys
    [staccato, sustain <> light <> vibrato, pfp <> vibrato <> sec5, fp,
        flutter, trill <> fast <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec9, sfz,
        flutter <> cresc, trill <> fast <> whole]

bass_trumpet = matrix keys
    [staccato, sustain <> vibrato, pfp <> vibrato <> sec6, fp, flutter,
        trill <> half]
    [portato <> short, sustain <> nv, pfp <> vibrato <> sec6, sfz,
        flutter <> cresc, trill <> whole]

horn_triple = matrix keys
    [staccato, sustain, pfp <> sec2, fp, flutter]
    [portato <> short, sustain, pfp <> sec4, sfz, flutter <> cresc]

bass_trombone = matrix_low
    [staccato, sustain <> vibrato, dyn <> medium <> sec2, fp, flutter]
    [portato <> medium, sustain <> nv, dyn <> medium <> sec4, sfz,
        flutter <> cresc]

contrabass_trombone = matrix_low
    [staccato, sustain, pfp <> sec4, fp, flutter]
    [portato <> medium, sustain, pfp <> sec6, sfz, flutter <> cresc]

contrabass_tuba = matrix_low
    [staccato, sustain, dyn <> sec2, fp, flutter, trill <> half]
    [portato <> medium, sustain, dyn <> sec4, sfz, flutter, trill <> whole]

wagner_tuba = matrix keys
    [staccato, sustain, pfp <> sec4, fp, flutter]
    [portato <> short, sustain, pfp <> sec6, sfz, flutter <> cresc]

cimbasso = matrix_low
    [staccato, sustain, dyn <> medium <> sec2, fp, flutter]
    [portato <> medium, sustain, dyn <> medium <> sec4, sfz, flutter]

horns_a8 = matrix keys
    [staccato, sustain, pfp <> sec6, fp, flutter, trill <> half]
    [portato <> short, sustain <> blared, pfp <> sec6, sfz, flutter <> cresc,
        trill <> whole]

-- * special_brass

special_brass :: [Instrument]
special_brass =
    [ ("cornet", cornet), ("alto-trombone", alto_trombone)
    , ("euphonium", euphonium), ("trumpets-a6", trumpets_a6)
    ]

cornet = matrix keys
    [staccato, sustain <> vibrato, fp, flutter]
    [portato <> short, sustain <> nv, sfz, flutter <> cresc]

alto_trombone = matrix keys
    [staccato, sustain, fp, flutter]
    [portato <> short, sustain, sfz, flutter <> cresc]

euphonium = matrix keys
    [staccato, sustain, fp, flutter]
    [portato <> short, sustain, sfz, flutter <> cresc]

trumpets_a6 = matrix keys
    [staccato, sustain, fp, flutter]
    [portato <> short, sustain, sfz, flutter <> cresc]

-- * harp

-- | Everyone's favorite unboxed piano.
harps :: [Instrument]
harps = concat
    [ inst "harp1" harp_art
    , [("harp2", harp_art)]
    ]
    where
    inst name ks = preset [Key.a7, Key.b7, Key.c8] name
        [ ("art", ks)
        , ("maj", expand harp1_gliss_major)
        , ("special", expand harp1_gliss_special)
        ]
    expand = expand_modes keys

harp_art :: [Keyswitch]
harp_art = add_mod $ map keyswitches
    [[mempty], [mute], [harmonic], [bisbig]]

harp1_gliss_major :: [Keyswitch]
harp1_gliss_major =
    keyswitches_from Key.c1 $ keyswitches $
        map ((Score.attr "major" <> gliss <>) . Score.attr)
            ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as"]

harp1_gliss_special :: [Keyswitch]
harp1_gliss_special = add_mod $ map (keyswitches_from Key.c1 . keyswitches)
    [ map ((Score.attr "diminished" <> gliss <>) . Score.attr) ["c", "cs", "d"]
    , map (Score.attr "pentatonic" <> gliss <>) [v1, v2, v3]
    , map ((whole <> gliss <>) . Score.attr) ["c", "cs"]
    ]

-- * util

-- | Keyswitch configuration.
data Keys = Keys {
    key_a :: Midi.Key
    , key_b :: Midi.Key
    , key_preset :: Midi.Key
    , key_art :: Midi.Key
    } deriving (Show)

-- | The standard configuration, but instruments with low ranges may displace
-- some keyswitches.
keys :: Keys
keys = Keys Key.a0 Key.b0 Key.c2 Key.c1

preset3_standard :: Keys -> String -> [Keyswitch] -> [Instrument]
preset3_standard keys = preset3 keys (keyswitches [legato])
    (add_mod $ map keyswitches [[legato], [portato], [staccato]])

preset3 :: Keys -> [Keyswitch] -> [Keyswitch] -> String -> [Keyswitch]
    -> [Instrument]
preset3 keys leg rep name ks = preset [key_preset keys ..] name
    [ ("leg", leg)
    , ("art", expand_modes keys ks)
    , ("rep", rep)
    ]

preset :: [Midi.Key] -> Instrument.InstrumentName
    -> [(Instrument.InstrumentName, [Keyswitch])] -> [Instrument]
preset midi_keys name matrices =
    (name, concat $ preset_switch midi_keys (map snd matrices))
    : [(name ++ "-" ++ matrix, ks) | (matrix, ks) <- matrices]

matrix_low :: [Attributes] -> [Attributes] -> [Keyswitch]
matrix_low = matrix (keys { key_art = Key.c6 })

matrix :: Keys -> [Attributes] -> [Attributes] -> [Keyswitch]
matrix keys = matrix_from (key_art keys)

matrix_from :: Midi.Key -> [Attributes] -> [Attributes] -> [Keyswitch]
matrix_from low_key line1 line2 = add_mod [mk line1, mk line2]
    where mk = keyswitches_from low_key . keyswitches

keyswitches_from :: Midi.Key -> [Keyswitch] -> [Keyswitch]
keyswitches_from low_key = zipWith add_keyswitch (keys_from low_key)

keys_from :: Midi.Key -> [Instrument.Keyswitch]
keys_from low_key = map Instrument.Keyswitch [low_key ..]

add_keyswitch :: Instrument.Keyswitch -> Keyswitch -> Keyswitch
add_keyswitch ks = fmap (ks:)

keyswitches :: [Attributes] -> [Keyswitch]
keyswitches attrs = [(attr, []) | attr <- attrs]

preset_switch :: [Midi.Key] -> [[Keyswitch]] -> [[Keyswitch]]
preset_switch midi_keys kss =
    [map (add_keyswitch (Instrument.Keyswitch key)) ks
        | (key, ks) <- zip midi_keys kss]

add_mod :: [[Keyswitch]] -> [Keyswitch]
add_mod rows = concat $ zipWith (add_cs CC.mod) vals rows
    where
    vals = map round $ iterate (+delta) 0
    delta :: Double
    delta = 0x7f / max 1 (fromIntegral (length rows - 1))

add_cs :: Midi.Control -> Midi.ControlValue -> [Keyswitch] -> [Keyswitch]
add_cs cc val = map $ add_keyswitch (Instrument.ControlSwitch cc val)

-- ** expand_modes

expand_modes :: Keys -> [Keyswitch] -> [Keyswitch]
expand_modes keys =
    concatMap (expand_modes_with modes (key_a keys) (key_b keys))
    where
    modes =
        [ (gliss, (up, down))
        , (dyn, (cresc, dim))
        ]

expand_modes_with :: [(Attributes, (Attributes, Attributes))]
    -> Midi.Key -> Midi.Key -> Keyswitch -> [Keyswitch]
expand_modes_with modes up_key down_key (attrs, ks) =
    case List.find (Score.attrs_contain attrs . fst) modes of
        Just (_, (up, down)) ->
            [ (attrs <> up, ks ++ [Instrument.Keyswitch up_key])
            , (attrs <> down, ks ++ [Instrument.Keyswitch down_key])
            ]
        Nothing -> [(attrs, ks)]
