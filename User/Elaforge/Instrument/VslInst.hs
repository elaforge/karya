-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.VslInst (
    module User.Elaforge.Instrument.VslInst, module Derive.Attrs
    -- User.Elaforge.Instrument.Vsl uses a lot of attributes, but it shouldn't
    -- have to know which ones are defined here and which are in Derive.Attrs.
) where
import qualified Prelude
import Prelude hiding (min, (.))
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Attrs as Attrs
import Derive.Attrs

import qualified Perform.RealTime as RealTime
import Global
import Types


-- | Easier to type and looks good without spaces.
(.) :: Attributes -> Attributes -> Attributes
(.) = (<>)

data Keys = Keys {
    -- | Base keyswitch for matrix x axis.
    key_x_axis :: Midi.Key
    -- | Base keyswitch for matrix y axis.
    , key_y_axis :: Midi.Key
    -- | First of the AB keyswitches.
    , key_ab :: Midi.Key
    -- | Base keyswitches for the presets.
    , key_matrix :: Midi.Key
    } deriving (Show)

-- | (name, keyswitch configuration, attributes)
type Instrument = (Text, Keys, [[Attributes]])

-- | Instruments that start above a1 use this.
low_keys :: Keys
low_keys = Keys
    { key_x_axis = Key.d_1
    , key_y_axis = Key.d0
    , key_ab = Key.d1
    , key_matrix = Key.e1
    }

-- | Instruments that go below a1 but don't go above d7 use this.
high_keys :: Keys
high_keys = Keys
    { key_x_axis = Key.d7
    , key_y_axis = Key.d8
    , key_ab = Key.d9
    , key_matrix = Key.e9
    }

-- * strings

-- ** solo strings

solo_strings :: [Instrument]
solo_strings =
    [ ("violin", low_keys, violin)
    , ("viola", low_keys, viola)
    , ("cello", low_keys, cello)
    , ("bass", high_keys, bass)
    ]
solo_violin, solo_viola, solo_cello, solo_bass :: Instrument
solo_violin = ("violin", low_keys, violin)
solo_viola = ("viola", low_keys, viola)
solo_cello = ("cello", low_keys, cello)
solo_bass = ("bass", high_keys, bass)

violin =
    [ violin_short_long_notes, violin_dynamics
    , violin_tremolo_trills, violin_pizz_legno
    , violin_harmonics, violin_ponticello
    , violin_tasto, violin_perf_interval
    , violin_perf_interval_fast, violin_perf_trill
    , violin_perf_repetition, violin_fast_repetition
    , violin_grace_notes, violin_glissandi, violin_scale_runs
    ]
violin_short_long_notes =
    [ staccato, detache.short, detache.long.vib, detache.long.nv
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.vib.espr, sus.progr
    , sus.vib_down, sus.nv
    ]
violin_dynamics =
    [ dyn.med.vib.sec 1.5, dyn.med.vib.sec 3, dyn.med.vib.sec 4
    , dyn.str.vib.sec 1.5, dyn.str.vib.sec 3, dyn.str.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 3
    , pfp.sec 2, pfp.sec 4, fp.vib, sfz.vib, sffz.vib
    ]
violin_tremolo_trills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 1.5, trem.dyn.sec 3
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.acc.half, trill.acc.whole, trill.acc.half.dyn, trill.acc.whole.dyn
    ]
violin_pizz_legno = [pizz, pizz.secco, pizz.snap, legno]
violin_harmonics = map (harm.)
    [ art.staccato, art.sus, art.perf.rep, art.gliss.updown
    , nat.sus, nat.rep, nat.gliss.updown
    ]
violin_ponticello = map (pont.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto
    , sfz, sffz, trem, trem.fa, trem.fa.auto
    , rep.legato, rep.dyn.legato
    ]
violin_tasto = map (tasto.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto
    , sfz, trem, trem.fa, trem.fa.auto
    ]
violin_perf_interval = map (perf.)
    [ legato, legato.sul, legato.progr, legato.zigane
    , porta, detache, marcato, spiccato
    ]
violin_perf_interval_fast = map (perf.fa.)
    [legato, marcato, spiccato, harsh]
violin_perf_trill = [perf.trill.legato]
violin_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, bow.slow, bow.fast
    , portato.slow, portato.med, portato.fast
    , staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.bow.slow, dyn5.bow.fast
    , dyn5.portato.slow, dyn5.portato.med, dyn9.portato.fast, dyn9.staccato
    , dyn9.spiccato, dyn9.harsh
    ]
violin_fast_repetition =
    fast_rep_bpm [(mempty, [15..20]), (dyn, [15..20])]
    ++ map (ricochet.) (map bpm [150, 160, 170, 180, 190, 210])
    ++ [ricochet.notes 3, ricochet.acc, ricochet.rit]
violin_grace_notes = map (grace.updown.)
    [ half.slow, whole.slow, half.fast, whole.fast
    , zigane.half, zigane.whole, zigane.min.third, zigane.maj.third
    ]
violin_glissandi = map (gliss.)
    [ perf.attr "g", perf.attr "d", perf.attr "a", perf.attr "e"
    , updown.oct.med, updown.oct.fast
    ]
violin_scale_runs = run_scales
    [ legato.maj, legato.min, legato.chrom, legato.whole
    , spiccato.maj, spiccato.min, spiccato.chrom, spiccato.whole
    ]

viola =
    [ viola_short_long_notes, viola_dynamics
    , violin_tremolo_trills, viola_pizz_legno
    , viola_harmonics, violin_ponticello
    , violin_tasto, viola_perf_interval
    , violin_perf_interval_fast, violin_perf_trill
    , viola_perf_repetition, viola_fast_repetition
    , viola_grace_notes, viola_glissandi
    ]
viola_short_long_notes =
    [ staccato, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.vib.espr
    , sus.vib.progr, sus.vib_down, sus.nv
    ]
viola_dynamics = seconds
    [ (dyn.med, [1.5, 3]), (dyn.str, [1.5, 3, 4]), (dyn.med.nv, [1.5, 3])
    , (pfp.vib, [2, 4])
    ] ++ [fp.vib, sfz.vib, sffz.vib]
viola_pizz_legno = [pizz, pizz.snap, legno]
viola_harmonics = map (harm.)
    [ art.staccato, art.sus, art.perf.rep
    , nat.sus, nat.perf.rep, nat.gliss
    ]
viola_perf_interval = map (perf.)
    [ legato, legato.sul, legato.progr, legato.zigane
    , porta, marcato, spiccato
    ]
viola_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato, staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn9.portato, dyn9.staccato
    , dyn9.spiccato, dyn9.harsh
    ]
viola_fast_repetition =
    fast_rep_bpm [(mempty, [15..19]), (dyn, [15..19])]
    ++ map (ricochet.) (map bpm [150, 160, 170, 180, 190, 210])
    ++ [ricochet.notes 3, ricochet.acc, ricochet.rit]
viola_grace_notes = map (grace.updown.)
    [ half, whole, zigane.half, zigane.whole
    , zigane.min.third, zigane.maj.third
    ]
viola_glissandi = map (gliss.)
    [perf.attr "c", perf.attr "g", perf.attr "d", perf.attr "a", oct.updown]

cello =
    [ cello_short_long_notes, cello_dynamics, cello_tremolo_trills
    , cello_pizz_legno, cello_harmonics, cello_ponticello, cello_tasto
    , cello_perf_interval, cello_perf_interval_fast, cello_trill
    , cello_perf_repetition, cello_fast_repetition, cello_grace_notes
    , cello_glissandi
    ]
cello_short_long_notes =
    [ staccato, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.vib.espr, sus.vib.progr, sus.vib_down, sus.nv
    ]
cello_dynamics = seconds
    [ (dyn.med.vib, [1.5, 3]), (dyn.str.vib, [1.5, 3, 4])
    , (dyn.med.nv, [1.5, 3])
    , (pfp.vib, [2, 4])
    ] ++ [fp.vib, sfz.vib, sffz.vib]
cello_tremolo_trills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 1.5, trem.dyn.sec 3
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.acc.half, trill.acc.whole, trill.acc.half.dyn, trill.acc.whole.dyn
    ]
cello_pizz_legno = [pizz, pizz.secco, pizz.snap, pizz.legno]
cello_harmonics = map (harm.)
    [art.staccato, art.sus, art.rep, nat.sus, nat.rep, nat.gliss]
cello_ponticello = map (pont.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto, sfz, sffz
    , trem, trem.fa, trem.fa.auto, rep.legato, rep.dyn.legato
    ]
cello_tasto = map (tasto.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto, sfz
    , trem, trem.fa, trem.fa.auto
    ]
cello_perf_interval = map (perf.)
    [ legato, legato.sul, legato.progr, legato.zigane, porta
    , marcato, spiccato
    ]
cello_perf_interval_fast = map (perf.fast.) [legato, marcato, spiccato, harsh]
cello_trill = [perf.trill.legato]
cello_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, bow, portato, staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.bow
    , dyn9.portato, dyn9.staccato, dyn9.spiccato, dyn9.harsh
    ]
cello_fast_repetition =
    fast_rep_bpm [(mempty, [14..18]), (dyn, [14..18])]
    ++ map (ricochet.) (map bpm [150, 160, 170, 180, 190, 210])
    ++ [ricochet.notes 3, ricochet.acc, ricochet.rit]
cello_grace_notes = map (grace.updown.)
    [half, whole, zigane.half, zigane.whole, zigane.min.third, zigane.maj.third]
cello_glissandi = map (gliss.)
    [ perf.attr "c", perf.attr "g", perf.attr "d", perf.attr "a"
    , updown.oct.med, updown.oct.fast
    ]

-- *** solo bass

bass =
    [ bass_short_long_notes, bass_dynamics
    , bass_tremolo_trills, bass_pizz_legno
    , bass_harmonics, violin_ponticello
    , violin_tasto, bass_perf_interval
    , violin_perf_interval_fast, violin_perf_trill
    , bass_perf_repetition, bass_glissandi
    ]

bass_short_long_notes =
    [ staccato, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.progr, sus.vib_down, sus.nv
    ]
bass_dynamics =
    [ dyn.str.vib.sec 1.5, dyn.str.vib.sec 3, dyn.str.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.str.nv.sec 3
    , pfp.vib.sec 2, pfp.vib.sec 4, fp.vib, sfz.vib, sffz.vib
    ]
bass_tremolo_trills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
bass_pizz_legno = [pizz, pizz.snap, legno]
bass_harmonics = map (harm.)
    [ art.staccato, art.sus, art.rep, nat.sus, nat.rep, nat.gliss]
bass_perf_interval = map (perf.) [legato, porta, marcato, spiccato]
bass_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato, staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato, dyn9.staccato, dyn9.spiccato, dyn9.harsh
    ]
bass_glissandi = [gliss, oct.gliss.updown.med, oct.gliss.updown.fast]

-- ** string sections

strings :: [Instrument]
strings =
    [ ("strings", Keys
            { key_x_axis = Key.d_1
            , key_y_axis = Key.d8
            , key_ab = Key.d9
            , key_matrix = Key.e9
            },
        [strings_orchestra])
    , ("violins", low_keys, violins)
    , ("violas", low_keys, violas)
    , ("cellos", low_keys, cellos)
    , ("basses", high_keys, basses)
    ]

strings_orchestra =
    [ staccato.long, detache.short, detache.long
    , sus.vib, fp, sfz, trem, pizz
    ]

violins = string_section "gdae"
violas = string_section "cgda"
cellos = string_section "cgda"

string_section strings =
    [ strings_short_long_notes, strings_dynamics
    , strings_tremolo_trills, strings_pizz_legno
    , strings_harmonics, strings_ponticello
    , strings_con_sordino_basic, strings_perf_interval
    , strings_perf_interval_fast, strings_perf_trill
    , strings_perf_repetition, strings_perf_upbeat_repetition
    , strings_fast_repetition, strings_grace_notes
    , strings_glissandi strings, strings_scale_runs
    ]

strings_short_long_notes =
    [ staccato.short, staccato.long, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.nv, sus.nv.fa, sus.nv.fa.auto
    , sus.flaut, sus.flaut.fa, sus.flaut.fa.auto
    ]
strings_dynamics = map (dyn.)
    [ med.vib.sec 1.5, med.vib.sec 3
    , str.vib.sec 1.5, str.vib.sec 3, str.vib.sec 6
    , med.nv.sec 2, dyn.med.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 6
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
strings_tremolo_trills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 2, trem.pfp.sec 3
    , trill.half, trill.whole, trill.third.min, trill.third.maj
    , trill.half.dyn, trill.whole.dyn, trill.half.pfp, trill.whole.pfp
    , trill.acc.half, trill.acc.whole
    , trill.acc.half.dyn, trill.acc.whole.dyn
    ]
strings_pizz_legno =
    [ pizz, pizz.slow, pizz.snap, pizz.rep.slow, pizz.rep.fast
    , legno, legno.slow
    ]
strings_harmonics =
    [ harm.art.staccato, harm.art.sus
    , harm.art.sus.fa, harm.art.sus.fa.auto, harm.art.rep
    ]
strings_ponticello = map (pont.)
    [ staccato, sus, sus.fa, sus.fa.auto
    , dyn.str.sec 1.5, dyn.str.sec 2, dyn.str.sec 4
    , sfz, trem, trem.fa, trem.fa.auto
    ]
strings_con_sordino_basic = map (mute.)
    [ staccato, detache, sus.vib, sus.vib.fa, sus.vib.fa.auto
    , dyn.med.sec 2, dyn.med.sec 4
    , fp, sfz, trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn, pizz
    ]
strings_perf_interval = map (perf.)
    [ legato, legato.sus.v4 -- 4 velocity layers
    , legato.sul, porta, trem
    , mute.legato, mute.porta
    ]
strings_perf_interval_fast = map (perf.fast.) [legato, marcato, spiccato]
strings_perf_trill = [perf.trill.legato]
strings_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, bow.slow, bow.fast
    , portato.slow, portato.fast
    , staccato, spiccato, harsh
    , mute.harsh, mute.portato, mute.staccato
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn5.bow.slow, dyn5.bow.fast
    , dyn9.portato.fast, dyn9.staccato, dyn9.spiccato
    , dyn9.harsh, dyn9.portato, dyn9.staccato
    ]
strings_perf_upbeat_repetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow
    , dyn4.n1.fast, dyn4.n2.fast
    ]
strings_fast_repetition = fast_rep_bpm [(mempty, [15..19]), (dyn, [15..19])]
strings_grace_notes = [grace.updown.half, grace.updown.whole]
strings_glissandi strings =
    [perf.gliss.attr (Text.singleton s) | s <- strings] ++ [gliss.updown.oct]
strings_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole, spiccato.maj]

-- *** basses

basses =
    [ strings_short_long_notes
    , basses_dynamics
    , basses_tremolo_trills
    , strings_pizz_legno
    , strings_harmonics
    , basses_ponticello
    , basses_perf_interval
    , strings_perf_interval_fast
    , strings_perf_trill
    , basses_perf_repetition
    , basses_scale_runs
    ]

basses_dynamics = map (dyn.)
    [ med.sec 2, med.sec 3, str.sec 2, str.sec 3, str.sec 5
    , pfp.sec 2, pfp.sec 4, pfp.sec 6
    , fp, sfz, sffz
    ]
basses_tremolo_trills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 2, trem.dyn.sec 4
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
basses_ponticello = map (pont.)
    [ staccato, sus, sus.fa, sus.fa.auto
    , dyn.str.sec 4
    , sfz, trem, trem.fa, trem.fa.auto
    ]
basses_perf_interval = map (perf.) [legato, porta]
basses_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow, dyn5.portato.fast
    , dyn9.portato.fast, dyn9.staccato, dyn9.spiccato, dyn9.harsh
    ]
basses_scale_runs = run_scales
    [ legato.maj, legato.min, legato.chrom
    , detache.whole.slow, detache.whole.fast
    ]

-- ** harps

harps :: [Instrument]
harps =
    [ ("harp1", low_keys, harp1)
    , ("harp2", low_keys, harp2)
    ]
harp1 =
    [ harp1_single_notes, harp1_glissandi
    , harp1_arpeggios_a3, harp1_arpeggios_a4
    ]

harp1_single_notes = [norm, mute, table, harm, bisbig, norm.rs.highlow]
harp1_glissandi = map (gliss.) $ concat
    [ [mode.speed.pitch | mode <- [maj, min], speed <- speeds3, pitch <- scale]
    , [notes 4.mode.speed.ver | mode <- [maj, min], speed <- speeds3,
        ver <- versions 5]
    , [dim.speed.pitch | speed <- speeds3, pitch <- map attr ["c", "cs", "d"]]
    , [pent.speed.ver | speed <- speeds3, ver <- versions 3]
    , [whole.speed.pitch | speed <- speeds3, pitch <- map attr ["c", "cs"]]
    ]
    where
    -- Actually, each glissando starts on a different note.  I can update this
    -- later if I need to.
    versions v = map version [1..v]
    speeds3 = [slow, med, fast]
harp1_arpeggios_a3 = map (notes 3 .) harp1_arpeggios
harp1_arpeggios_a4 = map (notes 4 .) harp1_arpeggios
harp1_arpeggios = map (arpup.) $ concat
    [ [mode.speed.pitch | mode <- [maj, min], speed <- speeds, pitch <- scale]
    , [mode.speed | mode <- [dim, aug], speed <- speeds]
    ]
    where speeds = [slow, fast, straight]

harp2 = [harp2_basic_articulations]

harp2_basic_articulations =
    [ norm, mute, damp, table, nail, harm, pedal.gliss
    , bisbig, norm.rs, norm.rs.pedal.gliss
    ]


-- * woodwinds

-- ** woodwinds 1

woodwinds1 :: [Instrument]
woodwinds1 =
    [ ("flute1", low_keys, flute1)
    , ("oboe2", low_keys, oboe2)
    , ("clarinet-bb", low_keys, clarinet_bb)
    , ("bassoon", low_keys, bassoon)
    , ("flutes", low_keys, flutes)
    , ("oboes", low_keys, woodwinds)
    , ("clarinets", low_keys, woodwinds)
    , ("bassoons", low_keys, woodwinds)
    ]

-- *** flute1

flute1 =
    [ flute1_short_long_notes, flute1_dynamics, flute1_flutter_trills
    , flute1_perf_interval, flute1_perf_interval_fast
    , flute1_perf_trill, flute1_perf_repetition
    , flute1_perf_upbeat_repetition, flute1_fast_repetition
    , flute1_grace_notes, flute1_scale_runs, flute1_arpeggios, flute1_mordents
    ]

flute1_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long, portato.long.vib, portato.long.nv
    , staccato.fx
    , sus.vib, sus.progr, sus.nv
    ]
flute1_dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.str.vib.sec 5
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 3, pfp.vib.sec 6, pfp.vib.sec 9
    , fpf.vib.sec 6, fpf.vib.sec 9
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
flute1_flutter_trills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.min.third, trill.maj.third
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn, trill.maj.third.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
flute1_perf_interval = map (perf.) [legato, legato.grace, perf.marcato]
flute1_perf_interval_fast = map (perf.fast.) [legato, marcato]
flute1_perf_trill = [perf.trill]
flute1_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast, staccato.triple.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.slow, dyn9.portato.fast, dyn9.staccato
    ]
flute1_perf_upbeat_repetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
flute1_fast_repetition = fast_rep_bpm [(mempty, [17..21]), (triple, [14..18])]
flute1_grace_notes = grace_intervals
flute1_scale_runs = run_scales
    [ legato.maj, legato.maj.fast, legato.min, legato.min.fast
    , legato.chrom, legato.whole, legato.chrom.fast, legato.whole.fast
    ]
flute1_arpeggios = arp_scales
    [ articulation.mode.speed
    | articulation <- [staccato, legato], mode <- [dim, maj, min]
    , speed <- [mempty, fast]
    ]
flute1_mordents = map (mord.) $ concat
    [ map (legato.) variants
    , map (staccato.) variants
    ] where variants = [v1, v2, v3, v4, v5, v6]

-- *** oboe2

oboe2 =
    [ oboe2_short_long_notes, oboe2_dynamics, oboe2_flutter_trills
    , oboe2_perf_interval, oboe2_perf_interval_fast
    , oboe2_perf_trill, oboe2_perf_repetition
    , oboe2_grace_notes, oboe2_scale_runs
    ]
oboe2_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.nv.ha, portato.long.nv.sa
    , sus.vib, sus.progr, sus.nv
    ]
oboe2_dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.med.vib.sec 4
    , dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 8
    , pfp.nv.sec 3, pfp.nv.sec 5, pfp.nv.sec 8
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
oboe2_flutter_trills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
oboe2_perf_interval = map (perf.) [legato, legato.grace, marcato]
oboe2_perf_interval_fast = map (perf.fast.) [legato, marcato]
oboe2_perf_trill = [perf.trill]
oboe2_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato.slow, dyn9.staccato.fast
    ]
oboe2_grace_notes = grace_intervals
oboe2_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]

-- *** clarinet bb

clarinet_bb =
    [ clarinet_bb_short_long_notes, clarinet_bb_dynamics
    , clarinet_bb_flutter_trills, clarinet_bb_perf_interval
    , clarinet_bb_perf_interval_fast, clarinet_bb_perf_trill
    , clarinet_bb_perf_repitition, clarinet_bb_perf_upbeat_repetition
    , clarinet_bb_fast_repetition, clarinet_bb_grace_notes
    , clarinet_bb_glissandi, clarinet_bb_scale_runs, clarinet_bb_arpeggios
    ]
clarinet_bb_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.na, portato.long.ha, portato.long.sa
    , sus.nv
    ]
clarinet_bb_dynamics = map (dyn.)
    [ li.sec 1.5, li.sec 2, li.sec 3, li.sec 4
    , med.sec 2, med.sec 3, med.sec 4, med.sec 6
    , str.sec 2, str.sec 3, str.sec 4, str.sec 6
    ] ++
    [ pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6, pfp.sec 8, pfp.sec 10
    , fpf.sec 4, fpf.sec 6
    , fp, sfz, sffz
    ]
clarinet_bb_flutter_trills =
    [ flutter, flutter.cresc, trill.half, trill.whole
    , trill.min.third, trill.maj.third
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn
    , trill.maj.third.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
clarinet_bb_perf_interval = [perf.legato, perf.legato.grace, perf.marcato]
clarinet_bb_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
clarinet_bb_perf_trill = [perf.trill]
clarinet_bb_perf_repitition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.med, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato.slow, dyn9.staccato.fast
    ]
clarinet_bb_perf_upbeat_repetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
clarinet_bb_fast_repetition =
    fast_rep_bpm [(mempty, [14..17]), (dyn, [14..17])]
clarinet_bb_grace_notes = grace_intervals
clarinet_bb_glissandi = map (gliss.) $ concat
    [ [ perf.slow, perf.fast ]
    , map (slow.updown.) intervals_to_oct
    , map (fast.updown.) intervals_to_oct
    ]
clarinet_bb_scale_runs = run_scales
    [legato.maj, legato.maj.fast, legato.min, legato.chrom, legato.whole]
clarinet_bb_arpeggios = arp_scales
    [ articulation.mode.speed
    | articulation <- [staccato, legato], mode <- [dim, maj, min]
    , speed <- [mempty, fast]
    ]

-- *** bassoon

bassoon =
    [ bassoon_short_long_notes, bassoon_dynamics
    , bassoon_flutter_trills, bassoon_perf_interval
    , bassoon_perf_interval_fast, bassoon_perf_trill
    , bassoon_perf_repetition, bassoon_perf_upbeat_repetition
    , bassoon_fast_repetition, bassoon_grace_notes
    , bassoon_scale_runs
    ]
bassoon_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.vib.str, portato.long.nv
    , portato.long.nv.marcato
    , sus.vib, sus.progr, sus.nv
    ]
bassoon_dynamics = map (dyn.)
    [ med.vib.sec 2, med.vib.sec 3, med.vib.sec 5
    , str.vib.sec 3, str.vib.sec 5
    , med.nv.sec 1.5, med.nv.sec 2, med.nv.sec 3, med.nv.sec 4, med.nv.sec 6
    , str.nv.sec 3, str.nv.sec 4, str.nv.sec 6
    ] ++
    [ pfp.vib.sec 3, pfp.vib.sec 5, pfp.vib.sec 8
    , fpf.vib.sec 5
    , pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6, pfp.nv.sec 8
    , pfp.nv.sec 10
    , fpf.nv.sec 6, fpf.nv.sec 8
    , fp.nv, sfz.nv, sffz.nv
    ]
bassoon_flutter_trills =
    [flutter, trill.half, trill.whole, trill.half.dyn, trill.whole.dyn]
bassoon_perf_interval = [perf.legato, perf.legato.grace.updown, perf.marcato]
bassoon_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
bassoon_perf_trill = [perf.trill]
bassoon_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast, staccato
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato
    ]
bassoon_perf_upbeat_repetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
bassoon_fast_repetition = fast_rep_bpm [(mempty, [14..18]), (dyn, [14..18])]
bassoon_grace_notes = grace_intervals
bassoon_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]

-- *** flutes

flutes =
    [ flutes_short_long_notes, flutes_dynamics
    , flutes_cluster_trills, flutes_perf_interval
    , flutes_perf_interval_fast, flutes_perf_trill
    , flutes_repetition, flutes_scale_runs
    ]
flutes_short_long_notes = [staccato, portato.short, portato.med, sus.vib]
flutes_dynamics =
    [ dyn.str.vib.sec 2, dyn.str.vib.sec 3, dyn.str.vib.sec 5
    , fp.vib, sfz.vib, sffz.vib
    ]
flutes_cluster_trills =
    [ cluster, cluster.sfz
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
flutes_perf_interval = [perf.legato, perf.marcato]
flutes_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
flutes_perf_trill = [perf.trill]
flutes_repetition = map (rep.)
    [ legato, portato, staccato
    , dyn5.legato, dyn9.portato, dyn9.staccato
    ]
flutes_scale_runs = run_scales [legato.chrom, legato.whole]

-- *** woodwinds

-- | Flutes, oboes, and bassoons all have the same articulations.
woodwinds =
    [ woodwinds_short_long_notes, woodwinds_dynamics
    , woodwinds_cluster_trills, woodwinds_perf_interval
    , woodwinds_perf_interval_fast, woodwinds_trill, woodwinds_perf_repetition
    ]
woodwinds_short_long_notes = [staccato, portato.short, portato.med, sus]
woodwinds_dynamics =
    [dyn.str.sec 2, dyn.str.sec 3, dyn.str.sec 5, fp, sfz, sffz]
woodwinds_cluster_trills =
    [ cluster, cluster.sfz
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
woodwinds_perf_interval = [perf.legato, perf.marcato]
woodwinds_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
woodwinds_trill = [perf.trill]
woodwinds_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]

-- ** woodwinds 2

woodwinds2 :: [Instrument]
woodwinds2 =
    [ ("piccolo", low_keys, piccolo)
    , ("flute2", low_keys, flute2)
    , ("alto-flute", low_keys, alto_flute)
    , ("oboe1", low_keys, oboe1)
    , ("english-horn1", low_keys, english_horn1)
    , ("english-horn2", low_keys, english_horn2)
    , ("clarinet-eb", low_keys, clarinet_eb)
    , ("bass-clarinet", low_keys, bass_clarinet)
    , ("contra-bassoon", high_keys, contra_bassoon)
    ]

-- *** piccolo

piccolo =
    [ piccolo_short_long_notes, piccolo_dynamics
    , piccolo_flutter_trills, piccolo_perf_interval
    , piccolo_perf_interval_fast, piccolo_perf_trill
    , piccolo_perf_repetition, piccolo_fast_repetition
    , piccolo_grace_notes, piccolo_scale_runs
    , piccolo_arpeggios, piccolo_mordent
    ]
piccolo_short_long_notes =
    [ staccato, portato.short, portato.med.vib, portato.med.nv
    , portato.long.vib
    , sus.vib.v1, sus.vib.v2, sus.progr
    ]
piccolo_dynamics =
    [ dyn.str.vib.sec 2, dyn.str.vib.sec 3, dyn.str.vib.sec 5
    , dyn.str.nv.sec 1.5, dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 6, fpf.vib.sec 5, fp.vib, sfz.vib.v1, sfz.vib.v2, sfz.vib.v3
    ]
piccolo_flutter_trills =
    [ flutter
    , trill.half, trill.whole, trill.min.third, trill.maj.third , trill.fourth
    ]
piccolo_perf_interval = map (perf.) [legato, legato.lyric, marcato]
piccolo_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
piccolo_perf_trill = [perf.trill]
piccolo_perf_repetition = map (rep.)
    [ legato.slow, legato.med, legato.fast
    , portato.slow, portato.med, portato.fast
    , staccato
    ]
piccolo_fast_repetition = fast_rep_bpm [(mempty, [15..20] ++ [22])]
piccolo_grace_notes = grace_intervals
piccolo_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]
piccolo_arpeggios = arp_scales
    [ articulation.mode.speed
    | articulation <- [staccato, legato], mode <- [dim, maj, min]
    , speed <- [mempty, fast]
    ]
piccolo_mordent = map (mord.) [v1, v2, v3, v4, v5, v6]

-- *** flute2

flute2 =
    [ flute2_short_long_notes, flute2_dynamics
    , flute2_flutter_trills, flute2_perf_interval
    , flute2_perf_interval_fast, flute2_perf_trill
    , flute2_perf_repetition, flute2_fast_repetition
    , flute2_grace_notes, flute2_scale_runs, flute2_arpeggios
    ]
flute2_short_long_notes =
    [ staccato, portato.short, portato.med, portato.long.vib, portato.long.nv
    , sus.progr, sus.nv
    ]
flute2_dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 8
    , fp.vib, sfz.vib, fp.nv, sfz.nv
    ]
flute2_flutter_trills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.min.third, trill.maj.third, trill.fourth
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn
    ]
flute2_perf_interval = map (perf.) [legato, legato.grace, marcato]
flute2_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
flute2_perf_trill = [perf.trill]
flute2_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato, staccato
    , dyn5.legato.slow, dyn5.legato.fast
    ]
flute2_fast_repetition = fast_rep_bpm [(mempty, [16..21]), (triple, [13..16])]
flute2_grace_notes = grace_intervals
flute2_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]
flute2_arpeggios = arp_scales $ concat
    [ [legato.mode.fast | mode <- [dim, maj, min]]
    , [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
    ]

-- *** alto flute

alto_flute =
    [ alto_flute_short_long_notes, alto_flute_dynamics
    , alto_flute_flutter, alto_flute_perf_interval
    , alto_flute_perf_interval_fast, alto_flute_perf_trill
    , alto_flute_perf_repetition, alto_flute_grace_notes
    ]
alto_flute_short_long_notes =
    [ staccato, portato.short
    , portato.med.vib, portato.med.nv, portato.long.vib
    , sus.vib, sus.progr, sus.nv
    ]
alto_flute_dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.str.vib.sec 5
    , pfp.vib.sec 3, pfp.vib.sec 5, pfp.vib.sec 8
    , fp.vib, sfz.vib, sffz.vib
    ]
alto_flute_flutter =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
alto_flute_perf_interval = [perf.legato, perf.legato.grace, perf.marcato]
alto_flute_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
alto_flute_perf_trill = [perf.trill]
alto_flute_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
alto_flute_grace_notes = grace_intervals

-- *** oboe1

oboe1 =
    [ oboe1_short_long_notes, oboe1_dynamics
    , oboe1_flutter_trills, oboe1_perf_interval
    , oboe1_perf_interval_fast, oboe1_perf_trill
    , oboe1_perf_repetition, oboe1_perf_upbeat_repetition
    , oboe1_grace_notes, oboe1_scale_runs
    , oboe1_arpeggios, oboe1_mordents
    ]
oboe1_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.na.nv, portato.long.sa.nv
    , sus.progr, sus.nv
    ]
oboe1_dynamics =
    [ dyn.str.vib.sec 3, dyn.str.vib.sec 5
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.med.nv.sec 6
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 6, pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6
    , pfp.nv.sec 8
    , fpf.nv.sec 4, fpf.nv.sec 6
    , fp.nv, sfz.nv, sffz.nv
    ]
oboe1_flutter_trills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.min.third, trill.maj.third
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn, trill.maj.third.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
oboe1_perf_interval = map (perf.)
    [legato.nv, legato.progr, legato.grace, marcato]
oboe1_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
oboe1_perf_trill = [perf.trill]
oboe1_perf_repetition = map (rep.)
    [ legato.slow, legato.med, legato.fast
    , portato.slow, portato.med, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.med, dyn9.legato.fast
    , dyn5.portato.slow, dyn9.portato.med, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
oboe1_perf_upbeat_repetition = map (perf.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
oboe1_grace_notes = grace_intervals
oboe1_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]
oboe1_arpeggios = arp_scales
    [legato.mode.speed | mode <- [dim, maj], speed <- [mempty, fast]]
oboe1_mordents = map (mord.) [v1, v2, v3, v4, v5, v6]

-- *** english horn 1

english_horn1 =
    [ english_horn1_short_long_notes, english_horn1_dynamics
    , english_horn1_flutter_trills, english_horn1_perf_interval
    , english_horn1_perf_interval_fast, english_horn1_perf_trill
    , english_horn1_perf_repetition, english_horn1_grace_notes
    , english_horn1_scale_runs
    ]
english_horn1_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.ha.nv, portato.long.sa.nv
    , sus.vib, sus.nv
    ]
english_horn1_dynamics =
    [ dyn.med.vib.sec 3, dyn.str.vib.sec 5
    , dyn.med.nv.sec 1, dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3
    , dyn.med.nv.sec 4, dyn.med.nv.sec 6
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 5, pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6
    , pfp.nv.sec 8, pfp.nv.sec 10
    , fpf.nv.sec 4, fpf.nv.sec 6, fp, sfz, sffz
    ]
english_horn1_flutter_trills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
english_horn1_perf_interval = map (perf.)
    [legato.nv, legato.vib, grace, marcato]
english_horn1_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
english_horn1_perf_trill = [perf.trill]
english_horn1_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
english_horn1_grace_notes = grace_intervals
english_horn1_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]

-- *** english horn 2

english_horn2 =
    [ english_horn2_short_long_notes, english_horn2_dynmics
    , english_horn2_flutter_trills, english_horn2_perf_interval
    , english_horn2_perf_interval_fast, english_horn2_perf_trill
    , english_horn2_perf_repetition, english_horn2_grace_notes
    ]
english_horn2_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.na.vib, portato.long.na.nv, portato.long.pa.vib
    , sus.vib, sus.progr
    ]
english_horn2_dynmics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.med.vib.sec 4
    , dyn.str.vib.sec 4, dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 8
    , fp, sfz, sffz
    ]
english_horn2_flutter_trills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
english_horn2_perf_interval = [perf.legato, perf.legato.grace, perf.marcato]
english_horn2_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
english_horn2_perf_trill = [perf.trill]
english_horn2_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn9.legato.fast, dyn5.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
english_horn2_grace_notes = grace_intervals

-- *** clarinet eb

clarinet_eb =
    [ clarinet_eb_short_long_notes, clarinet_eb_dynamics
    , clarinet_eb_flutter_trills, clarinet_eb_perf_interval
    , clarinet_eb_perf_interval_fast, clarinet_eb_perf_trill
    , clarinet_eb_perf_repetition, clarinet_eb_fast_repetition
    , clarinet_eb_grace_notes
    ]
clarinet_eb_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.na, portato.long.ha, portato.long.sa, sus.nv
    ]
clarinet_eb_dynamics =
    [ dyn.li.sec 1.5, dyn.li.sec 2, dyn.med.sec 1.5
    , dyn.med.sec 2, dyn.med.sec 3, dyn.med.sec 4
    , dyn.str.sec 2, dyn.str.sec 3, dyn.str.sec 4, dyn.str.sec 6
    , pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6, pfp.sec 8
    , fp, sfz, sffz
    ]
clarinet_eb_flutter_trills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
clarinet_eb_perf_interval = [perf.legato, perf.legato.grace, perf.marcato]
clarinet_eb_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
clarinet_eb_perf_trill = [perf.trill]
clarinet_eb_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato.slow, dyn9.staccato.fast
    ]
clarinet_eb_fast_repetition =
    fast_rep_bpm [(mempty, [14..18]), (dyn, [14..18])]
clarinet_eb_grace_notes = grace_intervals

-- *** bass clarinet

bass_clarinet =
    [ bass_clarinet_short_long_notes, bass_clarinet_dynamics
    , bass_clarinet_flutter_trill, bass_clarinet_perf_interval
    , bass_clarinet_perf_interval_fast, bass_clarinet_perf_trill
    , bass_clarinet_perf_repetition, bass_clarinet_scale_runs
    ]
bass_clarinet_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.nv, portato.sus.nv
    ]
bass_clarinet_dynamics =
    [ dyn.med.sec 1.5, dyn.med.sec 2, dyn.med.sec 3, dyn.med.sec 4
    , dyn.str.sec 3, dyn.str.sec 4, dyn.str.sec 6
    , pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6, pfp.sec 8, pfp.sec 10
    , fp, sfz, sffz
    ]
bass_clarinet_flutter_trill =
    [flutter, trill.half, trill.whole, trill.half.dyn, trill.whole.dyn]
bass_clarinet_perf_interval = [perf.legato, perf.marcato]
bass_clarinet_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
bass_clarinet_perf_trill = [perf.trill]
bass_clarinet_perf_repetition = map (rep.)
    [ legato, portato, staccato
    , dyn5.legato, dyn9.portato, dyn9.staccato
    ]
bass_clarinet_scale_runs = run_scales
    [legato.maj, legato.min, legato.chrom, legato.whole]


-- *** contra bassoon

contra_bassoon =
    [ contra_bassoon_short_long_notes, contra_bassoon_dynamics
    , contra_bassoon_flutter, contra_bassoon_perf_interval
    , contra_bassoon_perf_interval_fast, contra_bassoon_perf_trill
    , contra_bassoon_perf_repetition, contra_bassoon_grace_notes
    ]
contra_bassoon_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.nv
    , sus.vib, sus.nv, fx.crescdim
    ]
contra_bassoon_dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.med.vib.sec 5
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.med.nv.sec 6
    , dyn.str.nv.sec 2, dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.sec 1.5, pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6
    , fp, sfz, sffz
    ]
contra_bassoon_flutter = [flutter, flutter.cresc]
contra_bassoon_perf_interval = [perf.legato, perf.legato.grace, perf.marcato]
contra_bassoon_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
contra_bassoon_perf_trill = [perf.trill]
contra_bassoon_perf_repetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.med, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.med, dyn9.portato.fast, dyn9.staccato.slow
    , dyn9.staccato.fast
    ]
contra_bassoon_grace_notes = grace_intervals

-- ** special woodwinds

special_woodwinds :: [Instrument]
special_woodwinds =
    [ ("bass-flute", high_keys, bass_flute)
    , ("oboe-damore", low_keys, oboe_damore)
    , ("heckelphone", low_keys, heckelphone)
    , ("contrabass-clarinet", high_keys, contrabass_clarinet)
    , ("basset-horn", high_keys, basset_horn)
    ]

bass_flute =
    [ bass_flute_short_long_notes, bass_flute_dynamics
    , bass_flute_flutter, bass_flute_perf_interval
    , bass_flute_perf_interval_fast, bass_flute_perf_trill
    , bass_flute_perf_repetition, bass_flute_fast_repetition
    ]
bass_flute_short_long_notes =
    [staccato, portato.short, portato.med, sus.vib, sus.nv]
bass_flute_dynamics =
    [ dyn.med.vib.sec 1, dyn.med.vib.sec 2, dyn.med.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 4
    , dyn.str.nv.sec 1.5, dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 3, pfp.vib.sec 6, pfp.vib.sec 8
    , pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
bass_flute_flutter = [flutter, flutter.cresc]
bass_flute_perf_interval = [perf.legato, perf.marcato]
bass_flute_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
bass_flute_perf_trill = [perf.trill]
bass_flute_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
bass_flute_fast_repetition = fast_rep_bpm [(mempty, [12..18]), (dyn, [12..18])]

oboe_damore =
    [ oboe_damore_short_long_notes, oboe_damore_dynamics
    , oboe_damore_flutter, oboe_damore_perf_interval
    , oboe_damore_perf_interval_fast, oboe_damore_perf_trill
    , oboe_damore_perf_repetition, oboe_damore_fast_repetition
    ]
oboe_damore_short_long_notes =
    [ staccato, portato.short, portato.med
    , portato.long.sa.vib, portato.long.sa.nv, sus.vib, sus.nv
    ]
oboe_damore_dynamics =
    [ dyn.med.vib.sec 1.5, dyn.med.vib.sec 2, dyn.med.vib.sec 3
    , dyn.med.vib.sec 4
    , dyn.str.vib.sec 3, dyn.str.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 10
    , pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
oboe_damore_flutter = [flutter, flutter.cresc]
oboe_damore_perf_interval = [perf.legato]
oboe_damore_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
oboe_damore_perf_trill = [perf.trill]
oboe_damore_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
oboe_damore_fast_repetition =
    fast_rep_bpm [(mempty, [12..18]), (dyn, [12..18])]

heckelphone =
    [ heckelphone_short_long_notes, heckelphone_dynamics
    , heckelphone_flutter, heckelphone_perf_interval
    , heckelphone_perf_interval_fast, heckelphone_trill
    , heckelphone_perf_repetition
    ]
heckelphone_short_long_notes =
    [staccato, portato.short, portato.med, sus.vib, sus.nv]
heckelphone_dynamics = seconds
    [ (dyn.med.vib, [1.5, 2, 3, 5])
    , (dyn.str.nv, [1.5, 2, 3, 4, 6])
    , (pfp.vib, [2, 3, 5, 8])
    ] ++ [fp.vib, sfz.vib, sffz.vib]
heckelphone_flutter = [flutter]
heckelphone_perf_interval = [perf.legato, perf.marcato]
heckelphone_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
heckelphone_trill = [perf.trill]
heckelphone_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn5.portato, dyn9.staccato]

contrabass_clarinet =
    [ contrabass_clarinet_short_long_notes, contrabass_clarinet_dynamics
    , contrabass_clarinet_perf_interval, contrabass_clarinet_perf_interval_fast
    , contrabass_clarinet_perf_trill, contrabass_clarinet_perf_repetition
    ]
contrabass_clarinet_short_long_notes = [staccato, portato, sus]
contrabass_clarinet_dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (pfp, [2, 3, 4, 6, 8]) ]
    ++ [fp, sfz, sffz]
contrabass_clarinet_perf_interval = [perf.legato, perf.marcato]
contrabass_clarinet_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
contrabass_clarinet_perf_trill = [perf.trill]
contrabass_clarinet_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn5.portato, dyn9.staccato]

basset_horn =
    [ basset_horn_short_long_notes, basset_horn_dynamics
    , basset_horn_flutter, basset_horn_perf_interval
    , basset_horn_perf_interval_fast, basset_horn_perf_trill
    , basset_horn_perf_repetition, basset_horn_fast_repetition
    ]
basset_horn_short_long_notes =
    [ staccato, portato.short, portato.med, portato.long.sa, portato.long.ha
    , sus
    ]
basset_horn_dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4])
    , (dyn.str, [1.5, 2, 3, 4, 6])
    , (pfp, [2, 3, 4, 6, 8])
    ] ++ [fp, sfz, sffz]
basset_horn_flutter = [flutter, flutter.cresc]
basset_horn_perf_interval = [perf.legato, perf.marcato]
basset_horn_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
basset_horn_perf_trill = [perf.trill]
basset_horn_perf_repetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
basset_horn_fast_repetition =
    fast_rep_bpm [(mempty, [12..18]), (dyn, [12..18])]

-- * brass

-- ** brass1

brass1 :: [Instrument]
brass1 =
    [ ("trumpet-c", low_keys, trumpet_c)
    , ("trumpet-c-mute", low_keys, trumpet_c_mute)
    , ("horn-vienna", low_keys, horn_vienna)
    , ("tenor-trombone", low_keys, tenor_trombone)
    , ("tenor-trombone-mute-a", low_keys, tenor_trombone_mute_a)
    , ("tenor-trombone-mute-b", low_keys, tenor_trombone_mute_b)
    , ("tuba", high_keys, tuba)
    , ("trumpets-a3", low_keys, trumpets_a3)
    , ("trumpets-a3-mute", low_keys, trumpets_a3_mute)
    , ("horns-a4", low_keys, horns_a4)
    , ("horns-a4-stopped", low_keys, horns_a4_stopped)
    , ("trombones-a3", low_keys, trombones_a3)
    , ("trombones-a3-mute", low_keys, trombones_a3_mute)
    ]

trumpet_c =
    [ trumpet_c_short_long_notes, trumpet_c_dynamics
    , trumpet_c_flutter_trills, trumpet_c_perf_interval
    , trumpet_c_perf_interval_fast, trumpet_c_perf_trill
    , trumpet_c_perf_repetition, trumpet_c_fast_repetition
    , trumpet_c_upbeat_repetition, trumpet_c_grace_notes
    , trumpet_c_scale_runs, trumpet_c_arpeggios, trumpet_c_mordents
    ]
trumpet_c_short_long_notes =
    [ staccato, portato.short
    , portato.med, portato.med.marcato, portato.med.soft
    -- long-norm, long-marcato, long-soft
    , portato.long.norm.vib, portato.long.marcato.vib
    , portato.long.norm.vib_str, portato.long.marcato.vib_str
    , portato.long.norm.nv, portato.long.marcato.nv
    , portato.long.soft.nv
    , sus.vib, sus.vib_str, sus.progr, sus.nv
    ]
trumpet_c_dynamics = seconds
    [ (dyn.li.nv, [1.5, 2, 3, 4]), (dyn.med.vib, [2, 3, 4, 6])
    , (dyn.med.nv, [1.5, 2, 3, 4, 6]), (dyn.str.vib, [3, 6])
    , (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.vib, [4, 8]), (pfp.nv, [4, 6, 8])
    ] ++ [fp, sfz, sffz]
trumpet_c_flutter_trills =
    [ flutter, flutter.cresc, trill.slow.half, trill.slow.whole
    , trill.fast.half, trill.fast.whole, trill.acc.half, trill.acc.whole
    ]
trumpet_c_perf_interval = map (perf.)
    [ legato.nv, legato.nv.sus -- TODO what's this?
    , legato.vib, legato.vib.sus, marcato
    ]
trumpet_c_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
trumpet_c_perf_trill = [perf.trill]
trumpet_c_perf_repetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato, staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpet_c_fast_repetition = fast_rep_bpm
    [(mempty, [14, 16, 17, 19, 21]), (dyn, [14, 15, 16, 17, 19, 21])]
trumpet_c_upbeat_repetition = upbeat_rep_bpm
    [ (n1, [10, 11, 12, 13, 14, 16, 18])
    , (n2, [10, 11, 12, 13, 14, 16, 18, 20])
    , (n3, [10, 11, 12, 13, 14, 16, 18, 20])
    ]
trumpet_c_grace_notes = [grace.updown.half, grace.updown.whole]
trumpet_c_scale_runs = run_scales [legato.maj]
trumpet_c_arpeggios = arp_scales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
trumpet_c_mordents = map (mord.legato.) [v1, v2, v3, v4, v5, v6]

trumpet_c_mute =
    [ trumpet_c_mute_short_long_notes, trumpet_c_mute_dynamics
    , trumpet_c_mute_flutter_trills, trumpet_c_mute_perf_interval
    , trumpet_c_mute_perf_interval_fast, trumpet_c_mute_perf_trill
    , trumpet_c_mute_perf_repetition, trumpet_c_mute_fast_repetition
    , trumpet_c_mute_upbeat_repetition
    ]
trumpet_c_mute_short_long_notes =
    [ staccato, portato.short, portato.med, portato.long.vib
    , sus.vib, sus.progr, sus.nv
    ]
trumpet_c_mute_dynamics = seconds
    [ (dyn.med.vib, [2, 3, 4]), (dyn.med.nv, [2, 3, 4, 6])
    , (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.vib, [2, 5, 9]), (pfp.nv, [2, 3, 4, 5])
    ] ++ [fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv]
trumpet_c_mute_flutter_trills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
trumpet_c_mute_perf_interval =
    [perf.legato.nv, perf.legato.vib, perf.marcato.nv]
trumpet_c_mute_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
trumpet_c_mute_perf_trill = [perf.trill]
trumpet_c_mute_perf_repetition = map (perf.rep.)
    [ legato, portato.slow, portato.fast, staccato.slow, staccato.fast
    , dyn5.legato, dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpet_c_mute_fast_repetition = fast_rep_bpm
    [(mempty, [14, 15, 16, 17, 19]), (dyn, [14, 15, 16, 17, 19])]
trumpet_c_mute_upbeat_repetition =
    upbeat_rep_bpm [(n1, bpms), (n2, bpms), (n3, bpms)]
    where bpms = [9, 10, 11, 12, 13, 14, 16, 18, 20]

horn_vienna =
    [ horn_vienna_short_long_notes, horn_vienna_dynamics
    , horn_vienna_flutter_trills, horn_vienna_stopped
    , horn_vienna_perf_interval, horn_vienna_perf_interval_fast
    , horn_vienna_perf_trill, horn_vienna_perf_repetition
    , horn_vienna_fast_repetition, horn_vienna_upbeat_repetition
    , horn_vienna_grace_notes, horn_vienna_glissandi
    ]
horn_vienna_short_long_notes =
    [ staccato, portato.short, portato.short.soft, portato.med
    , portato.med.soft, portato.long.vib, portato.long.nv
    , portato.long.marcato, portato.long.blare
    , sus.vib, sus.nv, sus.blare
    ]
horn_vienna_dynamics = seconds
    [ (dyn.li.nv, [1, 1.5, 2, 3, 4, 6]), (dyn.med.vib, [4, 6])
    , (dyn.med.nv, [1, 1.5, 2, 3, 4, 6]), (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.nv.v1, [6]), (pfp.nv.v2, [6]), (pfp.nv, [8])
    ] ++ [fp, sfz, sffz]
horn_vienna_flutter_trills =
    [flutter, flutter.cresc, trill.half, trill.whole, trill.lip]
horn_vienna_stopped = map (stop.)
    [ staccato, portato.med, portato.long, sus
    , dyn.sec 1, dyn.sec 1.5, dyn.sec 2, dyn.sec 3, dyn.sec 4, dyn.sec 6
    , fp, sfz, sffz
    ]
horn_vienna_perf_interval = map (perf.)
    [legato.nv, legato.nv.sus, legato.vib.sus, marcato]
horn_vienna_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
horn_vienna_perf_trill = [perf.trill]
horn_vienna_perf_repetition = map (perf.rep.)
    [ legato.slow, legato.med, legato.fast, portato
    , staccato.slow, staccato.fast
    , dyn6.legato.slow, dyn6.legato.med, dyn6.legato.fast
    , dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
horn_vienna_fast_repetition =
    fast_rep_bpm [(mempty, [15..19]), (dyn, [15..19])]
horn_vienna_upbeat_repetition = upbeat_rep_bpm
    [ (n1, [9, 10, 11, 12, 13, 14, 16, 18])
    , (n2, bpms), (n3, bpms)
    ]
    where bpms = [9, 10, 11, 12, 13, 14, 16, 18, 20]
horn_vienna_grace_notes = [grace.updown.half, grace.updown.whole]
horn_vienna_glissandi = map (gliss.)
    [ fast, slow.up, slow.fourth, slow.dim.fifth, slow.fifth, slow.min.sixth
    , slow.maj.sixth, slow.min.seventh, slow.maj.seventh, slow.oct
    , fast.up.fourth, fast.up.fifth, fast.up.min.sixth, fast.up.maj.sixth
    , fast.up.min.seventh, fast.up.maj.seventh, fast.up.oct
    ]

tenor_trombone =
    [ tenor_trombone_short_long_notes, tenor_trombone_dynamics
    , tenor_trombone_flutter, tenor_trombone_perf_interval
    , tenor_trombone_perf_interval_fast, tenor_trombone_perf_trill
    , tenor_trombone_perf_repetition, tenor_trombone_fast_repetition
    , tenor_trombone_upbeat_repetition, tenor_trombone_grace_notes
    , tenor_trombone_glissandi, tenor_trombone_arpeggios
    ]
tenor_trombone_short_long_notes =
    [ staccato, portato.short, portato.short.soft, portato.med
    , portato.med.soft, portato.medium.marcato, portato.long.vib
    , portato.long.vib.soft, portato.long.nv, portato.long.marcato
    , sus.progr, sus.vib, sus.vib_str, sus.nv
    ]
tenor_trombone_dynamics = seconds
    [ (dyn.li.vib, [1.5, 2, 3, 4]), (dyn.li.nv, [1, 1.5, 2, 3, 4, 6])
    , (dyn.med.vib, [2, 3, 4, 6]), (dyn.med.nv, [1.5, 2, 3, 4, 6])
    , (dyn.str.vib, [4]), (dyn.str.nv, [2, 3, 4, 6]), (pfp.nv, [6])
    ] ++ [fp, sfz, sffz]
tenor_trombone_flutter = [flutter, flutter.cresc]
tenor_trombone_perf_interval = map (perf.)
    [legato.nv, legato.nv.sus, legato.vib, legato.vib.sus, marcato]
tenor_trombone_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
tenor_trombone_perf_trill = [perf.trill]
tenor_trombone_perf_repetition = map (perf.rep.)
    [ legato, portato, staccato.slow, staccato.fast
    , dyn6.legato.slow, dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
tenor_trombone_fast_repetition =
    fast_rep_bpm [(mempty, [15..19]), (dyn, [15..19])]
tenor_trombone_upbeat_repetition = upbeat_rep_bpm
    [(n1, bpms), (n2, bpms), (n3, [8..14] ++ [16, 18])]
    where bpms = [8..14] ++ [16, 18, 20, 22]
tenor_trombone_grace_notes = map (grace.updown.) [half, whole]
tenor_trombone_glissandi = map (gliss.)
    [ fast, slow, fast.half, fast.whole, fast.min.third, fast.maj.third
    , fast.fourth, fast.dim.fifth
    , slow.min.third, slow.maj.third, slow.fourth, slow.dim.fifth
    ]
tenor_trombone_arpeggios = arp_scales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]

tenor_trombone_mute_a =
    [ tenor_trombone_mute_a_short_long_notes, tenor_trombone_mute_a_dynamics
    , tenor_trombone_mute_a_flutter, tenor_trombone_mute_a_perf_interval
    , tenor_trombone_mute_a_perf_repetition
    , tenor_trombone_mute_a_fast_repetition
    , tenor_trombone_mute_a_upbeat_repetition
    ]
tenor_trombone_mute_a_short_long_notes =
    [ staccato, portato.short, portato.med, portato.long.vib
    , sus.progr, sus.nv
    ]
tenor_trombone_mute_a_dynamics = seconds
    [ (dyn.med.vib, [1.5, 2, 3, 4]), (dyn.med.nv, [1.5, 2, 3, 4, 6])
    , (pfp.nv, [2, 3, 4])
    ] ++ [fp, sfz, sffz]
tenor_trombone_mute_a_flutter = [flutter]
tenor_trombone_mute_a_perf_interval = [perf.legato, perf.marcato]
tenor_trombone_mute_a_perf_repetition = map (perf.rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
tenor_trombone_mute_a_fast_repetition =
    fast_rep_bpm [(mempty, [14..19]), (dyn, [14..19])]
tenor_trombone_mute_a_upbeat_repetition = upbeat_rep_bpm
    [(n1, [9..14]), (n2, [9..14] ++ [16, 18]), (n2, [9..14] ++ [16, 18])]

tenor_trombone_mute_b =
    [ tenor_trombone_mute_b_short_long_notes, tenor_trombone_mute_b_dynamics
    , tenor_trombone_mute_b_flutter, tenor_trombone_mute_b_perf_interval
    , tenor_trombone_mute_b_perf_repetition
    , tenor_trombone_mute_b_fast_repetition
    , tenor_trombone_mute_b_upbeat_repetition
    ]
tenor_trombone_mute_b_short_long_notes =
    [staccato, portato.short, portato.med, sus.progr, sus.nv]
tenor_trombone_mute_b_dynamics = seconds
    [ (dyn.med.vib, [1.5, 2, 3, 4]), (dyn.med.nv, [1.5, 2, 3, 4])
    , (pfp.nv, [2, 3, 4])
    ] ++ [fp, sfz, sffz]
tenor_trombone_mute_b_flutter = [flutter, flutter.cresc]
tenor_trombone_mute_b_perf_interval = [perf.legato, perf.marcato]
tenor_trombone_mute_b_perf_repetition = map (perf.rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
tenor_trombone_mute_b_fast_repetition =
    fast_rep_bpm [(mempty, [14..19]), (dyn, [14..19])]
tenor_trombone_mute_b_upbeat_repetition =
    upbeat_rep_bpm [(n1, bpms), (n2, bpms), (n3, bpms)]
    where bpms = [9..14] ++ [16, 18]

tuba =
    [ tuba_short_long_notes, tuba_dynamics, tuba_flutter_trills
    , tuba_mute_basic, tuba_perf_interval, tuba_perf_interval_fast
    , tuba_perf_repetition, tuba_fast_repetition, tuba_upbeat_repetition
    , tuba_grace_notes
    ]
tuba_short_long_notes =
    [ staccato, portato.med, portato.med.soft, portato.med.hard
    , portato.long.vib, portato.long.vib_str
    , portato.long.nv.soft, portato.long.nv.hard
    , sus.vib.v1, sus.vib.v2, sus.nv
    ]
tuba_dynamics = seconds
    [ (dyn.li.nv, [1, 1.5, 2, 3, 4]), (dyn.med.vib, [2, 3, 4, 6])
    , (dyn.med.nv, [2, 3, 4, 6]), (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.nv, [4])
    ] ++ [fp, sfz, sffz]
tuba_flutter_trills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.cresc, trill.whole.cresc
    ]
tuba_mute_basic = map (mute.)
    [ staccato, portato.med, portato.long, sus
    , dyn.sec 2, dyn.sec 3, dyn.sec 4
    , sfz, flutter, flutter.cresc
    ]
tuba_perf_interval = map (perf.legato.) [nv, nv.sus, vib.sus]
tuba_perf_interval_fast = [perf.legato.fast]
tuba_perf_repetition = map (perf.rep.)
    [ legato, portato.slow, portato.fast, staccato.slow, staccato.fast
    , dyn6.legato, cresc9.portato.slow, cresc9.portato.fast
    , cresc9.staccato.slow, cresc9.staccato.fast
    ]
tuba_fast_repetition =
    fast_rep_bpm [(mempty, [14..19]), (cresc, [15..19])]
tuba_upbeat_repetition =
    upbeat_rep_bpm [(n1, [8..14]), (n2, [8..14]), (n3, [8..14] ++ [16])]
tuba_grace_notes = map (grace.updown.) [half, whole]

trumpets_a3 =
    [ trumpets_a3_short_long_notes, trumpets_a3_dynamics
    , trumpets_a3_flutter_trills, trumpets_a3_perf_interval
    , trumpets_a3_perf_interval_fast, trumpets_a3_perf_trill
    , trumpets_a3_perf_repetition, trumpets_a3_perf_upbeat_repetition
    , trumpets_a3_fast_repetition, trumpets_a3_upbeat_repetition
    , trumpets_a3_scale_runs, trumpets_a3_arpeggios
    , trumpets_a3_fall_release
    ]
trumpets_a3_short_long_notes =
    [ staccato, portato.short, portato.med, portato.long
    , sus.vib, sus.nv, sus.marcato, sus.tune, sus.rip, fall, fall.fa
    ]
trumpets_a3_dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (dyn.str, [2, 3, 4, 6])
    , (pfp, [2, 3, 4, 6])
    ] ++ [fp, sfz, sffz]
trumpets_a3_flutter_trills =
    [ flutter, flutter.cresc, trill.half, trill.whole
    , trill.half.dyn, trill.whole.dyn
    ]
trumpets_a3_perf_interval = map (perf.)
    [legato.nv, legato.nv.sus, legato.vib, legato.tune, marcato]
trumpets_a3_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
trumpets_a3_perf_trill = [perf.trill]
trumpets_a3_perf_repetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpets_a3_perf_upbeat_repetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.med, n2.med, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.med, dyn4.n2.med
    , dyn4.n1.fast, dyn4.n2.fast
    ]
trumpets_a3_fast_repetition =
    fast_rep_bpm [(mempty, [15..19]), (dyn, [15..19])]
trumpets_a3_upbeat_repetition = upbeat_rep_bpm
    [(n1, [8..14]), (n2, [8..14] ++ [16, 18]), (n3, [8..14] ++ [16, 18])]
trumpets_a3_scale_runs =
    run_scales [legato.maj, legato.min, legato.chrom, legato.whole]
trumpets_a3_arpeggios = arp_scales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
trumpets_a3_fall_release = map (fall.)
    [ sus.vib, sus.nv, sus.marcato, sus.tune, sus.rip
    , perf.legato.nv, perf.legato.vib, perf.legato.tune, perf.marcato
    ]

trumpets_a3_mute =
    [ trumpets_a3_mute_short_long_notes, trumpets_a3_mute_dynamics
    , trumpets_a3_mute_flutter, trumpets_a3_mute_perf_interval
    , trumpets_a3_mute_perf_repetition, trumpets_a3_mute_fast_repetition
    , trumpets_a3_mute_upbeat_repetitions
    ]
trumpets_a3_mute_short_long_notes = [staccato, portato.short, portato.med, sus]
trumpets_a3_mute_dynamics =
    seconds [(dyn.str, [2, 3, 4, 6]), (pfp, [2, 3, 4, 6])]
    ++ [fp, sfz, sffz]
trumpets_a3_mute_flutter = [flutter, flutter.cresc]
trumpets_a3_mute_perf_interval = [perf.legato, perf.marcato]
trumpets_a3_mute_perf_repetition = map (rep.)
    [ legato, portato, staccato.slow, staccato.fast
    , dyn5.legato, dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpets_a3_mute_fast_repetition =
    fast_rep_bpm [(mempty, [14..19]), (dyn, [14..19])]
trumpets_a3_mute_upbeat_repetitions =
    upbeat_rep_bpm [(n1, bpms), (n2, bpms), (n3, bpms)]
    where bpms = [9..14] ++ [16, 18, 20]

horns_a4 =
    [ horns_a4_short_long_notes, horns_a4_dynamics
    , horns_a4_flutter, horns_a4_perf_interval
    , horns_a4_perf_interval_fast, horns_a4_perf_trill
    , horns_a4_perf_repetition, horns_a4_perf_upbeat_repetition
    , horns_a4_fast_repetition, horns_a4_upbeat_repetition
    , horns_a4_arpeggios, horns_a4_glissandi
    ]
horns_a4_short_long_notes =
    [ staccato, portato.short, portato.med, portato.long
    , sus, sus.marcato, sus.blare
    ]
horns_a4_dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (dyn.str, [3, 4, 6]), (pfp, [4, 6])
    ] ++ [fp, sfz, sffz]
horns_a4_flutter = [flutter, flutter.cresc]
horns_a4_perf_interval = [perf.legato, perf.legato.sus, perf.marcato]
horns_a4_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
horns_a4_perf_trill = [perf.trill]
horns_a4_perf_repetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
horns_a4_perf_upbeat_repetition = map (upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
horns_a4_fast_repetition =
    fast_rep_bpm [(mempty, [15..19]), (dyn, [15..19])]
horns_a4_upbeat_repetition = upbeat_rep_bpm
    [(n1, [8..13]), (n2, [8..14] ++ [16, 18]), (n3, [8..14] ++ [16, 18])]
horns_a4_arpeggios = arp_scales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
horns_a4_glissandi = map (gliss.)
    [ perf, fourth, dim.fifth, fifth, min.sixth, maj.sixth, min.seventh
    , maj.seventh, oct
    ]

horns_a4_stopped =
    [ horns_a4_stopped_short_long_notes, horns_a4_stopped_dynamics
    , horns_a4_stopped_perf_interval, horns_a4_stopped_perf_repetition
    , horns_a4_stopped_fast_repetition, horns_a4_stopped_upbeat_repetition
    ]
horns_a4_stopped_short_long_notes = [staccato, sus]
horns_a4_stopped_dynamics =
    seconds [(dyn.med, [1.5, 2, 3, 4])] ++ [fp, sfz, sffz]
horns_a4_stopped_perf_interval = [perf.legato, perf.legato.sus]
horns_a4_stopped_perf_repetition = map (perf.rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
horns_a4_stopped_fast_repetition =
    fast_rep_bpm [(mempty, [14..19]), (dyn, [14..19])]
horns_a4_stopped_upbeat_repetition =
    upbeat_rep_bpm [(n1, [9..14]), (n2, [9..14]), (n3, [9..14])]

trombones_a3 =
    [ trombones_a3_short_long_notes, trombones_a3_dynamics
    , trombones_a3_flutter, trombones_a3_cluster
    , trombones_a3_perf_interval, trombones_a3_perf_interval_fast
    , trombones_a3_perf_trill, trombones_a3_perf_repetition
    , trombones_a3_perf_upbeat_repetition, trombones_a3_fast_repetition
    , trombones_a3_upbeat_repetition, trombones_a3_arpeggios
    , trombones_a3_glissandi
    ]
trombones_a3_short_long_notes =
    [ staccato.short, staccato.med, portato.short, portato.med, portato.long
    , sus, sus.marcato
    ]
trombones_a3_dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (dyn.str, [2, 3, 4, 6])
    , (pfp, [4, 6, 8, 10])
    ] ++ [fp, sfz, sffz]
trombones_a3_flutter = [flutter, flutter.cresc]
trombones_a3_cluster = map (cluster.)
    [ staccato, sus, dyn.sec 1.5, dyn.sec 4, sfz
    , rep.legato, rep.dyn5.legato
    ]
trombones_a3_perf_interval = [perf.legato, perf.legato.sus, perf.marcato]
trombones_a3_perf_interval_fast = [perf.legato.fast, perf.marcato.fast]
trombones_a3_perf_trill = [perf.trill]
trombones_a3_perf_repetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast, dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
trombones_a3_perf_upbeat_repetition = map (upbeat.)
    [ n1.slow, n2.slow, n1.med, n2.med, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.med, dyn4.n2.med
    , dyn4.n1.fast, dyn4.n2.fast
    ]
trombones_a3_fast_repetition =
    fast_rep_bpm [(mempty, [14..18]), (dyn, [14..18])]
trombones_a3_upbeat_repetition = upbeat_rep_bpm
    [(n1, [8..14]), (n2, [8..14] ++ [16, 18]), (n3, [8..14] ++ [16, 18])]
trombones_a3_arpeggios = arp_scales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
trombones_a3_glissandi = map (gliss.)
    [perf, half, whole, min.third, maj.third, fourth, dim.fifth]

trombones_a3_mute =
    [ trombones_a3_mute_short_long_notes, trombones_a3_mute_dynamics
    , trombones_a3_mute_flutter, trombones_a3_mute_perf_interval
    , trombones_a3_mute_repetition, trombones_a3_mute_fast_repetition
    , trombones_a3_mute_upbeat_repetition
    ]
trombones_a3_mute_short_long_notes =
    [staccato, portato.short, portato.med, sus]
trombones_a3_mute_dynamics = seconds
    [(dyn.str, [2, 3, 4, 6]), (pfp, [2, 3, 4, 7])
    ] ++ [fp, sfz, sffz]
trombones_a3_mute_flutter = [flutter, flutter.cresc]
trombones_a3_mute_perf_interval = [perf.legato, perf.marcato]
trombones_a3_mute_repetition = map (perf.rep.)
    [ legato, portato, staccato.slow, staccato.fast
    , dyn5.legato, dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
trombones_a3_mute_fast_repetition = fast_rep_bpm [(mempty, [14..19])]
trombones_a3_mute_upbeat_repetition = upbeat_rep_bpm
    [(n1, [9..15]), (n2, [9..14] ++ [16, 18]), (n3, [9..14] ++ [16, 18])]


-- * util

run_scales :: [Attributes] -> [Attributes]
run_scales attrs = map (run.) (with_scale attrs)

arp_scales :: [Attributes] -> [Attributes]
arp_scales attrs = map (arp.) (with_scale attrs)

-- | Add an attr for each pitch, if it has 'maj' or 'min'.
with_scale :: [Attributes] -> [Attributes]
with_scale = concatMap make
    where
    make attr
        | any (Attrs.contain attr) [maj, min] = map (attr.) scale
        | otherwise = [attr]

scale :: [Attributes]
scale = map attr
    ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

grace_intervals :: [Attributes]
grace_intervals = map (grace.updown.) intervals_to_oct

-- | Intervals from half note to the octave.
intervals_to_oct :: [Attributes]
intervals_to_oct =
    [ half, whole, min.third, maj.third, fourth, dim.fifth, fifth
    , min.sixth, maj.sixth, min.seventh, maj.seventh, oct
    ]

fast_rep_bpm :: [(Attributes, [Int])] -> [Attributes]
fast_rep_bpm attrs =
    concat [map (attr.fast.rep.) (map bpm (map (10*) bpms))
        | (attr, bpms) <- attrs]

upbeat_rep_bpm :: [(Attributes, [Int])] -> [Attributes]
upbeat_rep_bpm attrs =
    concat [map (attr.perf.upbeat.rep.) (map bpm (map (10*) bpms))
        | (attr, bpms) <- attrs]

seconds :: [(Attributes, [RealTime])] -> [Attributes]
seconds attrs_secs =
    prefix_attrs [(prefix, map sec secs) | (prefix, secs) <- attrs_secs]

-- | Add a prefix to each list of attributes.
prefix_attrs :: [(Attributes, [Attributes])] -> [Attributes]
prefix_attrs attrs = concat [map (prefix.) attrs | (prefix, attrs) <- attrs]

-- * attrs

sec :: RealTime -> Attributes
sec n = attr $ "sec"
    <> Text.replace "." "-" (Num.showFloat0 Nothing (RealTime.to_seconds n))

parse_sec :: Attributes -> Maybe (RealTime, Attributes)
parse_sec attrs = case Seq.partition_on has_sec (Attrs.to_list attrs) of
    ([secs], rest) -> Just (secs, Attrs.attrs rest)
    _ -> Nothing
    where
    (.) = (Prelude..)
    has_sec = parse . Text.replace "-" "." <=< Text.stripPrefix "sec"
    parse t = case ParseText.parse ParseText.p_unsigned_float t of
        Left _ -> Nothing
        Right val -> Just $ RealTime.seconds val

bpm :: Int -> Attributes
bpm n = attr ("bpm" <> showt n)

-- | General-purpose version, for when I don't have a better way to describe
-- the difference.
version :: Int -> Attributes
version n = attr ("v" <> showt n)

-- | Number of notes.
notes :: Int -> Attributes
notes n = attr ("n" <> showt n)

n1 = notes 1
n2 = notes 2
n3 = notes 3

-- TODO make sure this is used consistently.  What does it mean really?
perf = attr "perf" -- interval performances

sus = attr "sus"
norm = attr "norm"
-- | An \"arpeggio\" variant where the notes are simultaneous.
straight = attr "straight"

acc = attr "acc" -- accelerando
rit = attr "rit" -- ritardando
secco = attr "secco"
legno = attr "legno"
harsh = attr "harsh"
art = Attrs.artificial -- artificial harmonics
nat = Attrs.natural -- natural harmonics
rep = attr "rep" -- repetitions
sul = attr "sul" -- sul, on the same string
zigane = attr "zigane" -- "gipsy style" portamento
bow = attr "bow" -- bow vibrato
ricochet = attr "ricochet"

table = attr "table" -- pres-de-la-table, TODO aka secco
nail = attr" nail"

hard = attr "hard"

-- ** brass

blare = attr "blare"
lip = attr "lip"
stop = attr "stop" -- stopped
rip = attr "rip"
fall = attr "fall"
tune = attr "tune"

fx = attr "fx"

-- ** dynamics

auto = attr "auto" -- attack automation, guesses fast attack or normal attack
fa = attr "fa" -- fast attack
ha = attr "ha" -- hard attack
na = attr "na" -- normal attack
sa = attr "sa" -- soft attack
pa = attr "pa" -- pressed attack
progr = attr "progr" -- progressive vibrato
vib_str = attr "vib-str" -- strong vibrato
vib_down = attr "vib-down" -- decreasing vibrato

lyric = attr "lyric"

rs = attr "rs" -- release samples

li = attr "li" -- light
med = attr "med" -- medium
str = attr "str" -- strong

-- ** intervals

min = attr "min"
maj = attr "maj"
aug = attr "aug"
chrom = attr "chrom"
pent = attr "pent"

third = attr "third"
fourth = attr "fourth"
fifth = attr "fifth"
sixth = attr "sixth"
seventh = attr "seventh"
oct = attr "oct"

-- ** rhythm

upbeat = attr "upbeat"
triple = attr "triple"

cluster = attr "cluster" -- cluster of tones

-- repetitions
dyn4 = dyn . notes 4 . crescdim
dyn5 = dyn . notes 5 . crescdim
dyn6 = dyn . notes 6 . crescdim
dyn9 = dyn . notes 9 . crescdim
cresc9 = notes 9 . cresc

-- ** a/b variants

dyn = attr "dyn" . crescdim
grace = attr "grace"
run = attr "run" . updown
arp = attr "arp" . updown
arpup = attr "arp" -- no updown
mord = attr "mord" . updown -- mordent

updown = attr "updown" -- split into up and down versions, mapped to a/b
crescdim = attr "crescdim" -- split into cresc and dim versions, mapped to a/b
highlow = attr "highlow" -- split into high and low versions
