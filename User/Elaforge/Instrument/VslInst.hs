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
import qualified Util.ParseA as ParseA
import qualified Util.Lists as Lists

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

data Keys = Keys
    -- | Base keyswitch for matrix x axis.
    { keyXAxis :: Midi.Key
    -- | Base keyswitch for matrix y axis.
    , keyYAxis :: Midi.Key
    -- | First of the AB keyswitches.
    , keyAb :: Midi.Key
    -- | Base keyswitches for the presets.
    , keyMatrix :: Midi.Key
    } deriving (Show)

-- | (name, keyswitch configuration, attributes)
type Instrument = (Text, Keys, [[Attributes]])

-- | Instruments that start above a1 use this.
lowKeys :: Keys
lowKeys = Keys
    { keyXAxis = Key.d_1
    , keyYAxis = Key.d0
    , keyAb = Key.d1
    , keyMatrix = Key.e1
    }

-- | Instruments that go below a1 but don't go above d7 use this.
highKeys :: Keys
highKeys = Keys
    { keyXAxis = Key.d7
    , keyYAxis = Key.d8
    , keyAb = Key.d9
    , keyMatrix = Key.e9
    }

-- * strings

-- ** solo strings

soloStrings :: [Instrument]
soloStrings =
    [ ("violin", lowKeys, violin)
    , ("viola", lowKeys, viola)
    , ("cello", lowKeys, cello)
    , ("bass", highKeys, bass)
    ]
soloViolin, soloViola, soloCello, soloBass :: Instrument
soloViolin = ("violin", lowKeys, violin)
soloViola = ("viola", lowKeys, viola)
soloCello = ("cello", lowKeys, cello)
soloBass = ("bass", highKeys, bass)

violin =
    [ violinShortLongNotes, violinDynamics
    , violinTremoloTrills, violinPizzLegno
    , violinHarmonics, violinPonticello
    , violinTasto, violinPerfInterval
    , violinPerfIntervalFast, violinPerfTrill
    , violinPerfRepetition, violinFastRepetition
    , violinGraceNotes, violinGlissandi, violinScaleRuns
    ]
violinShortLongNotes =
    [ staccato, detache.short, detache.long.vib, detache.long.nv
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.vib.espr, sus.progr
    , sus.vibDown, sus.nv
    ]
violinDynamics =
    [ dyn.med.vib.sec 1.5, dyn.med.vib.sec 3, dyn.med.vib.sec 4
    , dyn.str.vib.sec 1.5, dyn.str.vib.sec 3, dyn.str.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 3
    , pfp.sec 2, pfp.sec 4, fp.vib, sfz.vib, sffz.vib
    ]
violinTremoloTrills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 1.5, trem.dyn.sec 3
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.acc.half, trill.acc.whole, trill.acc.half.dyn, trill.acc.whole.dyn
    ]
violinPizzLegno = [pizz, pizz.secco, pizz.snap, legno]
violinHarmonics = map (harm.)
    [ art.staccato, art.sus, art.perf.rep, art.gliss.updown
    , nat.sus, nat.rep, nat.gliss.updown
    ]
violinPonticello = map (pont.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto
    , sfz, sffz, trem, trem.fa, trem.fa.auto
    , rep.legato, rep.dyn.legato
    ]
violinTasto = map (tasto.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto
    , sfz, trem, trem.fa, trem.fa.auto
    ]
violinPerfInterval = map (perf.)
    [ legato, legato.sul, legato.progr, legato.zigane
    , porta, detache, marcato, spiccato
    ]
violinPerfIntervalFast = map (perf.fa.)
    [legato, marcato, spiccato, harsh]
violinPerfTrill = [perf.trill.legato]
violinPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, bow.slow, bow.fast
    , portato.slow, portato.med, portato.fast
    , staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.bow.slow, dyn5.bow.fast
    , dyn5.portato.slow, dyn5.portato.med, dyn9.portato.fast, dyn9.staccato
    , dyn9.spiccato, dyn9.harsh
    ]
violinFastRepetition =
    fastRepBpm [(mempty, [15..20]), (dyn, [15..20])]
    ++ map (ricochet.) (map bpm [150, 160, 170, 180, 190, 210])
    ++ [ricochet.notes 3, ricochet.acc, ricochet.rit]
violinGraceNotes = map (grace.updown.)
    [ half.slow, whole.slow, half.fast, whole.fast
    , zigane.half, zigane.whole, zigane.min.third, zigane.maj.third
    ]
violinGlissandi = map (gliss.)
    [ perf.attr "g", perf.attr "d", perf.attr "a", perf.attr "e"
    , updown.oct.med, updown.oct.fast
    ]
violinScaleRuns = runScales
    [ legato.maj, legato.min, legato.chrom, legato.whole
    , spiccato.maj, spiccato.min, spiccato.chrom, spiccato.whole
    ]

viola =
    [ violaShortLongNotes, violaDynamics
    , violinTremoloTrills, violaPizzLegno
    , violaHarmonics, violinPonticello
    , violinTasto, violaPerfInterval
    , violinPerfIntervalFast, violinPerfTrill
    , violaPerfRepetition, violaFastRepetition
    , violaGraceNotes, violaGlissandi
    ]
violaShortLongNotes =
    [ staccato, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.vib.espr
    , sus.vib.progr, sus.vibDown, sus.nv
    ]
violaDynamics = seconds
    [ (dyn.med, [1.5, 3]), (dyn.str, [1.5, 3, 4]), (dyn.med.nv, [1.5, 3])
    , (pfp.vib, [2, 4])
    ] ++ [fp.vib, sfz.vib, sffz.vib]
violaPizzLegno = [pizz, pizz.snap, legno]
violaHarmonics = map (harm.)
    [ art.staccato, art.sus, art.perf.rep
    , nat.sus, nat.perf.rep, nat.gliss
    ]
violaPerfInterval = map (perf.)
    [ legato, legato.sul, legato.progr, legato.zigane
    , porta, marcato, spiccato
    ]
violaPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato, staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn9.portato, dyn9.staccato
    , dyn9.spiccato, dyn9.harsh
    ]
violaFastRepetition =
    fastRepBpm [(mempty, [15..19]), (dyn, [15..19])]
    ++ map (ricochet.) (map bpm [150, 160, 170, 180, 190, 210])
    ++ [ricochet.notes 3, ricochet.acc, ricochet.rit]
violaGraceNotes = map (grace.updown.)
    [ half, whole, zigane.half, zigane.whole
    , zigane.min.third, zigane.maj.third
    ]
violaGlissandi = map (gliss.)
    [perf.attr "c", perf.attr "g", perf.attr "d", perf.attr "a", oct.updown]

cello =
    [ celloShortLongNotes, celloDynamics, celloTremoloTrills
    , celloPizzLegno, celloHarmonics, celloPonticello, celloTasto
    , celloPerfInterval, celloPerfIntervalFast, celloTrill
    , celloPerfRepetition, celloFastRepetition, celloGraceNotes
    , celloGlissandi
    ]
celloShortLongNotes =
    [ staccato, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.vib.espr, sus.vib.progr, sus.vibDown, sus.nv
    ]
celloDynamics = seconds
    [ (dyn.med.vib, [1.5, 3]), (dyn.str.vib, [1.5, 3, 4])
    , (dyn.med.nv, [1.5, 3])
    , (pfp.vib, [2, 4])
    ] ++ [fp.vib, sfz.vib, sffz.vib]
celloTremoloTrills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 1.5, trem.dyn.sec 3
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.acc.half, trill.acc.whole, trill.acc.half.dyn, trill.acc.whole.dyn
    ]
celloPizzLegno = [pizz, pizz.secco, pizz.snap, pizz.legno]
celloHarmonics = map (harm.)
    [art.staccato, art.sus, art.rep, nat.sus, nat.rep, nat.gliss]
celloPonticello = map (pont.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto, sfz, sffz
    , trem, trem.fa, trem.fa.auto, rep.legato, rep.dyn.legato
    ]
celloTasto = map (tasto.)
    [ staccato, detache, sus, sus.fa, sus.fa.auto, sfz
    , trem, trem.fa, trem.fa.auto
    ]
celloPerfInterval = map (perf.)
    [ legato, legato.sul, legato.progr, legato.zigane, porta
    , marcato, spiccato
    ]
celloPerfIntervalFast = map (perf.fast.) [legato, marcato, spiccato, harsh]
celloTrill = [perf.trill.legato]
celloPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, bow, portato, staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.bow
    , dyn9.portato, dyn9.staccato, dyn9.spiccato, dyn9.harsh
    ]
celloFastRepetition =
    fastRepBpm [(mempty, [14..18]), (dyn, [14..18])]
    ++ map (ricochet.) (map bpm [150, 160, 170, 180, 190, 210])
    ++ [ricochet.notes 3, ricochet.acc, ricochet.rit]
celloGraceNotes = map (grace.updown.)
    [half, whole, zigane.half, zigane.whole, zigane.min.third, zigane.maj.third]
celloGlissandi = map (gliss.)
    [ perf.attr "c", perf.attr "g", perf.attr "d", perf.attr "a"
    , updown.oct.med, updown.oct.fast
    ]

-- *** solo bass

bass =
    [ bassShortLongNotes, bassDynamics
    , bassTremoloTrills, bassPizzLegno
    , bassHarmonics, violinPonticello
    , violinTasto, bassPerfInterval
    , violinPerfIntervalFast, violinPerfTrill
    , bassPerfRepetition, bassGlissandi
    ]

bassShortLongNotes =
    [ staccato, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.vib.marcato, sus.progr, sus.vibDown, sus.nv
    ]
bassDynamics =
    [ dyn.str.vib.sec 1.5, dyn.str.vib.sec 3, dyn.str.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.str.nv.sec 3
    , pfp.vib.sec 2, pfp.vib.sec 4, fp.vib, sfz.vib, sffz.vib
    ]
bassTremoloTrills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
bassPizzLegno = [pizz, pizz.snap, legno]
bassHarmonics = map (harm.)
    [ art.staccato, art.sus, art.rep, nat.sus, nat.rep, nat.gliss]
bassPerfInterval = map (perf.) [legato, porta, marcato, spiccato]
bassPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato, staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato, dyn9.staccato, dyn9.spiccato, dyn9.harsh
    ]
bassGlissandi = [gliss, oct.gliss.updown.med, oct.gliss.updown.fast]

-- ** string sections

strings :: [Instrument]
strings =
    [ ("strings", Keys
            { keyXAxis = Key.d_1
            , keyYAxis = Key.d8
            , keyAb = Key.d9
            , keyMatrix = Key.e9
            },
        [stringsOrchestra])
    , ("violins", lowKeys, violins)
    , ("violas", lowKeys, violas)
    , ("cellos", lowKeys, cellos)
    , ("basses", highKeys, basses)
    ]

stringsOrchestra =
    [ staccato.long, detache.short, detache.long
    , sus.vib, fp, sfz, trem, pizz
    ]

violins = stringSection "gdae"
violas = stringSection "cgda"
cellos = stringSection "cgda"

stringSection strings =
    [ stringsShortLongNotes, stringsDynamics
    , stringsTremoloTrills, stringsPizzLegno
    , stringsHarmonics, stringsPonticello
    , stringsConSordinoBasic, stringsPerfInterval
    , stringsPerfIntervalFast, stringsPerfTrill
    , stringsPerfRepetition, stringsPerfUpbeatRepetition
    , stringsFastRepetition, stringsGraceNotes
    , stringsGlissandi strings, stringsScaleRuns
    ]

stringsShortLongNotes =
    [ staccato.short, staccato.long, detache.short, detache.long
    , sus.vib, sus.vib.fa, sus.vib.fa.auto
    , sus.nv, sus.nv.fa, sus.nv.fa.auto
    , sus.flaut, sus.flaut.fa, sus.flaut.fa.auto
    ]
stringsDynamics = map (dyn.)
    [ med.vib.sec 1.5, med.vib.sec 3
    , str.vib.sec 1.5, str.vib.sec 3, str.vib.sec 6
    , med.nv.sec 2, dyn.med.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 6
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
stringsTremoloTrills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 2, trem.pfp.sec 3
    , trill.half, trill.whole, trill.third.min, trill.third.maj
    , trill.half.dyn, trill.whole.dyn, trill.half.pfp, trill.whole.pfp
    , trill.acc.half, trill.acc.whole
    , trill.acc.half.dyn, trill.acc.whole.dyn
    ]
stringsPizzLegno =
    [ pizz, pizz.slow, pizz.snap, pizz.rep.slow, pizz.rep.fast
    , legno, legno.slow
    ]
stringsHarmonics =
    [ harm.art.staccato, harm.art.sus
    , harm.art.sus.fa, harm.art.sus.fa.auto, harm.art.rep
    ]
stringsPonticello = map (pont.)
    [ staccato, sus, sus.fa, sus.fa.auto
    , dyn.str.sec 1.5, dyn.str.sec 2, dyn.str.sec 4
    , sfz, trem, trem.fa, trem.fa.auto
    ]
stringsConSordinoBasic = map (mute.)
    [ staccato, detache, sus.vib, sus.vib.fa, sus.vib.fa.auto
    , dyn.med.sec 2, dyn.med.sec 4
    , fp, sfz, trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn, pizz
    ]
stringsPerfInterval = map (perf.)
    [ legato, legato.sus.v4 -- 4 velocity layers
    , legato.sul, porta, trem
    , mute.legato, mute.porta
    ]
stringsPerfIntervalFast = map (perf.fast.) [legato, marcato, spiccato]
stringsPerfTrill = [perf.trill.legato]
stringsPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, bow.slow, bow.fast
    , portato.slow, portato.fast
    , staccato, spiccato, harsh
    , mute.harsh, mute.portato, mute.staccato
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn5.bow.slow, dyn5.bow.fast
    , dyn9.portato.fast, dyn9.staccato, dyn9.spiccato
    , dyn9.harsh, dyn9.portato, dyn9.staccato
    ]
stringsPerfUpbeatRepetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow
    , dyn4.n1.fast, dyn4.n2.fast
    ]
stringsFastRepetition = fastRepBpm [(mempty, [15..19]), (dyn, [15..19])]
stringsGraceNotes = [grace.updown.half, grace.updown.whole]
stringsGlissandi strings =
    [perf.gliss.attr (Text.singleton s) | s <- strings] ++ [gliss.updown.oct]
stringsScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole, spiccato.maj]

-- *** basses

basses =
    [ stringsShortLongNotes
    , bassesDynamics
    , bassesTremoloTrills
    , stringsPizzLegno
    , stringsHarmonics
    , bassesPonticello
    , bassesPerfInterval
    , stringsPerfIntervalFast
    , stringsPerfTrill
    , bassesPerfRepetition
    , bassesScaleRuns
    ]

bassesDynamics = map (dyn.)
    [ med.sec 2, med.sec 3, str.sec 2, str.sec 3, str.sec 5
    , pfp.sec 2, pfp.sec 4, pfp.sec 6
    , fp, sfz, sffz
    ]
bassesTremoloTrills =
    [ trem.sus, trem.sus.fa, trem.sus.fa.auto
    , trem.dyn.sec 2, trem.dyn.sec 4
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
bassesPonticello = map (pont.)
    [ staccato, sus, sus.fa, sus.fa.auto
    , dyn.str.sec 4
    , sfz, trem, trem.fa, trem.fa.auto
    ]
bassesPerfInterval = map (perf.) [legato, porta]
bassesPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato, spiccato, harsh
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow, dyn5.portato.fast
    , dyn9.portato.fast, dyn9.staccato, dyn9.spiccato, dyn9.harsh
    ]
bassesScaleRuns = runScales
    [ legato.maj, legato.min, legato.chrom
    , detache.whole.slow, detache.whole.fast
    ]

-- ** harps

harps :: [Instrument]
harps =
    [ ("harp1", lowKeys, harp1)
    , ("harp2", lowKeys, harp2)
    ]
harp1 =
    [ harp1SingleNotes, harp1Glissandi
    , harp1ArpeggiosA3, harp1ArpeggiosA4
    ]

harp1SingleNotes = [norm, mute, table, harm, bisbig, norm.rs.highlow]
harp1Glissandi = map (gliss.) $ concat
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
harp1ArpeggiosA3 = map (notes 3 .) harp1Arpeggios
harp1ArpeggiosA4 = map (notes 4 .) harp1Arpeggios
harp1Arpeggios = map (arpup.) $ concat
    [ [mode.speed.pitch | mode <- [maj, min], speed <- speeds, pitch <- scale]
    , [mode.speed | mode <- [dim, aug], speed <- speeds]
    ]
    where speeds = [slow, fast, straight]

harp2 = [harp2BasicArticulations]

harp2BasicArticulations =
    [ norm, mute, damp, table, nail, harm, pedal.gliss
    , bisbig, norm.rs, norm.rs.pedal.gliss
    ]


-- * woodwinds

-- ** woodwinds 1

woodwinds1 :: [Instrument]
woodwinds1 =
    [ ("flute1", lowKeys, flute1)
    , ("oboe2", lowKeys, oboe2)
    , ("clarinet-bb", lowKeys, clarinetBb)
    , ("bassoon", lowKeys, bassoon)
    , ("flutes", lowKeys, flutes)
    , ("oboes", lowKeys, woodwinds)
    , ("clarinets", lowKeys, woodwinds)
    , ("bassoons", lowKeys, woodwinds)
    ]

-- *** flute1

flute1 =
    [ flute1ShortLongNotes, flute1Dynamics, flute1FlutterTrills
    , flute1PerfInterval, flute1PerfIntervalFast
    , flute1PerfTrill, flute1PerfRepetition
    , flute1PerfUpbeatRepetition, flute1FastRepetition
    , flute1GraceNotes, flute1ScaleRuns, flute1Arpeggios, flute1Mordents
    ]

flute1ShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long, portato.long.vib, portato.long.nv
    , staccato.fx
    , sus.vib, sus.progr, sus.nv
    ]
flute1Dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.str.vib.sec 5
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 3, pfp.vib.sec 6, pfp.vib.sec 9
    , fpf.vib.sec 6, fpf.vib.sec 9
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
flute1FlutterTrills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.min.third, trill.maj.third
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn, trill.maj.third.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
flute1PerfInterval = map (perf.) [legato, legato.grace, perf.marcato]
flute1PerfIntervalFast = map (perf.fast.) [legato, marcato]
flute1PerfTrill = [perf.trill]
flute1PerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast, staccato.triple.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.slow, dyn9.portato.fast, dyn9.staccato
    ]
flute1PerfUpbeatRepetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
flute1FastRepetition = fastRepBpm [(mempty, [17..21]), (triple, [14..18])]
flute1GraceNotes = graceIntervals
flute1ScaleRuns = runScales
    [ legato.maj, legato.maj.fast, legato.min, legato.min.fast
    , legato.chrom, legato.whole, legato.chrom.fast, legato.whole.fast
    ]
flute1Arpeggios = arpScales
    [ articulation.mode.speed
    | articulation <- [staccato, legato], mode <- [dim, maj, min]
    , speed <- [mempty, fast]
    ]
flute1Mordents = map (mord.) $ concat
    [ map (legato.) variants
    , map (staccato.) variants
    ] where variants = [v1, v2, v3, v4, v5, v6]

-- *** oboe2

oboe2 =
    [ oboe2ShortLongNotes, oboe2Dynamics, oboe2FlutterTrills
    , oboe2PerfInterval, oboe2PerfIntervalFast
    , oboe2PerfTrill, oboe2PerfRepetition
    , oboe2GraceNotes, oboe2ScaleRuns
    ]
oboe2ShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.nv.ha, portato.long.nv.sa
    , sus.vib, sus.progr, sus.nv
    ]
oboe2Dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.med.vib.sec 4
    , dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 8
    , pfp.nv.sec 3, pfp.nv.sec 5, pfp.nv.sec 8
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
oboe2FlutterTrills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
oboe2PerfInterval = map (perf.) [legato, legato.grace, marcato]
oboe2PerfIntervalFast = map (perf.fast.) [legato, marcato]
oboe2PerfTrill = [perf.trill]
oboe2PerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato.slow, dyn9.staccato.fast
    ]
oboe2GraceNotes = graceIntervals
oboe2ScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]

-- *** clarinet bb

clarinetBb =
    [ clarinetBbShortLongNotes, clarinetBbDynamics
    , clarinetBbFlutterTrills, clarinetBbPerfInterval
    , clarinetBbPerfIntervalFast, clarinetBbPerfTrill
    , clarinetBbPerfRepitition, clarinetBbPerfUpbeatRepetition
    , clarinetBbFastRepetition, clarinetBbGraceNotes
    , clarinetBbGlissandi, clarinetBbScaleRuns, clarinetBbArpeggios
    ]
clarinetBbShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.na, portato.long.ha, portato.long.sa
    , sus.nv
    ]
clarinetBbDynamics = map (dyn.)
    [ li.sec 1.5, li.sec 2, li.sec 3, li.sec 4
    , med.sec 2, med.sec 3, med.sec 4, med.sec 6
    , str.sec 2, str.sec 3, str.sec 4, str.sec 6
    ] ++
    [ pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6, pfp.sec 8, pfp.sec 10
    , fpf.sec 4, fpf.sec 6
    , fp, sfz, sffz
    ]
clarinetBbFlutterTrills =
    [ flutter, flutter.cresc, trill.half, trill.whole
    , trill.min.third, trill.maj.third
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn
    , trill.maj.third.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
clarinetBbPerfInterval = [perf.legato, perf.legato.grace, perf.marcato]
clarinetBbPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
clarinetBbPerfTrill = [perf.trill]
clarinetBbPerfRepitition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.med, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato.slow, dyn9.staccato.fast
    ]
clarinetBbPerfUpbeatRepetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
clarinetBbFastRepetition =
    fastRepBpm [(mempty, [14..17]), (dyn, [14..17])]
clarinetBbGraceNotes = graceIntervals
clarinetBbGlissandi = map (gliss.) $ concat
    [ [ perf.slow, perf.fast ]
    , map (slow.updown.) intervalsToOct
    , map (fast.updown.) intervalsToOct
    ]
clarinetBbScaleRuns = runScales
    [legato.maj, legato.maj.fast, legato.min, legato.chrom, legato.whole]
clarinetBbArpeggios = arpScales
    [ articulation.mode.speed
    | articulation <- [staccato, legato], mode <- [dim, maj, min]
    , speed <- [mempty, fast]
    ]

-- *** bassoon

bassoon =
    [ bassoonShortLongNotes, bassoonDynamics
    , bassoonFlutterTrills, bassoonPerfInterval
    , bassoonPerfIntervalFast, bassoonPerfTrill
    , bassoonPerfRepetition, bassoonPerfUpbeatRepetition
    , bassoonFastRepetition, bassoonGraceNotes
    , bassoonScaleRuns
    ]
bassoonShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.vib.str, portato.long.nv
    , portato.long.nv.marcato
    , sus.vib, sus.progr, sus.nv
    ]
bassoonDynamics = map (dyn.)
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
bassoonFlutterTrills =
    [flutter, trill.half, trill.whole, trill.half.dyn, trill.whole.dyn]
bassoonPerfInterval = [perf.legato, perf.legato.grace.updown, perf.marcato]
bassoonPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
bassoonPerfTrill = [perf.trill]
bassoonPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast, staccato
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato
    ]
bassoonPerfUpbeatRepetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
bassoonFastRepetition = fastRepBpm [(mempty, [14..18]), (dyn, [14..18])]
bassoonGraceNotes = graceIntervals
bassoonScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]

-- *** flutes

flutes =
    [ flutesShortLongNotes, flutesDynamics
    , flutesClusterTrills, flutesPerfInterval
    , flutesPerfIntervalFast, flutesPerfTrill
    , flutesRepetition, flutesScaleRuns
    ]
flutesShortLongNotes = [staccato, portato.short, portato.med, sus.vib]
flutesDynamics =
    [ dyn.str.vib.sec 2, dyn.str.vib.sec 3, dyn.str.vib.sec 5
    , fp.vib, sfz.vib, sffz.vib
    ]
flutesClusterTrills =
    [ cluster, cluster.sfz
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
flutesPerfInterval = [perf.legato, perf.marcato]
flutesPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
flutesPerfTrill = [perf.trill]
flutesRepetition = map (rep.)
    [ legato, portato, staccato
    , dyn5.legato, dyn9.portato, dyn9.staccato
    ]
flutesScaleRuns = runScales [legato.chrom, legato.whole]

-- *** woodwinds

-- | Flutes, oboes, and bassoons all have the same articulations.
woodwinds =
    [ woodwindsShortLongNotes, woodwindsDynamics
    , woodwindsClusterTrills, woodwindsPerfInterval
    , woodwindsPerfIntervalFast, woodwindsTrill, woodwindsPerfRepetition
    ]
woodwindsShortLongNotes = [staccato, portato.short, portato.med, sus]
woodwindsDynamics =
    [dyn.str.sec 2, dyn.str.sec 3, dyn.str.sec 5, fp, sfz, sffz]
woodwindsClusterTrills =
    [ cluster, cluster.sfz
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
woodwindsPerfInterval = [perf.legato, perf.marcato]
woodwindsPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
woodwindsTrill = [perf.trill]
woodwindsPerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]

-- ** woodwinds 2

woodwinds2 :: [Instrument]
woodwinds2 =
    [ ("piccolo", lowKeys, piccolo)
    , ("flute2", lowKeys, flute2)
    , ("alto-flute", lowKeys, altoFlute)
    , ("oboe1", lowKeys, oboe1)
    , ("english-horn1", lowKeys, englishHorn1)
    , ("english-horn2", lowKeys, englishHorn2)
    , ("clarinet-eb", lowKeys, clarinetEb)
    , ("bass-clarinet", lowKeys, bassClarinet)
    , ("contra-bassoon", highKeys, contraBassoon)
    ]

-- *** piccolo

piccolo =
    [ piccoloShortLongNotes, piccoloDynamics
    , piccoloFlutterTrills, piccoloPerfInterval
    , piccoloPerfIntervalFast, piccoloPerfTrill
    , piccoloPerfRepetition, piccoloFastRepetition
    , piccoloGraceNotes, piccoloScaleRuns
    , piccoloArpeggios, piccoloMordent
    ]
piccoloShortLongNotes =
    [ staccato, portato.short, portato.med.vib, portato.med.nv
    , portato.long.vib
    , sus.vib.v1, sus.vib.v2, sus.progr
    ]
piccoloDynamics =
    [ dyn.str.vib.sec 2, dyn.str.vib.sec 3, dyn.str.vib.sec 5
    , dyn.str.nv.sec 1.5, dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 6, fpf.vib.sec 5, fp.vib, sfz.vib.v1, sfz.vib.v2, sfz.vib.v3
    ]
piccoloFlutterTrills =
    [ flutter
    , trill.half, trill.whole, trill.min.third, trill.maj.third , trill.fourth
    ]
piccoloPerfInterval = map (perf.) [legato, legato.lyric, marcato]
piccoloPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
piccoloPerfTrill = [perf.trill]
piccoloPerfRepetition = map (rep.)
    [ legato.slow, legato.med, legato.fast
    , portato.slow, portato.med, portato.fast
    , staccato
    ]
piccoloFastRepetition = fastRepBpm [(mempty, [15..20] ++ [22])]
piccoloGraceNotes = graceIntervals
piccoloScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]
piccoloArpeggios = arpScales
    [ articulation.mode.speed
    | articulation <- [staccato, legato], mode <- [dim, maj, min]
    , speed <- [mempty, fast]
    ]
piccoloMordent = map (mord.) [v1, v2, v3, v4, v5, v6]

-- *** flute2

flute2 =
    [ flute2ShortLongNotes, flute2Dynamics
    , flute2FlutterTrills, flute2PerfInterval
    , flute2PerfIntervalFast, flute2PerfTrill
    , flute2PerfRepetition, flute2FastRepetition
    , flute2GraceNotes, flute2ScaleRuns, flute2Arpeggios
    ]
flute2ShortLongNotes =
    [ staccato, portato.short, portato.med, portato.long.vib, portato.long.nv
    , sus.progr, sus.nv
    ]
flute2Dynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 8
    , fp.vib, sfz.vib, fp.nv, sfz.nv
    ]
flute2FlutterTrills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.min.third, trill.maj.third, trill.fourth
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn
    ]
flute2PerfInterval = map (perf.) [legato, legato.grace, marcato]
flute2PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
flute2PerfTrill = [perf.trill]
flute2PerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato, staccato
    , dyn5.legato.slow, dyn5.legato.fast
    ]
flute2FastRepetition = fastRepBpm [(mempty, [16..21]), (triple, [13..16])]
flute2GraceNotes = graceIntervals
flute2ScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]
flute2Arpeggios = arpScales $ concat
    [ [legato.mode.fast | mode <- [dim, maj, min]]
    , [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
    ]

-- *** alto flute

altoFlute =
    [ altoFluteShortLongNotes, altoFluteDynamics
    , altoFluteFlutter, altoFlutePerfInterval
    , altoFlutePerfIntervalFast, altoFlutePerfTrill
    , altoFlutePerfRepetition, altoFluteGraceNotes
    ]
altoFluteShortLongNotes =
    [ staccato, portato.short
    , portato.med.vib, portato.med.nv, portato.long.vib
    , sus.vib, sus.progr, sus.nv
    ]
altoFluteDynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.str.vib.sec 5
    , pfp.vib.sec 3, pfp.vib.sec 5, pfp.vib.sec 8
    , fp.vib, sfz.vib, sffz.vib
    ]
altoFluteFlutter =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
altoFlutePerfInterval = [perf.legato, perf.legato.grace, perf.marcato]
altoFlutePerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
altoFlutePerfTrill = [perf.trill]
altoFlutePerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
altoFluteGraceNotes = graceIntervals

-- *** oboe1

oboe1 =
    [ oboe1ShortLongNotes, oboe1Dynamics
    , oboe1FlutterTrills, oboe1PerfInterval
    , oboe1PerfIntervalFast, oboe1PerfTrill
    , oboe1PerfRepetition, oboe1PerfUpbeatRepetition
    , oboe1GraceNotes, oboe1ScaleRuns
    , oboe1Arpeggios, oboe1Mordents
    ]
oboe1ShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.na.nv, portato.long.sa.nv
    , sus.progr, sus.nv
    ]
oboe1Dynamics =
    [ dyn.str.vib.sec 3, dyn.str.vib.sec 5
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.med.nv.sec 6
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 6, pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6
    , pfp.nv.sec 8
    , fpf.nv.sec 4, fpf.nv.sec 6
    , fp.nv, sfz.nv, sffz.nv
    ]
oboe1FlutterTrills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.min.third, trill.maj.third
    , trill.half.dyn, trill.whole.dyn, trill.min.third.dyn, trill.maj.third.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
oboe1PerfInterval = map (perf.)
    [legato.nv, legato.progr, legato.grace, marcato]
oboe1PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
oboe1PerfTrill = [perf.trill]
oboe1PerfRepetition = map (rep.)
    [ legato.slow, legato.med, legato.fast
    , portato.slow, portato.med, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.med, dyn9.legato.fast
    , dyn5.portato.slow, dyn9.portato.med, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
oboe1PerfUpbeatRepetition = map (perf.upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
oboe1GraceNotes = graceIntervals
oboe1ScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]
oboe1Arpeggios = arpScales
    [legato.mode.speed | mode <- [dim, maj], speed <- [mempty, fast]]
oboe1Mordents = map (mord.) [v1, v2, v3, v4, v5, v6]

-- *** english horn 1

englishHorn1 =
    [ englishHorn1ShortLongNotes, englishHorn1Dynamics
    , englishHorn1FlutterTrills, englishHorn1PerfInterval
    , englishHorn1PerfIntervalFast, englishHorn1PerfTrill
    , englishHorn1PerfRepetition, englishHorn1GraceNotes
    , englishHorn1ScaleRuns
    ]
englishHorn1ShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.ha.nv, portato.long.sa.nv
    , sus.vib, sus.nv
    ]
englishHorn1Dynamics =
    [ dyn.med.vib.sec 3, dyn.str.vib.sec 5
    , dyn.med.nv.sec 1, dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3
    , dyn.med.nv.sec 4, dyn.med.nv.sec 6
    , dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.vib.sec 5, pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6
    , pfp.nv.sec 8, pfp.nv.sec 10
    , fpf.nv.sec 4, fpf.nv.sec 6, fp, sfz, sffz
    ]
englishHorn1FlutterTrills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
englishHorn1PerfInterval = map (perf.)
    [legato.nv, legato.vib, grace, marcato]
englishHorn1PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
englishHorn1PerfTrill = [perf.trill]
englishHorn1PerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
englishHorn1GraceNotes = graceIntervals
englishHorn1ScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]

-- *** english horn 2

englishHorn2 =
    [ englishHorn2ShortLongNotes, englishHorn2Dynmics
    , englishHorn2FlutterTrills, englishHorn2PerfInterval
    , englishHorn2PerfIntervalFast, englishHorn2PerfTrill
    , englishHorn2PerfRepetition, englishHorn2GraceNotes
    ]
englishHorn2ShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.na.vib, portato.long.na.nv, portato.long.pa.vib
    , sus.vib, sus.progr
    ]
englishHorn2Dynmics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.med.vib.sec 4
    , dyn.str.vib.sec 4, dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 8
    , fp, sfz, sffz
    ]
englishHorn2FlutterTrills =
    [ flutter, flutter.dyn
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
englishHorn2PerfInterval = [perf.legato, perf.legato.grace, perf.marcato]
englishHorn2PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
englishHorn2PerfTrill = [perf.trill]
englishHorn2PerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn9.legato.fast, dyn5.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
englishHorn2GraceNotes = graceIntervals

-- *** clarinet eb

clarinetEb =
    [ clarinetEbShortLongNotes, clarinetEbDynamics
    , clarinetEbFlutterTrills, clarinetEbPerfInterval
    , clarinetEbPerfIntervalFast, clarinetEbPerfTrill
    , clarinetEbPerfRepetition, clarinetEbFastRepetition
    , clarinetEbGraceNotes
    ]
clarinetEbShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.na, portato.long.ha, portato.long.sa, sus.nv
    ]
clarinetEbDynamics =
    [ dyn.li.sec 1.5, dyn.li.sec 2, dyn.med.sec 1.5
    , dyn.med.sec 2, dyn.med.sec 3, dyn.med.sec 4
    , dyn.str.sec 2, dyn.str.sec 3, dyn.str.sec 4, dyn.str.sec 6
    , pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6, pfp.sec 8
    , fp, sfz, sffz
    ]
clarinetEbFlutterTrills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    , trill.half.acc, trill.whole.acc, trill.half.acc.dyn, trill.whole.acc.dyn
    ]
clarinetEbPerfInterval = [perf.legato, perf.legato.grace, perf.marcato]
clarinetEbPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
clarinetEbPerfTrill = [perf.trill]
clarinetEbPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn5.portato.slow
    , dyn9.portato.fast, dyn9.staccato.slow, dyn9.staccato.fast
    ]
clarinetEbFastRepetition =
    fastRepBpm [(mempty, [14..18]), (dyn, [14..18])]
clarinetEbGraceNotes = graceIntervals

-- *** bass clarinet

bassClarinet =
    [ bassClarinetShortLongNotes, bassClarinetDynamics
    , bassClarinetFlutterTrill, bassClarinetPerfInterval
    , bassClarinetPerfIntervalFast, bassClarinetPerfTrill
    , bassClarinetPerfRepetition, bassClarinetScaleRuns
    ]
bassClarinetShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.nv, portato.sus.nv
    ]
bassClarinetDynamics =
    [ dyn.med.sec 1.5, dyn.med.sec 2, dyn.med.sec 3, dyn.med.sec 4
    , dyn.str.sec 3, dyn.str.sec 4, dyn.str.sec 6
    , pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6, pfp.sec 8, pfp.sec 10
    , fp, sfz, sffz
    ]
bassClarinetFlutterTrill =
    [flutter, trill.half, trill.whole, trill.half.dyn, trill.whole.dyn]
bassClarinetPerfInterval = [perf.legato, perf.marcato]
bassClarinetPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
bassClarinetPerfTrill = [perf.trill]
bassClarinetPerfRepetition = map (rep.)
    [ legato, portato, staccato
    , dyn5.legato, dyn9.portato, dyn9.staccato
    ]
bassClarinetScaleRuns = runScales
    [legato.maj, legato.min, legato.chrom, legato.whole]


-- *** contra bassoon

contraBassoon =
    [ contraBassoonShortLongNotes, contraBassoonDynamics
    , contraBassoonFlutter, contraBassoonPerfInterval
    , contraBassoonPerfIntervalFast, contraBassoonPerfTrill
    , contraBassoonPerfRepetition, contraBassoonGraceNotes
    ]
contraBassoonShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.vib, portato.long.nv
    , sus.vib, sus.nv, fx.crescdim
    ]
contraBassoonDynamics =
    [ dyn.med.vib.sec 2, dyn.med.vib.sec 3, dyn.med.vib.sec 5
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.med.nv.sec 6
    , dyn.str.nv.sec 2, dyn.str.nv.sec 4, dyn.str.nv.sec 6
    , pfp.sec 1.5, pfp.sec 2, pfp.sec 3, pfp.sec 4, pfp.sec 6
    , fp, sfz, sffz
    ]
contraBassoonFlutter = [flutter, flutter.cresc]
contraBassoonPerfInterval = [perf.legato, perf.legato.grace, perf.marcato]
contraBassoonPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
contraBassoonPerfTrill = [perf.trill]
contraBassoonPerfRepetition = map (rep.)
    [ legato.slow, legato.fast, portato.slow, portato.med, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.med, dyn9.portato.fast, dyn9.staccato.slow
    , dyn9.staccato.fast
    ]
contraBassoonGraceNotes = graceIntervals

-- ** special woodwinds

specialWoodwinds :: [Instrument]
specialWoodwinds =
    [ ("bass-flute", highKeys, bassFlute)
    , ("oboe-damore", lowKeys, oboeDamore)
    , ("heckelphone", lowKeys, heckelphone)
    , ("contrabass-clarinet", highKeys, contrabassClarinet)
    , ("basset-horn", highKeys, bassetHorn)
    ]

bassFlute =
    [ bassFluteShortLongNotes, bassFluteDynamics
    , bassFluteFlutter, bassFlutePerfInterval
    , bassFlutePerfIntervalFast, bassFlutePerfTrill
    , bassFlutePerfRepetition, bassFluteFastRepetition
    ]
bassFluteShortLongNotes =
    [staccato, portato.short, portato.med, sus.vib, sus.nv]
bassFluteDynamics =
    [ dyn.med.vib.sec 1, dyn.med.vib.sec 2, dyn.med.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 4
    , dyn.str.nv.sec 1.5, dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 3, pfp.vib.sec 6, pfp.vib.sec 8
    , pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4, pfp.nv.sec 6
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
bassFluteFlutter = [flutter, flutter.cresc]
bassFlutePerfInterval = [perf.legato, perf.marcato]
bassFlutePerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
bassFlutePerfTrill = [perf.trill]
bassFlutePerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
bassFluteFastRepetition = fastRepBpm [(mempty, [12..18]), (dyn, [12..18])]

oboeDamore =
    [ oboeDamoreShortLongNotes, oboeDamoreDynamics
    , oboeDamoreFlutter, oboeDamorePerfInterval
    , oboeDamorePerfIntervalFast, oboeDamorePerfTrill
    , oboeDamorePerfRepetition, oboeDamoreFastRepetition
    ]
oboeDamoreShortLongNotes =
    [ staccato, portato.short, portato.med
    , portato.long.sa.vib, portato.long.sa.nv, sus.vib, sus.nv
    ]
oboeDamoreDynamics =
    [ dyn.med.vib.sec 1.5, dyn.med.vib.sec 2, dyn.med.vib.sec 3
    , dyn.med.vib.sec 4
    , dyn.str.vib.sec 3, dyn.str.vib.sec 4
    , dyn.med.nv.sec 1.5, dyn.med.nv.sec 2, dyn.med.nv.sec 3, dyn.med.nv.sec 4
    , dyn.str.nv.sec 2, dyn.str.nv.sec 3, dyn.str.nv.sec 4
    , pfp.vib.sec 2, pfp.vib.sec 4, pfp.vib.sec 10
    , pfp.nv.sec 2, pfp.nv.sec 3, pfp.nv.sec 4
    , fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv
    ]
oboeDamoreFlutter = [flutter, flutter.cresc]
oboeDamorePerfInterval = [perf.legato]
oboeDamorePerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
oboeDamorePerfTrill = [perf.trill]
oboeDamorePerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
oboeDamoreFastRepetition =
    fastRepBpm [(mempty, [12..18]), (dyn, [12..18])]

heckelphone =
    [ heckelphoneShortLongNotes, heckelphoneDynamics
    , heckelphoneFlutter, heckelphonePerfInterval
    , heckelphonePerfIntervalFast, heckelphoneTrill
    , heckelphonePerfRepetition
    ]
heckelphoneShortLongNotes =
    [staccato, portato.short, portato.med, sus.vib, sus.nv]
heckelphoneDynamics = seconds
    [ (dyn.med.vib, [1.5, 2, 3, 5])
    , (dyn.str.nv, [1.5, 2, 3, 4, 6])
    , (pfp.vib, [2, 3, 5, 8])
    ] ++ [fp.vib, sfz.vib, sffz.vib]
heckelphoneFlutter = [flutter]
heckelphonePerfInterval = [perf.legato, perf.marcato]
heckelphonePerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
heckelphoneTrill = [perf.trill]
heckelphonePerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn5.portato, dyn9.staccato]

contrabassClarinet =
    [ contrabassClarinetShortLongNotes, contrabassClarinetDynamics
    , contrabassClarinetPerfInterval, contrabassClarinetPerfIntervalFast
    , contrabassClarinetPerfTrill, contrabassClarinetPerfRepetition
    ]
contrabassClarinetShortLongNotes = [staccato, portato, sus]
contrabassClarinetDynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (pfp, [2, 3, 4, 6, 8]) ]
    ++ [fp, sfz, sffz]
contrabassClarinetPerfInterval = [perf.legato, perf.marcato]
contrabassClarinetPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
contrabassClarinetPerfTrill = [perf.trill]
contrabassClarinetPerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn5.portato, dyn9.staccato]

bassetHorn =
    [ bassetHornShortLongNotes, bassetHornDynamics
    , bassetHornFlutter, bassetHornPerfInterval
    , bassetHornPerfIntervalFast, bassetHornPerfTrill
    , bassetHornPerfRepetition, bassetHornFastRepetition
    ]
bassetHornShortLongNotes =
    [ staccato, portato.short, portato.med, portato.long.sa, portato.long.ha
    , sus
    ]
bassetHornDynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4])
    , (dyn.str, [1.5, 2, 3, 4, 6])
    , (pfp, [2, 3, 4, 6, 8])
    ] ++ [fp, sfz, sffz]
bassetHornFlutter = [flutter, flutter.cresc]
bassetHornPerfInterval = [perf.legato, perf.marcato]
bassetHornPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
bassetHornPerfTrill = [perf.trill]
bassetHornPerfRepetition = map (rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
bassetHornFastRepetition =
    fastRepBpm [(mempty, [12..18]), (dyn, [12..18])]

-- * brass

-- ** brass1

brass1 :: [Instrument]
brass1 =
    [ ("trumpet-c", lowKeys, trumpetC)
    , ("trumpet-c-mute", lowKeys, trumpetCMute)
    , ("horn-vienna", lowKeys, hornVienna)
    , ("tenor-trombone", lowKeys, tenorTrombone)
    , ("tenor-trombone-mute-a", lowKeys, tenorTromboneMuteA)
    , ("tenor-trombone-mute-b", lowKeys, tenorTromboneMuteB)
    , ("tuba", highKeys, tuba)
    , ("trumpets-a3", lowKeys, trumpetsA3)
    , ("trumpets-a3-mute", lowKeys, trumpetsA3Mute)
    , ("horns-a4", lowKeys, hornsA4)
    , ("horns-a4-stopped", lowKeys, hornsA4Stopped)
    , ("trombones-a3", lowKeys, trombonesA3)
    , ("trombones-a3-mute", lowKeys, trombonesA3Mute)
    ]

trumpetC =
    [ trumpetCShortLongNotes, trumpetCDynamics
    , trumpetCFlutterTrills, trumpetCPerfInterval
    , trumpetCPerfIntervalFast, trumpetCPerfTrill
    , trumpetCPerfRepetition, trumpetCFastRepetition
    , trumpetCUpbeatRepetition, trumpetCGraceNotes
    , trumpetCScaleRuns, trumpetCArpeggios, trumpetCMordents
    ]
trumpetCShortLongNotes =
    [ staccato, portato.short
    , portato.med, portato.med.marcato, portato.med.soft
    -- long-norm, long-marcato, long-soft
    , portato.long.norm.vib, portato.long.marcato.vib
    , portato.long.norm.vibStr, portato.long.marcato.vibStr
    , portato.long.norm.nv, portato.long.marcato.nv
    , portato.long.soft.nv
    , sus.vib, sus.vibStr, sus.progr, sus.nv
    ]
trumpetCDynamics = seconds
    [ (dyn.li.nv, [1.5, 2, 3, 4]), (dyn.med.vib, [2, 3, 4, 6])
    , (dyn.med.nv, [1.5, 2, 3, 4, 6]), (dyn.str.vib, [3, 6])
    , (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.vib, [4, 8]), (pfp.nv, [4, 6, 8])
    ] ++ [fp, sfz, sffz]
trumpetCFlutterTrills =
    [ flutter, flutter.cresc, trill.slow.half, trill.slow.whole
    , trill.fast.half, trill.fast.whole, trill.acc.half, trill.acc.whole
    ]
trumpetCPerfInterval = map (perf.)
    [ legato.nv, legato.nv.sus -- TODO what's this?
    , legato.vib, legato.vib.sus, marcato
    ]
trumpetCPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
trumpetCPerfTrill = [perf.trill]
trumpetCPerfRepetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato, staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpetCFastRepetition = fastRepBpm
    [(mempty, [14, 16, 17, 19, 21]), (dyn, [14, 15, 16, 17, 19, 21])]
trumpetCUpbeatRepetition = upbeatRepBpm
    [ (n1, [10, 11, 12, 13, 14, 16, 18])
    , (n2, [10, 11, 12, 13, 14, 16, 18, 20])
    , (n3, [10, 11, 12, 13, 14, 16, 18, 20])
    ]
trumpetCGraceNotes = [grace.updown.half, grace.updown.whole]
trumpetCScaleRuns = runScales [legato.maj]
trumpetCArpeggios = arpScales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
trumpetCMordents = map (mord.legato.) [v1, v2, v3, v4, v5, v6]

trumpetCMute =
    [ trumpetCMuteShortLongNotes, trumpetCMuteDynamics
    , trumpetCMuteFlutterTrills, trumpetCMutePerfInterval
    , trumpetCMutePerfIntervalFast, trumpetCMutePerfTrill
    , trumpetCMutePerfRepetition, trumpetCMuteFastRepetition
    , trumpetCMuteUpbeatRepetition
    ]
trumpetCMuteShortLongNotes =
    [ staccato, portato.short, portato.med, portato.long.vib
    , sus.vib, sus.progr, sus.nv
    ]
trumpetCMuteDynamics = seconds
    [ (dyn.med.vib, [2, 3, 4]), (dyn.med.nv, [2, 3, 4, 6])
    , (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.vib, [2, 5, 9]), (pfp.nv, [2, 3, 4, 5])
    ] ++ [fp.vib, sfz.vib, sffz.vib, fp.nv, sfz.nv, sffz.nv]
trumpetCMuteFlutterTrills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.dyn, trill.whole.dyn
    ]
trumpetCMutePerfInterval =
    [perf.legato.nv, perf.legato.vib, perf.marcato.nv]
trumpetCMutePerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
trumpetCMutePerfTrill = [perf.trill]
trumpetCMutePerfRepetition = map (perf.rep.)
    [ legato, portato.slow, portato.fast, staccato.slow, staccato.fast
    , dyn5.legato, dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpetCMuteFastRepetition = fastRepBpm
    [(mempty, [14, 15, 16, 17, 19]), (dyn, [14, 15, 16, 17, 19])]
trumpetCMuteUpbeatRepetition =
    upbeatRepBpm [(n1, bpms), (n2, bpms), (n3, bpms)]
    where bpms = [9, 10, 11, 12, 13, 14, 16, 18, 20]

hornVienna =
    [ hornViennaShortLongNotes, hornViennaDynamics
    , hornViennaFlutterTrills, hornViennaStopped
    , hornViennaPerfInterval, hornViennaPerfIntervalFast
    , hornViennaPerfTrill, hornViennaPerfRepetition
    , hornViennaFastRepetition, hornViennaUpbeatRepetition
    , hornViennaGraceNotes, hornViennaGlissandi
    ]
hornViennaShortLongNotes =
    [ staccato, portato.short, portato.short.soft, portato.med
    , portato.med.soft, portato.long.vib, portato.long.nv
    , portato.long.marcato, portato.long.blare
    , sus.vib, sus.nv, sus.blare
    ]
hornViennaDynamics = seconds
    [ (dyn.li.nv, [1, 1.5, 2, 3, 4, 6]), (dyn.med.vib, [4, 6])
    , (dyn.med.nv, [1, 1.5, 2, 3, 4, 6]), (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.nv.v1, [6]), (pfp.nv.v2, [6]), (pfp.nv, [8])
    ] ++ [fp, sfz, sffz]
hornViennaFlutterTrills =
    [flutter, flutter.cresc, trill.half, trill.whole, trill.lip]
hornViennaStopped = map (stop.)
    [ staccato, portato.med, portato.long, sus
    , dyn.sec 1, dyn.sec 1.5, dyn.sec 2, dyn.sec 3, dyn.sec 4, dyn.sec 6
    , fp, sfz, sffz
    ]
hornViennaPerfInterval = map (perf.)
    [legato.nv, legato.nv.sus, legato.vib.sus, marcato]
hornViennaPerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
hornViennaPerfTrill = [perf.trill]
hornViennaPerfRepetition = map (perf.rep.)
    [ legato.slow, legato.med, legato.fast, portato
    , staccato.slow, staccato.fast
    , dyn6.legato.slow, dyn6.legato.med, dyn6.legato.fast
    , dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
hornViennaFastRepetition =
    fastRepBpm [(mempty, [15..19]), (dyn, [15..19])]
hornViennaUpbeatRepetition = upbeatRepBpm
    [ (n1, [9, 10, 11, 12, 13, 14, 16, 18])
    , (n2, bpms), (n3, bpms)
    ]
    where bpms = [9, 10, 11, 12, 13, 14, 16, 18, 20]
hornViennaGraceNotes = [grace.updown.half, grace.updown.whole]
hornViennaGlissandi = map (gliss.)
    [ fast, slow.up, slow.fourth, slow.dim.fifth, slow.fifth, slow.min.sixth
    , slow.maj.sixth, slow.min.seventh, slow.maj.seventh, slow.oct
    , fast.up.fourth, fast.up.fifth, fast.up.min.sixth, fast.up.maj.sixth
    , fast.up.min.seventh, fast.up.maj.seventh, fast.up.oct
    ]

tenorTrombone =
    [ tenorTromboneShortLongNotes, tenorTromboneDynamics
    , tenorTromboneFlutter, tenorTrombonePerfInterval
    , tenorTrombonePerfIntervalFast, tenorTrombonePerfTrill
    , tenorTrombonePerfRepetition, tenorTromboneFastRepetition
    , tenorTromboneUpbeatRepetition, tenorTromboneGraceNotes
    , tenorTromboneGlissandi, tenorTromboneArpeggios
    ]
tenorTromboneShortLongNotes =
    [ staccato, portato.short, portato.short.soft, portato.med
    , portato.med.soft, portato.medium.marcato, portato.long.vib
    , portato.long.vib.soft, portato.long.nv, portato.long.marcato
    , sus.progr, sus.vib, sus.vibStr, sus.nv
    ]
tenorTromboneDynamics = seconds
    [ (dyn.li.vib, [1.5, 2, 3, 4]), (dyn.li.nv, [1, 1.5, 2, 3, 4, 6])
    , (dyn.med.vib, [2, 3, 4, 6]), (dyn.med.nv, [1.5, 2, 3, 4, 6])
    , (dyn.str.vib, [4]), (dyn.str.nv, [2, 3, 4, 6]), (pfp.nv, [6])
    ] ++ [fp, sfz, sffz]
tenorTromboneFlutter = [flutter, flutter.cresc]
tenorTrombonePerfInterval = map (perf.)
    [legato.nv, legato.nv.sus, legato.vib, legato.vib.sus, marcato]
tenorTrombonePerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
tenorTrombonePerfTrill = [perf.trill]
tenorTrombonePerfRepetition = map (perf.rep.)
    [ legato, portato, staccato.slow, staccato.fast
    , dyn6.legato.slow, dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
tenorTromboneFastRepetition =
    fastRepBpm [(mempty, [15..19]), (dyn, [15..19])]
tenorTromboneUpbeatRepetition = upbeatRepBpm
    [(n1, bpms), (n2, bpms), (n3, [8..14] ++ [16, 18])]
    where bpms = [8..14] ++ [16, 18, 20, 22]
tenorTromboneGraceNotes = map (grace.updown.) [half, whole]
tenorTromboneGlissandi = map (gliss.)
    [ fast, slow, fast.half, fast.whole, fast.min.third, fast.maj.third
    , fast.fourth, fast.dim.fifth
    , slow.min.third, slow.maj.third, slow.fourth, slow.dim.fifth
    ]
tenorTromboneArpeggios = arpScales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]

tenorTromboneMuteA =
    [ tenorTromboneMuteAShortLongNotes, tenorTromboneMuteADynamics
    , tenorTromboneMuteAFlutter, tenorTromboneMuteAPerfInterval
    , tenorTromboneMuteAPerfRepetition
    , tenorTromboneMuteAFastRepetition
    , tenorTromboneMuteAUpbeatRepetition
    ]
tenorTromboneMuteAShortLongNotes =
    [ staccato, portato.short, portato.med, portato.long.vib
    , sus.progr, sus.nv
    ]
tenorTromboneMuteADynamics = seconds
    [ (dyn.med.vib, [1.5, 2, 3, 4]), (dyn.med.nv, [1.5, 2, 3, 4, 6])
    , (pfp.nv, [2, 3, 4])
    ] ++ [fp, sfz, sffz]
tenorTromboneMuteAFlutter = [flutter]
tenorTromboneMuteAPerfInterval = [perf.legato, perf.marcato]
tenorTromboneMuteAPerfRepetition = map (perf.rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
tenorTromboneMuteAFastRepetition =
    fastRepBpm [(mempty, [14..19]), (dyn, [14..19])]
tenorTromboneMuteAUpbeatRepetition = upbeatRepBpm
    [(n1, [9..14]), (n2, [9..14] ++ [16, 18]), (n2, [9..14] ++ [16, 18])]

tenorTromboneMuteB =
    [ tenorTromboneMuteBShortLongNotes, tenorTromboneMuteBDynamics
    , tenorTromboneMuteBFlutter, tenorTromboneMuteBPerfInterval
    , tenorTromboneMuteBPerfRepetition
    , tenorTromboneMuteBFastRepetition
    , tenorTromboneMuteBUpbeatRepetition
    ]
tenorTromboneMuteBShortLongNotes =
    [staccato, portato.short, portato.med, sus.progr, sus.nv]
tenorTromboneMuteBDynamics = seconds
    [ (dyn.med.vib, [1.5, 2, 3, 4]), (dyn.med.nv, [1.5, 2, 3, 4])
    , (pfp.nv, [2, 3, 4])
    ] ++ [fp, sfz, sffz]
tenorTromboneMuteBFlutter = [flutter, flutter.cresc]
tenorTromboneMuteBPerfInterval = [perf.legato, perf.marcato]
tenorTromboneMuteBPerfRepetition = map (perf.rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
tenorTromboneMuteBFastRepetition =
    fastRepBpm [(mempty, [14..19]), (dyn, [14..19])]
tenorTromboneMuteBUpbeatRepetition =
    upbeatRepBpm [(n1, bpms), (n2, bpms), (n3, bpms)]
    where bpms = [9..14] ++ [16, 18]

tuba =
    [ tubaShortLongNotes, tubaDynamics, tubaFlutterTrills
    , tubaMuteBasic, tubaPerfInterval, tubaPerfIntervalFast
    , tubaPerfRepetition, tubaFastRepetition, tubaUpbeatRepetition
    , tubaGraceNotes
    ]
tubaShortLongNotes =
    [ staccato, portato.med, portato.med.soft, portato.med.hard
    , portato.long.vib, portato.long.vibStr
    , portato.long.nv.soft, portato.long.nv.hard
    , sus.vib.v1, sus.vib.v2, sus.nv
    ]
tubaDynamics = seconds
    [ (dyn.li.nv, [1, 1.5, 2, 3, 4]), (dyn.med.vib, [2, 3, 4, 6])
    , (dyn.med.nv, [2, 3, 4, 6]), (dyn.str.nv, [2, 3, 4, 6])
    , (pfp.nv, [4])
    ] ++ [fp, sfz, sffz]
tubaFlutterTrills =
    [ flutter, flutter.cresc
    , trill.half, trill.whole, trill.half.cresc, trill.whole.cresc
    ]
tubaMuteBasic = map (mute.)
    [ staccato, portato.med, portato.long, sus
    , dyn.sec 2, dyn.sec 3, dyn.sec 4
    , sfz, flutter, flutter.cresc
    ]
tubaPerfInterval = map (perf.legato.) [nv, nv.sus, vib.sus]
tubaPerfIntervalFast = [perf.legato.fast]
tubaPerfRepetition = map (perf.rep.)
    [ legato, portato.slow, portato.fast, staccato.slow, staccato.fast
    , dyn6.legato, cresc9.portato.slow, cresc9.portato.fast
    , cresc9.staccato.slow, cresc9.staccato.fast
    ]
tubaFastRepetition =
    fastRepBpm [(mempty, [14..19]), (cresc, [15..19])]
tubaUpbeatRepetition =
    upbeatRepBpm [(n1, [8..14]), (n2, [8..14]), (n3, [8..14] ++ [16])]
tubaGraceNotes = map (grace.updown.) [half, whole]

trumpetsA3 =
    [ trumpetsA3ShortLongNotes, trumpetsA3Dynamics
    , trumpetsA3FlutterTrills, trumpetsA3PerfInterval
    , trumpetsA3PerfIntervalFast, trumpetsA3PerfTrill
    , trumpetsA3PerfRepetition, trumpetsA3PerfUpbeatRepetition
    , trumpetsA3FastRepetition, trumpetsA3UpbeatRepetition
    , trumpetsA3ScaleRuns, trumpetsA3Arpeggios
    , trumpetsA3FallRelease
    ]
trumpetsA3ShortLongNotes =
    [ staccato, portato.short, portato.med, portato.long
    , sus.vib, sus.nv, sus.marcato, sus.tune, sus.rip, fall, fall.fa
    ]
trumpetsA3Dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (dyn.str, [2, 3, 4, 6])
    , (pfp, [2, 3, 4, 6])
    ] ++ [fp, sfz, sffz]
trumpetsA3FlutterTrills =
    [ flutter, flutter.cresc, trill.half, trill.whole
    , trill.half.dyn, trill.whole.dyn
    ]
trumpetsA3PerfInterval = map (perf.)
    [legato.nv, legato.nv.sus, legato.vib, legato.tune, marcato]
trumpetsA3PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
trumpetsA3PerfTrill = [perf.trill]
trumpetsA3PerfRepetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpetsA3PerfUpbeatRepetition = map (rep.upbeat.)
    [ n1.slow, n2.slow, n1.med, n2.med, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.med, dyn4.n2.med
    , dyn4.n1.fast, dyn4.n2.fast
    ]
trumpetsA3FastRepetition =
    fastRepBpm [(mempty, [15..19]), (dyn, [15..19])]
trumpetsA3UpbeatRepetition = upbeatRepBpm
    [(n1, [8..14]), (n2, [8..14] ++ [16, 18]), (n3, [8..14] ++ [16, 18])]
trumpetsA3ScaleRuns =
    runScales [legato.maj, legato.min, legato.chrom, legato.whole]
trumpetsA3Arpeggios = arpScales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
trumpetsA3FallRelease = map (fall.)
    [ sus.vib, sus.nv, sus.marcato, sus.tune, sus.rip
    , perf.legato.nv, perf.legato.vib, perf.legato.tune, perf.marcato
    ]

trumpetsA3Mute =
    [ trumpetsA3MuteShortLongNotes, trumpetsA3MuteDynamics
    , trumpetsA3MuteFlutter, trumpetsA3MutePerfInterval
    , trumpetsA3MutePerfRepetition, trumpetsA3MuteFastRepetition
    , trumpetsA3MuteUpbeatRepetitions
    ]
trumpetsA3MuteShortLongNotes = [staccato, portato.short, portato.med, sus]
trumpetsA3MuteDynamics =
    seconds [(dyn.str, [2, 3, 4, 6]), (pfp, [2, 3, 4, 6])]
    ++ [fp, sfz, sffz]
trumpetsA3MuteFlutter = [flutter, flutter.cresc]
trumpetsA3MutePerfInterval = [perf.legato, perf.marcato]
trumpetsA3MutePerfRepetition = map (rep.)
    [ legato, portato, staccato.slow, staccato.fast
    , dyn5.legato, dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
trumpetsA3MuteFastRepetition =
    fastRepBpm [(mempty, [14..19]), (dyn, [14..19])]
trumpetsA3MuteUpbeatRepetitions =
    upbeatRepBpm [(n1, bpms), (n2, bpms), (n3, bpms)]
    where bpms = [9..14] ++ [16, 18, 20]

hornsA4 =
    [ hornsA4ShortLongNotes, hornsA4Dynamics
    , hornsA4Flutter, hornsA4PerfInterval
    , hornsA4PerfIntervalFast, hornsA4PerfTrill
    , hornsA4PerfRepetition, hornsA4PerfUpbeatRepetition
    , hornsA4FastRepetition, hornsA4UpbeatRepetition
    , hornsA4Arpeggios, hornsA4Glissandi
    ]
hornsA4ShortLongNotes =
    [ staccato, portato.short, portato.med, portato.long
    , sus, sus.marcato, sus.blare
    ]
hornsA4Dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (dyn.str, [3, 4, 6]), (pfp, [4, 6])
    ] ++ [fp, sfz, sffz]
hornsA4Flutter = [flutter, flutter.cresc]
hornsA4PerfInterval = [perf.legato, perf.legato.sus, perf.marcato]
hornsA4PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
hornsA4PerfTrill = [perf.trill]
hornsA4PerfRepetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast
    , dyn5.legato.slow, dyn5.legato.fast, dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
hornsA4PerfUpbeatRepetition = map (upbeat.)
    [ n1.slow, n2.slow, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.fast, dyn4.n2.fast
    ]
hornsA4FastRepetition =
    fastRepBpm [(mempty, [15..19]), (dyn, [15..19])]
hornsA4UpbeatRepetition = upbeatRepBpm
    [(n1, [8..13]), (n2, [8..14] ++ [16, 18]), (n3, [8..14] ++ [16, 18])]
hornsA4Arpeggios = arpScales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
hornsA4Glissandi = map (gliss.)
    [ perf, fourth, dim.fifth, fifth, min.sixth, maj.sixth, min.seventh
    , maj.seventh, oct
    ]

hornsA4Stopped =
    [ hornsA4StoppedShortLongNotes, hornsA4StoppedDynamics
    , hornsA4StoppedPerfInterval, hornsA4StoppedPerfRepetition
    , hornsA4StoppedFastRepetition, hornsA4StoppedUpbeatRepetition
    ]
hornsA4StoppedShortLongNotes = [staccato, sus]
hornsA4StoppedDynamics =
    seconds [(dyn.med, [1.5, 2, 3, 4])] ++ [fp, sfz, sffz]
hornsA4StoppedPerfInterval = [perf.legato, perf.legato.sus]
hornsA4StoppedPerfRepetition = map (perf.rep.)
    [legato, portato, staccato, dyn5.legato, dyn9.portato, dyn9.staccato]
hornsA4StoppedFastRepetition =
    fastRepBpm [(mempty, [14..19]), (dyn, [14..19])]
hornsA4StoppedUpbeatRepetition =
    upbeatRepBpm [(n1, [9..14]), (n2, [9..14]), (n3, [9..14])]

trombonesA3 =
    [ trombonesA3ShortLongNotes, trombonesA3Dynamics
    , trombonesA3Flutter, trombonesA3Cluster
    , trombonesA3PerfInterval, trombonesA3PerfIntervalFast
    , trombonesA3PerfTrill, trombonesA3PerfRepetition
    , trombonesA3PerfUpbeatRepetition, trombonesA3FastRepetition
    , trombonesA3UpbeatRepetition, trombonesA3Arpeggios
    , trombonesA3Glissandi
    ]
trombonesA3ShortLongNotes =
    [ staccato.short, staccato.med, portato.short, portato.med, portato.long
    , sus, sus.marcato
    ]
trombonesA3Dynamics = seconds
    [ (dyn.med, [1.5, 2, 3, 4, 6]), (dyn.str, [2, 3, 4, 6])
    , (pfp, [4, 6, 8, 10])
    ] ++ [fp, sfz, sffz]
trombonesA3Flutter = [flutter, flutter.cresc]
trombonesA3Cluster = map (cluster.)
    [ staccato, sus, dyn.sec 1.5, dyn.sec 4, sfz
    , rep.legato, rep.dyn5.legato
    ]
trombonesA3PerfInterval = [perf.legato, perf.legato.sus, perf.marcato]
trombonesA3PerfIntervalFast = [perf.legato.fast, perf.marcato.fast]
trombonesA3PerfTrill = [perf.trill]
trombonesA3PerfRepetition = map (perf.rep.)
    [ legato.slow, legato.fast, portato.slow, portato.fast
    , staccato.slow, staccato.fast, dyn5.legato.slow, dyn5.legato.fast
    , dyn9.portato.slow, dyn9.portato.fast
    , dyn9.staccato.slow, dyn9.staccato.fast
    ]
trombonesA3PerfUpbeatRepetition = map (upbeat.)
    [ n1.slow, n2.slow, n1.med, n2.med, n1.fast, n2.fast
    , dyn4.n1.slow, dyn4.n2.slow, dyn4.n1.med, dyn4.n2.med
    , dyn4.n1.fast, dyn4.n2.fast
    ]
trombonesA3FastRepetition =
    fastRepBpm [(mempty, [14..18]), (dyn, [14..18])]
trombonesA3UpbeatRepetition = upbeatRepBpm
    [(n1, [8..14]), (n2, [8..14] ++ [16, 18]), (n3, [8..14] ++ [16, 18])]
trombonesA3Arpeggios = arpScales
    [staccato.mode.speed | mode <- [dim, maj, min], speed <- [mempty, fast]]
trombonesA3Glissandi = map (gliss.)
    [perf, half, whole, min.third, maj.third, fourth, dim.fifth]

trombonesA3Mute =
    [ trombonesA3MuteShortLongNotes, trombonesA3MuteDynamics
    , trombonesA3MuteFlutter, trombonesA3MutePerfInterval
    , trombonesA3MuteRepetition, trombonesA3MuteFastRepetition
    , trombonesA3MuteUpbeatRepetition
    ]
trombonesA3MuteShortLongNotes =
    [staccato, portato.short, portato.med, sus]
trombonesA3MuteDynamics = seconds
    [(dyn.str, [2, 3, 4, 6]), (pfp, [2, 3, 4, 7])
    ] ++ [fp, sfz, sffz]
trombonesA3MuteFlutter = [flutter, flutter.cresc]
trombonesA3MutePerfInterval = [perf.legato, perf.marcato]
trombonesA3MuteRepetition = map (perf.rep.)
    [ legato, portato, staccato.slow, staccato.fast
    , dyn5.legato, dyn9.portato, dyn9.staccato.slow, dyn9.staccato.fast
    ]
trombonesA3MuteFastRepetition = fastRepBpm [(mempty, [14..19])]
trombonesA3MuteUpbeatRepetition = upbeatRepBpm
    [(n1, [9..15]), (n2, [9..14] ++ [16, 18]), (n3, [9..14] ++ [16, 18])]


-- * util

runScales :: [Attributes] -> [Attributes]
runScales attrs = map (run.) (withScale attrs)

arpScales :: [Attributes] -> [Attributes]
arpScales attrs = map (arp.) (withScale attrs)

-- | Add an attr for each pitch, if it has 'maj' or 'min'.
withScale :: [Attributes] -> [Attributes]
withScale = concatMap make
    where
    make attr
        | any (Attrs.contain attr) [maj, min] = map (attr.) scale
        | otherwise = [attr]

scale :: [Attributes]
scale = map attr
    ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

graceIntervals :: [Attributes]
graceIntervals = map (grace.updown.) intervalsToOct

-- | Intervals from half note to the octave.
intervalsToOct :: [Attributes]
intervalsToOct =
    [ half, whole, min.third, maj.third, fourth, dim.fifth, fifth
    , min.sixth, maj.sixth, min.seventh, maj.seventh, oct
    ]

fastRepBpm :: [(Attributes, [Int])] -> [Attributes]
fastRepBpm attrs =
    concat [map (attr.fast.rep.) (map bpm (map (10*) bpms))
        | (attr, bpms) <- attrs]

upbeatRepBpm :: [(Attributes, [Int])] -> [Attributes]
upbeatRepBpm attrs =
    concat [map (attr.perf.upbeat.rep.) (map bpm (map (10*) bpms))
        | (attr, bpms) <- attrs]

seconds :: [(Attributes, [RealTime])] -> [Attributes]
seconds attrsSecs =
    prefixAttrs [(prefix, map sec secs) | (prefix, secs) <- attrsSecs]

-- | Add a prefix to each list of attributes.
prefixAttrs :: [(Attributes, [Attributes])] -> [Attributes]
prefixAttrs attrs = concat [map (prefix.) attrs | (prefix, attrs) <- attrs]

-- * attrs

sec :: RealTime -> Attributes
sec n = attr $ "sec"
    <> Text.replace "." "-" (Num.showFloat0 Nothing (RealTime.toSeconds n))

parseSec :: Attributes -> Maybe (RealTime, Attributes)
parseSec attrs = case Lists.partitionOn hasSec (Attrs.toList attrs) of
    ([secs], rest) -> Just (secs, Attrs.attrs rest)
    _ -> Nothing
    where
    (.) = (Prelude..)
    hasSec = parse . Text.replace "-" "." <=< Text.stripPrefix "sec"
    parse t = case ParseA.parse1 ParseA.pUnsignedFloat t of
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
vibStr = attr "vib-str" -- strong vibrato
vibDown = attr "vib-down" -- decreasing vibrato

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
