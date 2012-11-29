-- | A common library of attributes for instrument definitians.
--
-- This is meant to be imported unqualified.
module Derive.Attrs (module Derive.Attrs, Attributes) where
import qualified Derive.Score as Score
import Derive.Score (Attributes, attr)


no_attrs :: Attributes
no_attrs = Score.no_attrs

-- TODO Dunno

auto = attr "auto" -- TODO what's this?

-- * articulations

sustain = attr "sustain"
-- | Modifies another articulation to grow in strength.
progressive = attr "progressive"
-- Like @progressive@, but fading in strength.
fading = attr "fading"
attack = attr "attack" -- hard attack
harsh = attr "harsh"

flautando = attr "flautando"
staccato = attr "staccato"
spiccato = attr "spiccato"
detache = attr "detache"
marcato = attr "marcato"
portato = attr "portato"
espres = attr "espres" -- espressivo
trill = attr "trill"
trem = attr "trem" -- tremolo
arpeggio = attr "arpeggio"
legato = attr "legato"
porta = attr "porta" -- portamento
gliss = attr "gliss" -- glissando
mute = attr "mute" -- brass mute, string mute, or guitar palm mute

vibrato = attr "vibrato"
nv = attr "nv" -- no vibrato

cluster = attr "cluster" -- cluster of tones

-- * dynamics

cresc = attr "cresc"
dim = attr "dim"
sfz = attr "sfz"
sffz = attr "sffz"
fp = attr "fp"
pfp = attr "pfp"
fpf = attr "fpf"
soft = attr "soft"
loud = attr "loud"

-- Whole note and half note variants, e.g. for trills.
half = attr "half"
whole = attr "whole"

-- * strings

pizz = attr "pizz" -- pizzicato
snap = attr "snap" -- snap pizz, aka Bartok pizz
harmonic = attr "harmonic"
one_string = attr "one-string"
zigane = attr "zigane" -- Apparently a gypsy thing.

pont = attr "pont" -- ponticello
tasto = attr "tasto" -- sul tasto

natural = attr "natural" -- as in natural harmonics
artificial = attr "artificial"

bisbig = attr "bisbig" -- bisbigliando

-- * winds

flutter = attr "flutter"
blared = attr "blared" -- brass

-- * drums

-- ** balinese

lanang = attr "lanang"
wadon = attr "wadon"
kebyar = attr "kebyar"
legong = attr "legong"

-- Kendang tunggal strokes don't really have names so I made some up.
-- For composite it would be: de tut, kum pung, ka pak, kam pang
-- If I took the wadon or lanang names, it would be de, kum, ka, kam, or
-- tut, pung, pak, pang, which both sound weird.

-- both
plak = attr "plak"

-- right
de = attr "de"
tut = attr "tut"
ka = attr "ka" -- neutral stroke

-- left
pak = attr "pak"
pang = attr "pang" -- rim

-- ** western

snare = attr "snare" -- snare
rim = attr "rim" -- rim on snare
stick = attr "stick"
bd = attr "bd" -- base drum
hh = attr "hh" -- high-hat
crash = attr "crash" -- crash cymbal
ride = attr "ride" -- ride cymbal
tom = attr "tom"

pedal = attr "pedal"

-- ** technique

left = attr "left" -- played with left hand
right = attr "right" -- played with right hand
thumb = attr "thumb"

center = attr "center"
edge = attr "edge"
slap = attr "slap"
knuckle = attr "knuckle"

open = attr "open"
closed = attr "closed"
muted = attr "muted"

low = attr "low"
middle = attr "middle"
high = attr "high"

up = attr "up"
down = attr "down"

short = attr "short"
long = attr "long"

fast = attr "fast"
medium = attr "medium"
slow = attr "slow"

light = attr "light"
heavy = attr "heavy"

-- * misc

-- Many of these attributes are meant to be combined with others.

-- | Generic variations.  E.g. if there are two snares you can have
-- @snare \<> v1@ and @snare \<> v2@.
v1 = attr "1"
v2 = attr "2"
v3 = attr "3"
v4 = attr "4"
