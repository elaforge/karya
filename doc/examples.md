[back to overview](overview.md.html)

## examples

### "Concert gift" for yangqin, kendang Bali, guzheng

[MP3](../data/music/concert gift-快版本.mp3),
[score](../data/score/concert-gift.pdf)

<a href="../data/img/screen/seq-concert-gift.png">
<img align=right width=500 src="../data/img/screen/seq-concert-gift.png"
    border=2>
</a>

The zheng part has characteristic trill and tremolo ornaments, as well as a
`gliss` that understands where open strings are.  Non-open strings require a
pitch bend and are highlighted in red.  The bend and release are applied
automatically.  Here are
[some experiments](../data/music/zheng experiments.mp3) demonstrating standard
tuning, and then a theoretical zheng tuned to the harmonic series.  Yangqin
uses a tremolo that can arpeggiate chords.

The kendang uses a specialized notation for kendang tunggal (solo).  There is
also a pasangan notation that uses a single notation to generate two
interlocking parts, with unwritten filler strokes filled in automatically.

Zheng and yangqin have annotations to generate a lilypond score.

The pitches are notated in a relative notation, so I can change the key or
scale by changing a single declaration in the local definitions
[ky file](ui.md.html#ky-file):

```
note transformer:
global = import ly | scale=twelve-r | key=b-min
    | trem-speed=12 | grace-place=0 | grace-dyn=1
    | %pedal=1
>zh = standard-strings | import china.zheng
    | bent-string | trem-speed = 10

e->q = ly-pre 'tempo \markup {
    \concat {
        ( \smaller \general-align #Y #DOWN \note #"8" #1
        " = "
        \smaller \general-align #Y #DOWN \note #"4" #1
        )
    } }'

q->e = ly-pre 'tempo \markup {
    \concat {
        ( \smaller \general-align #Y #DOWN \note #"4" #1
        " = "
        \smaller \general-align #Y #DOWN \note #"8" #1
        )
    } }'
```

This sets the scale to 12TET with relative pitch notation, and configures
grace note placement.  It configures the open strings for zheng `>zh` and
applies the string bending performance technique.  `e->q` and `q->e` emit
custom lilypond code.

### pieces for gender wayang

<a href="../data/img/screen/seq-cerucuk-punyah.png">
<img align=right width=400 src="../data/img/screen/seq-cerucuk-punyah.png"
    border=2>
</a>

- [cerucuk punyah](../data/music/cerucuk-punyah.mp3)
- [delima manis](../data/music/delima-manis.mp3)
- [p4](../data/music/p4.mp3)
- [merta1](../data/music/merta1.mp3)

These are traditional pieces I have transcribed.  They are notated in their own
scale, using Balinese solfege, even though, as far as I know, it's not usually
applied to the gender wayang scale.  The scale has two intonations, pengumbang
and pengisep.  Each instrument uses its own intonation, which gives a beating
effect on unison notes.

The notation has a fair amount of randomization, with alternate realizations of
certain ornaments, notes which may be played softly, muted, or omitted, and
optional phrases.  This means repeats come back slightly differently, and since
each part is doubled an octave up, the upper and lower parts also differ.

There is also extensive use of hierarchical score to express the sometimes
complicated structure of repeats, and the use of nested tempos to express broad
tempo changes combined with local expressive variations.

<br>
<br>
<br>
<br>
<br>
<br>

### "Five Hexanies" for electronic beeps and boops

<a href="../data/img/screen/seq-hex.png">
<img align=right width=500 src="../data/img/screen/seq-hex.png" border=2>
</a>

[MP3](../data/music/Five Hexanies.mp3)

This is an experiment with the hexany family of hexatonic scales.  Each scale
is generated from a set of seed primes, and the same music is repeated in five
different variants.  I also experiment with some fancy calls to generate breath
and modulation curves.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### "Viola Sonata" for piano and viola

<a href="../data/img/screen/seq-viola-sonata-vla1.png">
<img align=right width=200 src="../data/img/screen/seq-viola-sonata-vla1.png"
    border=2>
</a>

[MP3](../data/music/viola-sonata.mp3), [score](../data/score/viola-sonata.pdf)

This is pretty standard piece, but demonstrates use of the VSL library, with
many elaborate keyswitches, and more extensive lilypond annotation.  For
instance, on the viola part a slur enables the legato keyswitch unless locally
overridden, while on the piano part it detaches the first note for emphasis and
links the subsequent ones to it with legato.  Unslurred notes default to being
slightly detached.  I can change the feel of the piano part by adjusting slur
overlap, slur emphasis, unslurred note detachment, and the interpretation of
accents and staccato.


## browser

You probably don't have these same instruments, but here are some examples of
the kind of configuration you can do.


- `vsl/violin` has a huge number of keyswitches, and can automatically
infer grace notes, natural harmonics, tremolo, and trills.

    <a href="../data/img/screen/browser-vsl-violin.png">
    <img width=500 src="../data/img/screen/browser-vsl-violin.png" borde=2>
    </a>

- Z1 patches have a lot of controls inferred from the sysex
dump, and can automatically send the sysex if it's not built-in.  You can
search by tags to filter down the list.

    <a href="../data/img/screen/browser-z1-3osc-bass.png">
    <img width=500 src="../data/img/screen/browser-z1-3osc-bass.png" borde=2>
    </a>

- `kontakt/mridangam-d` has quite a bit of specialized notation.

    <a href="../data/img/screen/browser-kontakt-mridangam-d.png">
    <img width=500 src="../data/img/screen/browser-kontakt-mridangam-d.png"
        borde=2>
    </a>
