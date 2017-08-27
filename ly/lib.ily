% Copyright 2017 Evan Laforge
% This program is distributed under the terms of the GNU General Public
% License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

% Standard ly library for karya scores.

% Typeset a glissando that spans several tied notes or measures.
% Intermediate notes are shown to express timing, and of course that the
% string is still being played.  Ledger lines are hidden, for no good reason,
% but maybe it's less cluttered to draw attention to the gliss line.

% This goes after \glissando.
glissandoSkipOn = {
    \override NoteColumn.glissando-skip = ##t
    \override NoteHead.no-ledgers = ##t
}

% This goes before the destination note.
glissandoSkipOff = {
    \revert NoteColumn.glissando-skip
    \revert NoteHead.no-ledgers
}

% Without the hspace, it draws on top of a preceding hairpin.
alNiente = #(make-dynamic-script
    (markup #:hspace 4 #:normal-text #:italic "al niente"))

% This puts the accidental next to the tr sign.  My notation book advocates
% putting it above, adjacent seems more compact.
trSharp = \markup {
    \musicglyph #"scripts.trill" \fontsize #-4 \raise #1 \sharp }
trSharpSharp = \markup {
    \musicglyph #"scripts.trill" \doublesharp }
trNatural = \markup {
    \musicglyph #"scripts.trill" \fontsize #-2 \raise #1 \natural }
trFlat = \markup {
    \musicglyph #"scripts.trill" \fontsize #-2 \raise #1 \flat }
trFlatFlat = \markup {
    \musicglyph #"scripts.trill" \fontsize #-2 \raise #1 \doubleflat }
