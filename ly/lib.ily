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
