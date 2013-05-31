This directory has code dealing with the instrument DB.

The instrument DB has information about all the known instruments.  They are
divided up by their backend (only MIDI for now) and have data on how to
initialize the instrument (e.g. a program change or sysex for MIDI, or possibly
just instructions to print, for software synths that don't allow remote
configuration).  In addition, a set of arbitrary tag/value pairs are associated
with each instrument which can be used for better searches.

The DB itself is of course specific to the local configuration, and is found in
`Local/Instrument`.  This directory has library modules that help construct
the DB.

There are two clients:

- The main sequencer refers to instruments with 'Derive.Score.Instrument',
which is merely a string.  It looks these strings up in the instrument DB to
get information like backend, and for midi, initialization technique,
available controlers, and so forth.

- A separate program called `browser` is used to search the db interactively.
It accepts a simple query language and incrementally displays the search
results.  If you select an instrument it uses the lang socket to tell the
sequencer to initialize the instrument and set up midi thru to talk to it
(only for instruments that render in real-time component, of course).

To help with searches, there's a simple tag/value set associated with each
instrument.  There are some general conventions for tags expressed in
'Instrument.Tag', but in general they're arbitrary.

Since building the instrument db may involve parsing directories full of sysex
files, there's a separate program called `make_db` that generates the
expensive parts of the db and dumps them to files.  How this happens and where
the dumps go is up to the instrument definition, but they should put them in
`inst_db/db`.


## Old doc, integrate into current docs somehow:

Controller mapping:
The goal is to convert the symbolic controller names into MIDI controller
numbers.  There are a few controllers with hardcoded special meaning, like
pitch-bend and channel aftertouch.  The rest are looked up in a map that is
merged from a set of global controllers, a set of synth-specific controllers,
and then the instrument-specific controllers.

three data types:
Synth defines common features for a set of instruments, like device and
controllers.

Patch is information about one specific instrument.  Every patch has a Synth

Patch and Synth are combined to create the Instrument and the Config.
Instrument is the data attached to events, and defines only what is needed to
render notes.  The Config gives the chanel allocation for the instrument, and
is derived from the Synth (for the midi device) and local song config (for the
channels).

Also, each Patch defines an initialization procedure, which may depend on the
channel.  An instrument can be used to look up its originating Patch in order
to initialize it (send patch change, send sysex, etc.).  There should be
commands that look in the midi config at the active instruments and their
channels and do this.


At the UI level, an instrument is just a string.  Ultimately, there is one
large database of String -> Instrument.  When you put a string in an instrument
track, it goes into the song instrument list.  After derivation, each
instrument is looked up in the InstrumentDb, which looks through each of the
backend DBs in turn.  This initial lookup returns BackEnd, which determines the
converter.  The converter gets ([Score.Event], [Score.Instrument]).  It looks
up the instruments in the backend specific db, and uses that map to convert to
the backend specific instrument.

The 'initialize_instrument' command will go through the same procedure, except
it will use the Score.Instrument to get the InitializePatch and map that
through the channels in the midi_config (initialize is a midi only concept).
