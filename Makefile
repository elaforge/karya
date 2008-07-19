DEBUG = -ggdb
PORTMIDI = /usr/local/src/portmedia/portmidi/trunk
MIDI_LIBS = $(PORTMIDI)/pm_mac/libportmidi.a \
	$(PORTMIDI)/porttime/libporttime.a \
	-framework CoreFoundation -framework CoreMIDI -framework CoreAudio
CINCLUDE = -Ifltk -I$(PORTMIDI)/pm_common -I$(PORTMIDI)/porttime -I.
CXXFLAGS = `fltk-config --cxxflags` $(DEBUG) $(CINCLUDE) -Wall
LDFLAGS = `fltk-config --ldflags` $(DEBUG)
REZ = /Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r

GHC = ghc-6.8.2
GHC_LIB = /usr/local/lib/ghc-6.8.2

# hspp adds filename and lineno to various logging and testing functions.
BASIC_HFLAGS = -W $(CINCLUDE) -i../lib -pgmc g++ -pgml g++ \
	-optc -ggdb -optl -ggdb \
	-F -pgmF test/hspp

# These flags don't coexist with profiling.
HFLAGS = $(BASIC_HFLAGS) -threaded -debug

FLTK_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	Event.o P9Scrollbar.o SimpleScroll.o SeqInput.o MsgCollector.o \
	f_util.o alpha_draw.o types.o config.o
FLTK_OBJS := $(addprefix fltk/, $(FLTK_OBJS))

BINARIES := seq send repl browser make_db dump logview
TEST_BINARIES := test_block test_logview test_browser test_midi \
	test_obj/RunTests

# If -j is given, this runs the ghcs in parallel, which results in endless
# recompilation as the ghcs walk on each other.  So explicitly disable
# jobs.
.PHONY: all
all:
	for target in $(BINARIES) $(TEST_BINARIES); do \
		if ! $(MAKE) -j1 $$target; then break; fi \
	done

.PHONY: dep
dep: fixdeps
	g++ -MM $(CXXFLAGS) */*.cc | ./fixdeps >.depend
	echo -e '\n# hsc deps:\n' >>.depend
	./hscdeps.py $(CINCLUDE) -I$(GHC_LIB)/include */*.hsc >>.depend

include .depend

fixdeps: fixdeps.hs
	$(GHC) -o $@ $^

test/hspp: test/hspp.hs
	$(GHC) -O2 --make -W -o $@ $^

.PHONY: clean
clean:
	rm -f `find . -name '*.o' -or -name '*.hi' -or -name '*.pyc'` \
		fixdeps fltk/fltk.a \
		$(UI_HS) $(MIDI_HS) $(LOGVIEW_HS) $(BROWSER_HS) haddock/*  hpc/* \
		$(BINARIES) $(TEST_BINARIES) \
		seq_language
	rm -rf test_obj/* .hpc

fltk/fltk.a: $(FLTK_OBJS)
	ar -rs $@ $^

# Link against libfltk from src dir, for testing libfltk changes.
DIRECT_LINK = /usr/local/src/fltk/lib/libfltk.a \
	-lpthread -framework Carbon -framework ApplicationServices
test_block: fltk/test_block.o fltk/fltk.a
	$(CXX) -o $@ $^ $(LDFLAGS) # $(DIRECT_LINK)
	$(REZ)

test_logview: LogViewer/test_logview.o LogViewer/logview_ui.o fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(REZ)

test_browser: Instrument/test_browser.o Instrument/browser_ui.o fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(REZ)

UI_HSC = $(wildcard Ui/*.hsc)
UI_HS = $(UI_HSC:hsc=hs)
UI_OBJS = Ui/c_interface.o

MIDI_HSC = $(wildcard Midi/*.hsc)
MIDI_HS = $(MIDI_HSC:hsc=hs)

ALL_HS = $(shell ./all_hs.py)

all_hsc: $(UI_HS) $(MIDI_HS)

# PHONY convinces make to always run ghc, which figures out deps on its own
.PHONY: test_midi
test_midi: $(MIDI_HS)
	$(GHC) $(HFLAGS) --make \
		-main-is Midi.TestMidi Midi/TestMidi.hs $^ $(MIDI_LIBS) -o $@

.PHONY: seq
seq: $(UI_HS) $(UI_OBJS) $(MIDI_HS) fltk/fltk.a
	$(GHC) $(HFLAGS) -package ghc --make \
		-main-is App.Main App/Main.hs \
		$(UI_OBJS) fltk/fltk.a \
		$(MIDI_LIBS) `fltk-config --ldflags` \
		-o $@
	$(REZ)

.PHONY: send
send: App/Send.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: repl
repl: App/Repl.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: dump
dump: App/Dump.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: make_db
make_db: Instrument/MakeDb.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: sense
sense:
	@echo 'Nevairrrr!'

LOGVIEW_OBJ = LogViewer/LogView.hs LogViewer/LogViewC.hs \
	LogViewer/interface.o LogViewer/logview_ui.o
LOGVIEW_HS = LogViewer/LogViewC.hs

.PHONY: logview
logview: $(LOGVIEW_OBJ)
	$(GHC) $(HFLAGS) --make -main-is LogViewer.LogView $^ -o $@ \
		`fltk-config --ldflags`
	$(REZ)

BROWSER_OBJ = Instrument/Browser.hs \
	Instrument/interface.o Instrument/browser_ui.o \
	Util/fltk_interface.o
BROWSER_HS = Instrument/BrowserC.hs

.PHONY: browser
browser: $(BROWSER_OBJ) $(BROWSER_HS)
	$(GHC) $(HFLAGS) --make -main-is Instrument.Browser $^ -o $@ \
		`fltk-config --ldflags`
	$(REZ)

.PHONY: doc
doc:
	@# Unless there's some way to tell firefox to go to a certain line in
	@# a file, I can't use --source-entity without something like hscolour.
	haddock --html -B $(GHC_LIB) --source-module="../%F" -o haddock \
		$(filter-out %_test.hs, $(patsubst %.hsc, %.hs, $(ALL_HS)))

### tests ###

test_obj/RunTests.hs: $(ALL_HS)
	test/generate_run_tests.py $@ $(filter %_test.hs, $(ALL_HS))

# TODO a bug in ghc prevents .mix data from being emitted for files with LINE
# workaround by grep -v out the LINEs into test_obj hierarchy
# Compiles with -odir and -hidir into test_obj/ because they must be compiled
# with -fhpc.
# The 'hint' package uses 'Outputtable', which is from the ghc-6.8.2 package,
# which isn't compiled with profiling.
# -fprof -auto-all
test_obj/RunTests: test_obj/RunTests.hs all_hsc $(UI_OBJS) fltk/fltk.a
	./unline_hack
	$(GHC) $(BASIC_HFLAGS) -threaded -i -itest_obj:. -fhpc --make \
		-odir test_obj -hidir test_obj \
		test_obj/RunTests.hs -o $@ \
		$(UI_OBJS) fltk/fltk.a \
		$(MIDI_LIBS) `fltk-config --ldflags`
	rm -f *.tix # this sticks around and breaks things
	rm -f test.output # this gets reset on each new test run

.PHONY: tests
tests: test_obj/RunTests
	test/run_tests direct-

.PHONY: interactive
interactive: test_obj/RunTests
	test/run_tests init-

### misc ###

tags: $(ALL_HS)
	hasktags --ctags $^
	sort tags >tags.sorted
	mv tags.sorted tags

# include GHC_LIB/include since hsc includes HsFFI.h
%.hs: %.hsc
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof $(CINCLUDE) \
		-I$(GHC_LIB)/include $<
