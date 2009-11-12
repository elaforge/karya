# TODO BUGS
# .depend needs 'touch .depend' to bootstrap
# all hs depends on tools/hspp though none state that
# it should be able to get rid of GHC_LIB or find it out automatically

### c++ flags

CXX_DEBUG := -ggdb -O2
CXX_OPT := -O2
MIDI_LIBS := -framework CoreFoundation -framework CoreMIDI -framework CoreAudio
PORTMIDI := /usr/local/src/portmedia/portmidi/trunk
CINCLUDE := -Ifltk -I$(PORTMIDI)/pm_common -I$(PORTMIDI)/porttime -I.
LDFLAGS := `fltk-config --ldflags`

CXXFLAGS := `fltk-config --cxxflags` $(CXX_DEBUG) $(CINCLUDE) -Wall
# CXXFLAGS := `fltk-config --cxxflags` $(CXX_OPT) $(CINCLUDE) -Wall


### ghc flags

HFLAGS = $(BASIC_HFLAGS) $(HDEBUG) # -fforce-recomp

HDEBUG := -debug -O2
HPROF := -O2 -prof -auto-all -caf-all
HOPT = -O2
HTEST := -fhpc -prof -auto-all -caf-all # -O2

HLDFLAGS := `fltk-config --ldflags`

GHC := ghc-6.10.4
# This is unfortunately needed by hsc2hs, which seems kinda broken.
GHC_LIB := /Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-6.10.4

# hspp adds filename and lineno to various logging and testing functions.
BASIC_HFLAGS := -threaded -W $(CINCLUDE) -i../lib -pgmc g++ -pgml g++ \
	-optc -ggdb -optl -ggdb \
	-F -pgmF tools/hspp


### misc variables

# Directory for built binaries.
BUILD := build
BUNDLE = tools/make_bundle $@

### objects and binaries

FLTK_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	Event.o P9Scrollbar.o SimpleScroll.o SeqInput.o MsgCollector.o \
	SkeletonDisplay.o \
	f_util.o alpha_draw.o types.o config.o
FLTK_OBJS := $(addprefix fltk/, $(FLTK_OBJS))

BINARIES := $(addprefix $(BUILD)/, seq send repl browser make_db dump logview \
	timer logcat)
TEST_BINARIES := $(addprefix $(BUILD)/, test_block test_logview test_browser \
		test_core_midi) \
	test_obj/RunTests


### targets

# If -j is given, this runs the ghcs in parallel, which results in endless
# recompilation as the ghcs walk on each other.  So explicitly disable
# jobs.
.PHONY: all
all:
	for target in $(BINARIES) $(TEST_BINARIES); do \
		if ! $(MAKE) -j1 $$target; then break; fi \
	done

.PHONY: dep
dep: tools/fixdeps
	g++ -MM $(CXXFLAGS) */*.cc | tools/fixdeps >.depend
	printf '\n\n# hsc deps:\n' >>.depend
	tools/hscdeps.py $(CINCLUDE) -I$(GHC_LIB)/include */*.hsc >>.depend

include .depend

tools/fixdeps: tools/fixdeps.hs
	$(GHC) -o $@ $^

tools/hspp: tools/hspp.hs
	$(GHC) -O2 --make -W -o $@ $^

.PHONY: clean
clean:
	rm -f `find . -name '*.o' -or -name '*.hi' -or -name '*.pyc'` \
		fixdeps fltk/fltk.a \
		$(UI_HS) $(MIDI_HS) $(LOGVIEW_HS) $(BROWSER_HS) haddock/*  hpc/* \
		seq_language
	rm -rf test_obj/* $(BUILD)/* .hpc

fltk/fltk.a: $(FLTK_OBJS)
	ar -rs $@ $^

# Link against libfltk from src dir, for testing libfltk changes.
DIRECT_LINK = /usr/local/src/fltk/lib/libfltk.a \
	-lpthread -framework Carbon -framework ApplicationServices
$(BUILD)/test_block: fltk/test_block.o fltk/fltk.a
	# $(CXX) -o $@ $^ $(LDFLAGS)
	$(CXX) -o $@ $^ $(DIRECT_LINK)
	$(BUNDLE)

$(BUILD)/test_logview: LogView/test_logview.o LogView/logview_ui.o fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

$(BUILD)/test_browser: Instrument/test_browser.o Instrument/browser_ui.o fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

UI_HSC := $(wildcard Ui/*.hsc)
UI_HS := $(UI_HSC:hsc=hs)
UI_OBJS := Ui/c_interface.o

MIDI_HSC := $(wildcard Midi/*.hsc)
MIDI_HS := $(MIDI_HSC:hsc=hs)
MIDI_OBJS := Midi/core_midi.o

ALL_HS = $(shell tools/all_hs.py)

all_hsc: $(UI_HS) $(MIDI_HS)

# PHONY convinces make to always run ghc, which figures out deps on its own
# .PHONY: $(BUILD)/test_midi
# $(BUILD)/test_midi: $(MIDI_HS) $(UI_HS)
# 	$(GHC) $(HFLAGS) --make \
# 		-main-is Midi.TestMidi Midi/TestMidi.hs $(MIDI_LIBS) -o $@

.PHONY: $(BUILD)/test_core_midi
$(BUILD)/test_core_midi: $(UI_HS) $(MIDI_OBJS)
	$(GHC) $(HFLAGS) --make \
		-main-is Midi.TestCoreMidi Midi/TestCoreMidi.hs -o $@ \
		$(MIDI_OBJS) $(MIDI_LIBS) \

.PHONY: $(BUILD)/seq
$(BUILD)/seq: $(UI_HS) $(UI_OBJS) $(MIDI_OBJS) $(MIDI_HS) fltk/fltk.a
	$(GHC) $(HFLAGS) -package ghc --make \
		-main-is App.Main App/Main.hs \
		$(UI_OBJS) $(MIDI_OBJS) fltk/fltk.a $(MIDI_LIBS) \
		$(HLDFLAGS) \
		-o $@
	$(BUNDLE)

.PHONY: $(BUILD)/send
$(BUILD)/send: App/Send.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: $(BUILD)/repl
$(BUILD)/repl: App/Repl.hs
	$(GHC) $(HFLAGS) --make $^ -o $@ $(HLDFLAGS)
.PHONY: $(BUILD)/dump
$(BUILD)/dump: App/Dump.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: $(BUILD)/make_db
$(BUILD)/make_db: Instrument/MakeDb.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: sense
sense:
	@echo 'Nevairrrr!'

LOGVIEW_OBJ = LogView/LogView.hs LogView/LogViewC.hs \
	LogView/interface.o LogView/logview_ui.o
LOGVIEW_HS = LogView/LogViewC.hs

.PHONY: $(BUILD)/logview
# depend on Color because of Util.Log -> Peform.Warning import grossness
# someday I should remove that
$(BUILD)/logview: $(LOGVIEW_OBJ) Ui/Color.hs
	$(GHC) $(HFLAGS) --make -main-is LogView.LogView $^ -o $@ \
		$(HLDFLAGS)
	$(BUNDLE)

.PHONY: $(BUILD)/timer
$(BUILD)/timer: LogView/Timer.hs
	$(GHC) $(HFLAGS) --make -main-is LogView.Timer $^ -o $@

.PHONY: $(BUILD)/logcat
$(BUILD)/logcat: LogView/LogCat.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

BROWSER_OBJ = Instrument/Browser.hs \
	Instrument/interface.o Instrument/browser_ui.o \
	Util/fltk_interface.o
BROWSER_HS = Instrument/BrowserC.hs

.PHONY: $(BUILD)/browser
$(BUILD)/browser: $(BROWSER_OBJ) $(BROWSER_HS)
	$(GHC) $(HFLAGS) --make -main-is Instrument.Browser $^ -o $@ \
		$(HLDFLAGS)
	$(BUNDLE)

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
# Compiles with -odir and -hidir into test_obj/ because they are compiled with
# different flags.
test_obj/RunTests: test_obj/RunTests.hs all_hsc $(UI_OBJS) $(MIDI_OBJS) \
		fltk/fltk.a
	tools/unline_hack
	$(GHC) $(BASIC_HFLAGS) -i -itest_obj:. $(HTEST) --make \
		-odir test_obj -hidir test_obj \
		test_obj/RunTests.hs -o $@ \
		$(UI_OBJS) $(MIDI_OBJS) fltk/fltk.a \
		$(MIDI_LIBS) $(HLDFLAGS)
	rm -f *.tix # this sticks around and breaks things
	rm -f test.output # this gets reset on each new test run

.PHONY: tests
tests: test_obj/RunTests
	-test/run_tests direct-

.PHONY: interactive
interactive: test_obj/RunTests
	-test/run_tests init-

### misc ###

tags: $(ALL_HS)
	hasktags --ctags $^
	sort tags >tags.sorted
	(echo -e '!_TAG_FILE_SORTED\t1\t ~'; cat tags.sorted) >tags
	rm tags.sorted

# include GHC_LIB/include since hsc includes HsFFI.h
%.hs: %.hsc
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof $(CINCLUDE) \
		-I$(GHC_LIB)/include $<
