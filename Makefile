# TODO BUGS
# .depend needs 'touch .depend' to bootstrap
# all hs depends on tools/hspp though none state that

### c++ flags

CXX_DEBUG := -ggdb
CXX_OPT := -O2
OPT := $(CXX_DEBUG)

MIDI_LIBS := -framework CoreFoundation -framework CoreMIDI -framework CoreAudio
CINCLUDE := -Ifltk -I.

# Flags for all versions.
FLTK_LD := -lpthread -framework Carbon -framework ApplicationServices \
    -framework AudioToolbox
LIBFLTK_D := -D_THREAD_SAFE -D_REENTRANT

# vanilla fltk
LIBFLTK_INC := -I/usr/local/include -I/usr/local/include/FL/images
LIBFLTK_LD := -L/usr/local/lib -lfltk

# cocoa fltk
LIBFLTK_1_3_LD := /usr/local/src/fltk-dev/fltk-1.3/lib/libfltk.a \
	-framework Cocoa
LIBFLTK_1_3_INC := -I/usr/local/src/fltk-dev/fltk-1.3/

# fltk 1.1.9
LIBFLTK_1_1_LD := /usr/local/src/fltk-1.1.9/lib/libfltk.a
LIBFLTK_1_1_INC := -I/usr/local/src/fltk-1.1.9 -DOLD_FLTK

FLTK_CXX := $(LIBFLTK_1_3_INC) $(LIBFLTK_D)
CXXFLAGS = $(FLTK_CXX) $(OPT) $(CINCLUDE) -Wall

LDFLAGS := $(LIBFLTK_1_3_LD) $(FLTK_LD)


### ghc flags

HDEBUG := -debug
HPROF := -O2 -prof -auto-all -caf-all
HOPT = -O2
HTEST := -fhpc
HPROFILE := -prof -auto-all -caf-all -O2

# Compiler flags for the main app.
# INTERPRETER links the hint library to provide a haskell interpreter.  It
# adds about 10s to linking time, so it's off by default.
SEQ_FLAGS := $(HDEBUG) # -DINTERPRETER

# Flags for generic compiles that don't need to be debugging or profiling.
HFLAGS = $(BASIC_HFLAGS) $(HDEBUG) # -fforce-recomp

HLDFLAGS := $(LDFLAGS)

GHC := ghc-6.12.3
# Used by haddock to find system docs, but it doesn't work anyway.
# TODO Fix this someday.
GHC_LIB := /Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-6.12.3

# hspp adds filename and lineno to various logging and testing functions.
BASIC_HFLAGS := -threaded -W -fwarn-tabs \
	$(CINCLUDE) -i../lib -pgmc g++ -pgml g++ \
	-optc -ggdb -optl -ggdb \
	-F -pgmF tools/hspp


### misc variables

# Directory for built binaries.
BUILD := build
TBUILD := $(BUILD)/test
PBUILD := $(BUILD)/profile

BUNDLE = tools/make_bundle $@

### objects and binaries

FLTK_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	Event.o P9Scrollbar.o SimpleScroll.o SeqInput.o MsgCollector.o \
	SkeletonDisplay.o SymbolTable.o \
	f_util.o alpha_draw.o types.o config.o
FLTK_OBJS := $(addprefix fltk/, $(FLTK_OBJS))

BINARIES := $(addprefix $(BUILD)/, seq send repl browser make_db dump update \
	logview timer logcat)
TEST_BINARIES := $(addprefix $(BUILD)/, test_block test_logview test_browser \
		test_core_midi) \
	$(TBUILD)/RunTests


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
		$(UI_HS) $(PORTMIDI_HS) $(LOGVIEW_HS) $(BROWSER_HS) haddock/*  \
		hpc/* seq_language
	rm -rf $(BUILD)/* .hpc

fltk/fltk.a: $(FLTK_OBJS)
	ar -rs $@ $^

$(BUILD)/test_block: fltk/test_block.o fltk/fltk.a
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

UI_HSC := $(wildcard Ui/*.hsc)
UI_HS := $(UI_HSC:hsc=hs)
UI_OBJS := Ui/c_interface.o

COREMIDI_OBJS := Midi/core_midi.o

# No longer used, but I haven't deleted them quite yet.
OBSOLETE := Midi/PortMidiC.hs Midi/PortMidi.hsc Midi/TestMidi.hs

ALL_HS = $(filter-out $(OBSOLETE), $(shell tools/all_hs.py))

### main app

SEQ_CMDLINE = $(GHC) -package ghc $(BASIC_HFLAGS) \
	--make -main-is App.Main App/Main.hs \
	$(UI_OBJS) $(COREMIDI_OBJS) fltk/fltk.a $(MIDI_LIBS) $(HLDFLAGS)

# PHONY convinces make to always run ghc, which figures out deps on its own
.PHONY: $(BUILD)/seq
$(BUILD)/seq: $(UI_HS) $(UI_OBJS) $(COREMIDI_OBJS) fltk/fltk.a
	$(SEQ_CMDLINE) $(SEQ_FLAGS) -o $@
	$(BUNDLE) doc/seq.icns

# not_mine = Instrument/BrowserC.o LogView/LogViewC.o Util/Fltk.o
# ALL_O := $(patsubst %.hs, %.o, $(filter-out $(OBSOLETE), $(shell tools/all_hs.py notest hsc_as_hs)))
# $(BUILD)/seqp: $(UI_OBJS) $(COREMIDI_OBJS) fltk/fltk.a Derive/Derive_test.o \
# 		$(filter-out $(not_mine), $(ALL_O))
# 	$(GHC) $(HFLAGS) -package ghc -package parsec -package binary \
# 		-package mtl -package hint -package bytestring -package text \
# 		-package dlist -package storablevector -package data-ordlist \
# 		-package network -package stm \
# 		-package regex-base -package regex-pcre \
# 		-main-is App.Main $^ \
# 		$(MIDI_LIBS) $(HLDFLAGS) \
# 		-o $@

### midi

.PHONY: $(BUILD)/test_core_midi
$(BUILD)/test_core_midi: $(UI_HS) $(COREMIDI_OBJS)
	$(GHC) $(HFLAGS) --make \
		-main-is Midi.TestCoreMidi Midi/TestCoreMidi.hs -o $@ \
		$(COREMIDI_OBJS) $(MIDI_LIBS) \

### repl

.PHONY: $(BUILD)/send
$(BUILD)/send: App/Send.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: $(BUILD)/repl
$(BUILD)/repl: App/Repl.hs
	$(GHC) $(HFLAGS) --make $^ -o $@ $(HLDFLAGS)

### saved state

.PHONY: $(BUILD)/dump
$(BUILD)/dump: App/Dump.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

.PHONY: $(BUILD)/update
$(BUILD)/update: App/Update.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

### logview

LOGVIEW_OBJ = LogView/LogView.hs LogView/LogViewC.hs \
	LogView/interface.o LogView/logview_ui.o
LOGVIEW_HS = LogView/LogViewC.hs

# Fltk 1.3 has a buggy and slow Fl_Text_Display, so use 1.1 for now.
LOGVIEW_CXX := $(LIBFLTK_1_1_INC) $(LIBFLTK_D) $(OPT) $(CINCLUDE) -Wall
LOGVIEW_LD := $(LIBFLTK_1_1_LD) $(FLTK_LD)

.PHONY: $(BUILD)/logview
# depend on Color because of Util.Log -> Peform.Warning import grossness
# someday I should remove that
$(BUILD)/logview: $(LOGVIEW_OBJ) Ui/Color.hs
	$(GHC) $(HFLAGS) --make $^ -o $@ $(LOGVIEW_LD)
	$(BUNDLE)

$(BUILD)/test_logview: LogView/test_logview.o LogView/logview_ui.o fltk/f_util.o
	$(CXX) -o $@ $^ $(LOGVIEW_LD)
	$(BUNDLE)

$(addprefix LogView/,test_logview.o interface.o logview_ui.o): %.o: %.cc
	$(CXX) $(LOGVIEW_CXX) -c -o $@ $<

### log util

.PHONY: $(BUILD)/timer
$(BUILD)/timer: LogView/Timer.hs
	$(GHC) $(HFLAGS) --make -main-is LogView.Timer $^ -o $@

.PHONY: $(BUILD)/logcat
$(BUILD)/logcat: LogView/LogCat.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

### browser

BROWSER_OBJ = Instrument/Browser.hs \
	Instrument/interface.o Instrument/browser_ui.o \
	Util/fltk_interface.o
BROWSER_HS = Instrument/BrowserC.hs

.PHONY: $(BUILD)/browser
$(BUILD)/browser: $(BROWSER_OBJ) $(BROWSER_HS)
	$(GHC) $(HFLAGS) --make $^ -o $@ \
		$(HLDFLAGS)
	$(BUNDLE)

$(BUILD)/test_browser: Instrument/test_browser.o Instrument/browser_ui.o fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

.PHONY: $(BUILD)/make_db
$(BUILD)/make_db: Instrument/MakeDb.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: sense
sense:
	@echo 'Nevairrrr!'

### doc

ALL_HSC := $(patsubst %.hsc, %.hs, $(filter %.hsc, $(ALL_HS)))

.PHONY: doc
doc: $(ALL_HSC)
	@# Unless there's some way to tell firefox to go to a certain line in
	@# a file, I can't use --source-entity without something like hscolour.
	haddock --html -B $(GHC_LIB) --source-module="../%F" -o haddock \
		$(filter-out %_test.hs, $(patsubst %.hsc, %.hs, $(ALL_HS)))

### tests ###

TEST_CMDLINE = $(GHC) $(BASIC_HFLAGS) --make \
	$(UI_OBJS) $(COREMIDI_OBJS) fltk/fltk.a $(MIDI_LIBS) $(HLDFLAGS)

# Compiles with -odir and -hidir into $(TBUILD)/ because they are compiled with
# different flags.
$(TBUILD)/RunTests.hs: $(ALL_HS)
	test/generate_run_tests.py $@ $(filter %_test.hs, $(ALL_HS))
$(TBUILD)/RunTests: $(TBUILD)/RunTests.hs $(UI_HS) $(UI_OBJS) \
		$(COREMIDI_OBJS) fltk/fltk.a
	$(TEST_CMDLINE) -i -i$(TBUILD):. -odir $(TBUILD) -hidir $(TBUILD) \
		$(TBUILD)/RunTests.hs $(HTEST) -o $@
	rm -f *.tix # this sticks around and breaks hpc
	rm -f test.output # this gets reset on each new test run

$(PBUILD)/RunProfile.hs: $(ALL_HS)
	test/generate_run_tests.py $@ $(filter %_profile.hs, $(ALL_HS))
$(PBUILD)/RunProfile: $(PBUILD)/RunProfile.hs $(UI_HS) $(UI_OBJS) \
		$(COREMIDI_OBJS) fltk/fltk.a
	$(TEST_CMDLINE) -i -i$(PBUILD):. -odir $(PBUILD) -hidir $(PBUILD) \
		$(PBUILD)/RunProfile.hs -o $@ $(HPROFILE)

.PHONY: $(PBUILD)/seq
$(PBUILD)/seq: $(UI_HS) $(UI_OBJS) $(COREMIDI_OBJS) fltk/fltk.a
	$(SEQ_CMDLINE) -i -i$(PBUILD):. -odir $(PBUILD) -hidir $(PBUILD) \
		$(HPROFILE) -o $@
	$(BUNDLE) doc/seq.icns

.PHONY: tests
tests: $(TBUILD)/RunTests
	test/run_tests $^ auto-

.PHONY: interactive
interactive: $(TBUILD)/RunTests
	test/run_tests $^ interactive-

### misc ###

tags: $(ALL_HS)
	hasktags --ignore-close-implementation --ctags $^
	sort tags >tags.sorted
	(echo -e '!_TAG_FILE_SORTED\t1\t ~'; cat tags.sorted) >tags
	rm tags.sorted

%.hs: %.hsc
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof $(CINCLUDE) $(PORTMIDI_I) $<
	@# hsc2hs stil includes INCLUDE but ghc 6.12 doesn't like that
	grep -v INCLUDE $@ >$@.tmp
	mv $@.tmp $@

### portmidi ###
# I'm not using this now, but may use it again in the future

# PORTMIDI := /usr/local/src/portmedia/portmidi/trunk
# PORTMIDI_I := -Ifltk -I$(PORTMIDI)/pm_common -I$(PORTMIDI)/porttime -I.
#
# PORTMIDI_HSC := $(wildcard Midi/*.hsc)
# PORTMIDI_HS := $(PORTMIDI_HSC:hsc=hs)
#
# .PHONY: $(BUILD)/test_portmidi
# $(BUILD)/test_portmidi: Midi/PortMidi.hs $(UI_HS)
# 	$(GHC) $(HFLAGS) --make \
# 		-main-is Midi.TestMidi Midi/TestMidi.hs $(MIDI_LIBS) -o $@
