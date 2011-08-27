# TODO BUGS
# - .depend needs 'touch .depend' to bootstrap.
# - All hs depends on build/hspp though none state that.  So 'make build/hspp'
#   is also necessary for bootstrap.
# - hsc deps not automatically derived.  If a hs file imports an hsc then ghc
#   --make won't figure out to run hsc2hs on the hsc first.   Instead I am
#   forced to put the hsc deps in the Makefile prerequisites, but there's
#   nothing keeping that up to date.
#
# - It's grody how I have to make targets depend on their hsc files manually.
# Shouldn't I be able to ask ghc about all the module deps so I can hsc2hs
# on the ones that need it?
#
# The situation with flags is getting out of control.  Problems:
#
# - seq, test_core_midi need MIDI_LIBS
#
# - seq, test_block, logview, test_logview, browser, test_browser need fltk
#
# - tests and profiles are separate targets and require different flags

# Turn off built-in suffix rules, speeds up make?
# .SUFFIXES:

FLTK_CONFIG := /usr/local/src/fltk-1.3/fltk-config

# Directory for built binaries.
BUILD := build
TBUILD := $(BUILD)/test
PBUILD := $(BUILD)/profile
HSC := $(BUILD)/hsc

### OS dependent variables.

OS := $(shell uname)
ifeq ($(OS), Linux)

# Global -D flags for all C compilers.
DEFINE :=
MIDI_OBJS :=
MIDI_LIBS :=
# Default to the stub driver.
MIDI_DRIVER :=

else ifeq ($(OS), Darwin)

DEFINE := -DMAC_OS_X_VERSION_MAX_ALLOWED=1060 \
	-DMAC_OS_X_VERSION_MIN_REQUIRED=1050
MIDI_OBJS := Midi/core_midi.o
MIDI_LIBS := -framework CoreFoundation -framework CoreMIDI -framework CoreAudio
MIDI_DRIVER := -DCORE_MIDI

endif


### c++ flags

OPT := -ggdb # -O2
CINCLUDE := -Ifltk -I.

FLTK_LD := $(shell $(FLTK_CONFIG) --ldflags)
FLTK_C := $(shell $(FLTK_CONFIG) --cflags)

CXXFLAGS := $(FLTK_C) $(DEFINE) $(OPT) $(CINCLUDE) -Wall
LDFLAGS := $(FLTK_LD)


### ghc flags

# Turn this on to optimize the main app.
# SEQ_OPT := -O2
# INTERPRETER_HINT links the hint library to provide a haskell interpreter.  It
# adds about 10s to linking time and I often don't need it, so here's a switch
# for it.
# INTERPRETER_PLUGINS links the plugins library for a haskell interpreter.
# It links faster than hint but leaks memory.  And it's broken.
SEQ_INTERPRETER := $(if $(hint), -DINTERPRETER_HINT)

# w/o hint: make -j3 build/seq  9.06s user 1.17s system 99% cpu 10.333 total
# with hint: make -j3 build/seq  18.89s user 1.90s system 98% cpu 21.125 total
SEQ_FLAGS := $(SEQ_INTERPRETER) $(SEQ_OPT) $(MIDI_DRIVER)


# Flags to compile the profiles.
HPROFILE := -prof -O
HLDFLAGS := $(FLTK_LD) -rtsopts

### this changes with the ghc version
GHC := ghc-7.0.3
GHC_LIB := $(shell $(GHC) --print-libdir)

# hspp adds filename and lineno to various logging and testing functions.
HFLAGS := -threaded -W -fwarn-tabs $(CINCLUDE) -i$(HSC):. -pgml g++ \
	-F -pgmF $(BUILD)/hspp
# Unfortunately you can append to the ghc path but not prepend, so rules that
# want to prepend have to use -i -ifoo:$(GHC_PATH)
GHC_PATH := $(HSC):.

### misc variables

BUNDLE = tools/make_bundle $@

### objects and binaries

FLTK_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	P9Scrollbar.o SimpleScroll.o SeqInput.o MsgCollector.o \
	SkeletonDisplay.o StyleTable.o SymbolTable.o SymbolOutput.o \
	f_util.o alpha_draw.o types.o config.o util.o
FLTK_OBJS := $(addprefix fltk/, $(FLTK_OBJS))

BINARIES := $(addprefix $(BUILD)/, seq send repl browser make_db dump update \
	logview timer logcat)
TEST_BINARIES := $(addprefix $(BUILD)/, test_block test_logview test_browser \
		test_core_midi) \
	$(TBUILD)/RunTests


### targets

# Run the things I think should be run before checkin.
.PHONY: checkin
checkin:
	tools/make_all $(BUILD)/seq $(BUILD)/browser $(BUILD)/logview \
		$(PBUILD)/RunProfile tests

# Compile Everything.
.PHONY: all
all:
	tools/make_all $(BINARIES) $(TEST_BINARIES)

.PHONY: dep
dep: $(BUILD)/fixdeps
	g++ -MM $(CXXFLAGS) */*.cc | $(BUILD)/fixdeps >.depend
	printf '\n\n# hsc deps:\n' >>.depend
	tools/hscdeps.py $(CINCLUDE) -I$(GHC_LIB)/include $(HSC) */*.hsc >>.depend

include .depend

$(BUILD)/fixdeps: tools/fixdeps.hs
	$(GHC) -o $@ $^

.PHONY: $(BUILD)/hspp
$(BUILD)/hspp:
	$(GHC) -O2 --make -W -main-is Util.Hspp -o $@ Util/Hspp.hs

.PHONY: clean
clean:
	rm -f `find . -name '*.o' -or -name '*.hi' -or -name '*.pyc'` \
		fltk/fltk.a haddock/*  hpc/* seq_language
	rm -rf $(BUILD)/* .hpc

fltk/fltk.a: $(FLTK_OBJS)
	ar -rs $@ $^

$(BUILD)/test_block: fltk/test_block.o fltk/fltk.a
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

# I rely on ghc --make to decide what to recompile, so I don't need to put the
# dependencies in the Makefile.  Unfortunately, this doesn't work for *.hsc
# files that must be converted to *.hs.  I have to add those to the dep line
# explicitly.

# Ok so Util/CPUTime.hs is not technically a UI file but it's imported by the
# tests.
# TODO grody, maybe I should just make everything depend on ALL_HSC?
UI_HSC := $(addprefix $(HSC)/, $(wildcard Ui/*.hsc))
UI_HSC := $(UI_HSC:hsc=hs) $(HSC)/Util/CPUTime.hs
UI_OBJS := Ui/c_interface.o fltk/fltk.a

ALL_HS = $(shell tools/all_hs.py)

### main app

SEQ_CMDLINE = $(GHC) $(HFLAGS) --make -main-is App.Main App/Main.hs \
	$(UI_OBJS) $(MIDI_OBJS) $(MIDI_LIBS) $(HLDFLAGS)

# PHONY convinces make to always run ghc, which figures out deps on its own
.PHONY: $(BUILD)/seq
$(BUILD)/seq: $(UI_HSC) $(UI_OBJS) $(MIDI_OBJS)
	$(SEQ_CMDLINE) $(SEQ_FLAGS) -o $@
	$(BUNDLE) doc/seq.icns

### midi

.PHONY: $(BUILD)/test_core_midi
$(BUILD)/test_core_midi: $(UI_HSC) $(MIDI_OBJS)
	$(GHC) $(HFLAGS) --make \
		-main-is Midi.TestCoreMidi Midi/TestCoreMidi.hs -o $@ \
		$(MIDI_OBJS) $(MIDI_LIBS)

### repl

.PHONY: $(BUILD)/send
$(BUILD)/send: App/Send.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: $(BUILD)/repl
$(BUILD)/repl: App/Repl.hs
	$(GHC) $(HFLAGS) --make $^ -o $@ $(HLDFLAGS)

### misc

# PrintKeymap wants the global keymap, which winds up importing cmds that
# directly call UI level functions.  Even though it doesn't call the cmds,
# they're packaged together with the keybindings, so I wind up having to
# link in all that stuff anyway.
.PHONY: $(BUILD)/PrintKeymap
$(BUILD)/PrintKeymap: App/PrintKeymap.hs $(UI_OBJS)
	$(GHC) $(HFLAGS) --make -main-is App.PrintKeymap $^ $(HLDFLAGS) -o $@

.PHONY: doc/keymap.html
doc/keymap.html: $(BUILD)/PrintKeymap
	$^ >$@

### saved state

.PHONY: $(BUILD)/dump
$(BUILD)/dump: App/Dump.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

.PHONY: $(BUILD)/update
$(BUILD)/update: App/Update.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

### logview

LOGVIEW_PREREQ := LogView/LogView.hs LogView/interface.o LogView/logview_ui.o
# depend on Color because of Util.Log -> Peform.Warning import grossness
# someday I should remove that
LOGVIEW_HSC := $(HSC)/LogView/LogViewC.hs $(HSC)/Ui/Color.hs

.PHONY: $(BUILD)/logview
$(BUILD)/logview: $(LOGVIEW_PREREQ) $(LOGVIEW_HSC)
	$(GHC) $(HFLAGS) --make $^ -o $@ $(HLDFLAGS)
	$(BUNDLE)

$(BUILD)/test_logview: LogView/test_logview.o LogView/logview_ui.o \
		fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

### log util

.PHONY: $(BUILD)/timer
$(BUILD)/timer: LogView/Timer.hs
	$(GHC) $(HFLAGS) --make -main-is LogView.Timer $^ -o $@

.PHONY: $(BUILD)/logcat
$(BUILD)/logcat: LogView/LogCat.hs
	$(GHC) $(HFLAGS) --make $^ -o $@

### browser

BROWSER_PREREQ = Instrument/Browser.hs \
	Instrument/interface.o Instrument/browser_ui.o \
	Util/fltk_interface.o
BROWSER_HSC = $(HSC)/Instrument/BrowserC.hs $(HSC)/Ui/Types.hs

.PHONY: $(BUILD)/browser
$(BUILD)/browser: $(BROWSER_PREREQ) $(BROWSER_HSC)
	$(GHC) $(HFLAGS) --make $^ -o $@ $(HLDFLAGS)
	$(BUNDLE)

$(BUILD)/test_browser: Instrument/test_browser.o Instrument/browser_ui.o \
		fltk/f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(BUNDLE)

.PHONY: $(BUILD)/make_db
$(BUILD)/make_db: Instrument/MakeDb.hs
	$(GHC) $(HFLAGS) --make $^ -o $@
.PHONY: sense
sense:
	@echo 'Nevairrrr!'

### doc

ALL_HSC := $(patsubst %.hsc, $(HSC)/%.hs, $(filter %.hsc, $(ALL_HS)))

.PHONY: doc
doc: $(ALL_HSC)
	@# Unless there's some way to tell firefox to go to a certain line in
	@# a file, I can't use --source-entity without something like hscolour.
	haddock --html -B $(GHC_LIB) --source-module="../%F" -o haddock \
		$(shell tools/all_hs.py notest nomain hsc_as_hs)

### tests ###

TEST_CMDLINE = $(GHC) $(HFLAGS) --make -DTESTING \
	$(UI_OBJS) $(MIDI_OBJS) $(MIDI_LIBS) $(HLDFLAGS)

# Compiles with -odir and -hidir into $(TBUILD)/ because they are compiled with
# different flags.
$(TBUILD)/RunTests.hs: $(ALL_HS)
	test/generate_run_tests.py $@ $(filter %_test.hs, $(ALL_HS))
$(TBUILD)/RunTests: $(TBUILD)/RunTests.hs $(UI_HSC) $(UI_OBJS) $(MIDI_OBJS)
	$(TEST_CMDLINE) -i -i$(TBUILD):$(GHC_PATH) -odir $(TBUILD) \
		-hidir $(TBUILD) $(TBUILD)/RunTests.hs -fhpc -o $@
	rm -f *.tix # this sticks around and breaks hpc
	rm -f test.output # this gets reset on each new test run

$(PBUILD)/RunProfile.hs: $(ALL_HS)
	test/generate_run_tests.py $@ $(filter %_profile.hs, $(ALL_HS))

.PHONY: $(PBUILD)/RunProfile
$(PBUILD)/RunProfile: $(PBUILD)/RunProfile.hs $(UI_HSC) $(UI_OBJS) $(MIDI_OBJS)
	$(TEST_CMDLINE) -i -i$(PBUILD):$(GHC_PATH) -odir $(PBUILD) \
		-hidir $(PBUILD) $(PBUILD)/RunProfile.hs -o $@ $(HPROFILE)

.PHONY: $(PBUILD)/seq
$(PBUILD)/seq: $(UI_HSC) $(UI_OBJS) $(MIDI_OBJS)
	$(SEQ_CMDLINE) -i -i$(PBUILD):$(GHC_PATH) -odir $(PBUILD) \
		-hidir $(PBUILD) $(HPROFILE) -o $@
	$(BUNDLE) doc/seq.icns

.PHONY: profile
profile: $(PBUILD)/RunProfile
	tools/summarize_profile.py

.PHONY: tests
tests: $(TBUILD)/RunTests
	test/run_tests $^ auto-

.PHONY: interactive
interactive: $(TBUILD)/RunTests
	test/run_tests $^ interactive-

### misc ###

.PHONY: tags
tags: $(ALL_HS)
	@# hothasktags $^ | sort >tags.sorted
	hasktags --ignore-close-implementation --ctags $^
	sort tags >tags.sorted
	(echo -e '!_TAG_FILE_SORTED\t1\t ~'; cat tags.sorted) >tags
	rm tags.sorted

$(HSC)/%.hs: %.hsc
	@mkdir -p $(shell dirname $@)
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof -I$(GHC_LIB)/include \
		$(CINCLUDE) $(FLTK_C) $(DEFINE) $< -o $@

### portmidi ###
# I'm not using this now, but may use it again in the future

# PORTMIDI := /usr/local/src/portmedia/portmidi/trunk
# PORTMIDI_I := -Ifltk -I$(PORTMIDI)/pm_common -I$(PORTMIDI)/porttime -I.
#
# PORTMIDI_HSC := $(wildcard Midi/*.hsc)
# PORTMIDI_HS := $(PORTMIDI_HSC:hsc=hs)
#
# .PHONY: $(BUILD)/test_portmidi
# $(BUILD)/test_portmidi: Midi/PortMidi.hs $(UI_HSC)
# 	$(GHC) $(HFLAGS) --make \
# 		-main-is Midi.TestMidi Midi/TestMidi.hs $(MIDI_LIBS) -o $@
