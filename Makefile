DEBUG = -ggdb
PORTMIDI = /usr/local/src/portmedia/portmidi/trunk
MIDI_LIBS = $(PORTMIDI)/pm_mac/libportmidi.a \
	$(PORTMIDI)/porttime/libporttime.a \
	-framework CoreFoundation -framework CoreMIDI -framework CoreAudio
CINCLUDE = -Ifltk -I../../libcc -I$(PORTMIDI)/pm_common -I$(PORTMIDI)/porttime
CXXFLAGS = `fltk-config --cxxflags` $(DEBUG) $(CINCLUDE)
LDFLAGS = `fltk-config --ldflags` $(DEBUG)
REZ = /Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r

GHC = ghc-6.8.2
GHC_LIB = /usr/local/lib/ghc-6.8.2

HFLAGS = -W $(CINCLUDE) -i../lib -pgmc g++ -pgml g++ -threaded -debug \
	-optc -ggdb -optl -ggdb

FLTK_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	Event.o P9Scrollbar.o SimpleScroll.o SeqInput.o MsgCollector.o \
	f_util.o alpha_draw.o types.o config.o
FLTK_OBJS := $(addprefix fltk/, $(FLTK_OBJS))

all: seq test_block test_ui test_midi

.PHONY: dep
dep: fixdeps
	g++ -MM $(CXXFLAGS) */*.cc | ./fixdeps >.depend
	echo -e '\n# hsc deps:\n' >>.depend
	./hscdeps.py $(CINCLUDE) -I$(GHC_LIB)/include */*.hsc >>.depend

include .depend

fixdeps: fixdeps.hs
	$(GHC) -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.hi fixdeps \
		*/*.o */*.hi fltk/fltk.a $(UI_HS) $(MIDI_HS) haddock/*

fltk/fltk.a: $(FLTK_OBJS)
	ar -rs $@ $^

test_block: fltk/test_block.o fltk/fltk.a
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(REZ)

UI_HSC = $(wildcard Ui/*.hsc)
UI_HS = $(UI_HSC:hsc=hs)
UI_OBJS = Ui/c_interface.o

MIDI_HSC = $(wildcard Midi/*.hsc)
MIDI_HS = $(MIDI_HSC:hsc=hs)

# PHONY convinces make to always run ghc, which figures out deps on its own
.PHONY: test_ui
test_ui: $(UI_HS) $(UI_OBJS) fltk/fltk.a
	$(GHC) $(HFLAGS) --make \
		-main-is Ui.TestUi Ui/TestUi.hs \
		$(UI_OBJS) fltk/fltk.a \
		`fltk-config --ldflags` \
		-o $@
	$(REZ)

.PHONY: test_midi
test_midi: $(MIDI_HS)
	$(GHC) $(HFLAGS) --make \
		-main-is Midi.TestMidi Midi/TestMidi.hs $^ $(MIDI_LIBS) -o $@

.PHONY: seq
seq: $(UI_HS) $(UI_OBJS) $(MIDI_HS) fltk/fltk.a
	$(GHC) $(HFLAGS) --make \
		-main-is App.Main App/Main.hs \
		$(UI_OBJS) fltk/fltk.a \
		$(MIDI_LIBS) `fltk-config --ldflags` \
		-o $@
	$(REZ)

.PHONY: doc
doc:
	haddock --html -B $(GHC_LIB) -o haddock [A-Z]*/*.hs

%.hs: %.hsc
	@# include GHC_LIB/include since hsc includes HsFFI.h
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof $(CINCLUDE) \
		-I$(GHC_LIB)/include $<
