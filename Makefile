DEBUG = -ggdb
PORTMIDI = /usr/local/src/portmedia/portmidi/trunk
MIDI_LIBS = $(PORTMIDI)/pm_mac/libportmidi.a \
	$(PORTMIDI)/porttime/libporttime.a \
	-framework CoreFoundation -framework CoreMIDI -framework CoreAudio
CINCLUDE = -Iui -I../libcc -I$(PORTMIDI)/pm_common -I$(PORTMIDI)/porttime
CXXFLAGS = `fltk-config --cxxflags` $(DEBUG) $(CINCLUDE)
LDFLAGS = `fltk-config --ldflags` $(DEBUG)
REZ = /Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r

GHC = ghc-6.8.2
GHC_LIB = /usr/local/lib/ghc-6.8.2

HFLAGS = -W $(CINCLUDE) -i../lib -pgmc g++ -pgml g++ -threaded -debug \
	-optc -ggdb -optl -ggdb

UI_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	Event.o P9Scrollbar.o SimpleScroll.o EventCollector.o \
	f_util.o alpha_draw.o types.o config.o
UI_OBJS := $(addprefix ui/, $(UI_OBJS))

all: test_block test_interface test_midi

.PHONY: dep
dep: fixdeps
	g++ -MM $(CXXFLAGS) */*.cc | ./fixdeps >.depend
include .depend

fixdeps: fixdeps.hs
	$(GHC) -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.hi fixdeps \
		*/*.o */*.hi ui/ui.a $(INTERFACE_HS) $(MIDI_HS) haddock/*

test_block: $(UI_OBJS) ui/test_block.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(REZ)

ui/ui.a: $(UI_OBJS)
	ar -rs $@ $^

INTERFACE_HSC = $(wildcard Interface/*.hsc)
INTERFACE_HS = $(INTERFACE_HSC:hsc=hs)
INTERFACE_OBJS = Interface/c_interface.o

MIDI_HSC = $(wildcard Midi/*.hsc)
MIDI_HS = $(MIDI_HSC:hsc=hs)

# PHONY convinces make to always run ghc, which figures out deps on its own
.PHONY: test_interface
test_interface: $(INTERFACE_HS) $(INTERFACE_OBJS) \
		Interface/test_interface.o ui/ui.a
	$(GHC) $(HFLAGS) --make \
		-main-is Interface.TestInterface Interface/TestInterface.hs $^ \
		`fltk-config --ldflags` \
		-o $@
	$(REZ)

.PHONY: test_midi
test_midi: $(MIDI_HS)
	$(GHC) $(HFLAGS) --make \
		-main-is Midi.TestMidi Midi/TestMidi.hs $^ $(MIDI_LIBS) -o $@

.PHONY: doc
doc:
	haddock --html -B $(GHC_LIB) -o haddock [A-Z]*/*.hs

%.hs: %.hsc
	@# include GHC_LIB/include since hsc includes HsFFI.h
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof $(CINCLUDE) \
		-I$(GHC_LIB)/include $<
