DEBUG = -ggdb
BOOST = /usr/local/src/boost
CINCLUDE = -Iui -I$(BOOST) -I../libcc
CXXFLAGS = `fltk-config --cxxflags` $(DEBUG) $(CINCLUDE)
LDFLAGS = `fltk-config --ldflags` $(DEBUG)

HFLAGS=-W $(CINCLUDE) -pgmc g++ -pgml g++

UI_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	f_util.o
UI_OBJS := $(addprefix ui/, $(UI_OBJS))

all: test_block test_interface

.PHONY: dep
dep:
	g++ -MM $(CXXFLAGS) */*.cc >.depend
include .depend

.PHONY: clean
clean:
	rm */*.o */*.hi $(HSCS:hsc=hs)

test_block: $(UI_OBJS) ui/test_block.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	/Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r


HSCS = $(wildcard Interface/*.hsc)
INTERFACE_HS = $(HSCS:hsc=hs)
INTERFACE_OBJS = Interface/c_interface.cc

# -main-is Interface.TestInterface.main doesn't seem to work
.PHONY: test_interface
test_interface: $(INTERFACE_HS) $(INTERFACE_OBJS) Interface/test_interface.o
	ghc $(HFLAGS) --make \
	-main-is Interface.TestInterface Interface/TestInterface.hs $^ -o $@

%.hs: %.hsc
	hsc2hs -c g++ $(CINCLUDE) $<
