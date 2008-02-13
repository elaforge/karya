DEBUG = -ggdb
CINCLUDE = -Iui -I../libcc
CXXFLAGS = `fltk-config --cxxflags` $(DEBUG) $(CINCLUDE)
LDFLAGS = `fltk-config --ldflags` $(DEBUG)

HFLAGS=-W $(CINCLUDE) -pgmc g++ -pgml g++

UI_OBJS := Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o \
	f_util.o
UI_OBJS := $(addprefix ui/, $(UI_OBJS))

all: test_block test_interface

.PHONY: dep
dep: fixdeps
	g++ -MM $(CXXFLAGS) */*.cc | ./fixdeps >.depend
include .depend

fixdeps: fixdeps.hs
	ghc -o $@ $^

.PHONY: clean
clean:
	rm */*.o */*.hi ui/ui.a $(HSCS:hsc=hs)

test_block: $(UI_OBJS) ui/test_block.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	/Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r

ui/ui.a: $(UI_OBJS)
	ar -rs $@ $^

HSCS = $(wildcard Interface/*.hsc)
INTERFACE_HS = $(HSCS:hsc=hs)
INTERFACE_OBJS = Interface/c_interface.o

# -main-is Interface.TestInterface.main doesn't seem to work
.PHONY: test_interface
test_interface: $(INTERFACE_HS) $(INTERFACE_OBJS) Interface/test_interface.o \
		ui/ui.a
	ghc $(HFLAGS) --make \
	-main-is Interface.TestInterface Interface/TestInterface.hs $^ \
	`fltk-config --ldflags` \
	-o $@

%.hs: %.hsc
	hsc2hs -c g++ --cflag -Wno-invalid-offsetof $(CINCLUDE) $<
