DEBUG=-ggdb
BOOST=/usr/local/src/boost_1_34_1
CXXFLAGS=`fltk-config --cxxflags` -I. -I../../libcc -I$(BOOST) $(DEBUG)
LDFLAGS=`fltk-config --ldflags` $(DEBUG)

OBJS=Block.o TrackTile.o Track.o Ruler.o EventTrack.o MoveTile.o

all: test_block

.PHONY: dep
dep:
	g++ -MM $(CXXFLAGS) *.cc >.depend
include .depend

test_block: $(OBJS) test_block.o f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	/Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r
