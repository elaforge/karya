# $@ = target, $^ = prereqs
DEBUG=-ggdb
CXXFLAGS=-I/usr/local/include -I/usr/include/freetype2 -I/usr/X11R6/include \
	-Wall $(DEBUG)
LDFLAGS=/usr/local/src/fltk-1.1.5/lib/libfltk.a \
	-L/usr/local/lib -L/usr/X11R6/lib \
	-lXft -lpthread -lm -lXext -lX11 -lsupc++ $(DEBUG)
CXX=g++-3.4

OBJS=Block.o Ruler.o Track_tile.o Tile_ext.o Zoom.o Track.o f_util.o \
	s_util.o state.o Marklist.o view.o alpha_draw.o \
	Fl_Tile2.o

all: test_block

.PHONY: depend
depend:
	g++-3.4 -MM *.cc >.depend 
include .depend

test_block: $(OBJS) test_block.o
	$(CXX) -o $@ $^ $(LDFLAGS)
test_event: f_util.o test_event.o
	$(CXX) -o $@ $^ $(LDFLAGS)
