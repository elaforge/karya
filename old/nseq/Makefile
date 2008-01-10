# $@ = target, $^ = prereqs
fltk=$(HOME)/src/fltk-1.1.7

postbuild=/Developer/Tools/Rez -t APPL -o $@ $(fltk)/FL/mac.r

DEBUG=-ggdb
CXXFLAGS=-I/usr/local/include -I/usr/include/freetype2 -I/usr/X11R6/include \
	-Wall $(DEBUG)
CXXFLAGS=-I$(fltk)
LDFLAGS=/usr/local/src/fltk-1.1.5/lib/libfltk.a \
	-L/usr/local/lib -L/usr/X11R6/lib \
	-lXft -lpthread -lm -lXext -lX11 -lsupc++ $(DEBUG)
LDFLAGS=$(fltk)/lib/libfltk.a -lm -framework Carbon \
	-framework ApplicationServices $(DEBUG)

CXX=g++

OBJS=Block.o futil.o

all: test_block

.PHONY: depend
depend:
	$(CXX) $(CXXFLAGS) -MM *.cc >.depend 
include .depend

test_block: $(OBJS) test_block.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(postbuild)

test_event: futil.o test_event.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(postbuild)

test_tile: test_tile.o # Track_tile.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	$(postbuild)
