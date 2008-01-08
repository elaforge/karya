DEBUG=-g
CXXFLAGS=`fltk-config --cxxflags` -I. $(DEBUG)
LDFLAGS=`fltk-config --ldflags` $(DEBUG)

OBJS=Block.o

all: test_block

.PHONY: dep
dep:
	g++ -MM *.cc >.depend
include .depend

test_block: $(OBJS) test_block.o f_util.o
	$(CXX) -o $@ $^ $(LDFLAGS)
	/Developer/Tools/Rez -t APPL -o $@ /usr/local/include/FL/mac.r
