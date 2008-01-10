/*
        block
      /       \
title       body ______
         /     |       \
scroll_box time_sb    body_tile __
                      /           \
            ruler_group      track_group
           /       \            |       \
       ruler    ruler_box   track_sb   track_zoom
                                       /
                                  track_tile
                                   /
                             overlay_ruler
                             track, ...
                                /
                        event, ...

also, there should be readouts for insertion point pos, zoom box,
block length, ...
these should be in both Trackpos units and relative to Mark units
(controllable from interface)

*/

#ifndef _Block_H
#define _Block_H

#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Tile.H>

#include "S_input.h"
#include "S_scrollbar.h"
#include "Zoomer.h"
#include "Ruler.h"
#include "Track_tile.h"
#include "types.h"

class Block : public Fl_Group {
public:
	Block(int X, int Y, int W, int H, Orientation o);
	void resize(int X, int Y, int W, int H);

	void update_size(int title_sz, int ruler_sz, int sb_sz, Orientation o);
private:
	Orientation o;

	S_input title;
	Fl_Group body;
		Fl_Box scroll_box;
		S_scrollbar time_sb;
		Fl_Tile body_tile;
			Fl_Group ruler_group;
				Fl_Box ruler_box;
				Ruler ruler;
			Fl_Group track_group;
				S_scrollbar track_sb;
				Zoomer track_zoom;
					Track_tile track_tile;
};

#endif // _Block_H
