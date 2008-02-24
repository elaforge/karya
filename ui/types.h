#ifndef __TYPES_H
#define __TYPES_H

#include "util.h"

enum Orientation { HorizontalTime, VerticalTime };

typedef double TrackPos;


struct Selection {
    int start_track;
    TrackPos start_pos;
    int end_track;
    TrackPos end_pos;
};


struct ZoomInfo {
    ZoomInfo() : offset(0), factor(1) {}
    TrackPos offset;
    // 1.0 means that each TrackPos gets 1 pixel.
    // 2.0 each TrackPos gets 2 pixels.
    // etc.
    double factor;
};


struct TextStyle {
    char *font;
    char *style;
    int size;
    Color color;
};

#endif
