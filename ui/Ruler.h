#ifndef __RULER_H
#define __RULER_H

#include <FL/Fl_Box.H>

#include "Track.h"

struct Mark {
    Mark(int width, Color color, char *name, double name_zoom_level,
            double zoom_level) :
        width(width), color(color), name(name),
        name_zoom_level(name_zoom_level), zoom_level(zoom_level)
    {}

    int width;
    Color color;
    char *name;
    double name_zoom_level;
    double zoom_level;
};

class RulerModel {
private:
    // mark list
};

const Color ruler_color = Color(116, 70, 0);

class RulerView : public TracklikeView {
public:
    RulerView(const RulerModel &ruler) : model(ruler), bg_box(0, 0, 1, 1) {
        bg_box.box(FL_THIN_DOWN_BOX);
        bg_box.color(color_to_fl(ruler_color));
        this->add(bg_box);
    }

private:
    const RulerModel &model;
    Fl_Box bg_box;
};

#endif
