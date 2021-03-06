// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <stdlib.h>
#include <utility>
#include <vector>

#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Output.H>

#include "Color.h"
#include "geom.h"
#include "global.h"


// Show keycaps, which is just a bunch of boxes with text in them.
class Keycaps : public Fl_Widget {
public:
    struct Layout {
        Color bg_color;
        Color keycap_color; // Base keycap color.
        Color highlight_color; // Change keycap color on mouse over.
        Color label_color; // Color of labels_texts.
        Color binding_color; // Color of Binding::text.

        IRect *rects;
        int rects_len;

        IPoint *labels_points;
        const char **labels_texts;
        int labels_len;

        ~Layout() {
            free((void *) rects);
            free((void *) labels_points);
            for (int i = 0; i < labels_len; i++)
                free((void *) labels_texts[i]);
            free((void *) labels_texts);
        };
    };
    struct Binding {
        IPoint point;
        // Text to appear on the keycap, utf8 encoded.
        const char *text;
        // A longer description for the binding, utf8 encoded.
        const char *doc;
        // Replace Layout::keycap_color if != Color::black.
        Color color;

        Binding(IPoint point, const char *text, const char *doc, Color c) :
            point(point), text(text), doc(doc), color(c)
        {}
        ~Binding() {
            free((void *) text);
            free((void *) doc);
        };
    };

    Keycaps(int x, int y, int w, int h, const Layout *layout);
    ~Keycaps();
    // The bindings should be the same length and in the same order as
    // Layout::rects.
    void set_bindings(const std::vector<Binding *> &bindings);
    const char *highlighted() const;
    int handle(int evt) override;
    void handle_point(const IPoint pos);
protected:
    void draw() override;
private:
    const Layout *layout;
    // unique_ptr<const Layout> layout;
    std::vector<Binding *> bindings;
    // Index into Layout::rects of highlighted keycap, or -1 if none.
    int highlight_index;
};


class KeycapsWindow : public Fl_Double_Window {
public:
    KeycapsWindow(int x, int y, int w, int h, const char *title,
        const Keycaps::Layout *layout);
    void set_bindings(const std::vector<Keycaps::Binding *> &bindings);
    int handle(int evt) override;
    void handle_point(const IPoint pos) { keycaps.handle_point(pos); }
private:
    static void keycaps_cb(Fl_Widget *w, void *vp);

    Keycaps keycaps;
    Fl_Output doc;
};
