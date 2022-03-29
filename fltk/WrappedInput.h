// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <FL/Fl_Multiline_Input.H>

#include "global.h"


/*
    A customized Fl_Multiline_Input that wraps its text.

    Every time the text changes, figure out word wrapping, and insert newlines
    as apporpriate.  This only breaks on spaces, and newlines are not allowed
    in the input, so I can get unwrapped text back by converting newlines to
    spaces.

    The callback is called on the initial focus, on each size change, and
    on unfocus.  If the size changed, 'text_height' will return its new height
    and the parent should resize it appropriately.  If it's drawing on top of
    another widget, that widget has to redraw too.

    Since fltk has just the single callback argument which is used up imitating
    a closure, we have to infer why the callback was called by looking in
    Fl::event().

    WrappedInput is used in 3 places, each one a bit different:

    TrackTile::floating_input:
        focus -> nothing
        resize -> redraw TrackTile
        unfocus -> send msg, delete widget
    EventTrack::title_input (callback to TrackTile via Track::title_widget):
        focus -> expand size if necessary
        resize -> redraw TrackTile
        unfocus -> send msg, contract to track size
    Block::title
        focus -> nothing
        resize -> update Block widget positions, redraw Block
        unfocus -> send msg

    TODO: since there are 3 different purposes, maybe there should be an
    explicit enumeration saying what it's used for, because trying to serve
    everyone leads to some pretty ad-hoc semantics.
*/
class WrappedInput : public Fl_Multiline_Input {
public:
    // Pass max_width=0 to disable resizing.
    WrappedInput(int x, int y, int w, int h, bool strip, int max_width);
    enum {
        // Disable wrapping entirely.  This is for collapsed track titles.
        no_wrap = -1,
        // Always suggest the same width as the widget actually is.  This is
        // for block titles, which are resized by their window, not in response
        // to their contents.
        match_width = 0
    };
    void resize(int x, int y, int w, int h) override;
    // Must be mutable so TrackTile can set for EventTrack::title_input.
    void set_max_width(int w);
    // Use this to set newline-free unwrapped text.
    void set_text(const char *text);
    void unwrap();
    // True if the text has changed since the last time this widget got focus.
    // This is used to detect if the edit's changes were reverted by the escape
    // key.
    bool text_changed() const {
        return last_text != value();
    }
    int text_height() const;
    int suggested_width() const;
    int handle(int evt) override;
    void update_size();

protected:
    void draw() override;

private:
    void wrap_text();
    // Strip spaces from the text on unfocus.
    const bool strip;
    // Don't let suggested_width expand past this.  If 0, suggested_width()
    // always returns w().
    int max_width;
    // Keep the previous text, to revert on escape.
    std::string last_text;
};
