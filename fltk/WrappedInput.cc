// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <memory>
#include <FL/Fl.H>
#include <FL/fl_draw.H>

#include "MsgCollector.h"
#include "config.h"
#include "f_util.h"
#include "input_util.h"

#include "WrappedInput.h"


enum {
    // Account for input widget padding.
    vertical_padding = 3,
    // The input widget has a few pixels of padding, so I should wrap a little
    // conservatively.
    horizontal_padding = 5
};


WrappedInput::WrappedInput(
    int x, int y, int w, int h, bool strip, int max_width
) :
    Fl_Multiline_Input(x, y, w, h), strip(strip), max_width(max_width)
{
    color(FL_WHITE);
    textsize(Config::font_size::input);
    box(FL_THIN_DOWN_BOX);
    // FL_WHEN_RELEASE is documented as firing whenever focus leaves the input.
    // But that's not true, it doesn't fire if the text hasn't changed.
    // So I have to call 'do_callback' on FL_UNFOCUS myself.
    when(0);
}


void
WrappedInput::resize(int x, int y, int w, int h)
{
    int old_w = this->w();
    Fl_Multiline_Input::resize(x, y, w, h);

    // Only a horizontal change can affect wrapping.
    if (this->w() != old_w)
        this->wrap_text();
}

void
WrappedInput::set_max_width(int w)
{
    max_width = w;
    // Even if max_width didn't change, EventTrack::title_focused wants to
    // expand the size.
    wrap_text();
}

void
WrappedInput::set_text(const char *text)
{
    this->value(text);
    this->last_text = text;
    wrap_text();
}


const char *
WrappedInput::get_text() const
{
    static std::string unwrapped;

    unwrapped = this->value();
    for (size_t i = 0; i < unwrapped.length(); i++) {
        if (unwrapped[i] == '\n')
            unwrapped[i] = ' ';
    }
    return unwrapped.c_str();
}


int
WrappedInput::text_height() const
{
    int newlines = 0;
    for (const char *text = this->value(); *text; text++) {
        if (*text == '\n')
            newlines++;
    }
    fl_font(Config::font, Config::font_size::input);
    return (newlines + 1) * fl_height() + vertical_padding;
}


int
WrappedInput::suggested_width() const
{
    if (max_width == no_wrap || max_width == match_width)
        return w();
    double width = 0;
    const char *start = this->value();
    const char *end = this->value();
    fl_font(Config::font, Config::font_size::input);
    // Give one extra character of space, otherwise it wants to wrap.
    const static int extra_space = fl_width('m');

    // This prefers to wrap on |s, so measure the distance between each |.
    for (;;) {
        while (*end != '\0' && *end != '|')
            end++;
        if (*end == '|')
            end++;
        if (start == end)
            break;
        // DEBUG("max: " << max_width << " width: " <<
        //     extra_space + fl_width(start, end - start));
        width = std::max(width, extra_space + fl_width(start, end - start));
        width = std::min(double(max_width), width);
        start = end;
    }
    return std::max(w(), int(ceil(width)));
}


static bool
is_first_line(const char *text, int i)
{
    i--; // The end of the first line winds up being on a \n.
    for (; i > 0; i--) {
        if (text[i] == '\n')
            return false;
    }
    return true;
}

static bool
is_last_line(const char *text, int size, int i)
{
    for (; i < size; i++) {
        if (text[i] == '\n')
            return false;
    }
    return true;
}


int
WrappedInput::handle(int evt)
{
    if (evt == FL_MOVE)
        return 0;
    // if (evt != FL_NO_EVENT)
    //     DEBUG("input: " << f_util::show_widget(this)
    //         << ": " << f_util::show_event_info(evt));
    // This is a crazy delicate mess because I have to apply my own key
    // bindings but fall back on the Fl_Multiline_Input ones otherwise.
    if (evt == FL_KEYUP) {
        // If this is an edit input created in response to a keystroke, it gets
        // focus immediately and the keyup will wind up here.  So I have to
        // make sure the MsgCollector gets it.
        MsgCollector::get()->key_up(Fl::event_key());
    }
    if (input_util::should_ignore(evt))
        return 0;
    bool handled = input_util::handle(this, evt, true);
    if (!handled) {
        // Jump to the beginning of the text if you try to go up from first
        // line, and similar for the last line.
        if (evt == FL_KEYDOWN) {
            switch (Fl::event_key()) {
            case FL_Up:
                if (is_first_line(value(), position())) {
                    this->position(0, Fl::event_state(FL_SHIFT) ? mark() : 0);
                    handled = true;
                }
                break;
            case FL_Down:
                if (is_last_line(value(), size(), position())) {
                    this->position(
                        size(), Fl::event_state(FL_SHIFT) ? mark() : size());
                    handled = true;
                }
                break;
            }
        }
    }
    if (!handled) {
        handled = Fl_Multiline_Input::handle(evt);
    }
    if (evt == FL_KEYDOWN) {
        this->wrap_text();
        int key = Fl::event_key();
        if (key == FL_Escape || key == FL_Enter || key == FL_Tab) {
            if (key == FL_Escape) {
                this->value(last_text.c_str()); // Revert text.
            } else {
                if (this->strip)
                    input_util::strip_value(this);
            }
            Fl::focus(this->window());
        } else if (suggested_width() != w() || text_height() != h()) {
            this->do_callback();
        }
    }
    if (evt == FL_UNFOCUS) {
        this->do_callback();
    } else if (evt == FL_FOCUS) {
        // Save for possible later revert.
        this->last_text = this->value();
        this->do_callback();
    }
    return handled;
}

void
WrappedInput::update_size()
{
    size(this->suggested_width(), this->text_height());
}


void
WrappedInput::draw()
{
    Fl_Multiline_Input::draw();
    // if there is >1 line, or the line >w()
    fl_font(Config::font, Config::font_size::input);
    int pad = 3; // fl_width seems to guess a bit low.

    // If it has \n it must be focused and hence expanded, or is the block
    // title, so no abbreviation.  The fl_width is only valid without \n but we
    // clear those out on unfocus.
    bool abbreviated =
        strchr(value(), '\n') == nullptr && fl_width(value()) + pad > w();
    if (abbreviated) {
        // Indicate that there is hidden text.
        fl_color(Config::abbreviation_color.fl());
        fl_rectf(x(), y() + h() - 3, w(), 3);
    }
}


static char *
find_space(char *s, char *end)
{
    char *space = strchr(s, ' ');
    if (space == nullptr)
        space = end;
    char *nl = strchr(s, '\n');
    if (nl == nullptr)
        nl = end;
    return std::min(space, nl);
}

void
WrappedInput::wrap_text()
{
    if (this->max_width == no_wrap)
        return;
    // Just for memory management.
    std::unique_ptr<char> text_ptr(strdup(this->value()));
    char *text = &*text_ptr;
    char *end = text + strlen(text);
    bool changed = false;

    char *start_of_line = text;
    char *prev_space = nullptr;
    const int max_w =
        std::max(this->w(), this->suggested_width()) - horizontal_padding;

    // DEBUG("wrap '" << text << "' " << (end - text) << " w " << max_w);

    // Yes, it's yet another wrapping algorithm.  Fortunately, this one is
    // much simpler than the one in SymbolTable.
    // * prev_space ^ space | start_of_line
    //      |
    // aaa bbb ccc
    // *  ^             < max_w
    // |  *   ^         > max_w
    // aaa\n
    // bbb ccc
    // |* ^
    // |  *   ^
    // aaa
    // bbb
    // ccc
    // |* ^
    //
    //    |
    // aaabbbccc abc
    // |*       ^
    // |        ^   *
    //          |
    fl_font(Config::font, Config::font_size::input);
    for (;;) {
        // +1 to skip the space.
        char *space = find_space(
            prev_space ? prev_space + 1 : start_of_line, end);
        double w = fl_width(start_of_line, space - start_of_line);
        // DEBUG("space " << space - text << " w " << w << " < " << max_w);
        if (w > max_w) {
            // One unbreakable word is longer than max_w, so I have to
            // break at the soonest space.
            if (!prev_space)
                prev_space = space;
            // Ran out of text!
            if (!*prev_space)
                break;

            if (*prev_space == ' ') {
                // DEBUG("space to nl: " << prev_space - text);
                *prev_space = '\n';
                changed = true;
            }
            start_of_line = prev_space + 1;
            prev_space = nullptr;
        } else {
            if (prev_space && *prev_space == '\n') {
                // DEBUG("nl to space: " << prev_space - text);
                *prev_space = ' ';
                changed = true;
            }
            prev_space = space;
            // Ran out of text with room to spare.
            if (!*prev_space)
                break;
        }
    }

    if (changed) {
        int pos = this->position();
        int mark = this->mark();
        this->value(text);
        // Setting value will destroy the selection, so restore it.
        this->position(pos, mark);
    }
    return;
}
