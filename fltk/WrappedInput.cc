// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <memory>
#include <FL/Fl.H>

#include "f_util.h"
#include "config.h"
#include "input_util.h"

#include "MsgCollector.h"
#include "WrappedInput.h"


enum {
    // Account for input widget padding.
    vertical_padding = 3,
    // The input widget has a few pixels of padding, so I should wrap a little
    // conservatively.
    horizontal_padding = 5
};


WrappedInput::WrappedInput(int x, int y, int w, int h, bool strip) :
    Fl_Multiline_Input(x, y, w, h), strip(strip)
{
    this->color(FL_WHITE);
    this->textsize(Config::font_size::input);
    this->box(FL_THIN_DOWN_BOX);
    // FL_WHEN_RELEASE is documented as firing whenever focus leaves the input.
    // But that's not true, it doesn't fire if the text hasn't changed.
    // So I have to call 'do_callback' on FL_UNFOCUS myself.
    this->when(0);
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
WrappedInput::set_text(const char *text)
{
    this->value(text);
    this->last_text = text;
    wrap_text();
    do_callback();
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
        if (evt == FL_KEYDOWN) {
            this->wrap_text();
            this->do_callback();
        }
    }
    if (evt == FL_UNFOCUS) {
        if (Fl::event_key() == FL_Escape) {
            this->value(last_text.c_str());
        } else {
            if (this->strip && input_util::strip_value(this))
                this->wrap_text();
        }
        this->do_callback();
        this->position(0);
    } else if (evt == FL_FOCUS) {
        this->last_text = this->value();
        this->do_callback();
    }
    return handled;
}

static bool
has_multiple_lines(const char *text, int size)
{
    for (int i = 0; i < size; i++) {
        if (text[i] == '\n')
            return true;
    }
    return false;
}

void
WrappedInput::draw()
{
    Fl_Multiline_Input::draw();
    if (has_multiple_lines(value(), size())
            && h() <= Config::View::track_title_height) {
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

bool
WrappedInput::wrap_text()
{
    // Just for memory management.
    std::auto_ptr<char> text_ptr(strdup(this->value()));
    char *text = &*text_ptr;
    char *end = text + strlen(text);
    bool changed = false;

    char *start_of_line = text;
    char *prev_space = nullptr;
    int max_width = this->w() - horizontal_padding;

    // DEBUG("wrap '" << text << "' " << (end - text) << " w " << max_width);

    // Yes, it's yet another wrapping algorithm.  Fortunately, this one is
    // much simpler than the one in SymbolTable.
    // * prev_space ^ space | start_of_line
    //      |
    // aaa bbb ccc
    // *  ^             < max_width
    // |  *   ^         > max_width
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
        // DEBUG("space " << space - text << " w " << w << " < " << max_width);
        if (w > max_width) {
            // One unbreakable word is longer than max_width, so I have to
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
    return changed;
}
