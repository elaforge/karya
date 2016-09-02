// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>
#include <FL/Fl_Input.H>
#include <ctype.h>

#include "MsgCollector.h"
#include "f_util.h"
#include "utf8.h"

#include "input_util.h"


namespace input_util {

static const Color focus_color(255, 240, 220);

// First skip trailing spaces, then skip a token.  A token is a parenthesized
// expression, from a ` to the next `, or until a space.
static const char *
backward_token(const char *start, const char *pos)
{
    const char *p = utf8::backward(pos, start);
    while (p > start && *p == ' ')
        p = utf8::backward(p, start);
    if (*p == ')') {
        int parens = 1;
        do {
            p = utf8::backward(p, start);
            if (*p == '(')
                parens--;
            else if (*p == ')')
                parens++;
            if (parens == 0)
                break;
        } while (p > start);
        // Unbalanced parens, so just go back one character.
        if (parens)
            p = utf8::backward(pos, start);
    } else if (*p == '`') {
        do {
            p = utf8::backward(p, start);
            if (*p == '`')
                break;
        } while (p > start);
        // Unbalanced backticks, so just go back one character.
        if (*p != '`')
            p = utf8::backward(pos, start);
    } else {
        while (p > start && *p != ' ')
            p = utf8::backward(p, start);
        if (*p == ' ')
            p = utf8::forward(p, pos);
    }
    return p;
}

// Skip a token, and then any trailing spaces.  A token has the same definition
// as in 'backward_token'.
static const char *
forward_token(const char *end, const char *pos)
{
    const char *p = pos;
    if (p == end)
        return p;
    if (*p == '(') {
        int parens = 1;
        do {
            p = utf8::forward(p, end);
            if (*p == '(')
                parens++;
            else if (*p == ')')
                parens--;
            if (parens == 0)
                break;
        } while (p < end);
        // Unbalanced parens, so just go forward one character.
        if (parens)
            p = utf8::forward(pos, end);
        else
            p = utf8::forward(p, end);
    } else if (*p == '`') {
        do {
            p = utf8::forward(p, end);
            if (*p == '`')
                break;
        } while (p < end);
        // Unbalanced backticks, so just go forward one character.
        if (*p != '`')
            p = utf8::forward(pos, end);
        else
            p = utf8::forward(p, end);
    } else {
        while (p < end && *p != ' ')
            p = utf8::forward(p, end);
    }
    while (p < end && *p == ' ')
        p = utf8::forward(p, end);
    return p;
}

static void
move_backward(Fl_Input *w, bool shift)
{
    const char *text = w->value();
    const char *p = backward_token(text, text + w->position());
    if (shift)
        w->position(p - text, w->mark());
    else
        w->position(p - text);
}


static void
move_forward(Fl_Input *w, bool shift)
{
    const char *text = w->value();
    const char *p = forward_token(text + w->size(), text + w->position());
    if (shift)
        w->position(p - text, w->mark());
    else
        w->position(p - text);
}


static void
backspace_token(Fl_Input *w)
{
    const char *text = w->value();
    const char *p = backward_token(text, text + w->position());
    w->replace(p - text, w->position(), nullptr);
}

bool
should_ignore(int evt)
{
    if (evt == FL_KEYDOWN || evt == FL_KEYUP) {
        switch (Fl::event_key()) {
        case FL_Shift_L: case FL_Shift_R: case FL_Enter: case FL_Escape:
        case FL_Right: case FL_Left: case FL_Up: case FL_Down:
        case FL_BackSpace: case FL_Tab:
            break; // some non-prinables are handled here
        default:
            // but control chars and the like should be passed out
            if (!isprint(Fl::event_key()))
                return true;
        }
    }
    return false;
}

bool
handle(Fl_Input *input, int evt, bool multiline)
{
    int state = Fl::event_state();
    bool handled = false;
    switch (evt) {
    case FL_KEYDOWN:
        switch (Fl::event_key()) {
        case FL_Tab: case FL_Enter: case FL_Escape:
            Fl::focus(input->window());
            handled = true;
            break;
        case FL_Up:
            if (multiline)
                return false;
            else
                input->position(0);
            handled = true;
            break;
        case FL_Down:
            if (multiline)
                return false;
            else
                input->position(input->size());
            handled = true;
            break;
        case 'h':
            if (state & (FL_META | FL_CTRL)) {
                move_backward(input, state & FL_SHIFT);
                handled = true;
            }
            break;
        case 'l':
            if (state & (FL_META | FL_CTRL)) {
                move_forward(input, state & FL_SHIFT);
                handled = true;
            }
            break;
        case FL_BackSpace:
            if (state & (FL_SHIFT | FL_META | FL_CTRL)) {
                backspace_token(input);
                handled = true;
            }
            break;
        }
        break;
    case FL_KEYUP:
        // Eat keyups if I have focus.
        handled = true;
        break;
    case FL_FOCUS:
        input->color(focus_color.fl());
        input->redraw();
        // Don't set handled, because Fl_Input still needs to get this.
        break;
    case FL_UNFOCUS:
        input->color(FL_WHITE);
        // So inputs consistently display the same part of text.
        // input->position(9999);
        input->redraw();
        break;
    }
    return handled;
}


bool
strip_value(Fl_Input *w)
{
    // Fl_Input manages the storage.
    const char *s = w->value();

    // Why am I still writing functions like this?
    int start = 0, end = strlen(s);
    if (!((end > 0 && isspace(s[0])) || isspace(s[end-1])))
        return false;
    while (start < end && isspace(s[start]))
        start++;
    while (end > start && isspace(s[end-1]))
        end--;
    if (end - start <= 0)
        w->value(nullptr);
    else
        w->value(s + start, end - start);
    return true;
}

};
