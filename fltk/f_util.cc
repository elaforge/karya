// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <stdio.h>
#include <string.h>

#include <string>
#include <sstream>
#include <typeinfo>

#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Input_.H>

#include "f_util.h"


namespace f_util {

const char *
show_key(int key)
{
    static char buf[32];
    if (key <= 0x7f) {
        if (isprint(key))
            snprintf(buf, sizeof buf, "'%c'", key);
        else
            snprintf(buf, sizeof buf, "<%d>", key);
    } else if (FL_KP <= key && key < FL_KP_Last && key - FL_KP) {
        snprintf(buf, sizeof buf, "KP%c", key - FL_KP);
    } else if (FL_F <= key && key < FL_F_Last) {
        snprintf(buf, sizeof buf, "FN%d", key - FL_F);
    } else {
        const char *e = "unknown";
        #define K(KEY, STR) case FL_##KEY: e = #STR; break
        switch (key) {
        K(Escape, escape);
        K(BackSpace, backspace);
        K(Tab, tab);
        K(Enter, enter);
        K(Print, print);
        K(Scroll_Lock, scroll_lock);
        K(Pause, pause);
        K(Insert, insert);
        K(Home, home);
        K(Page_Up, page_up);
        K(Delete, delete);
        K(End, end);
        K(Page_Down, page_down);
        K(Left, left);
        K(Right, right);
        K(Down, down);
        K(Shift_L, shift_l);
        K(Shift_R, shift_r);
        K(Control_L, control_l);
        K(Control_R, control_r);
        K(Caps_Lock, caps_lock);
        K(Alt_L, alt_l);
        K(Alt_R, alt_r);
        K(Meta_L, meta_l);
        K(Meta_R, meta_r);
        K(Menu, menu);
        K(Num_Lock, num_lock);
        K(KP_Enter, kp_enter);
        K(F + 1, f1);
        K(F + 2, f2);
        K(F + 3, f3);
        K(F + 4, f4);
        K(F + 5, f5);
        K(F + 6, f6);
        K(F + 7, f7);
        K(F + 8, f8);
        K(F + 9, f9);
        K(F + 10, f10);
        K(F + 11, f11);
        K(F + 12, f12);
        #undef K
        }
        return e;
    }
    return buf;
}


const char *
show_event(int ev)
{
    switch (ev) {
    case FL_NO_EVENT: return "nothing";
    case FL_PUSH: return "push";
    case FL_DRAG: return "drag";
    case FL_RELEASE: return "release";
    case FL_MOVE: return "move";
    case FL_MOUSEWHEEL: return "mousewheel";
    case FL_ENTER: return "enter";
    case FL_LEAVE: return "leave";
    case FL_FOCUS: return "focus";
    case FL_UNFOCUS: return "unfocus";

    case FL_KEYDOWN: return "keydown";
    case FL_KEYUP: return "keyup";
    case FL_SHORTCUT: return "shortcut";
    case FL_DEACTIVATE: return "deactivate";
    case FL_ACTIVATE: return "activate";
    case FL_HIDE: return "hide";
    case FL_SHOW: return "show";

    case FL_SCREEN_CONFIGURATION_CHANGED: return "screen configuration";
    }
    static char buf[127];
    snprintf(buf, 127, "unknown:%d", ev);
    return buf;
}

const char *
show_event_info(int ev)
{
    static char buf[128];
    switch (ev) {
    case FL_PUSH: case FL_DRAG: case FL_RELEASE: case FL_MOVE:
    case FL_MOUSEWHEEL:
        snprintf(buf, sizeof buf, "(%d, %d)", Fl::event_x(), Fl::event_y());
        break;
    case FL_KEYDOWN: case FL_KEYUP:
        // Don't bother with Fl::event_length() because 'buf' isn't returned
        // with an explicit length.
        snprintf(buf, sizeof buf, "%s (\"%s\")",
            show_key(Fl::event_key()), Fl::event_text());
        break;
    default:
        return "unknown";
        break;
    }
    return buf;
}


const char *
show_event_state(int state)
{
    static char buf[128];
    *buf = '\0';
    if (state & FL_SHIFT)
        strcat(buf, "shift+");
    if (state & FL_CAPS_LOCK)
        strcat(buf, "caps_lock+");
    if (state & FL_CTRL)
        strcat(buf, "ctrl+");
    if (state & FL_ALT)
        strcat(buf, "alt+");
    if (state & FL_NUM_LOCK)
        strcat(buf, "num_lock+");
    if (state & FL_META)
        strcat(buf, "meta+");
    if (state & FL_SCROLL_LOCK)
        strcat(buf, "scroll_lock+");
    if (state & FL_BUTTON1)
        strcat(buf, "button1+");
    if (state & FL_BUTTON2)
        strcat(buf, "button2+");
    if (state & FL_BUTTON3)
        strcat(buf, "button3+");
    if (strlen(buf) == 0)
        return "NONE";
    else
        buf[strlen(buf)-1] = '\0';
    return buf;
}


const char *
show_damage(uchar d)
{
    static char buf[1024];
    *buf = '\0';
    if (d & FL_DAMAGE_ALL)
        strcat(buf, "all+");
    if (d & FL_DAMAGE_CHILD)
        strcat(buf, "child+");
    if (d & FL_DAMAGE_EXPOSE)
        strcat(buf, "expose+");
    if (d & FL_DAMAGE_SCROLL)
        strcat(buf, "scroll+");
    if (d & FL_DAMAGE_OVERLAY)
        strcat(buf, "overlay+");
    if (d & FL_DAMAGE_USER1)
        strcat(buf, "user1+");
    if (d & FL_DAMAGE_USER2)
        strcat(buf, "user2+");
    if (*buf)
        buf[strlen(buf)-1] = '\0';
    sprintf(buf+strlen(buf), "(%d)", d);
    buf[strlen(buf)] = '\0';
    return buf;
}


const char *
show_widget(const Fl_Widget *w)
{
    std::ostringstream out;
    out << typeid(*w).name() << ": ";
    out << rect(*w);
    if (w->label())
        out << " label=" << '"' << w->label() << '"';
    const Fl_Input_ *input = dynamic_cast<const Fl_Input_ *>(w);
    if (input)
        out << " input=\"" << input->value() << '"';
    if (w->damage())
        out << " dmg=" << show_damage(w->damage());
    static std::string outs;
    outs = out.str();
    return outs.c_str();
}


std::ostream &
operator<<(std::ostream &os, const show_string &s)
{
    os << '"';
    for (const char *c = s.s; *c; c++) {
        if (*c == '"')
            os << "\\\"";
        else
            os << *c;
    }
    return os << '"';
}


void
print_widget(const Fl_Widget *w)
{
    printf("%s\n", show_widget(w));
}


static void
do_show_children(const Fl_Widget *w, int nlevels, int recurse,
        std::ostringstream &out)
{
    out << show_widget(w) << '\n';
    if (nlevels-- == 0)
        return;
    const Fl_Group *g = dynamic_cast<const Fl_Group *>(w);
    if (g) {
        for (int i = 0; i < g->children(); i++) {
            for (int r = recurse; r; r--)
                out << "    ";
            out << 'c' << i << (g->child(i)->visible() ? "" : " (invisible)")
                << ": ";
            do_show_children(g->child(i), nlevels, recurse+1, out);
        }
    }
}


const char *
show_children(const Fl_Widget *w, int nlevels)
{
    std::ostringstream out;
    static std::string outs;
    do_show_children(w, nlevels, 0, out);
    outs = out.str();
    return outs.c_str();
}


void
print_children(const Fl_Widget *w, int nlevels)
{
    printf("%s\n", show_children(w, nlevels));
}


Fl_Color
color_cycle()
{
    static const Fl_Color colors[] =
        { FL_RED, FL_GREEN, FL_YELLOW, FL_BLUE, FL_MAGENTA, FL_CYAN };
    static const int len = sizeof colors / sizeof(colors[0]);
    static int i = -1;
    i = (i+1) % len;
    return colors[i];
}


void
draw_rect(const IRect &rect, Color color)
{
    fl_color(color.fl());
    fl_rect(rect.x, rect.y, rect.w, rect.h);
}

void
draw_rectf(const IRect &rect, Color color)
{
    fl_color(color.fl());
    fl_rectf(rect.x, rect.y, rect.w, rect.h);
}

}
