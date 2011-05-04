#include <stdio.h>
#include <string.h>

#include <string>
#include <sstream>
#include <typeinfo>

#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>

#include "f_util.h"


const char *
show_key(int key)
{
    static char buf[32];
    if (isprint(key))
        sprintf(buf, "'%c'", key);
    else if (isprint(key < FL_KP_Last && key - FL_KP))
        sprintf(buf, "kp-%c", key - FL_KP);
    else if (isprint(FL_F <= key && key < FL_F_Last))
        sprintf(buf, "fn-%d", key - FL_F);
    else {
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
    const char *e = "unknown";
    switch (ev) {
    case FL_NO_EVENT: e = "nothing"; break;
    case FL_PUSH: e = "push"; break;
    case FL_DRAG: e = "drag"; break;
    case FL_RELEASE: e = "release"; break;
    case FL_MOVE: e = "move"; break;
    case FL_MOUSEWHEEL: e = "mousewheel"; break;
    case FL_ENTER: e = "enter"; break;
    case FL_LEAVE: e = "leave"; break;
    case FL_FOCUS: e = "focus"; break;
    case FL_UNFOCUS: e = "unfocus"; break;

    case FL_KEYDOWN: e = "keydown"; break;
    case FL_KEYUP: e = "keyup"; break;
    case FL_SHORTCUT: e = "shortcut"; break;
    case FL_DEACTIVATE: e = "deactivate"; break;
    case FL_ACTIVATE: e = "activate"; break;
    case FL_HIDE: e = "hide"; break;
    case FL_SHOW: e = "show"; break;
    }
    return e;
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
    memset(buf, '\0', sizeof buf);
    if (d & FL_DAMAGE_ALL)
        strcat(buf, "all, ");
    if (d & FL_DAMAGE_CHILD)
        strcat(buf, "child, ");
    if (d & FL_DAMAGE_EXPOSE)
        strcat(buf, "expose, ");
    if (d & FL_DAMAGE_SCROLL)
        strcat(buf, "scroll, ");
    if (d & FL_DAMAGE_OVERLAY)
        strcat(buf, "overlay, ");
    if (d & FL_DAMAGE_USER1)
        strcat(buf, "user1, ");
    if (d & FL_DAMAGE_USER2)
        strcat(buf, "user2, ");
    sprintf(buf+strlen(buf), "(%d)", d);
    buf[strlen(buf)] = '\0';
    return buf;
}


const char *
show_widget(const Fl_Widget *w)
{
    static char buf[127];
    IRect r = rect(*w);
    snprintf(buf, sizeof buf, "(%d %d %d %d) %s \"%s\" '%s'",
        r.x, r.y, r.w, r.h,
        typeid(*w).name(), w->label(), show_damage(w->damage()));
    return buf;
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
            if (!g->child(i)->visible())
                continue;
            for (int r = recurse; r; r--)
                out << "    ";
            out << 'c' << i << ": ";
            do_show_children(g->child(i), nlevels, recurse+1, out);
        }
    }
}


const char *
show_children(const Fl_Widget *w, int nlevels, int recurse)
{
    std::ostringstream out;
    static std::string outs;
    do_show_children(w, nlevels, recurse, out);
    outs = out.str();
    return outs.c_str();
}


void
print_children(const Fl_Widget *w, int nlevels, int recurse)
{
    printf("%s\n", show_children(w, nlevels, recurse));
}
