#include <stdio.h>
#include <string.h>

#include <string>
#include <sstream>
#include <typeinfo>

#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>

#include "f_util.h"


// enum { DAMAGE_ZOOM = FL_DAMAGE_USER1 };


const char *
show_event(int ev)
{
    static char buf[1024];
    char *s;
    switch (ev) {
    case FL_NO_EVENT: return "nothing";
    case FL_PUSH: case FL_DRAG: case FL_RELEASE: case FL_MOVE:
        if (ev==FL_PUSH) s = "push";
        else if (ev == FL_DRAG) s = "drag";
        else if (ev == FL_RELEASE) s = "release";
        else if (ev == FL_MOVE) s = "move";
        sprintf(buf, "%s: %d (%d %d)", s, Fl::event_button(),
            Fl::event_x(), Fl::event_y());
        return buf;
    case FL_ENTER: return "enter";
    case FL_LEAVE: return "leave";
    case FL_FOCUS: return "focus";
    case FL_UNFOCUS: return "unfocus";

    case FL_KEYDOWN: case FL_KEYUP: case FL_SHORTCUT:
        if (ev == FL_KEYDOWN) s = "down";
        else if (ev == FL_KEYUP) s = "up";
        else s = "shortcut";
        sprintf(buf, "key%s: %.*s", s, Fl::event_length(), Fl::event_text());
        return buf;

    case FL_DEACTIVATE: return "deactivate";
    case FL_ACTIVATE: return "activate";
    case FL_HIDE: return "hide";
    case FL_SHOW: return "show";
    default:
        sprintf(buf, "unknown event: %d", ev);
        return buf;
    }
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
    if (d & FL_DAMAGE_OVERLAY)
        strcat(buf, "overlay, ");
    // if (d & DAMAGE_ZOOM)
    //  strcat(buf, "zoom, ");
    buf[strlen(buf)-2] = '\0';
    return buf;
}


const char *
show_widget(const Fl_Widget *w)
{
    static char buf[127];
    Rect r = rect(*w);
    snprintf(buf, sizeof buf, "(%d %d %d %d) %s \"%s\"", r.x, r.y, r.w, r.h,
        typeid(*w).name(), w->label());
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
