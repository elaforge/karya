#include <algorithm>
#include <FL/Fl_Window.H>

#include "util.h"
#include "f_util.h"

#include "MoveTile.h"

#define DEBUG(X) ;


/*
Differences from Fl_Tile:
When dragging, only resize the widget to the left.  The widgets to the right
get moved.

Some children may be marked as non-resizable.  They are never resized, and
their entire area is available for drag.

The callback is called when things are dragged.

*/


// Only resize widgets along the right and bottom edges.
// This is so that the tile proportions don't all grow when the widget grows.
void
MoveTile::resize(int x, int y, int w, int h)
{
    // Only resize the rightmost and bottommost widgets.  Shrink them down to 0
    // if necessary, but stop resizing children beyond that.
    Point edge(0, 0);
    for (int i = 0; i < this->children(); i++) {
        Rect c = rect(child(i));
        edge.x = std::max(edge.x, c.r());
        edge.y = std::max(edge.y, c.b());
    }
    for (int i = 0; i < this->children(); i++) {
        Rect c = rect(child(i));
        Rect new_c = c;
        if (c.r() == edge.x)
            new_c.w = std::max(0, (this->x() + w) - c.x);
        if (c.b() == edge.y)
            new_c.h = std::max(0, (this->y() + h) - c.y);
        if (new_c != c)
            child(i)->resize(new_c.x, new_c.y, new_c.w, new_c.h);
    }
    Fl_Widget::resize(x, y, w, h);
    this->init_sizes();
}


// unpack the crazy Fl_Group::sizes() array
// 0 1 2 3 - x r y b
// obj, resize rect, child 0, child 1, ...
Rect
MoveTile::original_box(int child)
{
    const short *p = this->sizes();
    p += 8 + child*4;
    return Rect(p[0], p[2], p[1] - p[0], p[3] - p[2]);
}


static int dist(int x, int y) { return abs(x-y); }

// Return true if y1 is closer to x than y2 is.
static bool closer(int x, int y1, int y2)
{
    return abs(x-y1) < abs(x-y2);
}

static void
set_cursor(Fl_Widget *widget, BoolPoint drag_state)
{
    static Fl_Cursor old_cursor;

    Fl_Cursor c;
    if (drag_state.x && drag_state.y)
        c = FL_CURSOR_MOVE;
    else if (drag_state.x)
        c = FL_CURSOR_WE;
    else if (drag_state.y)
        c = FL_CURSOR_NS;
    else
        c = FL_CURSOR_DEFAULT;

    if (c == old_cursor || !widget->window())
        return;
    old_cursor = c;
    widget->window()->cursor(old_cursor);
}


// This just sets up drag_state, it doesn't actually resize any children.
int
MoveTile::handle_move(int evt, BoolPoint &drag_state, Point &drag_from)
{
    Point mouse = mouse_pos();
    Point widget_edge(0, 0);

    // Set widget_edge to closest widget edge and drag_from to its according
    // original edge.  If drag_from.x is within the grab box, we're dragging.
    // Inside a stiff child counts as inside the grab box.
    // Since stiff children have area, I have to choose a direction
    // they drag since there's no way to figure it out.  I choose
    // horizontal.

    Rect tile_original_box = this->original_box(MoveTile::GROUP_SIZE);
    bool inside_stiff_child = false;
    for (int i = 0; i < this->children(); i++) {
        Rect child_box = rect(this->child(i));
        Rect child_original_box = this->original_box(i);

        // Only bother checking edges that are within the tile.
        // Resize the right and bottom edges.
        if (child_original_box.r() < tile_original_box.r()
                && closer(mouse.x, child_box.r(), widget_edge.x))
        {
            widget_edge.x = child_box.r();
            drag_from.x = child_original_box.r();
        }
        if (child_original_box.b() < tile_original_box.b()
                && closer(mouse.y, child_box.b(), widget_edge.y))
        {
            widget_edge.y = child_box.b();
            drag_from.y = child_original_box.b();
        }

        if (stiff_child(i)) {
            if (child_box.x <= mouse.x && mouse.x <= child_box.r())
                inside_stiff_child = true;
        }
    }
    if (dist(widget_edge.x, mouse.x) <= grab_area || inside_stiff_child) {
        drag_state.x = true;
        // drag_from.x stays the same
    } else {
        drag_state.x = false;
        drag_from.x = 0;
    }
    if (dist(widget_edge.y, mouse.y) <= grab_area) {
        drag_state.y = true;
    } else {
        drag_state.y = false;
        drag_from.y = 0;
    }

    set_cursor(this, drag_state);
    // DEBUG("state: " << drag_state << " drag_from: " << drag_from);
    if (drag_state.x || drag_state.y)
        return true; // I'm taking control now
    else
        return Fl_Group::handle(evt);
}

int
MoveTile::handle(int evt)
{
    static BoolPoint not_dragging(false, false);
    static BoolPoint drag_state = not_dragging;
    static Point drag_from(0, 0);

    Point mouse = mouse_pos();

    switch (evt) {
    case FL_MOVE: case FL_ENTER: case FL_PUSH:
        return handle_move(evt, drag_state, drag_from);

    case FL_LEAVE:
        drag_state = not_dragging;
        set_cursor(this, drag_state);
        break;

    case FL_DRAG: case FL_RELEASE: {
        ASSERT(drag_state.x || drag_state.y);
        Point drag_to(drag_state.x ? mouse.x : 0, drag_state.y ? mouse.y : 0);
        this->handle_drag_tile(drag_from, drag_to);
        if (evt == FL_DRAG)
            this->set_changed(); // this means "changed value" to a callback
        else if (evt == FL_RELEASE)
            this->init_sizes();
        do_callback();
        return 1;
    }
    }
    return Fl_Group::handle(evt);
}


static bool
child_wn_of(const Fl_Widget *c1, const Fl_Widget *c2)
{
    return c1->x() < c2->x() || c1->x() == c2->x() && c1->y() < c2->y();
}

// return indices of children going w->e, n->s
static const std::vector<int>
children_we_ns(Fl_Group *g)
{
    std::vector<Fl_Widget *> sorted(g->children());
    for (int i = 0; i < g->children(); i++)
        sorted[i] = g->child(i);
    std::sort(sorted.begin(), sorted.end(), child_wn_of);
    std::vector<int> indices(sorted.size());
    for (unsigned i = 0; i < sorted.size(); i++)
        indices[i] = g->find(sorted[i]);
    return indices;
}


static int
find_dragged_child(const std::vector<Rect> &original_boxes,
        const std::vector<int> &ordered, Point drag_from)
{
    int i = 0;
    for (; i < ordered.size(); i++) {
        if (original_boxes[ordered[i]].r() == drag_from.x)
            return i;
    }
    return i;
}


// Move neighbors to the right and down
// Doesn't actually resize any chidren, but modifies 'boxes' to the destination
// sizes and positions.
static void
jostle(const std::vector<Rect> &original_boxes, const std::vector<int> &ordered,
        std::vector<Rect> &boxes, Point drag_from, Point drag_to)
{
    DEBUG("jostle " << drag_from << " -> " << drag_to);
    Point shift(drag_to.x - drag_from.x, drag_to.y - drag_from.y);
    int i = find_dragged_child(original_boxes, ordered, drag_from);
    for (; i < ordered.size(); i++) {
        Rect &c = boxes[ordered[i]];
        if (c.r() != drag_from.x)
            break;
        DEBUG(ordered[i] << " resize from " << c.w << " -> " << c.w + shift.x);
        c.w += shift.x;
    }

    // continue to the right, pushing over children
    for (; i < ordered.size(); i++) {
        Rect &c = boxes[ordered[i]];
        if (c.x >= drag_from.x)
            c.x += shift.x;
        if (c.y >= drag_from.y)
            c.y += shift.y;

        if (c.x >= drag_from.x || c.y >= drag_from.y) {
            DEBUG(ordered[i] << " move by " << shift << " to " << c);
        }
    }
}


// This drag always drags from the original box point and only resets the sizes
// on mouse up.  An alternate approach would reset the drag from on every
// event.  This would leave widgets where they are.
void
MoveTile::handle_drag_tile(Point drag_from, Point drag_to)
{
    DEBUG("drag tile from " << drag_from << " to " << drag_to);
    // Resize and move according to hmove and vmove.
    // drag_from is always the *original* from point, i.e. this is always an
    // absolute action.
    // A 0 in drag_from mean no movement there.
    // Also, respect this->minimum_size.
    // TODO: except the the rightmost / bottommost widget, which resizes
    std::vector<Rect> original_boxes(this->children());
    for (unsigned i = 0; i < children(); i++)
        original_boxes[i] = this->original_box(i);

    std::vector<Rect> boxes(original_boxes.begin(), original_boxes.end());
    const std::vector<int> ordered = children_we_ns(this);

    /*
        if growing, jostle to right
        otherwise, for child in (from dragged to leftmost)
            jostle to min.x
    */
    Point shift(drag_to.x - drag_from.x, drag_to.y - drag_from.y);
    if (shift.x > 0) {
        // Going right is easy, just jostle over all children to the right.
        jostle(original_boxes, ordered, boxes, drag_from, drag_to);
    } else {
        // Going left is harder, go back to the left trying to shrink children
        // until I have enough space.
        int shrinkage = -shift.x;
        for (int i = find_dragged_child(original_boxes, ordered, drag_from);
                shrinkage > 0 && i >= 0; i--)
        {
            Rect child_box = boxes[ordered[i]];
            int shrink_to = std::max(this->minimum_size.x,
                    child_box.w - shrinkage);
            DEBUG(ordered[i] << " shrink " << child_box.w << "->" << shrink_to);
            jostle(original_boxes, ordered, boxes,
                    Point(child_box.r(), 0), Point(child_box.x + shrink_to, 0));
            shrinkage -= child_box.w - shrink_to;
        }
        ASSERT(shrinkage >= 0);
    }
    // ok, now what about y?

    for (unsigned i = 0; i < boxes.size(); i++) {
        const Rect r = boxes[i];
        DEBUG(i << ": " << original_boxes[i] << " -> " << boxes[i]);
        this->child(i)->resize(r.x, r.y, r.w, r.h);
        this->child(i)->redraw();
    }
}


void
MoveTile::set_stiff_child(int child)
{
    for (int i = this->stiff_children.size(); i < children(); i++) {
        this->stiff_children.push_back(false);
    }
    this->stiff_children[child] = true;
}


bool
MoveTile::stiff_child(int child)
{
    return child < this->stiff_children.size() && this->stiff_children[child];
}
