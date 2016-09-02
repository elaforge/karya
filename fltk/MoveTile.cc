// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <algorithm>
#include <FL/Fl_Window.H>

#include "f_util.h"
#include "util.h"

#include "MoveTile.h"

// #define DEBUG(X) ;


// Only resize widgets along the right and bottom edges.
// This is so that the tile proportions don't all grow when the widget grows.
void
MoveTile::resize(int x, int y, int w, int h)
{
    // DEBUG("resize " << f_util::rect(this) << " to " << IRect(x, y, w, h));
    // Only resize the rightmost and bottommost widgets.  Shrink them down to 1
    // if necessary, but stop resizing children beyond that.
    IPoint edge(0, 0);
    for (int i = 0; i < this->children(); i++) {
        IRect c = f_util::rect(child(i));
        edge.x = std::max(edge.x, c.r());
        edge.y = std::max(edge.y, c.b());
    }
    IPoint translate(x - this->x(), y - this->y());
    for (int i = 0; i < this->children(); i++) {
        IRect c = f_util::rect(child(i));
        IRect new_c = c;
        new_c.translate(translate);
        // Resize down to 1 pixel minimum, not 0.  0 width would make it
        // impossible to tell which widget was the right/bottom most.
        if (c.r() == edge.x)
            new_c.w = std::max(1, (this->x() + w) - c.x);
        if (c.b() == edge.y)
            new_c.h = std::max(1, (this->y() + h) - c.y);
        if (new_c != c) {
            // DEBUG("c" << i << f_util::rect(child(i)) << " to " << new_c);
            this->child(i)->resize(new_c.x, new_c.y, new_c.w, new_c.h);
        }
    }
    if (IRect(x, y, w, h) != f_util::rect(this)) {
        Fl_Widget::resize(x, y, w, h);
        this->init_sizes();
    }
}


void
MoveTile::remove_child(Fl_Widget *w)
{
    int c = this->find(w);
    if (c != this->children()) {
        this->remove(w);
        util::vector_erase(this->stiff_children, c);
    }
}


void
MoveTile::insert_child(Fl_Widget &w, int c)
{
    this->insert(w, c);
    this->stiff_children.insert(stiff_children.begin() + c, false);
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


int
MoveTile::handle(int evt)
{
    static BoolPoint drag_state(false, false);
    static IPoint drag_from(0, 0);

    // Let modified clicks through so a shift-click on a stiff child will
    // emit a msg.
    if (Fl::event_state() & (FL_SHIFT | FL_CTRL | FL_ALT | FL_META))
        return Fl_Group::handle(evt);
    IPoint mouse = f_util::mouse_pos();

    switch (evt) {
    case FL_MOVE: case FL_ENTER: case FL_PUSH: {
        int r = this->handle_move(evt, &drag_state, &this->dragged_child);
        if (drag_state.x)
            drag_from.x = mouse.x;
        if (drag_state.y)
            drag_from.y = mouse.y;
        if (drag_state.x || drag_state.y)
            ASSERT(0 <= this->dragged_child && dragged_child < children());
        return r;
    }

    case FL_LEAVE:
        drag_state = BoolPoint(false, false);
        set_cursor(this, drag_state);
        break;

    case FL_DRAG: case FL_RELEASE: {
        if (!drag_state.x && !drag_state.y) {
            return 0;
        }
        IPoint drag_to(drag_state.x ? mouse.x : 0, drag_state.y ? mouse.y : 0);
        this->handle_drag_tile(drag_from, drag_to, this->dragged_child);
        if (evt == FL_DRAG) {
            this->set_changed(); // this means "changed value" to a callback
        } else if (evt == FL_RELEASE) {
            this->init_sizes();
            // Unlike Fl_Tile, I only callback at the end of a drag.  This
            // means scrollbars don't get updated continuously, but I
            // only generate one event for a drag.
            do_callback();
            this->dragged_child = -1;
        }
        return 1;
    }
    }
    return Fl_Group::handle(evt);
}


void
MoveTile::drag_tile(IPoint drag_from, IPoint drag_to)
{
    BoolPoint drag_state;
    int dragged_child = this->find_dragged_child(drag_from, &drag_state);
    this->handle_drag_tile(drag_from, drag_to, dragged_child);
    this->init_sizes();
}


bool
MoveTile::stiff_child(int child)
{
    return static_cast<size_t>(child) < this->stiff_children.size()
        && this->stiff_children[child];
}


// unpack the crazy Fl_Group::sizes() array
// 0 1 2 3 - x r y b
// obj, resize rect, child 0, child 1, ...
IRect
MoveTile::original_box(int child)
{
    const int *p = this->sizes();
    p += 8 + child*4;
    return IRect(p[0], p[2], p[1] - p[0], p[3] - p[2]);
}


// sort_children /////////////

static bool
child_wn_of(const Fl_Widget *c1, const Fl_Widget *c2)
{
    return c1->x() < c2->x() || (c1->x() == c2->x() && c1->y() < c2->y());
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
    for (size_t i = 0; i < sorted.size(); i++)
        indices[i] = g->find(sorted[i]);
    return indices;
}


bool
MoveTile::sort_children()
{
    bool moved = false;
    const std::vector<int> ordered = children_we_ns(this);
    for (size_t i = 0; i < ordered.size(); i++) {
        if (i != (size_t) ordered[i]) {
            moved = true;
            this->insert(*this->child(i), i);
        }
    }
    return moved;
}



int
MoveTile::handle_move(int evt, BoolPoint *drag_state, int *dragged_child)
{
    *dragged_child = this->find_dragged_child(f_util::mouse_pos(), drag_state);
    // TODO Disable vertical drag for now.
    drag_state->y = false;
    set_cursor(this, *drag_state);
    // DEBUG("state: " << *drag_state << " child: " << *dragged_child);
    if (drag_state->x || drag_state->y)
        return true; // I'm taking control now
    else
        return Fl_Group::handle(evt);
}


// Move neighbors to the right and down
// Doesn't actually resize any chidren, but modifies 'boxes' to the destination
// sizes and positions.
// drag_from is relative to the boxes in 'boxes'.
// dragged_child should be the upper left most dragged child.
static void
jostle(std::vector<IRect> &boxes, const IPoint &tile_edge,
        IPoint drag_from, IPoint drag_to, int dragged_child)
{
    // DEBUG("jostle " << drag_from << " -> " << drag_to << " c"
    //         << dragged_child);
    IPoint shift(drag_to.x - drag_from.x, drag_to.y - drag_from.y);
    IRect dchild_box = boxes[dragged_child];
    size_t i = dragged_child;
    // Resize everyone lined up with the dragged child.
    for (; i < boxes.size() && boxes[i].r() == dchild_box.r(); i++) {
        // DEBUG(i << " resize from " << boxes[i].w
        //         << " -> " << boxes[i].w + shift.x);
        boxes[i].w += shift.x;
    }

    // Continue to the right, pushing over children, except the rightmost
    // track, which gets resized, unless it's already as small as it can get,
    // in which case, push over.  The minimum size is 1, not 0, so the
    // order of the children is still clear.
    // TODO this is error prone, I can let it reach 0 if I go by the rightmost
    // xpos, not the rightmost r()
    IPoint edge(0, 0);
    for (size_t j = 0; j < boxes.size(); j++)
        edge.x = std::max(edge.x, boxes[j].r());
    for (; i < boxes.size(); i++) {
        IRect &c = boxes[i];
        // DEBUG("box to right at x " << c.x);
        // drag_from is relative to the original positions, not the current
        // dragged positions.
        if (c.r() < edge.x) {
            // DEBUG(i << " move by " << shift << " from " << c.x << " to "
            //         << c.x + shift.x);
            // This relies on dragged_child being the upper left most.
            c.x += shift.x;
        } else {
            int new_x = c.x + shift.x;
            int new_r = std::max(tile_edge.x, new_x+1);
            // DEBUG(i << " outermost, (" << new_x << ", " << new_r << ")");
            c.x = new_x;
            c.w = new_r - new_x;
        }
        // TODO y drag
    }
}


// This drag always drags from the original box point and only resets the sizes
// on mouse up.  An alternate approach would reset the drag from on every
// event.  This would leave widgets where they are.
void
MoveTile::handle_drag_tile(const IPoint drag_from, const IPoint drag_to,
        int dragged_child)
{
    // DEBUG("drag tile from " << drag_from << " to " << drag_to);
    // drag_from is always the *original* from point, i.e. this is always an
    // absolute action.  That makes things easier here since otherwise
    // drag_from would be continually changing during a drag.  It also means
    // that widgets "remember" where they were as long as the mouse button is
    // down, which is possibly useful, possibly not.
    // A 0 in drag_from mean no movement there.
    // Also, respect this->minimum_size.
    // The right most / bottom most widget resizes instead of moving.
    // Stiff children don't resize at all.
    std::vector<IRect> original_boxes(this->children());
    for (int i = 0; i < children(); i++)
        original_boxes[i] = this->original_box(i);
    std::vector<IRect> boxes(original_boxes.begin(), original_boxes.end());

    // if growing, jostle to right
    // otherwise, for child in (from dragged to leftmost)
    //      jostle to min.x
    IPoint shift(drag_to.x - drag_from.x, drag_to.y - drag_from.y);
    IPoint tile_edges(this->x() + this->w(), this->y() + this->h());
    // DEBUG("shift is " << shift);
    if (shift.x > 0) {
        // Going right is easy, just resize the dragged child and jostle all
        // children to the right.  I know this child isn't stiff because
        // 'find_dragged_child' will have found the nearest leftwards unstiff
        // one.
        jostle(boxes, tile_edges, drag_from, drag_to, dragged_child);
    } else {
        // Going left is harder, go back to the left trying to shrink children
        // until I have enough space.
        int shrinkage = -shift.x;
        for (int i = dragged_child; shrinkage > 0 && i >= 0; i--) {
            // I need to shrink by the upper left most, so if there is someone
            // before me at the same x, I should skip.
            if (i > 0 && boxes[i-1].x == boxes[i].x)
                continue;
            IRect child_box = boxes[i];
            int shrink_to = std::max(this->minimum_size.x,
                    child_box.w - shrinkage);
            // Stiff children never change size.
            if (this->stiff_child(i))
                shrink_to = child_box.w;
            // DEBUG(i << show_widget(child(i)) << " shirk left " << shrinkage
            //         << " from " << child_box.w << "->" << shrink_to);
            if (child_box.w > shrink_to) {
                jostle(boxes, tile_edges, IPoint(child_box.r(), 0),
                        IPoint(child_box.x + shrink_to, 0),
                        i);
                shrinkage -= child_box.w - shrink_to;
            }
        }
        ASSERT(shrinkage >= 0);
        // DEBUG("shrink left " << shrinkage);
    }
    // TODO y drag

    for (size_t i = 0; i < boxes.size(); i++) {
        const IRect r = boxes[i];
        // DEBUG(i << ": " << original_boxes[i] << " -> " << boxes[i]);
        this->child(i)->resize(r.x, r.y, r.w, r.h);
        this->child(i)->redraw();
    }
}


static int dist(int x, int y) { return abs(x-y); }

// Find the upper left most child from drag_from, if any, and return its
// index.  Also return dragging status into drag_state.  If 'drag_from'
// doesn't indicate any child, return -1 and drag_state is (false, false).
//
// Since stiff children can't be resized, dragging a stiff child counts as
// dragging the nearest leftwards nonstiff one.  Kids can be so stubborn.
// TODO this only does x drag since that's all I need right now
int
MoveTile::find_dragged_child(IPoint drag_from, BoolPoint *drag_state)
{
    // Edges on or outside the tile never get dragged.
    IRect tile_box = this->original_box(MoveTile::GROUP_SIZE);
    *drag_state = BoolPoint(false, false);
    int prev_r = 0;
    int prev_nonstiff = -1;
    for (int i = 0; i < this->children(); i++) {
        IRect box = f_util::rect(this->child(i));
        // Handle one vertical block of children at a time.
        if (i > 0 && box.r() == prev_r)
            continue;

        bool in_bounds = box.r() < tile_box.r();
        bool grabbable = dist(drag_from.x, box.r()) <= this->grab_area;
        bool inside = box.x <= drag_from.x && drag_from.x <= box.r();
        if (in_bounds && (grabbable || (this->stiff_child(i) && inside))) {
            if (this->stiff_child(i)) {
                // You just can't drag if all left children are stiff.
                if (prev_nonstiff == -1) {
                    return -1;
                } else {
                    drag_state->x = true;
                    return prev_nonstiff;
                }
            } else {
                drag_state->x = true;
                return i;
            }
        }

        prev_r = box.r();
        if (!this->stiff_child(i))
            prev_nonstiff = i;
    }
    return -1;
}
