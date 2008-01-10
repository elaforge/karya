#include <vector>

#include <FL/Fl_Box.H>
#include <FL/Fl_Group.H>

#include "marklist.h"

#include "zoom.h"
#include "trackpos.h"

#include "futil.h"

/*


*/

class Overlay_ruler : public Fl_Group {
public:
	Overlay_ruler(int X, int Y, int W, int H, Orientation o) :
		Fl_Group(X, Y, W, H), o(o),
		show_names(true)
	{}
	void resize(int X, int Y, int W, int H);
	void draw();
	void orientation(Orientation no) {
		o = no;
		damage(FL_DAMAGE_ALL);
	}
	void zoom(Zoom z) {
		if (z != _zoom) {
			// damage(DAMAGE_ZOOM);
			_zoom = z;
		}
	}
	void scroll(Trackpos p) {
		if (p != offset) {
			damage(FL_DAMAGE_SCROLL);
			offset = p;
		}
	}
	void insert_marklist(int i, const Marklist *m) {
		i = int(container_clamp(_marklists, i));
		_marklists.insert(_marklists.begin()+i, m);
		damage(FL_DAMAGE_ALL);
	}
	void remove_marklist(int i) {
		i = int(container_clamp(_marklists, i));
		_marklists.erase(_marklists.begin()+i);
		damage(FL_DAMAGE_ALL);
	}
protected:
	virtual void draw_area(Rect c);
	virtual Rect mark_rect(const Mark *m, int xoffset, int i) const;
	void draw_marks(Rect d);
	void draw_mark(const Mark *mark, int xoffset, int i);
	/*
	int to_x(Trackpos p) const {
		return (p - offset).to_screen(_zoom) + o.x(this);
	}
	*/
	// int to_w(Trackpos p) const { return p.to_screen(_zoom); }
	Orientation o;
	std::vector<const Marklist *> _marklists;
	Zoom _zoom;
	Trackpos offset;
	int pixel_offset;
	bool show_names;
private:
	static void draw_area_cb(void *vp, int x, int y, int w, int h);
};

class Selection_ruler : public Overlay_ruler {
public:
	Selection_ruler(int X, int Y, int W, int H, Orientation o,
			int cursor_width) :
		Overlay_ruler(X, Y, W, H, o),
		cursor_width(cursor_width)
	{}
	void selection(const Trange r) {
		_selection.selection(r);
		selection_damage(_selection);
	}
	void clear_selection() {
		_selection.selection(false);
		selection_damage(_selection);
	}
	void selection_color(Fl_Color c) {
		_selection.color = c;
		damage(FL_DAMAGE_ALL); // XXX just damage selection area
	}
protected:
	void draw_area(Rect c);
	void selection_damage(Selection<Trackpos> &sel);
	void draw_selection(const Selection<Trackpos> &sel);
	void draw_cursor(const Selection<Trackpos> &sel);
	Selection<Trackpos> _selection;
private:
	int cursor_width;
};

class Ruler : public Selection_ruler {
public:
	Ruler(int X, int Y, int W, int H, Orientation o) :
		Selection_ruler(X, Y, W, H, o, 2),
		background(X, Y, W, H),
		_mouse_pos(-1)
	{
		end();
		background.box(FL_FLAT_BOX);
	}
	void color(Fl_Color c) { background.color(c); }
	// draw line based on position of mouse
	void mouse_pos(Point p);
protected:
	void draw_area(Rect c);
	Rect mark_rect(const Mark *m, int xoffset, int i) const;
private:
	Rect mouse_pos_rect() const {
		return o.rect(_mouse_pos - pixel_offset, o.y(this), 2, o.h(this));
	}
	void draw_mouse_pos();
	Fl_Box background;
	int _mouse_pos;
};
