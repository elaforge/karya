/*
XXX
*/

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Tile.H>

#include <FL/Fl_Box.H>
#include <FL/Fl_Pack.H>

#include <FL/fl_draw.H>

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "util.c"

static Point event_pos()
{
	return Point(Fl::event_x(), Fl::event_y());
}

enum Orientation { Horizontal_time, Vertical_time };

class Zoom : public Fl_Scroll {
	// possibly draw zoom-center overlay?
public:
	Zoom(int x, int y, int w, int h, char *label = 0) :
		Fl_Scroll(x, y, w, h, label),
		do_zoomx(false), do_zoomy(false),
		cur_zoom(1, 1),
		magnification_speed(100, 100),
		shift_down(0), zoom_button(1), zooming(false),
		_csizes(0), _max_size(0, 0)
	{
		type(0);
	};
	void zoom_add(Fl_Widget *w);
	~Zoom() { if (_csizes) delete[] _csizes; }
	void zoom(double zx, double zy, Point center, Dpoint scaled_center);
	void zoom(double zx, double zy, Point center);
	double zoomx() const { return cur_zoom.x; }
	double zoomy() const { return cur_zoom.y; }
	bool is_zooming() const { return zooming; }
	bool do_zoomx, do_zoomy;
	virtual void resize(int, int, int, int);
	Point max_size() {
		csizes();
		return _max_size;
	};
protected:
	virtual int handle(int ev);
	// virtual void draw();
private:
	void drag_zoom(int dx, int dy);
	Drect *csizes();

	Dpoint cur_zoom;
	Dpoint old_zoom; // to undo zoom if a zoom drag is cancelled
	Point last_event_pos;
	Point zoom_center;
	Dpoint scaled_center;

	Dpoint magnification_speed;
	int shift_down, zoom_button;
	bool zooming;
	Drect *_csizes;
	// bounding box of all the children, to avoid zooming in on childless
	// areas
	Point _max_size;
};

bool is_zooming_r(const Fl_Widget *w)
{
	const Zoom *z;
	for (; w; w = w->parent()) {
		if ((z = dynamic_cast<const Zoom *>(w)) && z->is_zooming())
			return true;
	}
	return false;
}

Drect *Zoom::csizes()
{
	// XXX hackery: changes to the children will cause sizes to change via
	// init_sizes, probably getting a new address.
	// Fl_Group already has sizes, but it can't be used because it doesn't
	// record the zoom factor the sizes were recorded at
	// resize(x(), y(), w(), h());
	static short *last_sizes;
	if (sizes() == last_sizes)
		return _csizes;
	if (_csizes)
		delete[] _csizes;
	_csizes = new Drect[children() - 2];

	int dx = xposition(), dy = yposition();
	Fl_Widget *const *a = array();
	_max_size = Point(0, 0);
	for (int i = 0; i < children() - 2; i++) {
		Fl_Widget *c = a[i];
		double x = c->x(), y = c->y(), w = c->w(), h = c->h();
		_csizes[i] = Drect((x+dx) / zoomx(), (y+dy) / zoomy(),
				w / zoomx(), h / zoomy());
		_max_size.x = max(_max_size.x, int(_csizes[i].x + _csizes[i].w));
		_max_size.y = max(_max_size.y, int(_csizes[i].y + _csizes[i].h));
		// printf("child: %g %g %g %g\n", _csizes[i].x, _csizes[i].y,
		// 		_csizes[i].w, _csizes[i].h);
	}
	last_sizes = sizes();
	return _csizes;
}

int Zoom::handle(const int ev)
{
	const int key = Fl::event_key();
	const int button = Fl::event_button();
	switch (ev) {
	case FL_FOCUS:
		Fl_Scroll::handle(ev);
		return true;
	case FL_KEYDOWN:
		if ((key == FL_Shift_L || key == FL_Shift_R) && !shift_down
				&& (do_zoomx || do_zoomy))
		{
			shift_down = key;
			if (do_zoomx && do_zoomy)
				fl_cursor(FL_CURSOR_MOVE);
			else if (do_zoomx)
				fl_cursor(FL_CURSOR_WE);
			else if (do_zoomy)
				fl_cursor(FL_CURSOR_NS);
			return true;
		}
		break;
	case FL_KEYUP:
		if (shift_down && key == shift_down) {
			shift_down = 0;
			fl_cursor(FL_CURSOR_DEFAULT);
			return true;
		}
		break;
	case FL_PUSH:
		if (button == zoom_button && shift_down) {
			zoom_center = event_pos();
			scaled_center = Dpoint((zoom_center.x + xposition()) / zoomx(),
					(zoom_center.y + yposition()) / zoomy());
			old_zoom = cur_zoom;
			last_event_pos = zoom_center;
			zooming = true;
			return true;
		} else if (zooming) {
			// since we're zooming, zoom_button must be down
			assert(button != zoom_button);
			// cancel zoom
			zoom(old_zoom.x, old_zoom.y, zoom_center, scaled_center);
			zooming = false;
			return true;
		}
		break;
	case FL_RELEASE:
		if (zooming) {
			zooming = false;
			return true;
		}
		break;
	case FL_DRAG:
		if (zooming) {
			int dx = Fl::event_x() - last_event_pos.x;
			int dy = Fl::event_y() - last_event_pos.y;
			last_event_pos = event_pos();
			drag_zoom(dx, dy);
			zoom(do_zoomx ? zoomx() + (dy / magnification_speed.x) : 1,
					do_zoomy ? zoomy() + (dx / magnification_speed.y) : 1,
					zoom_center, scaled_center);
			return true;
		}
		break;
	case FL_ENTER:
		if (shift_down && !Fl::get_key(shift_down)) {
			shift_down = 0;
			fl_cursor(FL_CURSOR_DEFAULT);
		}
	}
	if (shift_down)
		return true; // no one else gets events in a zoom
	else
		return Fl_Scroll::handle(ev);
}

void Zoom::resize(int X, int Y, int W, int H)
{
	// printf("new size: %d %d %d %d\n", x, y, w, h);
	// w changes do no resizing
	// h changes resize only if it gets larger
	// Fl_Group::resize(X, Y, W, H);
	Fl_Scroll::resize(X, Y, W, H);
	printf("zoom size: %d %d %d %d\n", this->x(), this->y(), this->w(), this->h());
	print_children(this);

	/*
	for (int i = 0; i < children(); i++) {
		Fl_Widget *w = child(i);
		w->resize
	}
	*/
}

void Zoom::drag_zoom(int dx, int dy)
{
	double zx = do_zoomx ? zoomx() + (dy / magnification_speed.x) : 1;
	double zy = do_zoomy ? zoomy() + (dx / magnification_speed.y) : 1;
	zoom(zx, zy, zoom_center, scaled_center);
}

// zoom by (zx, zy) around scaled_center, which is located at center in the
// window.  zoom() needs both to keep the view in place when zooming multiple
// times around a point.
void Zoom::zoom(double zx, double zy, Point center, Dpoint scaled_center)
{
	zx = max(0.1, zx);
	zy = max(0.1, zy);

	int xp = xposition(), yp = yposition();
	Drect *sz = csizes();
	for (int i = 0; i < children() - 2; i++, sz++) {
		child(i)->resize(int(sz->x * zx - xp),
				int(sz->y * zy - yp),
				int(sz->w * zx),
				int(sz->h * zy));
	}

	// XXX it would probably be better to actually make above-left of
	// widgets draw bg correctly
	int nx = int(scaled_center.x * zx - center.x);
	int ny = int(scaled_center.y * zy - center.y);
	// avoid zooming in on area outside of the original dimensions
	// but prefer to show empty area to the right and bottom on zoom out
	Point m = max_size();
	// nx = min(int(w() * zx - w()), nx);
	nx = min(int(m.x * zx - w()), nx);
	// printf("%d, %d\n", int(w() * zx - w()), int(m.x * zx - w()));
	nx = max(0, nx);
	ny = min(int(h() * zy - h()), ny);
	ny = max(0, ny);
	position(nx, ny);
	cur_zoom.x = zx;
	cur_zoom.y = zy;
	do_callback();
	damage(FL_DAMAGE_ALL);
}

void Zoom::zoom(double zx, double zy, Point center)
{
	zoom(zx, zy, center, Dpoint((center.x + xposition()) / zoomx(),
					(center.y + yposition()) / zoomy()));
}

class Track : public Fl_Box {
public:
	Track(int x, int y, int w, int h, char *lab = 0) :
		Fl_Box(x, y, w, h)
	{
		static int n = 0;
		char buf[16];
		sprintf(buf, "track %d", n);
		label(strdup(buf));
		n++;
		color(fl_rgb_color(0xff, 0xff, 0xf0));
		box(FL_DOWN_BOX);
	};
};

class Track_tile : public Fl_Tile {
public:
	Track_tile(int X, int Y, int W, int H, Orientation o);
	virtual void resize(int X, int Y, int W, int H);
protected:
	virtual void draw();
private:
	Fl_Box fill;
	Orientation orientation;
};

Track_tile::Track_tile(int X, int Y, int W, int H, Orientation o) :
	Fl_Tile(X, Y, W, H, "tracks"),
	fill(X, Y, W, H, "fill"),
	orientation(o)
{
	// fill.color(FL_GRAY);
	fill.color(FL_CYAN);
	fill.box(FL_FLAT_BOX);
}

void Track_tile::resize(int X, int Y, int W, int H)
{
}

#define H_OR_V(x, y) (orientation == Horizontal_time ? x : y)
void Track_tile::draw()
{
	// make sure the fill is the last child
	// then 
	if (child(children()-1) != &fill) {
		insert(fill, children());
		int n = 0;
		for (int i = 0; i < children()-1; i++)
			n = max(n, H_OR_V(child(i)->y() + child(i)->h(),
						child(i)->x() + child(i)->w()));
		if (n > h())
			fill.hide();
		else {
			fill.set_visible();
			H_OR_V(fill.resize(0, n, w(), 5000),
					fill.resize(n, 0, 5000, h()));
		}
	}
	Fl_Tile::draw();
}

class Block : public Fl_Group {
public:
	Block(int x, int y, int w, int h, char *label, Orientation o);
	void new_track();
	bool remove_track(int n);
	const Orientation orientation;
	int default_track_sz;
private:
	static void update_time_sb(Fl_Widget *w, void *);
	static void time_sb_cb(Fl_Widget *w, void *);
	const int sb_sz;

	// Fl_Box block_label;
	Fl_Scrollbar time_sb, track_sb;
	Zoom zoom;
	Track_tile tracks;
};

Block::Block(int x, int y, int w, int h, char *label, Orientation o) :
	Fl_Group(x, y, w, h, label),
	orientation(o),
	default_track_sz(64),
	sb_sz(16),
	time_sb(0, H_OR_V(h-sb_sz, 0), H_OR_V(w, sb_sz), H_OR_V(sb_sz, h)),
	track_sb(H_OR_V(0, sb_sz), 0,
			H_OR_V(sb_sz, h-sb_sz), H_OR_V(h-sb_sz, sb_sz)),
	zoom(H_OR_V(0, sb_sz), 0, H_OR_V(w, w-sb_sz), H_OR_V(h-sb_sz, h)),
	tracks(H_OR_V(0, sb_sz), 0, H_OR_V(w, w-sb_sz), H_OR_V(h-sb_sz, h), o)
{
	zoom.add(tracks);
	zoom.end();
	/*
	Fl_Box *limit = new Fl_Box(tracks.x()+20, tracks.y()+20,
			tracks.w()-40, tracks.h()-40);
	tracks.add(limit);
	tracks.resizable(limit);
	*/
	tracks.add(new Track(0, 0, w, 50));
	tracks.add(new Track(0, 50, w, 50));

	H_OR_V(zoom.do_zoomx, zoom.do_zoomy) = true;
	zoom.callback(update_time_sb);
	track_sb.hide(); // XXX later, check if we need a track_sb
	// don't do the callback here since we can't get zoom's children's sizes
	// since Fl_Scroll shuffles its (unused) scrollbars which screws up
	// Fl_Group::sizes if sizes is called first...
	time_sb.value(0, 1, 0, 1);

	time_sb.type(H_OR_V(FL_HORIZONTAL, FL_VERTICAL));
	// time_sb.callback(H_OR_V(hscroll_cb, vscroll_cb));
	time_sb.callback(time_sb_cb);
	resizable(zoom);
}
#undef H_OR_V

void Block::new_track()
{
	/*
	Track *t;
	if (orientation == Horizontal_time)
		t = new Track(0, b, w(), default_track_sz);
	else
		t = new Track(b, 0, default_track_sz, h());
		*/
}

void Block::time_sb_cb(Fl_Widget *w, void *)
{
	Fl_Scrollbar *sb = (Fl_Scrollbar *) w;
	Block *b = (Block *) sb->parent();
	if (b->orientation == Horizontal_time)
		b->zoom.position(sb->value(), 0);
	else
		b->zoom.position(0, sb->value());
}

void Block::update_time_sb(Fl_Widget *w, void *)
{
	// int Fl_Slider::scrollvalue(int p, int W, int t, int l) {
	//  p = position, first line displayed
	//  w = window, number of lines displayed
	//  t = top, number of first line
	//  l = length, total number of lines
	Block *b = (Block *) w->parent();
	if (b->orientation == Horizontal_time) {
		int len = int(b->zoom.max_size().x * b->zoom.zoomx());
		b->time_sb.value(b->zoom.xposition(), b->zoom.w(), 0, len);
	} else {
		int len = int(b->zoom.max_size().y * b->zoom.zoomy());
		b->time_sb.value(b->zoom.yposition(), b->zoom.h(), 0, len);
	}
}

int
main(int argc, char **argv)
{
	Fl_Window w(300, 300);
	w.resizable(w);
	Block b(0, 0, 300, 300, "tracks", Horizontal_time);
	w.show(argc, argv);
	return Fl::run();
}

	/*
	Fl_Pack *pack = new Fl_Pack(0, 0, w, h-sb_sz);
	pack->type(FL_VERTICAL);
	zoom.add(pack);
	// XXX cast necessary because otherwise it thinks w is int&... WTF?
	pack->add(new Track(0, 0, (int) w, 50));
	pack->add(new Track(0, 0, (int) w, 50));
	*/
