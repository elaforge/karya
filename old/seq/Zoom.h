#include <FL/Fl_Scroll.H>

namespace widgets {

class Zoom : public Fl_Group {
public:
	Zoom(int X, int Y, int W, int H);
	void draw();
	const Zoom_info &zoom_win() const { return cur_zoom; }
	void zoom_win(const Zoom_info &z) {
		damage(FL_DAMAGE_ALL | DAMAGE_ZOOM);
		cur_zoom = z;
	}
	virtual void zoom_speed(Dpoint zs) { _zoom_speed = zs; }
protected:
	int handle(int ev);
	virtual bool is_zoom_key(int key) {
		return key == FL_Shift_L || key == FL_Shift_R;
	}
	virtual bool is_box_key(int key) {
		return key == FL_Alt_L || key == FL_Alt_R;
	}
private:
	void zoom(const Zoom_info &to);
	Zoom_info handle_drag(const Zoom_info &old_zoom, const Point &center,
		const Point &mouse) const;

	Dpoint _zoom_speed;
	Zoom_info cur_zoom;
	int zoom_button;
	bool zooming;

	// could make these static to handle(), if I'm sure they get reset after
	// FL_ENTER
	Point zoom_center;
	int zoom_key_down;
	Zoom_info old_zoom;

	bool box_zooming;
	int box_key_down;
	Point box_start;
};

}
