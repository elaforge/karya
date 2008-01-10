#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

#include <vector>

/*
handle selection event level selection
dragging 
*/

namespace widgets {

class Track : public Selection_ruler {
	static bool Debug;
	enum { Max_button = 4 };
public:
	Track(int X, int Y, int W, int H, Orientation o, Tpoint sz);
	void resize(int X, int Y, int W, int H) {
		Overlay_ruler::resize(X, Y, W, H);
		// std::cout << "Track resize: " << Rect(X, Y, W, H) << '\n';
	}

	void selection_colors(const std::vector<Fl_Color> *cs) {
		for (unsigned int i = 0; i < cs->size(); i++)
			getsel(i).color = cs->at(i);
		damage(FL_DAMAGE_ALL);
	}
	void bg_color(Fl_Color c) { _bg_color = c; color(c); }
	Tpoint dimensions() const { return _dimensions; }
	// resize self, possibly delete events out of range
	void dimensions(const Tpoint &d);
	// make a selection of type 'n' from 's' to 'e'
	void select_range(int n, Trange r);
	// selection is complete
	void select_release(int n);
	void clear_selection(int n);
protected:
	int handle(int ev);
	void draw();
	void draw_area(Rect c);
private:
	Selection<Trackpos> &getsel(int i) {
		return i == 0 ? _selection : selections.at(i-1);
	}
	Fl_Color _bg_color;
	Tpoint _dimensions;
	std::vector<Selection<Trackpos> > selections;
};

}
