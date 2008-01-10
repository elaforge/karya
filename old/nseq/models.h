#include <vector>

struct Event_model {
	const char *title;
	Trackpos start, extent;
	// subranges...
	struct Colors {
		Color bg, text;
	} colors;
};

struct Track_model {
	const char *title;
	std::vector<Event_model *> events;
	Trackpos length;
	struct Colors {
		Color bg;
	};
};

struct Block_model {
	const char *title;
	std::vector<Marklist *> marklists;
	struct Colors {
		Color scroll_box, ruler_box, ruler, track_tile;
		Color selection;
	} colors;
};


