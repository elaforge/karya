
enum Orientation { HorizontalTime, VerticalTime };

typedef double TrackPos;

struct Selection {
    int start_tracknum;
    TrackPos start_trackpos;
    int end_tracknum;
    TrackPos end_trackpos;
};

class ZoomInfo {
};
