#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"

TrackModel::TrackModel(EventTrackModel *t, RulerTrackModel *r, DividerModel *d)
{
    if (t) track = t;
    else if (r) ruler = r;
    else divider = d;
}
