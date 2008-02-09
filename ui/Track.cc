#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"

TrackModel::TrackModel(boost::shared_ptr<EventTrackModel> t,
        boost::shared_ptr<RulerTrackModel> r,
        boost::shared_ptr<DividerModel> d)
{
    if (t) track = t;
    else if (r) ruler = r;
    else divider = d;
}
