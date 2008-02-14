#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"


static const boost::shared_ptr<EventTrackModel> null_track;
static const boost::shared_ptr<RulerTrackModel> null_ruler;
static const boost::shared_ptr<DividerModel> null_divider;

TrackModel::TrackModel(boost::shared_ptr<EventTrackModel> t,
        boost::shared_ptr<RulerTrackModel> r) :
    track(t), ruler(r), divider(null_divider) {}

TrackModel::TrackModel(boost::shared_ptr<RulerTrackModel> r) :
    track(null_track), ruler(r), divider(null_divider) {}

TrackModel::TrackModel(boost::shared_ptr<DividerModel> d) :
    track(null_track), ruler(null_ruler), divider(d) {}
