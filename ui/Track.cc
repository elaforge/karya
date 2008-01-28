#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"

TrackModel::~TrackModel()
{
    if (this->track)
        this->track->decref();
    else if (this->ruler)
        this->ruler->decref();
    else
        delete this->divider;
}
