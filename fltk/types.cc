#include "types.h"


std::ostream &
operator<<(std::ostream &os, const TrackPos &pos)
{
    return os << "TrackPos(" << pos._val << ")";
}

std::ostream &
operator<<(std::ostream &os, const Selection &sel)
{
    return os << "Selection(" << sel.start_track << ", " << sel.start_pos
        << ", " << sel.tracks << ", " << sel.duration << ")";
}

std::ostream &
operator<<(std::ostream &os, const ZoomInfo &z)
{
    return os << "ZoomInfo(" << z.offset << ", " << z.factor << ")";
}
