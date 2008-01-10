#include <FL/Fl.H>
#include <FL/Fl_Tile.H>

#include "util.h"
#include "s_util.h"
#include "Marklist.h"
#include "state.h"

#include "f_util.h"
#include "Event.h"
#include "Ruler.h"
#include "Track.h"
#include "Tile_ext.h"

namespace widgets {

int Tile_ext::handle(int ev) { return Fl_Tile2::handle(ev); }

}
