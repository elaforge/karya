// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>

#include "f_util.h"
#include "browser_ui.h"


void
msg_callback(int msg_type, const char *msg)
{
}

int
main(int argc, char **argv)
{
    BrowserWindow view(50, 50, 350, 200, "title", msg_callback);
    f_util::print_children(&view);
    view.show();
    Fl::run();
}
