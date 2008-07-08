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
    BrowserWindow view(50, 50, 350, 200, msg_callback);
    print_children(&view);
    view.show();
    Fl::run();
}
