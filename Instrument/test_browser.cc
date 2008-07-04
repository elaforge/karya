#include <FL/Fl.H>

#include "f_util.h"

#include "browser_ui.h"


int
main(int argc, char **argv)
{
    BrowserWindow view(50, 50, 350, 200);
    print_children(&view);
    view.show();
    Fl::run();
}
