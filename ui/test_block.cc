#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"

BlockConfig block_config()
{
    BlockConfig c;
    c.orientation = HorizontalTime;
    c.zoom_speed = 1;
    c.title_size = 20;
    c.sb_size = 18;
    c.ruler_size = 26;
    return c;
}

BlockColorConfig color_config()
{
    BlockColorConfig c;
    c.select.push_back(Color(0xffff00));
    c.bg = Color(0xdddddd);
    c.track_box = Color(0x44ffff);
    c.sb_box = Color(0x00ffff);
    return c;
}

int
main(int argc, char **argv)
{
    Fl_Double_Window win(200, 200);
    win.resizable(win);

    BlockColorConfig cconfig = color_config();
    BlockConfig config = block_config();
    BlockModel *model = new BlockModel(cconfig);
    BlockView *view = new BlockView(0, 0, 200, 200, *model, config);

    // print_children(view);

    win.show(argc, argv);
    Fl::run();
}
