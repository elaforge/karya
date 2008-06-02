#include <stdio.h>
#include <FL/Fl.H>

#include "f_util.h"
#include "logview_ui.h"


void
msg_callback(int cb_type, const char *msg)
{
    DEBUG("cb " << cb_type << ' ' << msg);
}


int
main(int argc, char **argv)
{
    DEBUG("argc " << argc);
    LogViewWindow view(50, 50, 500, 200, msg_callback);
    view.show(argc, argv);
    print_children(&view);

    FILE *fp = fopen("LogViewer/seq.log", "r");
    char line[1024];
    char style[1024];
    for (int i = 0; fgets(line, sizeof line, fp); i++) {
        int s;
        if (i % 4 == 0)
            s = 'B';
        else
            s = 'A';
        memset(style, s, strlen(line));
        style[strlen(line)] = '\0';
        view.view.append_log(line, style);
    }
    fclose(fp);
    Fl::run();
}
