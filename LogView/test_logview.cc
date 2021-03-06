// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    char *filename = NULL;
    if (argc > 1)
        filename = argv[1];
    LogViewWindow win(50, 50, 500, 200, "test", msg_callback, 1024 * 128);

    if (filename) {
        FILE *fp = fopen(filename, "r");
        if (!fp) {
            DEBUG("can't open file " << filename);
            return 1;
        }
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
            printf("putting line %d\n", i+1);
            fflush(stdout);
            win.view.append_log(line, style);
        }
        fclose(fp);
        argc--;
        argv++;
        printf("\n");
    }
    const char *status = "abcdefg || 1234567 || ABCDEFG";
    const char *style  = "AAAAAAAACCABBBBBBBACCAAAAAAAA";
    win.view.set_status(status, style);
    win.show(argc, argv);
    // print_children(&win);
    Fl::run();
}
