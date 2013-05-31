// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

extern "C" {

typedef void (*MsgCallback)(int callback_type, const char *msg);

void initialize();
void ui_wait();
void ui_awake();
int has_windows();

};

