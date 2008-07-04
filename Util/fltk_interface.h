extern "C" {

typedef void (*MsgCallback)(int callback_type, const char *msg);

void initialize();
void ui_wait();
void ui_awake();
int has_windows();

};

