extern "C" {

typedef int Error;
typedef int RDevId;
typedef int WDevId;
typedef unsigned long Timestamp;
typedef void (*ReadCallback)(void *p, Timestamp timestamp, int len,
        const unsigned char *bytes);

Error core_midi_initialize(ReadCallback cb);
void core_midi_terminate();
Error core_midi_get_read_devices(int *len, RDevId **ids, char ***names);
Error core_midi_get_write_devices(int *len, WDevId **ids, char ***names);

Error core_midi_connect_read_device(RDevId rdev, void *p);

Error core_midi_write_message(WDevId wdev, Timestamp timestamp, int len,
        const unsigned char *bytes);
Error core_midi_abort();
Timestamp core_midi_get_now();

}
