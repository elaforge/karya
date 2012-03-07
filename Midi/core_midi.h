extern "C" {

typedef int Error;

typedef SInt32 DeviceId;
typedef unsigned long Timestamp;
typedef void (*ReadCallback)(void *p, Timestamp timestamp, int len,
    const unsigned char *bytes);
typedef void (*NotifyCallback)(const char *name, DeviceId dev_id,
    int is_added, int is_read);

Error core_midi_initialize(const char *name, ReadCallback read_cb,
    NotifyCallback notify_cb);
void prime_runloop();
void core_midi_terminate();

// lookup devices
int get_devices(int is_read, const char ***names_out);
int lookup_device_id(int is_read, const char *dev_name, DeviceId *dev_id_out);

Error core_midi_connect_read_device(DeviceId rdev, void *p);
Error core_midi_write_message(DeviceId wdev, Timestamp timestamp, int len,
        const unsigned char *bytes);

// misc
Error core_midi_abort();
Timestamp core_midi_get_now();

}
