// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// This header is technically unnecessary, but it serves as documentation for
// the exported functions.
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
void core_midi_prime_runloop();
void core_midi_terminate();

// lookup devices
int core_midi_get_devices(int is_read, const char ***names_out);
int core_midi_lookup_device_id(
    int is_read, const char *dev_name, DeviceId *dev_id_out);

Error core_midi_connect_read_device(DeviceId rdev, void *p);
Error core_midi_disconnect_read_device(DeviceId dev);
Error core_midi_write_message(DeviceId wdev, Timestamp timestamp, int len,
    const unsigned char *bytes);

// misc
Error core_midi_abort();
Timestamp core_midi_get_now();

}
