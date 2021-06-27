// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Binding to OS X CoreMIDI.
#include <stdio.h>
#include <map>
#include <vector>

#include <CoreAudio/HostTime.h>
#include <CoreMIDI/MIDIServices.h>
#include "core_midi.h"

#include "fltk/util.h"


// Provide C functions for the FFI.
extern "C" {

// Output has a separate g_out_port and g_thru_port.  This is because (I think)
// MIDISend() only wants to receive messages with increasing timestamps.  So I
// keep g_thru_port for 0 timestamp "instant" outputs.
static MIDIClientRef g_client;
static MIDIPortRef g_in_port, g_out_port, g_thru_port;
static ReadCallback g_read_callback;

enum { SOX = 0xf0, EOX = 0xf7, STATUS_MASK = 0x80 };
#define NANO_FACTOR ((UInt64) 1000000)

// Sysex msgs are variable length, so they must be accumulated over multiple
// 'read_proc' callbacks.
struct SysexState {
    SysexState() : in_progress(false), timestamp(0), buf(1024) {}
    bool in_progress;
    Timestamp timestamp;
    std::vector<Byte> buf;
};
// Map port ID (by pointer) to its receive state.
typedef std::map<void *, SysexState *> SysexMap;
static SysexMap g_sysex_state;

static void
process_sysex(SysexState *state, int len, const Byte *byte, void *p)
{
    for (int i = 0; i < len; i++, byte++) {
        if (*byte == EOX) {
            state->buf.push_back(*byte);
            g_read_callback(p, state->timestamp, state->buf.size(),
                &*state->buf.begin());
        }
        if (*byte & STATUS_MASK) {
            if (*byte != EOX) {
                printf("got %lu sysex bytes and ended with %hhx\n",
                    state->buf.size(), *byte);
            }
            state->in_progress = false;
            break;
        } else {
            // This could allocate inside the read_proc, which you're not
            // supposed to do, but I haven't had to care yet.
            state->buf.push_back(*byte);
        }
    }
}

static void
process_packet(const MIDIPacket *packet, void *p)
{
    if (packet->length == 0)
        return;
    Timestamp timestamp =
        AudioConvertHostTimeToNanos(packet->timeStamp) / NANO_FACTOR;
    SysexMap::iterator iter = g_sysex_state.find(p);
    assert(iter != g_sysex_state.end());
    SysexState *state = iter->second;
    if (state->in_progress) {
        process_sysex(state, packet->length, packet->data, p);
    } else {
        if (packet->data[0] == SOX) {
            state->in_progress = true;
            state->timestamp = timestamp;
            state->buf.clear();
            state->buf.push_back(packet->data[0]);
            process_sysex(state, packet->length-1, packet->data+1, p);
        } else {
            g_read_callback(p, timestamp, packet->length, packet->data);
        }
    }
}

static void
read_proc(const MIDIPacketList *packets, void *_read_proc_p, void *src_con_p)
{
    if (!g_read_callback)
        return;
    MIDIPacket *packet = (MIDIPacket *) packets->packet;
    for (unsigned i = 0; i < packets->numPackets; i++) {
        process_packet(packet, src_con_p);
        packet = MIDIPacketNext(packet);
    }
}

static const char *
get_name(MIDIEndpointRef dev)
{
    CFStringRef pname;
    char name[64];
    MIDIObjectGetStringProperty(dev, kMIDIPropertyDisplayName, &pname);
    if (!pname)
        return strdup("");
    CFStringGetCString(pname, name, sizeof name, kCFStringEncodingUTF8);
    CFRelease(pname);
    return strdup(name);
}

// Called when a new device is pluggd in or unplugged.
static void
midi_notification(const MIDINotification *note, void *ref)
{
    NotifyCallback notify = (NotifyCallback) ref;
    int is_added;
    if (note->messageID == kMIDIMsgObjectAdded)
        is_added = 1;
    else if (note->messageID == kMIDIMsgObjectRemoved)
        is_added = 0;
    else
        return;

    MIDIObjectAddRemoveNotification *msg =
        (MIDIObjectAddRemoveNotification *) note;
    int is_read;
    if (msg->childType == kMIDIObjectType_Source)
        is_read = 1;
    else if (msg->childType == kMIDIObjectType_Destination)
        is_read = 0;
    else
        return;

    // name and dev_id is always "" and 0 for a remove, how then am I supposed
    // to know what was removed?
    MIDIEndpointRef dev = (MIDIEndpointRef) msg->child;
    const char *name = get_name(dev);
    DeviceId dev_id;
    MIDIObjectGetIntegerProperty(dev, kMIDIPropertyUniqueID, &dev_id);
    notify(name, dev_id, is_added, is_read);
    free((void *) name);
}

Error
core_midi_initialize(const char *name, ReadCallback read_cb,
    NotifyCallback notify_cb)
{
    OSStatus err;

    g_read_callback = read_cb;
    CFStringRef cfname = CFStringCreateWithCString(
        NULL, name, kCFStringEncodingUTF8);
    err = MIDIClientCreate(
        cfname, midi_notification, (void *) notify_cb, &g_client);
    if (err != noErr) goto error;
    err = MIDIInputPortCreate(
        g_client, CFSTR("input port"), read_proc, NULL, &g_in_port);
    if (err != noErr) goto error;
    err = MIDIOutputPortCreate(g_client, CFSTR("output port"), &g_out_port);
    if (err != noErr) goto error;
    err = MIDIOutputPortCreate(g_client, CFSTR("thru port"), &g_thru_port);
    if (err != noErr) goto error;

    return noErr;
error:
    core_midi_terminate();
    return err;
}

// This bit of awkwardness is documented in 'Midi.CoreMidi.initialize'.
void
core_midi_prime_runloop()
{
    CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0, false);
}

void
core_midi_terminate()
{
    if (g_thru_port)
        MIDIPortDispose(g_thru_port);
    if (g_out_port)
        MIDIPortDispose(g_out_port);
    if (g_in_port)
        MIDIPortDispose(g_in_port);
    if (g_client)
        MIDIClientDispose(g_client);
}


// lookup devices

int
core_midi_get_devices(int is_read, const char ***names_out)
{
    *names_out = NULL;
    int devs = is_read
        ? MIDIGetNumberOfSources() : MIDIGetNumberOfDestinations();
    const char **names = (const char **) calloc(devs, sizeof(char *));
    for (int i = 0; i < devs; i++) {
        MIDIEndpointRef dev = is_read
            ? MIDIGetSource(i) : MIDIGetDestination(i);
        names[i] = get_name(dev);
    }
    *names_out = names;
    return devs;
}

int
core_midi_lookup_device_id(
    int is_read, const char *dev_name, DeviceId *dev_id_out)
{
    int devs = is_read
        ? MIDIGetNumberOfSources() : MIDIGetNumberOfDestinations();
    for (int i = 0; i < devs; i++) {
        MIDIEndpointRef dev = is_read
            ? MIDIGetSource(i) : MIDIGetDestination(i);
        const char *name = get_name(dev);
        if (strcmp(name, dev_name) == 0) {
            free((void *) name);
            MIDIObjectGetIntegerProperty(dev, kMIDIPropertyUniqueID,
                dev_id_out);
            return true;
        }
        free((void *) name);
    }
    return false;
}


// connect

Error
core_midi_connect_read_device(DeviceId dev, void *p)
{
    MIDIObjectRef obj;
    MIDIObjectType type;

    OSStatus err = MIDIObjectFindByUniqueID(dev, &obj, &type);
    if (err != noErr)
        return err;
    MIDIEndpointRef src = (MIDIEndpointRef) obj;
    g_sysex_state[p] = new SysexState();
    return MIDIPortConnectSource(g_in_port, src, p);
}

Error
core_midi_disconnect_read_device(DeviceId dev)
{
    MIDIObjectRef obj;
    MIDIObjectType type;

    OSStatus err = MIDIObjectFindByUniqueID(dev, &obj, &type);
    if (err != noErr)
        return err;
    MIDIEndpointRef src = (MIDIEndpointRef) obj;
    // I could delete g_sysex_state[p] here, only I'd have to make sure
    // 'process_packet' wasn't working on it.  I'll take the simpler way out
    // and just leak a bit.
    return MIDIPortDisconnectSource(g_in_port, src);
}



// write messages

static void
sysex_complete(MIDISysexSendRequest *req)
{
    Byte *bytes = (Byte *) req->completionRefCon;
    delete[] bytes;
    delete req;
}

static Error
write_sysex(MIDIEndpointRef dest, int len, const unsigned char *bytes)
{
    MIDISysexSendRequest *req = new MIDISysexSendRequest;
    req->destination = dest;
    req->data = new Byte[len];
    memcpy(const_cast<Byte *>(req->data), bytes, len);
    req->bytesToSend = len;
    req->complete = false;
    req->completionProc = sysex_complete;
    req->completionRefCon = (void *) req->data;
    return MIDISendSysex(req);
}


Error
core_midi_write_message(DeviceId dev, Timestamp timestamp, int len,
    const unsigned char *bytes)
{
    OSStatus err = noErr;

    if (!len)
        return noErr;

    MIDIObjectRef obj;
    MIDIObjectType type;
    err = MIDIObjectFindByUniqueID(dev, &obj, &type);
    if (err != noErr)
        return err;
    MIDIEndpointRef dest = (MIDIEndpointRef) obj;

    if (bytes[0] == SOX) {
        write_sysex(dest, len, bytes);
    } else if (!(bytes[0] & STATUS_MASK)) {
        printf("first byte not a status byte: %hhx\n", bytes[0]);
    } else {
        MIDIPacketList packets;
        MIDIPacket *packet = MIDIPacketListInit(&packets);
        if (timestamp <= 0) {
            MIDIPacketListAdd(&packets, sizeof packets, packet, 0, len, bytes);
            err = MIDISend(g_thru_port, dest, &packets);
        } else {
            MIDITimeStamp ts =
                AudioConvertNanosToHostTime(timestamp) * NANO_FACTOR;
            MIDIPacketListAdd(
                &packets, sizeof packets, packet, ts, len, bytes);
            err = MIDISend(g_out_port, dest, &packets);
        }
    }
    return err;
}


// misc

Error
core_midi_abort()
{
    return MIDIFlushOutput(0);
}

Timestamp
core_midi_get_now()
{
    return AudioConvertHostTimeToNanos(AudioGetCurrentHostTime())
        / NANO_FACTOR;
}

}
