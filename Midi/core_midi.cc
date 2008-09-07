#include <stdio.h>
#include <map>
#include <vector>

#include <CoreAudio/HostTime.h>
#include <CoreMIDI/MIDIServices.h>
#include "core_midi.h"

extern "C" {

static MIDIClientRef g_client;
static MIDIPortRef g_in_port, g_out_port, g_thru_port;
static ReadCallback g_read_callback;

enum { SOX = 0xf0, EOX = 0xf7, STATUS_MASK = 0x80 };
#define NANO_FACTOR ((UInt64) 1000000)

struct SysexState {
    SysexState() : in_progress(false), timestamp(0), buf(1024) {}
    bool in_progress;
    Timestamp timestamp;
    std::vector<Byte> buf;
};
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

Error
core_midi_initialize(ReadCallback cb)
{
    OSStatus err;

    g_read_callback = cb;
    err = MIDIClientCreate(CFSTR("core midi"), NULL, NULL, &g_client);
    if (err != noErr) goto error;
    err = MIDIInputPortCreate(g_client, CFSTR("input port"), read_proc, NULL,
            &g_in_port);
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

Error
core_midi_get_read_devices(int *len, RDevId **ids_out, char ***names_out)
{
    *len = 0;
    *ids_out = NULL;
    *names_out = NULL;
    int sources = MIDIGetNumberOfSources();
    if (sources == 0)
        return noErr;
    RDevId *ids = (RDevId *) calloc(sources, sizeof(RDevId));
    char **names = (char **) calloc(sources, sizeof(char *));
    for (int i = 0; i < sources; i++) {
        CFStringRef pname;
        char name[64];
        SInt32 unique_id;

        MIDIEndpointRef src = MIDIGetSource(i);
        MIDIObjectGetStringProperty(src, kMIDIPropertyDisplayName, &pname);
        CFStringGetCString(pname, name, sizeof name, 0);
        CFRelease(pname);
        names[i] = strdup(name);

        MIDIObjectGetIntegerProperty(src, kMIDIPropertyUniqueID, &unique_id);
        ids[i] = unique_id;
    }
    *ids_out = ids;
    *names_out = names;
    *len = sources;
    return noErr;
}

Error
core_midi_get_write_devices(int *len, WDevId **ids_out, char ***names_out)
{
    *len = 0;
    *ids_out = NULL;
    *names_out = NULL;
    int dests = MIDIGetNumberOfDestinations();
    if (dests == 0)
        return noErr;
    RDevId *ids = (RDevId *) calloc(dests, sizeof(RDevId));
    char **names = (char **) calloc(dests, sizeof(char *));
    for (int i = 0; i < dests; i++) {
        CFStringRef pname;
        char name[64];
        SInt32 unique_id;

        MIDIEndpointRef src = MIDIGetDestination(i);
        MIDIObjectGetStringProperty(src, kMIDIPropertyDisplayName, &pname);
        CFStringGetCString(pname, name, sizeof name, 0);
        CFRelease(pname);
        names[i] = strdup(name);

        MIDIObjectGetIntegerProperty(src, kMIDIPropertyUniqueID, &unique_id);
        ids[i] = unique_id;
    }
    *ids_out = ids;
    *names_out = names;
    *len = dests;
    return noErr;
}


Error
core_midi_connect_read_device(RDevId rdev, void *p)
{
    OSStatus err;
    MIDIObjectRef obj;
    MIDIObjectType type;
    MIDIEndpointRef src;

    err = MIDIObjectFindByUniqueID(rdev, &obj, &type);
    if (err != noErr) return err;
    src = (MIDIEndpointRef) obj;
    // This is never deallocated.
    g_sysex_state[p] = new SysexState();
    err = MIDIPortConnectSource(g_in_port, src, p);
    return err;
}


static void
sysex_complete(MIDISysexSendRequest *req)
{
    Byte *bytes = (Byte *) req->completionRefCon;
    delete[] bytes;
    delete req;
}

static Error
write_sysex(MIDIEndpointRef dest, WDevId wdev, int len,
        const unsigned char *bytes)
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
core_midi_write_message(WDevId wdev, Timestamp timestamp, int len,
        const unsigned char *bytes)
{
    OSStatus err = noErr;

    if (!len)
        return noErr;

    MIDIObjectRef obj;
    MIDIObjectType type;
    err = MIDIObjectFindByUniqueID(wdev, &obj, &type);
    if (err != noErr) return err;
    MIDIEndpointRef dest = (MIDIEndpointRef) obj;

    if (bytes[0] == SOX) {
        write_sysex(dest, wdev, len, bytes);
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
            MIDIPacketListAdd(&packets, sizeof packets, packet, ts, len, bytes);
            err = MIDISend(g_out_port, dest, &packets);
        }
    }
    return err;
}

Error
core_midi_abort()
{
    return MIDIFlushOutput(NULL);
}

Timestamp
core_midi_get_now()
{
    return AudioConvertHostTimeToNanos(AudioGetCurrentHostTime()) / NANO_FACTOR;
}

}
