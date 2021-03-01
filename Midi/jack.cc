// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Binding to the MIDI part of JACK.
#include <string>
#include "jack.h"

// just for DEBUG
#include "fltk/util.h"


// create_client

Client::Client()
{
    for (int i = 0; i < MAX_PORTS; i++) {
        read_ports[i] = 0;
        write_ports[i] = 0;
    }
    sem_init(&available, 0, 0);

    // TODO memset(ring->buf, 0, ring->size) to avoid page faults?
    // failed = jack_ringbuffer_mlock(client.immediate_output_ring);
    immediate_output = jack_ringbuffer_create(immediate_output_buffer_size);
    output = jack_ringbuffer_create(output_buffer_size);
    input = jack_ringbuffer_create(input_buffer_size);
}

Client::~Client()
{
    sem_destroy(&available);
    jack_ringbuffer_free(immediate_output);
    jack_ringbuffer_free(output);
    jack_ringbuffer_free(input);
}

void
Client::add_read_port(jack_port_t *port)
{
    for (int i = 0; i < MAX_PORTS; i++) {
        if (__sync_bool_compare_and_swap(&read_ports[i], 0, port))
            break;
    }
}

void
Client::remove_read_port(jack_port_t *port)
{
    for (int i = 0; i < MAX_PORTS; i++) {
        if (__sync_bool_compare_and_swap(&read_ports[i], port, 0))
            break;
    }
}

void
Client::add_write_port(jack_port_t *port)
{
    for (int i = 0; i < MAX_PORTS; i++) {
        if (__sync_bool_compare_and_swap(&write_ports[i], 0, port))
            break;
    }
}


// Called when a new port is registered or unregistered.
// When a new MIDI input appears, I should have haskell connect it up
// to a local port if I want it, likewise for a MIDI output.
static void
port_registration_callback(jack_port_id_t port_id, int is_add, void *arg)
{
    Client *client = static_cast<Client *>(arg);
    jack_port_t *port = jack_port_by_id(client->client, port_id);
    if (port) {
        const char *name = jack_port_name(port);
        int is_read = jack_port_flags(port) & JackPortIsOutput;
        client->notify(client, name, is_add, is_read);
    } else {
        DEBUG("registered unknown port");
    }
}


// whenever a port is connected or disconnected
static void
port_connect_callback(jack_port_id_t a, jack_port_id_t b, int connect,
    void *arg)
{
}


static void
write_midi_event(jack_nframes_t nframes, const midi_event &event)
{
    void *buf = jack_port_get_buffer(event.port, nframes);
    if (!buf) {
        DEBUG("port has no buffer");
        return;
    }
    jack_midi_data_t *midi = jack_midi_event_reserve(
        buf, event.event.time, event.event.size);
    if (!midi) {
        DEBUG("no space in output port " << event.port);
    } else {
        // DEBUG("write event on " <<  event.port);
        memcpy(midi, event.event.buffer, event.event.size);
    }
}

// This runs in a high priority thread.
static int
process(jack_nframes_t nframes, void *arg)
{
    Client *client = static_cast<Client *>(arg);
    const jack_nframes_t now = jack_last_frame_time(client->client);

    // Read incoming MIDI.  To guarantee all the ports are still valid, I
    // never unregister a port, only disconnect them.
    for (int i = 0; i < MAX_PORTS; i++) {
        jack_port_t *port = client->read_ports[i];
        if (!port)
            continue;
        void *buf = jack_port_get_buffer(port, nframes);

        jack_nframes_t count = jack_midi_get_event_count(buf);
        for (jack_nframes_t j = 0; j < count; j++) {
            midi_event event;
            jack_midi_event_get(&event.event, buf, j);
            event.port = port;
            event.event.time += now;
            if (!jack_ringbuffer_write(
                client->input, (char *) &event, sizeof(event)))
            {
                DEBUG("input buffer overrun");
            } else {
                sem_post(&client->available);
            }
        }
    }

    // Write outgoing MIDI.  To guarantee all the ports are still valid, I
    // never unregister ports, only disconnect them.  The problem is that it's
    // hard to tell if any instances of the old port are still in the
    // output ringbuffer.
    midi_event event;
    for (int i = 0; i < MAX_PORTS; i++) {
        if (client->write_ports[i]) {
            // DEBUG("clear buffer for " << client->write_ports[i]);
            jack_midi_clear_buffer(
                jack_port_get_buffer(client->write_ports[i], nframes));
        }
    }
    while (jack_ringbuffer_read(client->immediate_output, (char *) &event,
        sizeof(event)))
    {
        event.event.time = 0;
        write_midi_event(nframes, event);
    }
    while (jack_ringbuffer_peek(client->output, (char *) &event, sizeof(event)))
    {
        event.event.time = now > event.event.time ? 0 : event.event.time - now;
        if (event.event.time >= nframes)
            break;
        jack_ringbuffer_read_advance(client->output, sizeof(event));
        write_midi_event(nframes, event);
    }
    return 0; // no error, but who knows what returning an error would do
}


// Provide C functions for the FFI.
extern "C" {

// Create a Client and return it into the 'client' parameter, or NULL and
// return an error message.
const char *
create_client(const char *client_name, NotifyCallback notify,
    Client **out_client)
{
    jack_status_t status;
    int failed;
    static char errorbuf[127];

    *out_client = NULL;
    Client *client = new Client();
    client->notify = notify;

    // ensure client_name < jack_client_name_size()
    // Automatically starting the server leads to hangs on NixOS with
    // "Cannot connect to server socket err = No such file or directory"
    client->client = jack_client_open(client_name, JackNoStartServer, &status);
    if (!client->client) {
        delete client;
        // Yeah I know it's a bitmask, but these seem mutually exclusive.
        if (status & JackServerFailed)
            return "unable to connect to JACK server";
        else if (status & JackServerError)
            return "communication error with JACK server";
        else if (status & JackInitFailure)
            return "unable to initialize client";
        else if (status & JackShmFailure)
            return "unable to access shared memory";
        else if (status & JackVersionError)
            return "client's protocol version does not match";
        else {
            sprintf(errorbuf, "unknown error: 0x%x\n", status);
            return errorbuf;
        }
    }

    // jack_set_port_connect_callback
    // This is called called whenever a port is connected or disconnected
    failed = jack_set_port_registration_callback(
        client->client, port_registration_callback, client);
    if (failed)
        return "jack_set_port_registration_callback() failed";

    failed = jack_set_port_connect_callback(
        client->client, port_connect_callback, client);
    if (failed)
        return "jack_set_port_connect_callback() failed";

    failed = jack_set_process_callback(client->client, process, client);
    if (failed)
        return "jack_set_process_callback failed";

    failed = jack_activate(client->client);
    if (failed) {
        return "jack_activate() failed";
    }
    *out_client = client;
    return NULL;
}


// ports

static std::string
prepend_client(Client *client, const char *short_name)
{
    std::string name = jack_get_client_name(client->client);
    name += ':';
    name += short_name;
    return name;
}

const char *
create_read_port(Client *client, const char *remote_name)
{
    // I create a local name with the same name as the full remote name.  So
    // if the remote name is 'system:out', then the local name will be
    // 'client:system:out'.  Jack seems happy to accept names with more than
    // one colon.
    std::string local_name = remote_name;
    std::string local_long_name = prepend_client(client, local_name.c_str());

    // DEBUG("connect read: " << local_long_name << " <- " << remote_name);
    jack_port_t *port = jack_port_by_name(
        client->client, local_long_name.c_str());
    if (!port) {
        port = jack_port_register(
            client->client, local_name.c_str(), JACK_DEFAULT_MIDI_TYPE,
            JackPortIsInput | JackPortIsTerminal, 0);
        if (!port)
            return "can't create local port";
        client->add_read_port(port);
    }

    if (jack_connect(client->client, remote_name, local_long_name.c_str())) {
        // It's normal to create a port with no corresponding remote one.
        // It'll get connected if and when it does appears.
        return NULL;
    }
    return NULL;
}

const char *
remove_read_port(Client *client, const char *remote_name)
{
    std::string local_name = prepend_client(client, remote_name);

    jack_port_t *port = jack_port_by_name(client->client, local_name.c_str());
    if (!port)
        return "local port not found";
    // DEBUG("disconnect read: " << local_name << " <- " << remote_name);
    client->remove_read_port(port);
    jack_port_disconnect(client->client, port);
    // I never unregister ports.  This ensures that any read ports still on
    // the input ringbuffer won't cause jack_port_short_name to crash.
    return NULL;
}

const char *
create_write_port(Client *client, const char *remote_name)
{
    std::string local_name = remote_name;
    std::string local_long_name = prepend_client(client, local_name.c_str());

    // DEBUG("connect write: " << local_long_name << " <- " << remote_name);
    jack_port_t *port = jack_port_by_name(
        client->client, local_long_name.c_str());
    if (!port) {
        port = jack_port_register(
            client->client, local_name.c_str(), JACK_DEFAULT_MIDI_TYPE,
            JackPortIsOutput | JackPortIsTerminal, 0);
        if (!port)
            return "can't create local port";
        // DEBUG("registered write " << local_name << ": " << port);
        client->add_write_port(port);
    }

    if (jack_connect(client->client, local_long_name.c_str(), remote_name)) {
        // It's normal to create a port with no corresponding remote one.
        // It'll get connected if and when it does appears.
        return NULL;
    }
    // DEBUG("jack_connect() completed");
    return NULL;
}

const char **
get_midi_ports(Client *client, unsigned long flags)
{
    return jack_get_ports(client->client, NULL, JACK_DEFAULT_MIDI_TYPE, flags);
}

const char *
get_port_aliases(Client *client, const char *name, char **alias1, char **alias2)
{
    static char *aliases[2];
    if (aliases[0] == NULL) {
        aliases[0] = (char *) malloc(jack_port_name_size());
        aliases[1] = (char *) malloc(jack_port_name_size());
    }

    jack_port_t *port = jack_port_by_name(client->client, name);

    *alias1 = NULL;
    *alias2 = NULL;
    if (!port) {
        return "port not found";
    }

    int n = jack_port_get_aliases(port, aliases);
    if (n >= 1)
        *alias1 = aliases[0];
    if (n >= 2)
        *alias2 = aliases[1];
    return NULL;
}

// read / write

const char *
write_message(Client *client, const char *port, uint64_t time, void *bytes,
    int size)
{
    midi_event event;
    event.port = jack_port_by_name(client->client, port);
    if (!event.port) {
        // This just means a MIDI msg went to a nonexistent device.
        // Return "" to tell the caller to report a failure but not otherwise
        // freak out.
        return "";
    }
    // DEBUG("write msg port " << port << ": " << event.port);
    event.event.size = size;
    event.event.time = jack_time_to_frames(client->client, time);
    event.event.buffer = (jack_midi_data_t *) bytes;
    jack_ringbuffer_t *ring =
        time == 0 ? client->immediate_output : client-> output;
    if (!jack_ringbuffer_write(ring, (char *) &event, sizeof(event))) {
        return "output buffer overrun";
    }
    return NULL;
}

int
read_event(Client *client, const char **port, uint64_t *time, void **mevent)
{
    midi_event event;
try_again:
    // Incremented for every msg put on the ringbuffer.
    sem_wait(&client->available);

    size_t read = jack_ringbuffer_read(
        client->input, (char *) &event, sizeof(event));
    if (read < sizeof(event)) {
        // This should never happen!
        goto try_again;
    }
    *port = jack_port_short_name(event.port);
    *time = jack_frames_to_time(client->client, event.event.time);
    *mevent = event.event.buffer;
    return event.event.size;
}

void
jack_abort(Client *client)
{
    // jack_ringbuffer_reset is documented not thread safe, so I probably
    // can't just call it while someone else may be reading it.  And
    // jack_ringbuffer_read_advance doesn't say if it was able to advance or
    // not.  So just suck all the events out one-by-one before process() can
    // get them.
    midi_event evt;
    while (jack_ringbuffer_read(client->output, (char *) &evt, sizeof(evt)))
        ;
}

uint64_t
now(Client *client)
{
    return jack_frames_to_time(
        client->client, jack_frame_time(client->client));
}

};
