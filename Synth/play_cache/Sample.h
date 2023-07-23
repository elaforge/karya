// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>
#include <vector>

#include "Audio.h"
#include "Wav.h"


// Stream from a directory of samples.  Files are in sorted order, and are
// opened and closed on demand.  Each *.wav file is expected to be
// CHUNK_SECONDS long, which is used to find the initial sample given the
// offset.
class SampleDirectory : public Audio {
public:
    SampleDirectory(std::ostream &log, int channels, int sample_rate,
        const std::string &dir, Frames offset);
    ~SampleDirectory();
    bool read(int channels, Frames frames, float **out) override;

private:
    std::ostream &log;
    const int sample_rate;
    const std::string dir;

    // Current file to stream.  This goes to "" when I run out.
    std::string fname;
    Wav *wav;
    // How many frames are left in the current chunk, which is the one in
    // 'fname' and 'wav'.
    Frames frames_left;
    std::vector<float> buffer;

    void open(int channels, Frames offset);
};


// Just stream a single sample file.  This is used by ResampleStreamer for
// playing preview.
class SampleFile : public Audio {
public:
    SampleFile(std::ostream &log, int channels, int sample_rate,
        const std::string &fname, Frames offset);
    ~SampleFile();
    bool read(int channels, Frames frames, float **out) override;

private:
    std::ostream &log;
    const std::string fname;
    Wav *wav;
    int file_channels;
    std::vector<float> buffer;
    std::vector<float> expand_buffer;
};
