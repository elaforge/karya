// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Audio.h"
#include "Flac.h"
#include "Wav.h"


// Stream from a directory of samples.  Files are in sorted order, and are
// opened and closed on demand.  Each *.wav file is expected to be
// CHUNK_SECONDS long, which is used to find the initial sample given the
// offset.
//
// This is specifically designed for im output, so it works only for
// directories of float32 wav files in the chunk format created by im synths.
// channels and sample_rate are simply assertions, this will log an error
// if the files don't match.  This is because I should have set up the DAW
// and im synths to agree about sample_rate, and channels should always be 2.
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
//
// At the moment this sample_rate just asserts that the file has that
// sample_rate.  If thru wants to tweak the frequency it may do a resample,
// so I could hook this up to play a file of any sample_rate and adjust it
// to suit, but so far I've just offline resampled everything to 44.1k.
class SampleFile : public Audio {
public:
    SampleFile(std::ostream &log, int channels, int sample_rate,
        const std::string &fname, Frames offset);
    ~SampleFile() {}
    bool read(int channels, Frames frames, float **out) override;

private:
    std::ostream &log;
    const std::string fname;
    std::unique_ptr<Flac> file;
    std::vector<float> buffer;
};
