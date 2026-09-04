// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Audio.h"
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
    ~SampleDirectory() {}
    bool read(int channels, Frames frames, float **out) override;

private:
    std::ostream &log;
    const int sample_rate;
    const std::string dir;

    // Sorted sample names, filled in by the constructor.  I used to
    // just reread the directory whenever I needed a new sample and it was fine,
    // but perhaps it would get slow with large directories and many tracks,
    // so let's cache it.  'refresh' will re-read it if I run off the end,
    // since the synth may still be writing chunks while I play.
    std::vector<std::string> fnames;
    // Index into 'fnames' of the chunk to stream.  This goes off the end when
    // I run out.
    size_t index;
    std::unique_ptr<Wav> wav;
    // How many frames are left in the current chunk, which is the one at
    // 'index' and in 'wav'.
    Frames frames_left;
    std::vector<float> buffer;

    const std::string &cur_fname() const;
    void refresh();
    void open(int channels, Frames offset);
};
