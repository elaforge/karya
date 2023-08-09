// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once


// Generic audio file.
class AudioFile {
public:
    typedef size_t Frames;
    virtual ~AudioFile() {}
    virtual size_t read(float *samples, Frames frames) = 0;
    virtual void close() = 0;
    virtual int channels() const = 0;
    virtual int srate() const = 0;
    virtual int bits() const = 0;
    virtual const char *error() const  = 0;
};
