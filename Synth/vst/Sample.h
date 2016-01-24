#pragma once
#include <memory>
#include <sstream>
#include <string>

// Hold one sample.
class Sample {
public:
    Sample(const char *fname);
    ~Sample();
    std::string error() const { return error_; }
    int samplerate() const { return info.samplerate; }
    const sf_count_t read(sf_count_t fromFrame, float **frames) const;
private:
    // If set, there was an error reading the sample.
    std::string error_;
    SNDFILE *sndfile;
    SF_INFO info;
    std::unique_ptr<float[]> frames;
};
