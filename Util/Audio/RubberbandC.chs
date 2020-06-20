-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Low level bindings to the rubberband library.
module Util.Audio.RubberbandC where
import           Data.Bits ((.|.))
import           Foreign (Ptr, castPtr)
import qualified Foreign.C as C


#include <rubberband-c.h>

type Frames = C.CUInt

-- RubberBandOption is actually several overlapping options.
{#enum RubberBandOption as Option {}
    with prefix = "RubberBandOption"
    deriving (Eq, Show)
#}


makeOptions :: [Option] -> C.CInt
makeOptions = fromIntegral . foldr (.|.) 0 . map fromEnum

defaultOptions :: [Option]
defaultOptions = []

percussiveOptions :: [Option]
percussiveOptions = [WindowShort, PhaseIndependent]

{#pointer RubberBandState as State newtype #}

-- RubberBandState rubberband_new(unsigned int sampleRate,
--                                unsigned int channels,
--                                RubberBandOptions options,
--                                double initialTimeRatio,
--                                double initialPitchScale);
{#fun rubberband_new
    { id `Frames', `Int', makeOptions `[Option]', `Double', `Double' }
        -> `State'
#}

{#fun rubberband_delete { `State' } -> `()' #}

-- void rubberband_study(RubberBandState, const float *const *input,
-- unsigned int samples, int final);
{#fun rubberband_study
    { `State', castPtr `Ptr (Ptr Float)', id `Frames', `Bool' } -> `()'
#}

-- void rubberband_process(RubberBandState, const float *const *input,
-- unsigned int samples, int final);
{#fun rubberband_process
    { `State', castPtr `Ptr (Ptr Float)', id `Frames', `Bool' } -> `()'
#}

-- int rubberband_available(const RubberBandState);
-- -1 on final completion
{#fun rubberband_available { `State' } -> `Int' #}

-- Write up to the given Frames to the pointers.
-- unsigned int rubberband_retrieve(const RubberBandState,
-- float *const *output, unsigned int samples);
{#fun rubberband_retrieve { `State', castPtr `Ptr (Ptr Float)', id `Frames' }
    -> `Frames' id
#}

-- void rubberband_set_expected_input_duration(RubberBandState,
-- unsigned int samples);
{#fun rubberband_set_expected_input_duration { `State', id `Frames' } -> `()' #}

-- unsigned int rubberband_get_samples_required(const RubberBandState);
{#fun rubberband_get_samples_required { `State' } -> `Frames' id #}

-- void rubberband_set_time_ratio(RubberBandState, double ratio);
-- void rubberband_set_pitch_scale(RubberBandState, double scale);
--
-- double rubberband_get_time_ratio(const RubberBandState);
-- double rubberband_get_pitch_scale(const RubberBandState);
--
-- unsigned int rubberband_get_latency(const RubberBandState);
--
-- void rubberband_set_transients_option(RubberBandState,
-- RubberBandOptions options);
-- void rubberband_set_detector_option(RubberBandState,
-- RubberBandOptions options);
-- void rubberband_set_phase_option(RubberBandState, RubberBandOptions options);
-- void rubberband_set_formant_option(RubberBandState,
-- RubberBandOptions options);
-- void rubberband_set_pitch_option(RubberBandState, RubberBandOptions options);
--
-- void rubberband_set_max_process_size(RubberBandState, unsigned int samples);
-- void rubberband_set_key_frame_map(RubberBandState,
-- unsigned int keyframecount, unsigned int *from, unsigned int *to);
--
-- unsigned int rubberband_get_channel_count(const RubberBandState);
--
-- void rubberband_calculate_stretch(RubberBandState);
--
-- void rubberband_set_debug_level(RubberBandState, int level);
-- void rubberband_set_default_debug_level(int level);
