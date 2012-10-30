/*
Copyright (C) Grame 2002-2012

This library is free software; you can redistribute it and modify it under
the terms of the GNU Library General Public License as published by the
Free Software Foundation version 2 of the License, or any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
research@grame.fr

*/

#include "TAudioGlobals.h"
#include "TSampleRateAudioStream.h"
#include "UTools.h"

TSampleRateAudioStream::TSampleRateAudioStream(TAudioStreamPtr stream, double ratio, unsigned int quality)
{
    switch (quality) {
       case 0:
            quality = SRC_LINEAR;
            break;
        case 1:
            quality = SRC_ZERO_ORDER_HOLD;
            break;
        case 2:
            quality = SRC_SINC_FASTEST;
            break;
        case 3:
            quality = SRC_SINC_MEDIUM_QUALITY;
            break;
        case 4:
            quality = SRC_SINC_BEST_QUALITY;
            break;
        default:
            quality = SRC_LINEAR;
            printf("Out of range resample quality\n");
            break;
    }
    int error;
	fResampler = src_new(quality, stream->Channels(), &error);
    fRatio = ratio;
    if (error != 0) {
        throw - 1;
    }
}

TSampleRateAudioStream::~TSampleRateAudioStream()
{
	src_delete(fResampler);
}

TAudioStreamPtr TSampleRateAudioStream::CutBegin(long frames)
{
    return new TSampleRateAudioStream(fStream->CutBegin(frames * fRatio), fRatio);
}

long TSampleRateAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
    /*
    SRC_DATA src_data;
    src_data.data_in = (jack_default_audio_sample_t*)ring_buffer_data[j].buf;
    src_data.data_out = &buffer[written_frames];
    src_data.input_frames = ring_buffer_data[j].len / sizeof(jack_default_audio_sample_t);
    src_data.output_frames = frames_to_write;
    src_data.end_of_input = 0;
    src_data.src_ratio = fRatio;

	return written;
    */
    return 0;
}

void TSampleRateAudioStream::Reset()
{
    src_reset(fResampler);
}

TAudioStreamPtr TSampleRateAudioStream::Copy()
{
    return new TSampleRateAudioStream(fStream->Copy(), fRatio);
}



