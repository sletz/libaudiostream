/*
Copyright (C) Grame 2002-2013

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
#include "TLASException.h"
#include "UTools.h"

TSampleRateAudioStream::TSampleRateAudioStream(TAudioStreamPtr stream, double ratio, unsigned int quality)
    :TDecoratedAudioStream(stream)
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
    // Resampler always openned in stereo mode even if only one (mono stream...) will be actually used.
    fResampler = src_new(quality, TAudioGlobals::fOutput, &error);
    fRatio = ratio;
    if (error != 0) {
        throw TLASException(src_strerror(error));
    }
     
    fReadPos = 0;
    fReadFrames = 0;    
    fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
}

TSampleRateAudioStream::~TSampleRateAudioStream()
{
	src_delete(fResampler);
    delete fBuffer;
}

TAudioStreamPtr TSampleRateAudioStream::CutBegin(long frames)
{
    return new TSampleRateAudioStream(fStream->CutBegin(frames / fRatio), fRatio);
}

long TSampleRateAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    int written = 0;
    SRC_DATA src_data;
    bool end = false;
    
    float* temp1[fBuffer->GetChannels()];
    float* temp2[buffer->GetChannels()];
    
    while (written < framesNum && !end) {
    
        if (fReadFrames == 0) {
            // Read input
            UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0, temp1), TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
            fReadFrames = fStream->Read(fBuffer, TAudioGlobals::fBufferSize, 0);
            fReadPos = 0;
            end = fReadFrames < TAudioGlobals::fBufferSize;
        }
        
        float tmp_buffer_in[fStream->Channels() * framesNum];
        float tmp_buffer_out[fStream->Channels() * framesNum];
        
        UAudioTools::Interleave(tmp_buffer_in, fBuffer->GetFrame(fReadPos, temp1), framesNum, fStream->Channels());
 
        //src_data.data_in = fBuffer->GetFrame(fReadPos);
        //src_data.data_out = buffer->GetFrame(framePos);
        
        src_data.data_in = tmp_buffer_in;
        src_data.data_out = tmp_buffer_out;
        
        src_data.input_frames = fReadFrames;
        src_data.output_frames = int(framesNum - written);
        src_data.end_of_input = end;
        src_data.src_ratio = fRatio;
        
        int res = src_process(fResampler, &src_data);
        if (res != 0) {
            printf("TSampleRateAudioStream::Read ratio = %f err = %s", fRatio, src_strerror(res));
            return written;
        }
        
        written += src_data.output_frames_gen;
        framePos += src_data.output_frames_gen;
        
        fReadPos += src_data.input_frames_used;
        fReadFrames -= src_data.input_frames_used; 
        
        UAudioTools::Deinterleave(buffer->GetFrame(framePos, temp2), tmp_buffer_out, framesNum, fStream->Channels());
    }
      
    return written;
}

void TSampleRateAudioStream::Reset()
{
    TDecoratedAudioStream::Reset();
    src_reset(fResampler);
}

TAudioStreamPtr TSampleRateAudioStream::Copy()
{
    return new TSampleRateAudioStream(fStream->Copy(), fRatio);
}



