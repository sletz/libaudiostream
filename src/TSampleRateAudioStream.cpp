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
	fResampler = src_new(quality, stream->Channels(), &error);
    fRatio = ratio;
    if (error != 0) {
        throw - 1;
    }
     
    fReadPos = 0;
    fReadFrames = 0;    
    fBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, TAudioGlobals::fOutput);
}

TSampleRateAudioStream::~TSampleRateAudioStream()
{
	src_delete(fResampler);
    delete fBuffer;
}

TAudioStreamPtr TSampleRateAudioStream::CutBegin(long frames)
{
    return new TSampleRateAudioStream(fStream->CutBegin(frames * fRatio), fRatio);
}

long TSampleRateAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
    int written = 0;
    SRC_DATA src_data;
    bool end = false;
    
    while (written < framesNum && !end) {
    
        if (fReadFrames == 0) {
            // Read input
            UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
            fReadFrames = fStream->Read(fBuffer, TAudioGlobals::fBufferSize, 0, channels);
            fReadPos = 0;
            end = fReadFrames < TAudioGlobals::fBufferSize;
        }
        
        src_data.data_in = fBuffer->GetFrame(fReadPos);
        src_data.data_out = buffer->GetFrame(framePos);
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



