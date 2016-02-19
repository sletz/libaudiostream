/*
Copyright (C) Grame 2002-2014

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
    fResampler = src_new(quality, stream->Channels(), &error);
    fRatio = ratio;
    if (error != 0) {
        throw TLASException(src_strerror(error));
    }
     
    fReadPos = 0;
    fReadFrames = 0;    
    fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, stream->Channels());
    
    fTmpBufferIn = new float[stream->Channels() * TAudioGlobals::fBufferSize];
    fTmpBufferOut = new float[stream->Channels() * TAudioGlobals::fBufferSize];
}

TSampleRateAudioStream::~TSampleRateAudioStream()
{
	src_delete(fResampler);
    delete fBuffer;
    delete [] fTmpBufferIn;
    delete [] fTmpBufferOut;
}

TAudioStreamPtr TSampleRateAudioStream::CutBegin(long frames)
{
    return new TSampleRateAudioStream(fStream->CutBegin(frames / fRatio), fRatio);
}

long TSampleRateAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);
    
    int written = 0;
    bool end = false;
    
    float** temp1 = (float**)alloca(fBuffer->GetChannels()*sizeof(float*));
    float** temp2 = (float**)alloca(buffer->GetChannels()*sizeof(float*));

    while (written < framesNum && !end) {
    
        if (fReadFrames == 0) {
            // Read input
            UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0, temp1), TAudioGlobals::fBufferSize, fStream->Channels());
            fReadFrames = fStream->Read(fBuffer, TAudioGlobals::fBufferSize, 0);
            fReadPos = 0;
            end = fReadFrames < TAudioGlobals::fBufferSize;
        }
        
        UAudioTools::Interleave(fTmpBufferIn, fBuffer->GetFrame(fReadPos, temp1), fReadFrames, fStream->Channels());
       
        SRC_DATA src_data;
        src_data.data_in = fTmpBufferIn;
        src_data.data_out = fTmpBufferOut;
        src_data.input_frames = fReadFrames;
        src_data.output_frames = int(framesNum - written);
        src_data.end_of_input = end;
        src_data.src_ratio = fRatio;
        
        int res = src_process(fResampler, &src_data);
        if (res != 0) {
            printf("TSampleRateAudioStream::Read ratio = %f err = %s\n", fRatio, src_strerror(res));
            return written;
        }
        
        UAudioTools::Deinterleave(buffer->GetFrame(framePos, temp2), fTmpBufferOut, int(framesNum - written), fStream->Channels());
        
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

