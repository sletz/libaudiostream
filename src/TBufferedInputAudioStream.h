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

#ifndef __TBufferedInputAudioStream__
#define __TBufferedInputAudioStream__

#include "TBufferedAudioStream.h"
#include "TSharedBuffers.h"
#include "TAudioConstants.h"

using namespace std;

//---------------------------------
// Class TBufferedInputAudioStream
//---------------------------------
/*!
\brief Buffered input stream (singleton like use)
*/

class TBufferedInputAudioStream : public TBufferedAudioStream
{
    private:
        
        FLOAT_BUFFER fTmpBuffer;
    
    public:
     
        TBufferedInputAudioStream(long endFrame): TBufferedAudioStream()
        {
            fFramesNum = endFrame;
            fChannels = TAudioGlobals::fInput;
            
            // Dynamic allocation
            fMemoryBuffer = new TLocalNonInterleavedAudioBuffer<float>(endFrame, fChannels, true);
            fTmpBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, fChannels);
        }
        virtual ~TBufferedInputAudioStream()
        {
            delete fMemoryBuffer;
            delete fTmpBuffer;
        }
        
        long ReadImp(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return framesNum;
        }
        
        long WriteImp(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return framesNum;
        }

        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return 0;
        }
    
        virtual long Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            assert_stream(framesNum, framePos);
            
            // Read input
            assert(TSharedBuffers::GetInBuffer());
            /*
            // Audio thru
            UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos),
                                             TSharedBuffers::GetInBuffer(),
                                             framesNum,
                                             channels);
            */
                                             
            // Read input and write it to memory
            float* temp1[fTmpBuffer->GetChannels()];
            float* temp2[TAudioGlobals::fInput];
            
            UAudioTools::ZeroFloatBlk(fTmpBuffer->GetFrame(0, temp1), TAudioGlobals::fBufferSize, TAudioGlobals::fInput);
            UAudioTools::MixFrameToFrameBlk1(fTmpBuffer->GetFrame(framePos, temp1),
                                             TSharedBuffers::GetInBuffer(framesNum, TAudioGlobals::fInput, temp2),
                                             //TSharedBuffers::GetInBuffer(),
                                             framesNum, TAudioGlobals::fInput);
                                             
            return TBufferedAudioStream::Write(fTmpBuffer, framesNum, framePos); 
        }

        virtual TAudioStreamPtr CutBegin(long frames)
        {
            printf("TBufferedInputAudioStream::CutBegin error\n");
            assert(false);
            return NULL;
        }
        virtual long Length() 
        {
            return fFramesNum;
        } 
        virtual TAudioStreamPtr Copy() 
        {
            printf("TBufferedInputAudioStream::Copy error\n");
            assert(false);
            return NULL;
        }
};

typedef TBufferedInputAudioStream * TBufferedInputAudioStreamPtr;

#endif

