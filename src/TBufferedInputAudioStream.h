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
\brief Buffered input stream
*/

class TBufferedInputAudioStream : public TBufferedAudioStream
{
    private:
        
        FLOAT_BUFFER fTmpBuffer;
    
    public:
     
        TBufferedInputAudioStream(long endFrame): TBufferedAudioStream()
        {
            fFramesNum = endFrame;
            
            // Hack : always stereo for now
            fChannels = 2;
            
            // Dynamic allocation
            fMemoryBuffer = new TLocalAudioBuffer<float>(endFrame, fChannels);
            fTmpBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fBufferSize, fChannels);
        }
        virtual ~TBufferedInputAudioStream()
        {
            delete fMemoryBuffer;
            delete fTmpBuffer;
        }
        
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            //printf("Memory read framesNum = %d framePos = %d\n", framesNum, framePos);
        }
        
        long Write(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            //printf("Memory write framesNum = %d framePos = %d\n", framesNum, framePos);
        }

        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
        {
            return 0;
        }
    
        virtual long Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
        {
            // Read input
            assert(TSharedBuffers::GetInBuffer());
            UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos),
                                             TSharedBuffers::GetInBuffer(),
                                             framesNum,
                                             channels);
                                             
            // Read input and write it to memory
            UAudioTools::ZeroFloatBlk(fTmpBuffer->GetFrame(0), TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
            UAudioTools::MixFrameToFrameBlk1(fTmpBuffer->GetFrame(framePos),
                                             TSharedBuffers::GetInBuffer(),
                                             framesNum,
                                             channels);
            return TBufferedAudioStream::Write(fTmpBuffer, framesNum, framePos, channels); 
        }

        virtual TAudioStreamPtr CutBegin(long frames)
        {
            printf("TBufferedInputAudioStream::CutBegin Error\n");
            assert(false);
            return NULL;
        }
        virtual long Length() 
        {
            return fFramesNum;
        } 
        virtual TAudioStreamPtr Copy() 
        {
            printf("TBufferedInputAudioStream::Copy Error\n");
            assert(false);
            return NULL;
        }
};

typedef TBufferedInputAudioStream * TBufferedInputAudioStreamPtr;

#endif

