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
\brief Buffered RT stream
*/

class TBufferedInputAudioStream : public TBufferedAudioStream
{

    protected:
   
 
    public:
     
        TBufferedInputAudioStream(long framesNum): TBufferedAudioStream()
        {
            fFramesNum = framesNum;
            // Dynamic allocation
            fMemoryBuffer = new TLocalAudioBuffer<float>(framesNum, fChannels);
        }
        virtual ~TBufferedInputAudioStream()
        {
            delete fMemoryBuffer;
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
            assert(TSharedBuffers::GetInBuffer());
            UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos),
                                             TSharedBuffers::GetInBuffer(),
                                             framesNum,
                                             channels);
            // Write buffer in memory
            return TBufferedAudioStream::Write(buffer, framesNum, framePos, channels); 
        }

        virtual void Reset() {}
		virtual TAudioStreamPtr CutBegin(long frames)
        {
            printf("TBufferedInputAudioStream::CutBegin Error\n");
            return NULL;
        }
        virtual long Length() 
        {
            return fFramesNum;
        } 
        virtual TAudioStreamPtr Copy() 
        {
            printf("TBufferedInputAudioStream::Copy Error\n");
            return NULL;
        }
};

typedef TBufferedInputAudioStream * TBufferedInputAudioStreamPtr;

#endif

