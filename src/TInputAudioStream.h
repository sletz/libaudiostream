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

#ifndef __TInputAudioStream__
#define __TInputAudioStream__

#include "TAudioStream.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"

//-------------------------
// Class TInputAudioStream
//-------------------------
/*!
\brief The real-time input stream.
*/

class TInputAudioStream : public TAudioStream
{

    public:

        TInputAudioStream()
        {}
        virtual ~TInputAudioStream()
        {}

        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            assert_stream(framesNum, framePos);
            assert(TSharedBuffers::GetInBuffer());
            
            //printf("TInputAudioStream::Read framesNum %ld\n", framesNum);
            
            float** temp1 = (float**)alloca(buffer->GetChannels()*sizeof(float*));
            float** temp2 = (float**)alloca(TAudioGlobals::fInput*sizeof(float*));
            
            UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos, temp1),
                                             //TSharedBuffers::GetInBuffer(framesNum, TAudioGlobals::fInput, temp2),
                                             TSharedBuffers::GetInBuffer(),
                                             framesNum, TAudioGlobals::fInput);
			return framesNum;
        }

        void Reset()
        {}

        // Cut the beginning of the stream
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            printf("TInputAudioStream::CutBegin error\n");
            assert(false);
            return NULL;
        }

        // Length in frames
        virtual long Length()
        {
            return LONG_MAX;
        } 	
        virtual long Channels()
        {
            return TAudioGlobals::fInput;
        } 	
        virtual TAudioStreamPtr Copy()
        {
            printf("TInputAudioStream::Copy error\n");
            assert(false);
            return NULL;
        }
};

typedef TInputAudioStream * TInputAudioStreamPtr;

#endif
