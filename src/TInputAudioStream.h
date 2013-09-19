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

#ifndef __TInputAudioStream__
#define __TInputAudioStream__

#include "TAudioStream.h"
#include "TSharedBuffers.h"

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
            assert(TSharedBuffers::GetInBuffer());
            UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos),
                                             TSharedBuffers::GetInBuffer(),
                                             framesNum);
            return framesNum;
        }

        void Reset()
        {}

        // Cut the beginning of the stream
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            printf("TInputAudioStream::CutBegin Error\n");
            assert(false);
            return NULL;
        }

        // Length in frames
        virtual long Length()
        {
            return 0x07FFFFFF;
        } 	
        virtual long Channels()
        {
            return 2;
        } 	
        virtual TAudioStreamPtr Copy()
        {
            printf("TInputAudioStream::Copy Error\n");
            assert(false);
            return NULL;
        }
};

typedef TInputAudioStream * TInputAudioStreamPtr;

#endif
