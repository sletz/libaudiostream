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

#ifndef __TLoopAudioStream__
#define __TLoopAudioStream__

#include "TAudioStream.h"
#include "TAudioBuffer.h"

//------------------------
// Class TLoopAudioStream
//------------------------
/*!
\brief A TLoopAudioStream loops the decorated stream n times.
*/

class TLoopAudioStream : public TDecoratedAudioStream
{

    private:

        long fLoopNum;
        long fCurLoop;

    public:

        TLoopAudioStream(TAudioStreamPtr stream, long n);
        virtual ~TLoopAudioStream()
        {}

        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);

        void Reset();

        TAudioStreamPtr CutBegin(long frames);

        long Length()
        {
            uint64_t num = fLoopNum;
            uint64_t length = fStream->Length();
            uint64_t res = num * length;
            if ((num != 0 && res / num != length) || res >= LONG_MAX)
            {
                // Overflow
                return LONG_MAX;
            }
            else
            {
                return res;
            }
        }

        TAudioStreamPtr Copy()
        {
            return new TLoopAudioStream(fStream->Copy(), fLoopNum);
        }

        long SetPos(long frames)
        {
            long len = fStream->Length();
            fLoopNum = frames / len;
            return fStream->SetPos(frames % len);
        }
};

typedef TLoopAudioStream * TLoopAudioStreamPtr;

#endif
