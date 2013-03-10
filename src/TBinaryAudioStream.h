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

#ifndef __TBinaryAudioStream__
#define __TBinaryAudioStream__

#include "TAudioStream.h"

//--------------------------
// Class TBinaryAudioStream
//--------------------------
/*!
\brief  A TBinaryAudioStream contains two streams, not directly instanciated.
*/

class TBinaryAudioStream : public TDecoratedAudioStream
{

    protected:

        TAudioStreamPtr fStream1, fStream2;

    public:

        TBinaryAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2, TAudioStreamPtr init)
                : TDecoratedAudioStream(init), fStream1(s1), fStream2(s2)
        {}
        virtual ~TBinaryAudioStream()
        {}
	
        virtual long Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels) = 0;

        virtual void Reset()
        {
            fStream1->Reset();
            fStream2->Reset();
        }
		
		virtual TAudioStreamPtr CutBegin(long frames) = 0;
        virtual long Length() = 0;
        virtual long Channels() = 0;
        virtual TAudioStreamPtr Copy() = 0;

        TAudioStreamPtr GetBranch1()
        {
            return fStream1;
        }
        TAudioStreamPtr GetBranch2()
        {
            return fStream2;
        }

};

typedef TBinaryAudioStream * TBinaryAudioStreamPtr;

#endif
