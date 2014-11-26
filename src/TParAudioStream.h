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

#ifndef __TParAudioStream__
#define __TParAudioStream__

#include "TBinaryAudioStream.h"
#include "UTools.h"

//-----------------------
// Class TParAudioStream
//-----------------------
/*!
\brief A TParAudioStream mix two streams.
*/

class TParAudioStream : public TBinaryAudioStream
{

    private:
    
        FLOAT_BUFFER fBuffer;
        
    public:

        TParAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2);
        virtual ~TParAudioStream();
    
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);

        void Reset();
        
        TAudioStreamPtr CutBegin(long frames);
        
        long Length()
        {
            return UTools::Max(fStream1->Length(), fStream2->Length());
        }
        
        long Channels()
        {
            return fStream1->Channels() + fStream2->Channels();
        }
        
        TAudioStreamPtr Copy()
        {
            return new TParAudioStream(fStream1->Copy(), fStream2->Copy());
        }
        
        long SetPos(long frames)
        {
            long res1 = fStream1->SetPos(frames);
            long res2 = fStream2->SetPos(frames);
            return (res1 != NO_ERR) ? res1 : res2;
        }
};

typedef TParAudioStream * TParAudioStreamPtr;

#endif
