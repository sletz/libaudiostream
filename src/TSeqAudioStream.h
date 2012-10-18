/*

Copyright (C) Grame 2002-2012

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

#ifndef __TSeqAudioStream__
#define __TSeqAudioStream__

#include "TBinaryAudioStream.h"
#include "UTools.h"

//-----------------------
// Class TSeqAudioStream
//-----------------------
/*!
\brief  A TSeqAudioStream plays two streams in sequence.
*/

class TSeqAudioStream : public TBinaryAudioStream
{

    protected:

        long fCurFrame;
        long fFramesNum;
        long fCrossFade;

    public:

        TSeqAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade);
        virtual ~TSeqAudioStream()
        {}

        long Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);

        void Reset();
        TAudioStreamPtr CutBegin(long frames);
        long Length()
        {
            return fStream1->Length() + fStream2->Length() - fCrossFade;
        }
        long Channels()
        {
            return UTools::Max(fStream1->Channels(), fStream2->Channels());
        }
        TAudioStreamPtr Copy()
        {
            return new TSeqAudioStream(fStream1->Copy(), fStream2->Copy(), fCrossFade);
        }
};

typedef TSeqAudioStream * TSeqAudioStreamPtr;

#endif
