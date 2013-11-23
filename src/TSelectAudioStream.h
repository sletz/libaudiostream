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

#ifndef __TSelectAudioStream__
#define __TSelectAudioStream__

#include "TAudioStream.h"
#include "TAudioGlobals.h"
#include <vector>

//--------------------------
// Class TSelectAudioStream
//--------------------------
/*!
\brief  A TSelectAudioStream object keeps a selection of audio channels
*/

class TSelectAudioStream : public TDecoratedAudioStream
{

    private:

        std::vector<int> fSelection;
        FLOAT_BUFFER fBuffer;
     
    public:

        TSelectAudioStream(TAudioStreamPtr stream, const std::vector<int>& selection)
            :TDecoratedAudioStream(stream), fSelection(selection)
        {
            fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, stream->Channels());
        }
        virtual ~TSelectAudioStream()
        {
            delete fBuffer;
        }

        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return 0;
        }
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);
        
        long Channels()
        {
            return fSelection.size();
        }

        TAudioStreamPtr CutBegin(long frames);    // Length in frames
        TAudioStreamPtr Copy();
};

typedef TSelectAudioStream * TSelectAudioStreamPtr;

#endif
