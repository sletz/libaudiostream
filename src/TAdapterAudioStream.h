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

#ifndef __TAdapterAudioStream__
#define __TAdapterAudioStream__

#include "TAudioStream.h"
#include "TAudioGlobals.h"

//---------------------------
// Class TAdapterAudioStream
//---------------------------
/*!
\brief  A TAdapterAudioStream adapts channels of the decorated stream.
*/

class TAdapterAudioStream : public TDecoratedAudioStream
{

    private:

        long fChannels;
        FLOAT_BUFFER fAdaptBuffer;
   
    public:
	
        TAdapterAudioStream(TAudioStreamPtr stream, long channels):TDecoratedAudioStream(stream),fChannels(channels)
		{
            fAdaptBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, fChannels);
        }
		
        virtual ~TAdapterAudioStream()
        {
            delete fAdaptBuffer;
        }
        
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);
       	
		long Channels()
        {
            return fChannels;
        }

		TAudioStreamPtr CutBegin(long frames)
        {
            assert(fStream);
            return new TAdapterAudioStream(fStream->CutBegin(frames), fChannels);
        }

		TAudioStreamPtr Copy()
        {
            return new TAdapterAudioStream(fStream->Copy(), fChannels);
        }
};

typedef TAdapterAudioStream * TAdapterAudioStreamPtr;

#endif

