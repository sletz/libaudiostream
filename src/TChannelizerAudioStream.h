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
grame@rd.grame.fr

*/

#ifndef __TChannelizerAudioStream__
#define __TChannelizerAudioStream__

#include "TAudioStream.h"

//-------------------------------
// Class TChannelizerAudioStream
//-------------------------------
/*!
\brief  A TChannelizerAudioStream reorganize channels of the decorated stream.
*/

class TChannelizerAudioStream : public TDecoratedAudioStream
{

    private:

        long fChannels;
   
    public:
	
		// To be improved with a matrix based "in channels" to "out channels" description 
        TChannelizerAudioStream(TAudioStreamPtr stream, long output):TDecoratedAudioStream(stream),fChannels(output)
		{}
		
        virtual ~TChannelizerAudioStream()
        {}

		// The used UAudioTools::Short2FloatMix already does a limited form of "channels mapping", thus Read does not need to be redefined
          	
		long Channels()
        {
            return fChannels;
        }

		TAudioStreamPtr CutBegin(long frames)
        {
            assert(fStream);
            return new TChannelizerAudioStream(fStream->CutBegin(frames), fChannels);
        }

		TAudioStreamPtr Copy()
        {
            return new TChannelizerAudioStream(fStream->Copy(), fChannels);
        }
};

typedef TChannelizerAudioStream * TChannelizerAudioStreamPtr;

#endif

