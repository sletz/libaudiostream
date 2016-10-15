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

#ifndef __TFileAudioStream__
#define __TFileAudioStream__

#include "TBufferedAudioStream.h"
#include "TCmdHandler.h"
#include "TAudioConstants.h"
#include <sndfile.h>
#include <string>

//------------------------
// Class TFileAudioStream
//------------------------
/*!
\brief The base class for LibSndFile based disk streams.
*/

class TFileAudioStream : public TBufferedAudioStream, public TCmdHandler
{

    protected:

        std::string fName;
        SNDFILE* fFile;
        float* fFileBuffer;
     
        static void ReadBufferAux(TFileAudioStream* obj, FLOAT_BUFFER buffer, long framesNum, long framePos);
        static void WriteBufferAux(TFileAudioStream* obj, FLOAT_BUFFER buffer, long framesNum, long framePos);

        void ReadBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos);
        void WriteBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos);

    public:
     
        TFileAudioStream(std::string name): TBufferedAudioStream(), fName(name)
        {}
        virtual ~TFileAudioStream()
        {}

        virtual void Reset() = 0;
		virtual TAudioStreamPtr CutBegin(long frames) = 0;
        virtual long Length() = 0;
        virtual TAudioStreamPtr Copy() = 0;
};

typedef TFileAudioStream * TFileAudioStreamPtr;

#endif

