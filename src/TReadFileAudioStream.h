/*
Copyright © Grame 2002-2007

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

#ifndef __TReadFileAudioStream__
#define __TReadFileAudioStream__

#include "TFileAudioStream.h"
#include "TAudioConstants.h"

//----------------------------
// Class TReadFileAudioStream
//----------------------------
/*!
\brief A TReadFileAudioStream is a LibSndFile based disk reader.
*/

class TReadFileAudioStream : public TFileAudioStream
{

    private:

        TAudioBuffer<short>* fCopyBuffer;
        long fBeginFrame;

        virtual long Read(TAudioBuffer<short>* buffer, long framesNum, long framePos);
        static void ReadEndBufferAux(TReadFileAudioStream* obj, long framesNum, long framePos);

    public:

        TReadFileAudioStream(string name, long beginFrame);
        virtual ~TReadFileAudioStream();

        // Ajouter constructor avec buffer pour partager le stream
        void ReadEndBuffer(long framesNum, long framePos);

        virtual void Reset();
		virtual TAudioStreamPtr CutBegin(long frames);
        virtual long Length()
        {
            return fFramesNum;
        }
        virtual TAudioStreamPtr Copy()
        {
            return new TReadFileAudioStream(fName, fBeginFrame);
        }
};

typedef TReadFileAudioStream * TReadFileAudioStreamPtr;

#endif

