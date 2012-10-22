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

#ifndef __TWriteFileAudioStream__
#define __TWriteFileAudioStream__

#include "TFileAudioStream.h"
#include "TAudioConstants.h"
#include <string>

using namespace std;

//-----------------------------
// Class TWriteFileAudioStream
//-----------------------------
/*!
\brief  A TWriteFileAudioStream is a LibSndFile based disk writer.
*/

class TWriteFileAudioStream : public TFileAudioStream, public TUnaryAudioStream
{

    private:

        long fFormat;
		void Open();
		void Close();
		void Flush();
		
		static void CloseAux(TWriteFileAudioStream* obj, long u1, long u2, long u3);

    protected:

        long Write(SHORT_BUFFER buffer, long framesNum, long framePos);

    public:

        TWriteFileAudioStream(string name, TAudioStreamPtr stream, long format);
        TWriteFileAudioStream(string name, SHORT_BUFFER buffer, TAudioStreamPtr stream, long format);
        virtual ~TWriteFileAudioStream();

        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels);

        void Reset();
       
        TAudioStreamPtr CutBegin(long frames)
        {
            return new TWriteFileAudioStream(fName, fStream->CutBegin(frames), fFormat);
        }
        long Length()
        {
            return fStream->Length();
        }
        TAudioStreamPtr Copy()
        {
            return new TWriteFileAudioStream(fName, fStream->Copy(), fFormat);
        }
};

typedef TWriteFileAudioStream * TWriteFileAudioStreamPtr;

#endif

