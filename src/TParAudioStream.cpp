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

#include "TParAudioStream.h"
#include "UAudioTools.h"
#include "TAudioGlobals.h"
#include <assert.h>

long TParAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    // TODO
    /*
    if (fStream) { // One of the 2 stream is finished
        return fStream->Read(buffer, framesNum, framePos);
    } else {
        long res1 = fStream1->Read(buffer, framesNum, framePos);

        if (res1 < framesNum) {
            fStream = fStream2; // Stream1 is finished, fStream variable is used as the remaining stream
            return fStream2->Read(buffer, framesNum, framePos);
        } else {
            long res2 = fStream2->Read(buffer, framesNum, framePos);
            if (res2 < framesNum) {
                fStream = fStream1; // Stream2 is finished, fStream variable is used as the remaining stream
            }
            return res1;
        }
    }
    */
}

/*
CutBegin (Par (s1, s2), n) ==> Par (CutBegin (s1, n), CutBegin (s2, n)) 
*/

TAudioStreamPtr TParAudioStream::CutBegin(long frames)
{
    return new TParAudioStream(fStream1->CutBegin(frames), fStream2->CutBegin(frames));
}

void TParAudioStream::Reset()
{
    TBinaryAudioStream::Reset();
    fStream = 0; // The unique stream is reset to NULL
}

