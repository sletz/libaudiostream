/*

Copyright © Grame 2002

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

#include "TLoopAudioStream.h"
#include "TSeqAudioStream.h"
#include "TAudioGlobals.h"

TLoopAudioStream::TLoopAudioStream(TAudioStreamPtr stream, long loop): TDecoratedAudioStream(stream)
{
    fLoopNum = loop;
    fCurLoop = 0;
}

long TLoopAudioStream::Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
{
    long res = fStream->Read(buffer, framesNum, framePos, channels);

    if ((res < framesNum) && (++fCurLoop < fLoopNum)) { // Loop
        fStream->Reset();
        return res + Read(buffer, framesNum - res, framePos + res, channels); // Read the end of the buffer
    } else {
        return res;
    }
}

/*
CutBegin(Loop (s, l), n) ==> Seq (CutBegin(s,n % Length(s)), Loop (Copy (s), n / Length(s)))
*/

TAudioStreamPtr TLoopAudioStream::CutBegin(long frames)
{
    long length = fStream->Length();
    long n1 = frames / length + 1;
    long n2 = frames % length;
    return new TSeqAudioStream(fStream->CutBegin(n2), new TLoopAudioStream(fStream->Copy(), fLoopNum - n1), 0);
}

void TLoopAudioStream::Reset()
{
    assert(fStream);
    fCurLoop = 0;
    fStream->Reset();
}


