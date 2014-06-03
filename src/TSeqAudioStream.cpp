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

#include "TSeqAudioStream.h"
#include "TFadeAudioStream.h"
#include "TNullAudioStream.h"
#include "TAudioGlobals.h"
#include "TCutEndAudioStream.h"
#include "TMixAudioStream.h"

TSeqAudioStream::TSeqAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2): TBinaryAudioStream(s1, s2, s1)
{}

long TSeqAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert(fStream);
    assert_stream(framesNum, framePos);
     
    long res = fStream->Read(buffer, framesNum, framePos);
 
    if (fStream == fStream1) {
        if (res < framesNum) { // End of fStream1
            fStream = fStream2;
            return res + Read(buffer, framesNum - res, framePos + res); // Read the end of the buffer
        }
    }

    return res;
}

/*
 CutBegin(Seq(s1, s2), n) ==> NullStream if n >= Length(Seq(s1, s2))
 CutBegin(Seq(s1, s2), n) ==> Seq(CutBegin(s1, n) s2) if n < Length(s1)
 CutBegin(Seq(s1, s2), n) ==> CutBegin(s2, n - Length(s1)) if n >= Length(s1)
*/

TAudioStreamPtr TSeqAudioStream::CutBegin(long frames)
{
    long length1 = fStream1->Length();
	long length2 = fStream2->Length();

	if (frames < length1) {						// in first stream
        return new TSeqAudioStream(fStream1->CutBegin(frames), fStream2->Copy());
    } else if (frames < length1 + length2) {	// in second stream
        return fStream2->CutBegin(frames - length1);
    } else {
		return new TNullAudioStream(0);
    }
}

void TSeqAudioStream::Reset()
{
    TBinaryAudioStream::Reset();
    fStream = fStream1;
}




