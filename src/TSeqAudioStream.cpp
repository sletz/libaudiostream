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

#include "TSeqAudioStream.h"
#include "TFadeAudioStream.h"
#include "TNullAudioStream.h"
#include "TAudioGlobals.h"

TSeqAudioStream::TSeqAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade): TBinaryAudioStream(s1, s2, s1)
{
    // A FINIR : aligner le crossFade sur des multiples de la taille des buffers

    if (crossFade > 0) {
        fStream1 = new TFadeAudioStream(s1, TAudioGlobals::fBufferSize, crossFade);
        fStream2 = new TFadeAudioStream(s2, crossFade, TAudioGlobals::fBufferSize);
    } else {
        fStream1 = s1;
        fStream2 = s2;
    }
	
    fFramesNum = UTools::Max(0, s1->Length() - crossFade); 
    fCrossFade = crossFade;
    fStream = fStream1;
    fCurFrame = 0;
}

long TSeqAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
    assert(fStream);
    long res = fStream->Read(buffer, framesNum, framePos, channels);
    fCurFrame += res;

    if (fStream == fStream1) {
        if (res < framesNum) { // End of fStream1
            fStream = fStream2;
            if (fCurFrame > fFramesNum) { // CrossFade
                return fStream->Read(buffer, framesNum, framePos, channels); // Mix with the end of the buffer
            } else {
                return res + Read(buffer, framesNum - res, framePos + res, channels); // Read the end of the buffer
            }
        } else if (fCurFrame > fFramesNum) {
            // Mix FadeOut of fStream1 with FadeIn of fStream2
            fStream2->Read(buffer, framesNum, framePos, channels);
        }
    }

    return res;
}

/*
 CutBegin(Seq(s1, s2), n) ==> NullStream if n >= Length(Seq(s1, s2))
 CutBegin(Seq(s1, s2), n) ==> Seq(CutBegin (s1, n) s2) if n < Length(s1)
 CutBegin(Seq(s1, s2), n) ==> CutBegin(s2, n - Length(s1)) if n >= Length(s1)
*/

TAudioStreamPtr TSeqAudioStream::CutBegin(long frames)
{
    long length1 = fStream1->Length();
	long length2 = fStream2->Length();

	if (frames < length1) {						// in first stream
        return new TSeqAudioStream(fStream1->CutBegin(frames), fStream2->Copy(), fCrossFade);
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
    fCurFrame = 0;
}




