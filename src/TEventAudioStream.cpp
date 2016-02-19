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

#include "TEventAudioStream.h"

/*
long TEventSeqAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert(fStream);
    assert_stream(framesNum, framePos);
    
    long res = fStream->Read(buffer, framesNum, framePos);
    fCurFrame += res;

    if (fStream == fStream1) {
        if (res < framesNum || fFired) { // End of fStream1
            fStream = fStream2;
            if (fCurFrame > fFramesNum) { // CrossFade
                return fStream->Read(buffer, framesNum, framePos); // Mix with the end of the buffer
            } else {
                return res + Read(buffer, framesNum - res, framePos + res); // Read the end of the buffer
            }
        } else if (fCurFrame > fFramesNum) {
            // Mix FadeOut of fStream1 with FadeIn of fStream2
            fStream2->Read(buffer, framesNum, framePos);
        }
    }

    return res;
}

void TEventSeqAudioStream::Reset()
{
    fFired = false;
    TSeqAudioStream::Reset();
}

*/