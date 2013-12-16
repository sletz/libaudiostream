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

TParAudioStream::TParAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2): TBinaryAudioStream(s1, s2, NULL)
{
    fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, s2->Channels());  // TO CHECK: is s2->Channels() correct? 
}
TParAudioStream::~TParAudioStream()
{
    delete fBuffer;
}

long TParAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);
    
    if (fStream) { // One of the 2 stream is finished
        return fStream->Read(buffer, framesNum, framePos);
    } else {
        float* temp1[fBuffer->GetChannels()];
        float* temp2[buffer->GetChannels()];
        long res1 = fStream1->Read(buffer, framesNum, framePos);
        UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0, temp1), TAudioGlobals::fBufferSize, fStream2->Channels());
        if (res1 < framesNum) {
            fStream = fStream2; // Stream1 is finished, fStream variable is used as the remaining stream
            long res2 = fStream2->Read(fBuffer, framesNum, 0);
            UAudioTools::CopyChannelsTo(buffer->GetFrame(0, temp2), fBuffer->GetFrame(0, temp1), res2, fStream1->Channels(), fStream2->Channels());
            return res2;
        } else {
            long res2 = fStream2->Read(fBuffer, framesNum, 0);
            UAudioTools::CopyChannelsTo(buffer->GetFrame(0, temp2), fBuffer->GetFrame(0, temp1), res2, fStream1->Channels(), fStream2->Channels());
            if (res2 < framesNum) {
                fStream = fStream1; // Stream2 is finished, fStream variable is used as the remaining stream
            }
            return res1;
        }
    }
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

