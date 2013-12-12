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

#include "TEffectAudioStream.h"
#include "TNullAudioStream.h"
#include "TSeqAudioStream.h"

TEffectAudioStream::TEffectAudioStream(TAudioStreamPtr stream, TAudioEffectInterfacePtr effect, long fadeIn, long fadeOut)
{
    // Add rest
    fStream = new TFadeAudioStream(new TSeqAudioStream(stream, new TNullAudioStream(fadeOut), fadeIn), fadeIn, fadeOut);
    fEffect = effect;
    fFadeIn = fadeIn;
    fFadeOut = fadeOut;
	fBufferIn = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, fEffect->Inputs());
    fBufferOut = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, fEffect->Outputs());
}

TAudioStreamPtr TEffectAudioStream::CutBegin(long frames)
{
    return new TEffectAudioStream(fStream->CutBegin(frames), fEffect->Copy(), fFadeIn, fFadeOut);
}

long TEffectAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);
    
    float* temp1[fBufferIn->GetChannels()];
    float* temp2[fBufferOut->GetChannels()];
    float* temp3[buffer->GetChannels()];
    
    /* Cleanup temporary fBuffer */
    UAudioTools::ZeroFloatBlk(fBufferIn->GetFrame(0, temp1), TAudioGlobals::fBufferSize, fEffect->Inputs());
    
    // Use temporary fBuffer from the beginning
    long res = fStream->Read(fBufferIn, framesNum, 0);
     
    // Use temporary fBuffer from the beginning
    fEffect->Process(fBufferIn->GetFrame(0, temp1), fBufferOut->GetFrame(0, temp2), framesNum);
    
    // Mix in buffer
	UAudioTools::MixFrameToFrameBlk(buffer->GetFrame(framePos, temp3),
									fBufferOut->GetFrame(0, temp2),
                                    framesNum,
									fStream->Channels());
      
    return res;
}

void TEffectAudioStream::Reset()
{
    fStream->Reset();
    fEffect->Reset();
}

TAudioStreamPtr TEffectAudioStream::Copy()
{
    return new TEffectAudioStream(fStream->Copy(), fEffect->Copy(), fFadeIn, fFadeOut);
}


