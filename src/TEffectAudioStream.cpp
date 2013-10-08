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
    fEffect = effect;
    // Add rest
    fStream = new TFadeAudioStream(new TSeqAudioStream(stream, new TNullAudioStream(fadeOut), fadeIn), fadeIn, fadeOut);
    fFadeIn = fadeIn;
    fFadeOut = fadeOut;
	fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, stream->Channels());
}

TAudioStreamPtr TEffectAudioStream::CutBegin(long frames)
{
    return new TEffectAudioStream(fStream->CutBegin(frames), fEffect->Copy(), fFadeIn, fFadeOut);
}

long TEffectAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    /* Cleanup temporary fBuffer */
	UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBufferSize, fStream->Channels());
    
    /* Use temporary fBuffer from the beginning */
    long res = fStream->Read(fBuffer, framesNum, 0);
     
    /* Use temporary fBuffer from the beginning */
    fEffect->Process(fBuffer->GetFrame(0), buffer->GetFrame(framePos), framesNum);
      
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


