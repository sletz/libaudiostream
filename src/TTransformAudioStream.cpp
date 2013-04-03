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

#include "TTransformAudioStream.h"
#include "TNullAudioStream.h"
#include "TSeqAudioStream.h"

TTransformAudioStream::TTransformAudioStream(TAudioStreamPtr stream, TAudioEffectListPtr effectList, long fadeIn, long fadeOut)
{
    fEffectList = effectList;
    // Add rest
    fStream = new TFadeAudioStream(new TSeqAudioStream(stream, new TNullAudioStream(fadeOut), fadeIn), fadeIn, fadeOut);
    fFadeIn = fadeIn;
    fFadeOut = fadeOut;
	fBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, TAudioGlobals::fOutput);
}

TAudioStreamPtr TTransformAudioStream::CutBegin(long frames)
{
    return new TTransformAudioStream(fStream->CutBegin(frames), fEffectList->Copy(), fFadeIn, fFadeOut);
}

long TTransformAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
    /* Cleanup temporary fBuffer */
	UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
    
    /* Use temporary fBuffer from the beginning */
    long res = fStream->Read(fBuffer, framesNum, 0, channels);
     
    /* Use temporary fBuffer from the beginning */
    fEffectList->Process(fBuffer->GetFrame(0), framesNum, channels);
    
   /* Use temporary fBuffer from the beginning */
    UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos), fBuffer->GetFrame(0), framesNum, channels);
     
    return res;
}

void TTransformAudioStream::Reset()
{
    fStream->Reset();
    fEffectList->Reset();
}

TAudioStreamPtr TTransformAudioStream::Copy()
{
    return new TTransformAudioStream(fStream->Copy(), fEffectList->Copy(), fFadeIn, fFadeOut);
}


