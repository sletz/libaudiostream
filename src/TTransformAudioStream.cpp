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

#include "TTransformAudioStream.h"
#include "TNullAudioStream.h"
#include "TSeqAudioStream.h"

TTransformAudioStream::TTransformAudioStream(TAudioStreamPtr stream, TAudioEffectPtr effectList, long fadeIn, long fadeOut)
{
    fCurEffectList = effectList;
	fNextEffectList = 0;
    // Add rest
    fStream = new TFadeAudioStream(new TSeqAudioStream(stream, new TNullAudioStream(fadeOut), fadeIn), fadeIn, fadeOut);
    fFadeIn = fadeIn;
    fFadeOut = fadeOut;
	fBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fStream_Buffer_Size, TAudioGlobals::fOutput);
}

TAudioStreamPtr TTransformAudioStream::CutBegin(long frames)
{
    // A REVOIR
    return new TTransformAudioStream(fStream->CutBegin(frames), fCurEffectList->Copy(), fFadeIn, fFadeOut);
}

long TTransformAudioStream::Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
{
	UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBuffer_Size, TAudioGlobals::fOutput);
	// Possibly switch to a new effect list
	/*
	if (fNextEffectList) {
		delete fCurEffectList; //???
		fCurEffectList = fNextEffectList;
		fNextEffectList = 0;
	}
	*/
    int res = fStream->Read(fBuffer, framesNum, framePos, channels);
    fCurEffectList->Process(fBuffer->GetFrame(framePos), framesNum, channels);
	UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(framePos), fBuffer->GetFrame(framePos), framesNum, channels);
    return res;
}

void TTransformAudioStream::Reset()
{
    fStream->Reset();
    fCurEffectList->Reset();
}

TAudioStreamPtr TTransformAudioStream::Copy()
{
    return new TTransformAudioStream(fStream->Copy(), fCurEffectList->Copy(), fFadeIn, fFadeOut);
}


