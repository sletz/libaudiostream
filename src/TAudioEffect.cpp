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

#include "TAudioEffect.h"
#include "UAudioTools.h"

void TAudioEffectList::Init(float fade_in_val, float fade_in_time, float fade_out_val, float fade_out_time)
{
    fFadeIn.setValue(fade_in_val);
    fFadeIn.setTarget(1.0f);
    fFadeIn.setTime(UAudioTools::ConvertFrameToSec(fade_in_time));

    fFadeOut.setValue(fade_out_val);
    fFadeOut.setTarget(0.0f);
    fFadeOut.setTime(UAudioTools::ConvertFrameToSec(fade_out_time));
}

void TAudioEffectList::FadeIn(long fadeIn, long fadeOut)
{
	if (size() > 0) {
		fFadeInFrames = fadeIn;
		fFadeOutFrames = fadeOut;
		Init(0.0f, float(fFadeInFrames), 1.0f, float(fFadeOutFrames));
		fStatus = kFadeIn;
	}
}

void TAudioEffectList::FadeOut()
{
	if (size() > 0) {
		Init(0.0f, float(fFadeInFrames), 1.0f, float(fFadeOutFrames));
		fStatus = kFadeOut;
	}
}

void TAudioEffectList::Process(float* buffer, long framesNum, long channels)
{
	if (size() > 0) {
		float** input = fTemp1;
		float** tmp_output = fTemp2;
		float** output = fTemp2;
		int i;
		
		// Fades
		switch (fStatus) {
		
			case kFadeIn:
				for (i = 0 ; i < framesNum; i++) {
					UAudioTools::MultFrame(&buffer[i * channels], fFadeIn.tick(), channels); // To improve...
				}
				if (fFadeIn.lastOut() >= 1.0f) {
					fStatus = kPlaying;
                }
				break;
			
			case kFadeOut: 
				for (i = 0 ; i < framesNum; i++) {
					UAudioTools::MultFrame(&buffer[i * channels], fFadeOut.tick(), channels); // To improve...
				}
				if (fFadeOut.lastOut() <= 0.0f) {
					fStatus = kIdle;
                }
				break;
		}
		
		// Deinterleave...
		UAudioTools::Deinterleave(input, buffer, framesNum, channels);
		
		// Process effects
		for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
			TAudioEffectInterfacePtr process = *iter;
			process->ProcessAux(input, tmp_output, framesNum, channels);
			output = tmp_output;
			// Swap buffers
			float** tmp = input;
			input = tmp_output;
			tmp_output = tmp;
		}
		
		// Interleave...
		UAudioTools::Interleave(buffer, output, framesNum, channels);
	}
}

void TAudioEffectList::Reset()
{
    for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
        TAudioEffectInterfacePtr process = *iter;
        process->Reset();
    }
}

TAudioEffectList::~TAudioEffectList()
{
	int i;
	
	for (i = 0; i < MAX_PLUG_CHANNELS; i++) {
		free(fTemp1[i]);
	}
	for (i = 0; i < MAX_PLUG_CHANNELS; i++) {
		free(fTemp2[i]);
	}
}

TAudioEffectListPtr TAudioEffectList::Copy()
{
    TAudioEffectListPtr copy = new TAudioEffectList();

    for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
        TAudioEffectInterfacePtr process = *iter;
        copy->push_front(process->Copy());
    }

    return copy;
}

void TAudioEffectListManager::Process(float* buffer, long framesNum, long channels)
{
	if (fMutex.TryLock() == 0) {
	
		if (fSwitchEffect) {
			if (fCurEffectList->GetStatus() == TAudioEffectList::kIdle) { // End of fCurEffectList FadeOut
				fCurEffectList = fNextEffectList;
				fSwitchEffect = false;
			} else { // CrossFade both effects
				memcpy(fTempBuffer, buffer, sizeof(float) * framesNum * channels);
				fCurEffectList->Process(buffer, framesNum, channels);
				fNextEffectList->Process(fTempBuffer, framesNum, channels);
				UAudioTools::MixFrameToFrameBlk1(buffer, fTempBuffer, framesNum, channels);
			}
		} else {
			fCurEffectList->Process(buffer, framesNum, channels);
		}
		
		fMutex.Unlock();
	}
}

