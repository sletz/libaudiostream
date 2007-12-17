/*
Copyright � Grame 2002-2007

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

#include "TAudioGlobals.h"
#include "TSoundTouchAudioStream.h"
#include "Utools.h"

TSoundTouchAudioStream::TSoundTouchAudioStream(TAudioStreamPtr stream, double* pitch_shift, double* time_strech)
{
	fStream = stream;
    fPitchShift = pitch_shift;
    fTimeStretch = time_strech;
	fPitchShiftVal = *pitch_shift;
	fTimeStretchVal = *time_strech;
	
	fSoundTouch = new soundtouch::SoundTouch();
	fBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fStream_Buffer_Size, TAudioGlobals::fOutput);
	
	fSoundTouch->setSampleRate(TAudioGlobals::fSample_Rate);
    fSoundTouch->setChannels(TAudioGlobals::fOutput);
	fSoundTouch->setSetting(SETTING_USE_AA_FILTER, 1);
	fSoundTouch->setTempo(fTimeStretchVal);
	fSoundTouch->setPitch(fPitchShiftVal);
}

TSoundTouchAudioStream::~TSoundTouchAudioStream()
{
	delete fSoundTouch;
	delete fBuffer;
}

TAudioStreamPtr TSoundTouchAudioStream::CutBegin(long frames)
{
    return new TSoundTouchAudioStream(fStream->CutBegin(frames), fPitchShift, fTimeStretch);
}

long TSoundTouchAudioStream::Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
{
	long read, produced, written = 0;
	int available;
	
	printf("TSoundTouchAudioStream PROCESS --------------\n");
	
	if (fTimeStretchVal != *fTimeStretch) {
		fTimeStretchVal = *fTimeStretch;
		fSoundTouch->setTempo(fTimeStretchVal);
	}
	if (fPitchShiftVal != *fPitchShift) {
		fPitchShiftVal = *fPitchShift;
		fSoundTouch->setPitch(fPitchShiftVal);
	}
		
	// Frames still available in the effect
	if ((available = fSoundTouch->numSamples()) > 0) {
		produced = fSoundTouch->receiveSamples(buffer->GetFrame(framePos), UTools::Min(available, int(framesNum)));
		printf("TSoundTouchAudioStream old available %d\n", available);
		
		// Move index
		framePos += produced;
		written += produced;
	}
	
	// End case
	if (written == framesNum) {
		printf("TSoundTouchAudioStream FINISH produced = %ld\n", produced);
	} else {
	
		printf("TSoundTouchAudioStream LOOP --------------\n");
		// Compute remaining needed frames
		do {
			// Read input
			UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBuffer_Size, TAudioGlobals::fOutput);
			read = fStream->Read(fBuffer, TAudioGlobals::fBuffer_Size, 0, channels);
			printf("TSoundTouchAudioStream read = %ld \n", read);
			
			// Process buffer
			fSoundTouch->putSamples(fBuffer->GetFrame(0), read);
			available = fSoundTouch->numSamples();
			produced = fSoundTouch->receiveSamples(buffer->GetFrame(framePos), UTools::Min(available, int(framesNum - written)));
			printf("TSoundTouchAudioStream available = %d \n", available);
	
			// Move index
			framePos += produced;
			written += produced;
			printf("TSoundTouchAudioStream written = %ld \n", written);
			
		} while (written < framesNum);
	}
	
	printf("TSoundTouchAudioStream RES written = %ld \n", written);
	return written;
}

void TSoundTouchAudioStream::Reset()
{
    fStream->Reset();
    fSoundTouch->clear();
}

TAudioStreamPtr TSoundTouchAudioStream::Copy()
{
    return new TSoundTouchAudioStream(fStream->Copy(), fPitchShift, fTimeStretch);
}



