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

#include "TAudioGlobals.h"
#include "TSoundTouchAudioStream.h"
#include "UTools.h"

TSoundTouchAudioStream::TSoundTouchAudioStream(TAudioStreamPtr stream, double* pitch_shift, double* time_strech)
{
	fStream = stream;
    fPitchShift = pitch_shift;
    fTimeStretch = time_strech;
	fPitchShiftVal = *pitch_shift;
	fTimeStretchVal = *time_strech;
	
	fSoundTouch = new soundtouch::SoundTouch();
	fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
	
	fSoundTouch->setSampleRate(TAudioGlobals::fSampleRate);
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

long TSoundTouchAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);
    
	long read, produced, written = 0;
	int available;
	
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
		
		// Move index
		framePos += produced;
		written += produced;
	}
	
	// End case
	if (written == framesNum) {
		// End
	} else {
	
		// Compute remaining needed frames
		do {
			// Read input
			UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
			read = fStream->Read(fBuffer, TAudioGlobals::fBufferSize, 0);
			
			// Process buffer
			fSoundTouch->putSamples(fBuffer->GetFrame(0), read);
			available = fSoundTouch->numSamples();
			produced = fSoundTouch->receiveSamples(buffer->GetFrame(framePos), UTools::Min(available, int(framesNum - written)));
	
			// Move index
			framePos += produced;
			written += produced;
			
		} while (written < framesNum);
	}
	
	return written;
}

void TSoundTouchAudioStream::Reset()
{
    fStream->Reset();
    fSoundTouch->clear();
    fSoundTouch->setTempo(fTimeStretchVal);
    fSoundTouch->setPitch(fPitchShiftVal);
}

TAudioStreamPtr TSoundTouchAudioStream::Copy()
{
    return new TSoundTouchAudioStream(fStream->Copy(), fPitchShift, fTimeStretch);
}



