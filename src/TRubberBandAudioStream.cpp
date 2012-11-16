/*
Copyright (C) Grame 2002-2012

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
#include "TRubberBandAudioStream.h"
#include "UTools.h"

using namespace RubberBand;

TRubberBandAudioStream::TRubberBandAudioStream(TAudioStreamPtr stream, double* pitch_shift, double* time_strech)
{
	fStream = stream;
    fPitchShift = pitch_shift;
    fTimeStretch = time_strech;
	fPitchShiftVal = *pitch_shift;
	fTimeStretchVal = *time_strech;
   
	fRubberBand = new RubberBandStretcher(TAudioGlobals::fSampleRate, stream->Channels(), RubberBandStretcher::OptionProcessRealTime);
	fBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, TAudioGlobals::fOutput);
	
	fRubberBand->setTimeRatio(1/fTimeStretchVal);
	fRubberBand->setPitchScale(fPitchShiftVal);
    
    int i;
    for (i = 0; i < 2; i++) {
        fTemp1[i] = (float*)calloc(TAudioGlobals::fBufferSize, sizeof(float));
        fTemp2[i] = (float*)calloc(TAudioGlobals::fBufferSize, sizeof(float));
    }
 }

TRubberBandAudioStream::~TRubberBandAudioStream()
{
	delete fRubberBand;
	delete fBuffer;
    
    int i;
	for (i = 0; i < 2; i++) {
		free(fTemp1[i]);
        free(fTemp2[i]);
	}
}

TAudioStreamPtr TRubberBandAudioStream::CutBegin(long frames)
{
    return new TRubberBandAudioStream(fStream->CutBegin(frames), fPitchShift, fTimeStretch);
}

long TRubberBandAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
  	if (fTimeStretchVal != *fTimeStretch) {
		fTimeStretchVal = *fTimeStretch;
		fRubberBand->setTimeRatio(1/fTimeStretchVal);
	}
	if (fPitchShiftVal != *fPitchShift) {
		fPitchShiftVal = *fPitchShift;
		fRubberBand->setPitchScale(fPitchShiftVal);
	}
    
    while (fRubberBand->available() < framesNum) {
        int needFrames = std::min((int)framesNum, (int)fRubberBand->getSamplesRequired());
        if (needFrames > 0) {
            UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0), TAudioGlobals::fBufferSize, TAudioGlobals::fOutput);
            int res = fStream->Read(fBuffer, needFrames, 0, channels);
            // Deinterleave...
            UAudioTools::Deinterleave(fTemp1, fBuffer->GetFrame(0), needFrames, channels);
            fRubberBand->process(fTemp1, needFrames, false);
        }
    }
    
    size_t res1 = fRubberBand->retrieve(fTemp2, std::min((int)framesNum, fRubberBand->available()));
     
    // Interleave...
    UAudioTools::Interleave(buffer->GetFrame(0), fTemp2, framesNum, channels);
	return framesNum;
}

void TRubberBandAudioStream::Reset()
{
    fStream->Reset();
    fRubberBand->reset();
    fRubberBand->setTimeRatio(1/fTimeStretchVal);
    fRubberBand->setPitchScale(fPitchShiftVal);
}

TAudioStreamPtr TRubberBandAudioStream::Copy()
{
    return new TRubberBandAudioStream(fStream->Copy(), fPitchShift, fTimeStretch);
}



