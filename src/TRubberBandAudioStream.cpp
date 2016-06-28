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
#include "TRubberBandAudioStream.h"
#include "UTools.h"
#undef min
#undef max

using namespace RubberBand;

TRubberBandAudioStream::TRubberBandAudioStream(
        TAudioStreamPtr stream,
        double* pitch_shift,
        double* time_stretch):
    TDecoratedAudioStream(stream),
    fPitchShift(pitch_shift),
    fTimeStretch(time_stretch),
    fPitchShiftVal(*fPitchShift),
    fTimeStretchVal(*fTimeStretch),
    fRubberBand(TAudioGlobals::fSampleRate, stream->Channels(), RubberBandStretcher::OptionProcessRealTime),
    fBuffer(TAudioGlobals::fBufferSize, stream->Channels())
{
    fRubberBand.setTimeRatio(1. / fTimeStretchVal);
    fRubberBand.setPitchScale(fPitchShiftVal);
}

TRubberBandAudioStream::~TRubberBandAudioStream()
{
}

TAudioStreamPtr TRubberBandAudioStream::CutBegin(long frames)
{
    return new TRubberBandAudioStream(fStream->CutBegin(frames), fPitchShift, fTimeStretch);
}

long TRubberBandAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);

    if (fTimeStretchVal != *fTimeStretch) {
        fTimeStretchVal = *fTimeStretch;
        fRubberBand.setTimeRatio(1./fTimeStretchVal);
    }
    if (fPitchShiftVal != *fPitchShift) {
        fPitchShiftVal = *fPitchShift;
        fRubberBand.setPitchScale(fPitchShiftVal);
    }

    float** temp1 = (float**)alloca(fBuffer.GetChannels()*sizeof(float*));
    float** temp2 = (float**)alloca(buffer->GetChannels()*sizeof(float*));

    while (fRubberBand.available() < framesNum) {
        int needFrames = std::min((int)framesNum, (int)fRubberBand.getSamplesRequired());
        if (needFrames > 0) {
            UAudioTools::ZeroFloatBlk(fBuffer.GetFrame(0, temp1), TAudioGlobals::fBufferSize, fStream->Channels());
            fStream->Read(&fBuffer, needFrames, 0);
            fRubberBand.process(fBuffer.GetFrame(0, temp1), needFrames, false);
        }
    }

    fRubberBand.retrieve(fBuffer.GetFrame(0, temp1), std::min((int)framesNum, fRubberBand.available()));

    UAudioTools::MixFrameToFrameBlk1(buffer->GetFrame(0, temp2),
                                     fBuffer.GetFrame(0, temp1),
                                     framesNum,
                                     Channels());
    return framesNum;
}

void TRubberBandAudioStream::Reset()
{
    fStream->Reset();
    fRubberBand.reset();
    fRubberBand.setTimeRatio(1/fTimeStretchVal);
    fRubberBand.setPitchScale(fPitchShiftVal);
}

TAudioStreamPtr TRubberBandAudioStream::Copy()
{
    return new TRubberBandAudioStream(fStream->Copy(), fPitchShift, fTimeStretch);
}



