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

#include "TFadeAudioStream.h"
#include "TRendererAudioStream.h"
#include "TAudioGlobals.h"
#include "UTools.h"

TFadeAudioStream::TFadeAudioStream(TAudioStreamPtr stream, long fadeIn, long fadeOut):TDecoratedAudioStream(stream)
{
    fStatus = kFadeIn; // Starting state for the stream with a fade
    fCurFadeInFrames = fFadeInFrames = fadeIn;
    fCurFadeOutFrames = fFadeOutFrames = fadeOut;
    fCurFrame = 0;
	fFramesNum = UTools::Max(0, fStream->Length() - fFadeOutFrames); // Number of frames - FadeOut 
    fMixBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, fStream->Channels());
    Init(0.0f, float(fadeIn), 1.0f, float(fadeOut));
}

long TFadeAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);
    
    switch (fStatus) {
    
        case kIdle:
            return 0;

        case kPlaying:
            return Play(buffer, framesNum, framePos);

        case kFadeIn:
            return FadeIn(buffer, framesNum, framePos);

        case kFadeOut:
            return FadeOut(buffer, framesNum, framePos);

        default:
            return 0;
    }
}

long TFadeAudioStream::Play(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    long playingFrames = UTools::Min(framesNum, fFramesNum - fCurFrame);
    
    //printf("Play playingFrames %ld fFramesNum %ld fCurFrame %ld\n", playingFrames, fFramesNum, fCurFrame);
    
    long res = fStream->Read(buffer, playingFrames, framePos);
    
    if (playingFrames < framesNum) { // Last buffer of Play
        fStatus = kFadeOut;
        return res + FadeOut(buffer, framesNum - playingFrames, framePos + playingFrames);
    } else {
        fCurFrame += res;
        return res;
    }
}

long TFadeAudioStream::Fade(FLOAT_BUFFER buffer, long framesNum, long framePos, Envelope& fade)
{
    float** temp1 = (float**)alloca(fMixBuffer->GetChannels()*sizeof(float*));
    float** temp2 = (float**)alloca(buffer->GetChannels()*sizeof(float*));
      
    UAudioTools::ZeroFloatBlk(fMixBuffer->GetFrame(0, temp1), TAudioGlobals::fBufferSize, fStream->Channels());
    // Use temporary fBuffer from the beginning
    long res = fStream->Read(fMixBuffer, framesNum, 0);
    fCurFrame += res;

    for (int i = 0; i < framesNum; i++) {
        UAudioTools::MultFrame(fMixBuffer->GetFrame(i, temp1), fade.tick(), fStream->Channels());
    }

    UAudioTools::MixFrameToFrameBlk(buffer->GetFrame(framePos, temp2),
                                    fMixBuffer->GetFrame(0, temp1),
                                    framesNum, fStream->Channels());
    return res;
}

// TODO : start FadeOut while doing FadeIn

long TFadeAudioStream::FadeIn(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    long fadeInFrames = UTools::Min(framesNum, fCurFadeInFrames);
    
    //printf("FadeIn %ld %f\n", fadeInFrames, fFadeIn.lastOut());
    
    // Render Fade
    long res = Fade(buffer, fadeInFrames, framePos, fFadeIn);
    
    //printf("FadeIn %ld %f\n", fadeInFrames, fFadeIn.lastOut());
    
    if (fadeInFrames < framesNum) { // Last buffer of FadeIn
        // Switch in kPlaying state and finish rendering the buffer
        fStatus = kPlaying;
        return res + Play(buffer, framesNum - fadeInFrames, framePos + fadeInFrames);
    } else {
        fCurFadeInFrames -= framesNum;
        return res;
    }
}

long TFadeAudioStream::FadeOut(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    long fadeOutFrames = UTools::Min(framesNum, fCurFadeOutFrames);
    
    //printf("FadeOut %ld %f\n", fadeOutFrames, fFadeOut.lastOut());
    
    // Render Fade
    long res = Fade(buffer, fadeOutFrames, framePos, fFadeOut);
    
    //printf("FadeOut %ld %f\n", fadeOutFrames, fFadeOut.lastOut());
    
    if (fadeOutFrames < framesNum) { // Last buffer of FadeOut
        // Switch in kIdle state
        fStatus = kIdle;
    } else {
        fCurFadeOutFrames -= framesNum;
    }
    
    return res;
}

void TFadeAudioStream::Init(float fade_in_val, float fade_in_time, float fade_out_val, float fade_out_time)
{
    fFadeIn.setValue(fade_in_val);
    fFadeIn.setTarget(1.0f);
    fFadeIn.setTime(UAudioTools::ConvertFrameToSec(fade_in_time));
    
    //printf("TFadeAudioStream::Init fFadeIn %f\n", fFadeIn.lastOut());

    fFadeOut.setValue(fade_out_val);
    fFadeOut.setTarget(0.0f);
    fFadeOut.setTime(UAudioTools::ConvertFrameToSec(fade_out_time));
    
    //printf("TFadeAudioStream::Init fFadeOut %f\n", fFadeOut.lastOut());
}

/*
CutBegin(Fade(s, f1, f2), n) ==> Fade(CutBegin(s, n), f1, f2) 
*/

TAudioStreamPtr TFadeAudioStream::CutBegin(long frames)
{
    if (fFadeInFrames + fFadeOutFrames > fStream->Length()) {
        float ratio = float(fFadeInFrames + fFadeOutFrames) / float(fStream->Length());
        return new TFadeAudioStream(fStream->CutBegin(frames), int(float(fFadeInFrames)/ratio), int(float(fFadeOutFrames)/ratio));
    } else {
        return new TFadeAudioStream(fStream->CutBegin(frames), fFadeInFrames, fFadeOutFrames);
    }
}

void TFadeAudioStream::Reset()
{
    fStream->Reset();
    fStatus = kFadeIn; // Starting state for the stream with a fade
	fCurFrame = 0;
    fCurFadeInFrames = fFadeInFrames;
    fCurFadeOutFrames = fFadeOutFrames;
    Init(0.f, float(fFadeInFrames), 1.f, float(fFadeOutFrames));
}

// Additional interface
void TChannelFadeAudioStream::SetStream(TAudioStreamPtr stream, long fadeIn, long fadeOut)
{
    fFadeInFrames = fadeIn;
    fFadeOutFrames = fadeOut;
    fStream = stream;
    fStatus = kIdle;
    fCurFrame = 0;
	fFramesNum = fStream->Length() - fFadeOutFrames; // Number of frames - FadeOut
    Init(0.f, float(fFadeInFrames), 1.f, float(fFadeOutFrames));
}

void TChannelFadeAudioStream::FadeIn()
{
    if (fCurFrame < fFramesNum) {
        fStatus = kFadeIn;
        Init(0.f, float(fFadeInFrames), 1.f, float(fFadeOutFrames));
    }
}

void TChannelFadeAudioStream::FadeOut()
{
    if (fCurFrame < fFramesNum) {
        fStatus = kFadeOut;
        Init(0.f, float(fFadeInFrames), 1.f, float(fFadeOutFrames));
    }
}



