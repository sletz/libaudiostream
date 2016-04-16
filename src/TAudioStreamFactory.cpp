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

#include "TAudioStreamFactory.h"
#include "TAudioGlobals.h"
#include "TFaustAudioEffect.h"
#include "TFadeAudioStream.h"
#include "TLoopAudioStream.h"
#include "TCutEndAudioStream.h"
#include "TSelectAudioStream.h"
#include "TSeqAudioStream.h"
#include "TMixAudioStream.h"
#include "TParAudioStream.h"
#include "TNullAudioStream.h"
#include "TReadFileAudioStream.h"
#include "TInputAudioStream.h"
#include "TBufferedInputAudioStream.h"
#include "TEffectAudioStream.h"
#include "TEventAudioStream.h"
#include "TRubberBandAudioStream.h"
#ifdef SOUND_TOUCH
#include "TSoundTouchAudioStream.h"
#endif
#include "TWriteFileAudioStream.h"
#include "TRendererAudioStream.h"
#include "TChannelizerAudioStream.h"
#include "TSampleRateAudioStream.h"
#include "TAudioGlobals.h"
#include "TLASException.h"
#include "UAudioTools.h"
#include <assert.h>
#include <stdio.h>

/*--------------------------------------------------------------------------*/
// External API
/*--------------------------------------------------------------------------*/

TAudioStreamPtr TAudioStreamFactory::MakeNullSound(long length)
{
    return new TNullAudioStream(length);
}

TAudioStreamPtr TAudioStreamFactory::MakeMultiNullSound(long channels, long length)
{
    return new TNullAudioStream(channels, length);
}

TAudioStreamPtr TAudioStreamFactory::MakeConstantSound(long channels, long length, float value)
{
    return new TConstantAudioStream(channels, length, value);
}

TAudioStreamPtr TAudioStreamFactory::MakeBufferSound(float** buffer, long length, long channels, bool clear)
{
    TRY_CALL
    return new TMemoryBufferedAudioStream(0, new TSharedNonInterleavedAudioBuffer<float>(buffer, length, channels), clear);
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeReadSound(string name)
{
    TRY_CALL
    TReadFileAudioStreamPtr sound = new TReadFileAudioStream(name, 0);
    if (sound->SampleRate() != TAudioGlobals::fSampleRate) {
        return new TSampleRateAudioStream(sound, double(TAudioGlobals::fSampleRate) / double(sound->SampleRate()), 2);
    } else {
        return sound;
    }
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeRegionSound(string name, long beginFrame, long endFrame)
{
    TRY_CALL
    if (beginFrame >= 0 && beginFrame <= endFrame) {
        TReadFileAudioStreamPtr sound = new TReadFileAudioStream(name, beginFrame);
        if (sound->SampleRate() != TAudioGlobals::fSampleRate) {
            return new TSampleRateAudioStream(new TCutEndAudioStream(sound, UTools::Min(endFrame - beginFrame, sound->Length())), double(TAudioGlobals::fSampleRate) / double(sound->SampleRate()), 2);
        } else {
            return new TCutEndAudioStream(sound, UTools::Min(endFrame - beginFrame, sound->Length()));
        }
    } else {
        TAudioGlobals::AddLibError("MakeRegionSound : beginFrame < 0 or endFrame > sound length");
        return 0;
    }
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeLoopSound(TAudioStreamPtr sound, long n)
{
    TRY_CALL
    return (sound) ? new TLoopAudioStream(sound, n) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeFadeSound(TAudioStreamPtr sound, long fadeIn, long fadeOut)
{
    TRY_CALL
    if (sound) {
        if (fadeIn + fadeOut > sound->Length()) {
            TAudioGlobals::AddLibError("MakeFadeSound : fadeIn + fadeOut > sound length");
        } else {
            return new TFadeAudioStream(sound, fadeIn, fadeOut);
        }
    }
    return 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeCutSound(TAudioStreamPtr sound, long beginFrame, long endFrame)
{
    TRY_CALL
    if (beginFrame >= 0 && beginFrame < endFrame && sound) {
		if (beginFrame >= sound->Length()) {
            TAudioGlobals::AddLibError("MakeCutSound : beginFrame > sound length");
		} else {
			TAudioStreamPtr begin = sound->CutBegin(beginFrame);
            return new TCutEndAudioStream(sound->CutBegin(beginFrame), UTools::Min(endFrame - beginFrame, begin->Length()));
        }
    }
    return 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeSeqSound(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade)
{
    TRY_CALL
    if (s1 && s2) {
        if (crossFade > s1->Length() || crossFade > s2->Length()) {
            TAudioGlobals::AddLibError("MakeSeqSound : crossFade > sound length");
        } else if (crossFade > 0) {
            TAudioStream* stream1 = new TCutEndAudioStream(s1, s1->Length() - crossFade);
            TAudioStream* crossFadeStream = new TMixAudioStream(new TFadeAudioStream(s1->CutBegin(s1->Length() - crossFade), 0, crossFade),   
                                                                new TFadeAudioStream(new TCutEndAudioStream(s2, crossFade), crossFade, 0));
            TAudioStream* stream2 = new TSeqAudioStream(crossFadeStream, s2->CutBegin(crossFade));  
            return new TSeqAudioStream(stream1, stream2);
        } else {
            return new TSeqAudioStream(s1, s2);
        }
    }
    return 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeMixSound(TAudioStreamPtr s1, TAudioStreamPtr s2)
{
    TRY_CALL
    return (s1 && s2) ? new TMixAudioStream(s1, s2) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeParSound(TAudioStreamPtr s1, TAudioStreamPtr s2)
{
    TRY_CALL
    return (s1 && s2) ? new TParAudioStream(s1, s2) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeSelectSound(TAudioStreamPtr s, long* selection, long channels)
{
    TRY_CALL
    if (s) {   
        // Check selection channels
        std::vector<long> selection_aux;
        for (long i = 0; i < channels; i++) {
            if (selection[i] >= s->Channels()) {
                stringstream error;
                error << "MakeSelectSound : channel " << selection[i] << " is out of stream channels";
                TAudioGlobals::AddLibError(error.str());
                return 0;
            } else {
                selection_aux.push_back(selection[i]);
            }
        }
        return new TSelectAudioStream(s, selection_aux);
    }
    return 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeInputSound()
{
    TRY_CALL
    return new TInputAudioStream();
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeSharedInputSound()
{
    TRY_CALL
    return (TAudioGlobals::fSharedInput) ? new TSharedBufferedAudioStream(0, TAudioGlobals::fSharedInput->GetMemoryBuffer()) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeEffectSound(TAudioStreamPtr s1, TAudioEffectInterfacePtr effect, long fadeIn, long fadeOut)
{
    TRY_CALL
    TAudioStream* res = 0;
    if (s1 && effect) {
        if (fadeIn + fadeOut > s1->Length()) {
            TAudioGlobals::AddLibError("MakeEffectSound : fadeIn + fadeOut > sound length");
        // If pure instrument...
        } else if (effect->Inputs() == 0) {
            res = new TEffectAudioStream(s1, effect);
        // If stream and effect are compatible...
        } else if (s1->Channels() == effect->Inputs()) {
            res = new TEffectAudioStream(s1, effect);
        } else if ((s1->Channels() > effect->Inputs()) && (s1->Channels() % effect->Inputs() == 0)) {
            res = new TEffectAudioStream(s1, TLocalCodeFaustAudioEffectFactory::DuplicateEffect(effect, s1->Channels()/effect->Inputs()));
        } else if ((effect->Inputs() > s1->Channels()) && (effect->Inputs() % s1->Channels() == 0)) {
            res = new TEffectAudioStream(s1, TLocalCodeFaustAudioEffectFactory::SplitEffect(effect, s1->Channels()));
        } else {
            stringstream error;
            error << "MakeEffectSound : stream with " << s1->Channels() << " channels is incompatible with " << effect->Inputs() << " inputs effect";
            TAudioGlobals::AddLibError(error.str());
        }
    }
    return (res) ? new TFadeAudioStream(res, fadeIn, fadeOut) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeRubberBandSound(TAudioStreamPtr s1, double* pitch_shift, double* time_strech)
{
    TRY_CALL
    return (s1) ? new TRubberBandAudioStream(s1, pitch_shift, time_strech) : 0;
    CATCH_EXCEPTION_RETURN
}

/*
#ifdef SOUND_TOUCH
TAudioStreamPtr TAudioStreamFactory::MakeSoundTouchSound(TAudioStreamPtr s1, double* pitch_shift, double* time_strech)
{
    TRY_CALL
    return (s1) ? new TSoundTouchAudioStream(s1, pitch_shift, time_strech) : 0;
    CATCH_EXCEPTION_RETURN
}
#endif
*/

TAudioStreamPtr TAudioStreamFactory::MakeWriteSound(string name, TAudioStreamPtr sound, long format)
{
    TRY_CALL
    return (sound) ? new TWriteFileAudioStream(name, sound, format) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeRTRenderer(TAudioStreamPtr sound)
{
    TRY_CALL
    return (sound) ? new TRTRendererAudioStream(sound) : 0;
    CATCH_EXCEPTION_RETURN
}

TAudioStreamPtr TAudioStreamFactory::MakeDTRenderer(TAudioStreamPtr sound)
{
    TRY_CALL
    return (sound) ? new TDTRendererAudioStream(sound) : 0;
    CATCH_EXCEPTION_RETURN
}

