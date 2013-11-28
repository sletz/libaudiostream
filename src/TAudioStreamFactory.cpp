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
#include "TTransformAudioStream.h"
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

#define TRY_CALL                    \
    TAudioGlobals::ClearLibError(); \
    try {                           \
    
#define CATCH_EXCEPTION                                     \
    } catch (TLASException& e) {                            \
        printf("LAS error = %s", e.Message().c_str());      \
        TAudioGlobals::AddLibError(e.Message());            \
        return 0;                                           \
    } catch (...) {                                         \
        printf("LAS runtime error");                        \
        TAudioGlobals::AddLibError("LAS runtime error");    \
        return 0;                                           \
    }                                                       \
    
TAudioStreamPtr TAudioStreamFactory::MakeNullSound(long length)
{
    return new TNullAudioStream(length);
}

TAudioStreamPtr TAudioStreamFactory::MakeMultiNullSound(long channels, long length)
{
    return new TNullAudioStream(channels, length);
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
    CATCH_EXCEPTION
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
        TAudioGlobals::AddLibError("beginFrame < O or endFrame > sound length");
        return 0;
    }
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeStereoSound(TAudioStreamPtr sound)
{
    TRY_CALL
	return (sound && sound->Channels() == 1) ? new TChannelizerAudioStream(sound, 2) : sound;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeLoopSound(TAudioStreamPtr sound, long n)
{
    TRY_CALL
    return (sound) ? new TLoopAudioStream(sound, n) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeFadeSound(TAudioStreamPtr sound, long fadeIn, long fadeOut)
{
    TRY_CALL
    return (sound) ? new TFadeAudioStream(sound, fadeIn, fadeOut) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeCutSound(TAudioStreamPtr sound, long beginFrame, long endFrame)
{
    TRY_CALL
    if (beginFrame >= 0 && beginFrame < endFrame && sound) {
		if (beginFrame > sound->Length()) {
            TAudioGlobals::AddLibError("beginFrame < O or endFrame > sound length");
			return 0;
		} else {
			TAudioStreamPtr begin = sound->CutBegin(beginFrame);
			assert(begin);
            return new TCutEndAudioStream(begin, UTools::Min(endFrame - beginFrame, begin->Length()));
        }
    } else {
        return 0;
    }
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeSeqSound(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade)
{
    TRY_CALL
    return (s1 && s2) ? new TSeqAudioStream(s1, s2, crossFade) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeMixSound(TAudioStreamPtr s1, TAudioStreamPtr s2)
{
    TRY_CALL
    return (s1 && s2) ? new TMixAudioStream(s1, s2) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeParSound(TAudioStreamPtr s1, TAudioStreamPtr s2)
{
    TRY_CALL
    return (s1 && s2) ? new TParAudioStream(s1, s2) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeSelectSound(TAudioStreamPtr s1, const std::vector<int>& selection)
{
    TRY_CALL
    return (s1) ? new TSelectAudioStream(s1, selection) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeInputSound()
{
    TRY_CALL
    return new TInputAudioStream();
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeSharedInputSound()
{
    TRY_CALL
    assert(TAudioGlobals::fSharedInput);
    return new TSharedBufferedAudioStream(0, TAudioGlobals::fSharedInput->GetMemoryBuffer());
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeEffectSound(TAudioStreamPtr s1, TAudioEffectInterfacePtr effect, long fadeIn, long fadeOut)
{
    TRY_CALL
    if (s1 && effect) {
        // If stream and effect are compatible...
        if (s1->Channels() == effect->Inputs()) {
            return new TEffectAudioStream(s1, effect, fadeIn, fadeOut);
        } else if ((s1->Channels() > effect->Inputs()) && (s1->Channels() % effect->Inputs() == 0)) {
            return new TEffectAudioStream(s1, TCodeFaustAudioEffectFactory::DuplicateEffect(effect, s1->Channels()/effect->Inputs()), fadeIn, fadeOut);
        } else if ((effect->Inputs() > s1->Channels()) && (effect->Inputs() % s1->Channels() == 0)) {
            return new TEffectAudioStream(s1, TCodeFaustAudioEffectFactory::SplitEffect(effect, s1->Channels()), fadeIn, fadeOut);
        } else {
            stringstream error;
            error << "Stream with " << s1->Channels() << " channels is incompatible with " << effect->Inputs() << " inputs effect";
            TAudioGlobals::AddLibError(error.str());
        }
    }
    return 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeRubberBandSound(TAudioStreamPtr s1, double* pitch_shift, double* time_strech)
{
    TRY_CALL
    //return (s1) ? new TRubberBandAudioStream(s1, pitch_shift, time_strech) : 0;
    CATCH_EXCEPTION
}

/*
#ifdef SOUND_TOUCH
TAudioStreamPtr TAudioStreamFactory::MakeSoundTouchSound(TAudioStreamPtr s1, double* pitch_shift, double* time_strech)
{
    TRY_CALL
    return (s1) ? new TSoundTouchAudioStream(s1, pitch_shift, time_strech) : 0;
    CATCH_EXCEPTION
}
#endif
*/

TAudioStreamPtr TAudioStreamFactory::MakeWriteSound(string name, TAudioStreamPtr sound, long format)
{
    TRY_CALL
    return (sound) ? new TWriteFileAudioStream(name, sound, format) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeRTRenderer(TAudioStreamPtr sound)
{
    TRY_CALL
    return (sound) ? new TRTRendererAudioStream(sound) : 0;
    CATCH_EXCEPTION
}

TAudioStreamPtr TAudioStreamFactory::MakeDTRenderer(TAudioStreamPtr sound)
{
    TRY_CALL
    return (sound) ? new TDTRendererAudioStream(sound) : 0;
    CATCH_EXCEPTION
}

