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
#include "TFadeAudioStream.h"
#include "TLoopAudioStream.h"
#include "TCutEndAudioStream.h"
#include "TSeqAudioStream.h"
#include "TMixAudioStream.h"
#include "TNullAudioStream.h"
#include "TReadFileAudioStream.h"
#include "TInputAudioStream.h"
#include "TTransformAudioStream.h"
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

TAudioStreamPtr TAudioStreamFactory::MakeReadSound(string name)
{
    try {
        TReadFileAudioStreamPtr sound = new TReadFileAudioStream(name, 0);
        // Force stereo mode here...
        TAudioStreamPtr stereo_sound = MakeStereoSound(sound);
        if (sound->SampleRate() != TAudioGlobals::fSampleRate) {
            return new TSampleRateAudioStream(stereo_sound, double(TAudioGlobals::fSampleRate) / double(sound->SampleRate()), 2);
        } else {
            return stereo_sound;
        }
    } catch (int n) {
        printf("MakeReadSound exception %d \n", n);
        return 0;
    }
}

TAudioStreamPtr TAudioStreamFactory::MakeRegionSound(string name, long beginFrame, long endFrame)
{
	if (beginFrame >= 0 && beginFrame <= endFrame) {
        try {
            TReadFileAudioStreamPtr sound = new TReadFileAudioStream(name, beginFrame);
            // Force stereo mode here...
            TAudioStreamPtr stereo_sound = MakeStereoSound(sound);
 	        if (sound->SampleRate() != TAudioGlobals::fSampleRate) {
                return new TSampleRateAudioStream(new TCutEndAudioStream(stereo_sound, UTools::Min(endFrame - beginFrame, sound->Length())), double(TAudioGlobals::fSampleRate) / double(sound->SampleRate()), 2);
            } else {
                return new TCutEndAudioStream(stereo_sound, UTools::Min(endFrame - beginFrame, sound->Length()));
            }
        } catch (int n) {
            printf("MakeRegionSound exception %d \n", n);
            return 0;
        }
    } else {
        return 0;
    }
}

TAudioStreamPtr TAudioStreamFactory::MakeStereoSound(TAudioStreamPtr sound)
{
	return (sound && sound->Channels() == 1) ? new TChannelizerAudioStream(sound, 2) : sound;
}

TAudioStreamPtr TAudioStreamFactory::MakeLoopSound(TAudioStreamPtr sound, long n)
{
    return (sound) ? new TLoopAudioStream(sound, n) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeFadeSound(TAudioStreamPtr sound, long fadeIn, long fadeOut)
{
    return (sound) ? new TFadeAudioStream(sound, fadeIn, fadeOut) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeCutSound(TAudioStreamPtr sound, long beginFrame, long endFrame)
{
    if (beginFrame >= 0 && beginFrame < endFrame && sound) {
		if (beginFrame > sound->Length()) {
			return 0;
		} else {
			TAudioStreamPtr begin = sound->CutBegin(beginFrame);
			assert(begin);
            return new TCutEndAudioStream(begin, UTools::Min(endFrame - beginFrame, begin->Length()));
        }
    } else {
        return 0;
    }
}

TAudioStreamPtr TAudioStreamFactory::MakeSeqSound(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade)
{
    return (s1 && s2) ? new TSeqAudioStream(s1, s2, crossFade) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeMixSound(TAudioStreamPtr s1, TAudioStreamPtr s2)
{
    return (s1 && s2) ? new TMixAudioStream(s1, s2) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeInputSound()
{
    return new TInputAudioStream();
}

TAudioStreamPtr TAudioStreamFactory::MakeTransformSound(TAudioStreamPtr s1, TAudioEffectListPtr effect, long fadeIn, long fadeOut)
{
    return (s1 && effect) ? new TTransformAudioStream(s1, effect, fadeIn, fadeOut) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeRubberBandSound(TAudioStreamPtr s1,  double* pitch_shift, double* time_strech)
{
    return (s1) ? new TRubberBandAudioStream(s1, pitch_shift, time_strech) : 0;
}

#ifdef SOUND_TOUCH
TAudioStreamPtr TAudioStreamFactory::MakeSoundTouchSound(TAudioStreamPtr s1,  double* pitch_shift, double* time_strech)
{
    return (s1) ? new TSoundTouchAudioStream(s1, pitch_shift, time_strech) : 0;
}
#endif

TAudioStreamPtr TAudioStreamFactory::MakeWriteSound(string name, TAudioStreamPtr sound, long format)
{
    return (sound) ? new TWriteFileAudioStream(name, sound, format) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeRTRenderer(TAudioStreamPtr sound)
{
    return (sound) ? new TRTRendererAudioStream(sound) : 0;
}

TAudioStreamPtr TAudioStreamFactory::MakeDTRenderer(TAudioStreamPtr sound)
{
    return (sound) ? new TDTRendererAudioStream(sound) : 0;
}

