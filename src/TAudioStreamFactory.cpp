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
#include "TWriteFileAudioStream.h" 
#include "TRendererAudioStream.h"

#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include <assert.h>
#include <stdio.h>

/*--------------------------------------------------------------------------*/
// External API
/*--------------------------------------------------------------------------*/

/*
TAudioStreamPtr TAudioStreamFactory::MakeGCSound(TAudioStreamPtr sound)
{
	return new TGCAudioStream(sound);
}
*/

TAudioStreamPtr TAudioStreamFactory::MakeNullSound(long length)
{
    return new TNullAudioStream(length);
}

TAudioStreamPtr TAudioStreamFactory::MakeReadSound(string name)
{
    try {
        return new TReadFileAudioStream(name, 0);
    } catch (int n) {
        printf("MakeFileSound exception %d \n", n);
        return 0;
    }
}

TAudioStreamPtr TAudioStreamFactory::MakeRegionSound(string name, long beginFrame, long endFrame)
{
    assert((beginFrame < endFrame));
    TAudioStreamPtr sound = new TReadFileAudioStream(name, beginFrame);
    endFrame = UTools::Min(sound->Length(), endFrame);

    try {
        return new TCutEndAudioStream (sound, endFrame - beginFrame);
    } catch (int n) {
        printf("MakeFileSound exception %d \n", n);
        return 0;
    }
}

TAudioStreamPtr TAudioStreamFactory::MakeLoopSound(TAudioStreamPtr sound, long n)
{
    return new TLoopAudioStream(sound, n);
}

TAudioStreamPtr TAudioStreamFactory::MakeFadeSound(TAudioStreamPtr sound, long fadeIn, long fadeOut)
{
    return new TFadeAudioStream(sound, fadeIn, fadeOut);
}

TAudioStreamPtr TAudioStreamFactory::MakeCutSound(TAudioStreamPtr sound, long beginFrame, long endFrame)
{
    assert((beginFrame < endFrame));
    TAudioStreamPtr begin = sound->CutBegin(beginFrame);
    endFrame = UTools::Min(sound->Length(), endFrame);
    return new TCutEndAudioStream(begin, endFrame - beginFrame);
}

TAudioStreamPtr TAudioStreamFactory::MakeSeqSound(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade)
{
    return new TSeqAudioStream(s1, s2, crossFade);
}

TAudioStreamPtr TAudioStreamFactory::MakeMixSound(TAudioStreamPtr s1, TAudioStreamPtr s2)
{
    return new TMixAudioStream(s1, s2);
}

TAudioStreamPtr TAudioStreamFactory::MakeInputSound()
{
    return new TInputAudioStream();
}

TAudioStreamPtr TAudioStreamFactory::MakeTransformSound
(TAudioStreamPtr s1, TAudioEffectPtr effect, long fadeIn, long fadeOut)
{
    return new TTransformAudioStream(s1, effect, fadeIn, fadeOut);
}

TAudioStreamPtr TAudioStreamFactory::MakeWriteSound(string name, TAudioStreamPtr s, long format)
{
    return new TWriteFileAudioStream(name, s, format);
}

TAudioStreamPtr TAudioStreamFactory::MakeRTRenderer(TAudioStreamPtr s)
{
    return new TRTRendererAudioStream(s);
}

TAudioStreamPtr TAudioStreamFactory::MakeDTRenderer(TAudioStreamPtr s)
{
    return new TDTRendererAudioStream(s);
}

/*
TAudioStreamPtr TAudioStreamFactory::MakeConvertSound(TAudioStreamPtr s, long converter, double ratio)
{
    return new TConvertAudioStream(s, converter, ratio);
}
*/

