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

#ifndef __TAudioStreamFactory__
#define __TAudioStreamFactory__

#include "AudioExports.h"
#include "TAudioStream.h"
#include "TAudioEffect.h"
#include <string>

using namespace std;

/*
Stream Temps Réel :
 
TR = TBufferedAudioStream (sharedBuffer...) avec sharedBuffer accessible globalement pour tous les streams TR
 
Clonage du stream TR :
    
        - si lecture dans le passé à plus de RTSTREAM_BUFFER_SIZE : stream File (avec pb d'initialisation à résoudre)
        - si lecture dans le passé à moins de RTSTREAM_BUFFER_SIZE : TBufferedAudioStream (sharedBuffer, framePos)
        avec framePos la position de lecture dans le passé
        - si lecture dans le passé de FrameNum : Seq (NullStream (FrameNum), BufferedAudioStream (sharedBuffer, 0)) 
        (a revoir : pb de la longueur??)
 
pb des gestion de l'ordre du Read : le Read du stream TR d'origine doit se faire AVANT les Read de tous les streams Clone
*/

typedef TAudioStreamPtr BinayOp(TAudioStreamPtr s1, TAudioStreamPtr s2);

//---------------------------
// Class TAudioStreamFactory
//---------------------------
/*!
\brief A factory for streams.
*/

class TBufferedAudioStream;

class AUDIO_EXPORTS TAudioStreamFactory
{
    private:
 
    public:

        TAudioStreamFactory()
        {}
        virtual ~TAudioStreamFactory()
        {}

        static TAudioStreamPtr MakeInputSound();
        static TAudioStreamPtr MakeSharedInputSound();
        static TAudioStreamPtr MakeNullSound(long lengthFrame);
        static TAudioStreamPtr MakeReadSound(string name);
        static TAudioStreamPtr MakeRegionSound(string name, long beginFrame, long endFrame);
		static TAudioStreamPtr MakeStereoSound(TAudioStreamPtr sound);
        static TAudioStreamPtr MakeLoopSound(TAudioStreamPtr sound, long n);
        static TAudioStreamPtr MakeFadeSound(TAudioStreamPtr sound, long fadeIn, long fadeOut);
        static TAudioStreamPtr MakeCutSound(TAudioStreamPtr s1, long beginFrame, long endFrame);
        static TAudioStreamPtr MakeSeqSound(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade);
        static TAudioStreamPtr MakeMixSound(TAudioStreamPtr s1, TAudioStreamPtr s2);
        static TAudioStreamPtr MakeParSound(TAudioStreamPtr s1, TAudioStreamPtr s2);
        static TAudioStreamPtr MakeTransformSound(TAudioStreamPtr s1, TAudioEffectListPtr effect, long fadeIn, long fadeOut);
        static TAudioStreamPtr MakeEffectSound(TAudioStreamPtr s1, TAudioEffectInterfacePtr effect, long fadeIn, long fadeOut);
		static TAudioStreamPtr MakeRubberBandSound(TAudioStreamPtr s1, double* pitch_shift, double* time_strech);
    #ifdef SOUND_TOUCH
		static TAudioStreamPtr MakeSoundTouchSound(TAudioStreamPtr s1, double* pitch_shift, double* time_strech);
    #endif
        static TAudioStreamPtr MakeWriteSound(string name, TAudioStreamPtr s, long format);
        static TAudioStreamPtr MakeRTRenderer(TAudioStreamPtr s);
        static TAudioStreamPtr MakeDTRenderer(TAudioStreamPtr s);
};

typedef TAudioStreamFactory * TAudioStreamFactoryPtr;

#endif
