/*
Copyright (C) Grame 2002

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

#ifndef __TWrapperAudioEffect__
#define __TWrapperAudioEffect__

#include "TAudioEffectInterface.h"

//---------------------------
// Class TWrapperAudioEffect
//---------------------------
/*!
\brief A decorator effect that takes a normal effect pointer as parameter.
*/

class TWrapperAudioEffect : public TAudioEffectInterface
{

    private:

		TAudioEffectInterface* fEffect;

    public:

        TWrapperAudioEffect(TAudioEffectInterface* effect): TAudioEffectInterface(),fEffect(effect)
        {}
        virtual ~TWrapperAudioEffect()
        {
			delete fEffect;
		}

        void Process(float** input, float** output, long framesNum, long channels)
        {
            fEffect->Process(input, output, framesNum, channels);
        }

        TAudioEffectInterface* Copy()
        {
            return NULL; // Should never happen...
        }
        void Reset()
        {
			fEffect->Reset();
		}
        long Channels()
        {
           return fEffect->Channels();
        }
		
		long GetControlCount()
		{
			return fEffect->GetControlCount();
		}
		
		void GetControlParam(long param, char* label, float* min, float* max, float* init)
		{
			fEffect->GetControlParam(param, label, min, max, init);
		}
		
		void SetControlValue(long param, float f)
		{
			fEffect->SetControlValue(param, f);
		}
		float GetControlValue(long param)
		{
			return fEffect->GetControlValue(param);
		}
};

typedef TWrapperAudioEffect * TWrapperAudioEffectPtr;

#endif
