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

#ifndef __TVolAudioEffect__
#define __TVolAudioEffect__

#include "TAudioEffectInterface.h"

//-----------------------
// Class TVolAudioEffect
//-----------------------
/*!
\brief Volume effect.
*/

class TVolAudioEffect : public TAudioEffectInterface
{

    private:

        float fVol;

    public:

        TVolAudioEffect(float vol): TAudioEffectInterface(), fVol(vol)
        {}
        virtual ~TVolAudioEffect()
        {}

        void Process(float** input, float** output, long framesNum)
        {
            for (int i = 0; i < framesNum; i++) {
                for (int j = 0; j < Outputs(); j++) {
                    output[j][i] = input[j][i] * fVol;
                }
            }
        }

        TAudioEffectInterface* Copy()
        {
            return new TVolAudioEffect(fVol);
        }
        void Reset()
        {}
        
        long Inputs()
        {
            return 2;
        }
        
        long Outputs()
        {
            return 2;
        }
	
		long GetControlCount()
		{
			return 1;
		}
		
		void GetControlParam(long param, char* label, float* min, float* max, float* init)
		{
			strcpy(label, "Vol");
			*min = 0.0f;
			*max = 1.0f;
			*init = 1.0f;
		}
		
		void SetControlValue(long param, float value)
		{
			if (param == 0) {
				fVol = value;
            }
		}
		float GetControlValue(long param)
		{
			return (param == 0) ? fVol : 0.0f;
		}
};

typedef TVolAudioEffect * TVolAudioEffectPtr;

#endif
