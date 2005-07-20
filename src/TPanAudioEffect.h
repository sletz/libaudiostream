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

#ifndef __TPanAudioEffect__
#define __TPanAudioEffect__

#include <math.h>

#if !defined(PI)
 #define PI (MY_FLOAT) 3.14159265359
#endif

#include "TAudioEffectInterface.h"
#include "TPanTable.h"

//-----------------------
// Class TPanAudioEffect
//-----------------------
/*!
\brief Pan effect.
*/

class TPanAudioEffect : public TAudioEffectInterface
{

    private:

        float fPan;
		float fLeftVol,fRightVol;

    public:

        TPanAudioEffect(float pan): TAudioEffectInterface(), fPan(pan)
        {
			TPanTable::GetLR(1.0f, fPan, &fLeftVol, &fRightVol);
		}
        virtual ~TPanAudioEffect()
        {}
		
		void Process(float** input, float** output, long framesNum, long channels)
        {
		      for (int i = 0; i < framesNum; i++) {
				output[0][i] = input[0][i] * fLeftVol;
				output[1][i] = input[1][i] * fRightVol;
			}
        }

		TAudioEffectInterface* Copy()
        {
            return new TPanAudioEffect(fPan);
        }
        
		void Reset() 
		{}
		
        long Channels()
		{
            return 2;
        }
		
		void SetControlValue(long param, float f)
		{
			if (param == 0) {
				fPan = f;
				TPanTable::GetLR(1.0f, fPan, &fLeftVol, &fRightVol);
			}
		}
		
		float GetControlValue(long param)
		{
			return (param == 0) ? fPan : 0.0f;
		}
		
		long GetControlCount()
		{
			return 1;
		}
		
		void GetControlParam(long param, char* label, float* min, float* max, float* init)
		{
			strcpy(label, "Pan");
			*min = 0.0f;
			*max = 1.0f;
			*init = 0.5f;
		}
};

typedef TPanAudioEffect * TPanAudioEffectPtr;

#endif
