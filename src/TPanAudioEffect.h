/*

Copyright (C) Grame 2002-2012

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

#ifndef __TPanAudioEffect__
#define __TPanAudioEffect__

#if !defined(PI)
 #define PI (float) 3.14159265359
#endif

#include "TAudioEffectInterface.h"
#include "TPanTable.h"
#include <math.h>

//-----------------------
// Class TPanAudioEffect
//-----------------------
/*!
\brief Mono pan effect.
*/

class TMonoPanAudioEffect : public TAudioEffectInterface
{

    private:

        float fPan;
		float fLeftVol,fRightVol;

    public:

        TMonoPanAudioEffect(float pan): TAudioEffectInterface(), fPan(pan)
        {
			TPanTable::GetLR(1.0f, fPan, &fLeftVol, &fRightVol);
		}
        virtual ~TMonoPanAudioEffect()
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
            return new TMonoPanAudioEffect(fPan);
        }
        
		void Reset() 
		{}
		
        long Channels()
		{
            return 2;
        }
		
		void SetControlValue(long param, float pan)
		{
			if (param == 0) {
				fPan = pan;
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
			*init = DEFAULT_PAN;
		}
};

typedef TMonoPanAudioEffect * TMonoPanAudioEffectPtr;

/*!
\brief Stereo pan effect.
*/

class TStereoPanAudioEffect : public TAudioEffectInterface
{

    private:

		float fPanLeft;		// Pan for left signal
		float fPanRight;	// Pan for right signal
		float fLLVol;		// Volume for left output for left channel
		float fLRVol;		// Volume for right output for left channel
		float fRLVol;		// Volume for left output for right channel
		float fRRVol;		// Volume for right output for right channel

    public:

        TStereoPanAudioEffect(float panLeft, float panRight): TAudioEffectInterface(), fPanLeft(panLeft), fPanRight(panRight)
        {
			TPanTable::GetLR(1.0f, fPanLeft, &fLLVol, &fLRVol);
			TPanTable::GetLR(1.0f, fPanRight, &fRLVol, &fRRVol);
		}
        virtual ~TStereoPanAudioEffect()
        {}
		
		void Process(float** input, float** output, long framesNum, long channels)
        {
			for (int i = 0; i < framesNum; i++) {
				output[0][i] = input[0][i] * fLLVol + input[1][i] * fRLVol;
				output[1][i] = input[0][i] * fLRVol + input[1][i] * fRRVol;
			}
        }

		TAudioEffectInterface* Copy()
        {
            return new TStereoPanAudioEffect(fPanLeft, fPanRight);
        }
        
		void Reset() 
		{}
		
        long Channels()
		{
            return 2;
        }
		
		void SetControlValue(long param, float pan)
		{
			if (param == 0) {
				fPanLeft = pan;
				TPanTable::GetLR(1.0f, fPanLeft, &fLLVol, &fLRVol);
			}
			if (param == 1) {
				fPanRight = pan;
				TPanTable::GetLR(1.0f, fPanRight, &fRLVol, &fRRVol);
			}
		}
		
		float GetControlValue(long param)
		{
			if (param == 0)  
				return fPanLeft;
			else if (param == 1)  
				return fPanRight;
			else 
				return 0.0f;
		}
		
		long GetControlCount()
		{
			return 2;
		}
		
		void GetControlParam(long param, char* label, float* min, float* max, float* init)
		{
			*min = 0.0f;
			*max = 1.0f;
			if (param == 0) {
				strcpy(label, "PanLeft");
				*init = DEFAULT_PAN_LEFT; 
			}
			if (param == 1) {
				strcpy(label, "PanRight");
				*init = DEFAULT_PAN_RIGHT;
			}
		}
};

typedef TStereoPanAudioEffect * TStereoPanAudioEffectPtr;

#endif
