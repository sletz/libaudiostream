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
research@grame.fr
TAudioBuffer<float>*
*/

#ifndef __TAudioEffectInterface__
#define __TAudioEffectInterface__

#include "smartpointer.h"

//-----------------------------
// Class TAudioEffectInterface
//-----------------------------
/*!
\brief The base class for audio effects.
*/

class TAudioEffectInterface;

typedef SMARTP<TAudioEffectInterface>  TAudioEffectInterfacePtr;

class TAudioEffectInterface : public smartable1
{

    private:

        bool fState;	// Running state

    public:

        TAudioEffectInterface(): fState(true)
        {}
        virtual ~TAudioEffectInterface()
        {}

        // Internal methods

        void SetState(bool state)
        {
            fState = state;
        }
        bool GetState()
        {
            return fState;
        }

        void ProcessAux(float** input, float** output, long framesNum, long channels)
        {
            if (fState)
                Process(input, output, framesNum, channels);
        }

        // Pure virtual : to be implemented by sub-classes
        virtual void Process(float** input, float** output, long framesNum, long channels) = 0;
        virtual TAudioEffectInterface* Copy() = 0;
        virtual void Reset() = 0;
        virtual long Channels() = 0;
		
		virtual long GetControlCount() = 0;
		virtual void GetControlParam(long param, char* label, float* min, float* max, float* init) = 0;
		virtual void SetControlValue(long param, float f) = 0; 
		virtual float GetControlValue(long param) = 0;
};

//typedef TAudioEffectInterface * TAudioEffectInterfacePtr;

#endif
