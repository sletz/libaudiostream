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

#ifndef __TAudioEffectInterface__
#define __TAudioEffectInterface__

#include "la_smartpointer.h"

//-----------------------------
// Class TAudioEffectInterface
//-----------------------------

/*!
\brief The base class for audio effects.
*/

// Using smartable1 cause crah when desallocating the object: desactivated for now
// class TAudioEffectInterface : public la_smartable1

class LA_EXPORT TAudioEffectInterface : public la_smartable
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

        void ProcessAux(float** input, float** output, long framesNum)
        {
            if (fState) {
                Process(input, output, framesNum);
            }
        }

        // Pure virtual : to be implemented by sub-classes
		
        virtual void Process(float** input, float** output, long framesNum) = 0;
        virtual TAudioEffectInterface* Copy() = 0;
        virtual void Reset() = 0;
        virtual long Inputs() = 0;
        virtual long Outputs() = 0;
		
		virtual long GetControlCount() = 0;
		virtual void GetControlParam(long param, char* label, float* min, float* max, float* init) = 0;
		virtual void SetControlValue(long param, float value) = 0; 
		virtual float GetControlValue(long param) = 0;

};

typedef LA_SMARTP<TAudioEffectInterface>  TAudioEffectInterfacePtr;

#endif
