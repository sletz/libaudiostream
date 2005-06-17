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
TAudioBuffer<float>*
*/

#ifndef __TAudioEffect__
#define __TAudioEffect__

#include "TAudioBuffer.h"
#include "TAudioEffectInterface.h"
#include "TAudioGlobals.h"
#include "TAudioConstants.h"
#include <list>

using namespace std;

//--------------------
// Class TAudioEffect
//--------------------
/*!
\brief  Effect list management for subclasses of TAudioEffectInterface
*/

class TAudioEffect : public list<TAudioEffectInterfacePtr>
{

	private:
	
		float* fTemp1[MAX_PLUG_CHANNELS];
		float* fTemp2[MAX_PLUG_CHANNELS];
		
    public:

        TAudioEffect()
        {
			int i;
			for (i = 0; i < MAX_PLUG_CHANNELS; i++) {
				fTemp1[i] = (float*)calloc(TAudioGlobals::fBuffer_Size, sizeof(float));
			}
			for (i = 0; i < MAX_PLUG_CHANNELS; i++) {
				fTemp2[i] = (float*)calloc(TAudioGlobals::fBuffer_Size, sizeof(float));
			}
		}
        virtual ~TAudioEffect();

        void Process(float* buffer, long framesNum, long channels);
        TAudioEffect* Copy();
        void Reset();
};

typedef TAudioEffect * TAudioEffectPtr;

#endif
