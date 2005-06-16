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

#include "TAudioEffect.h"
#include <stdio.h>

static inline void SwapBuffers(float** input, float** output) {float** tmp = input; input = output; output = tmp;}

void TAudioEffect::Process(float* buffer, long framesNum, long channels)
{
	float** input = fTemp1;
	float** tmp_output = fTemp2;
	float** output = output;
	
	for (int i = 0; i < framesNum; i++) {
		for (int j = 0; j < channels; j++) {
			input[j][i] = buffer[i * channels + j];
		}
	}

	for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
	    TAudioEffectInterfacePtr process = *iter;
        process->ProcessAux(input, tmp_output, framesNum, channels);
		output = tmp_output;
		SwapBuffers(input, tmp_output);
    }
	
	for (int i = 0; i < framesNum; i++) {
		for (int j = 0; j < channels; j++) {
			buffer[i * channels + j] = output[j][i];
		}
	}
}

void TAudioEffect::Reset()
{
    for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
        TAudioEffectInterfacePtr process = *iter;
        process->Reset();
    }
}

TAudioEffect::~TAudioEffect()
{
    for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
        TAudioEffectInterfacePtr process = *iter;
        delete process;
    }
	
	for (int i = 0; i < MAX_PLUG_CHANNELS; i++) {
		free(fTemp1[i]);
	}
	for (int i = 0; i < MAX_PLUG_CHANNELS; i++) {
		free(fTemp2[i]);
	}
}

TAudioEffectPtr TAudioEffect::Copy()
{
    TAudioEffectPtr copy = new TAudioEffect();

    for (list<TAudioEffectInterfacePtr>::iterator iter = begin(); iter != end(); iter++) {
        TAudioEffectInterfacePtr process = *iter;
        copy->push_front(process->Copy());
    }

    return copy;
}

