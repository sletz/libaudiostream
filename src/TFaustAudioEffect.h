/*
Copyright © Grame 2005

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

#ifndef __TFaustAudioEffect__
#define __TFaustAudioEffect__

#include "TAudioEffectInterface.h"
#include "TAudioGlobals.h"

#include <dlfcn.h>

//-------------------------
// Class TFaustAudioEffect
//-------------------------

class dsp;
class UI;

typedef dsp* (* newDsp) ();
typedef void (* deleteDsp) (dsp* self); 						
typedef int (* getNumInputs) (dsp* self);
typedef int (* getNumOutputs) (dsp* self);
typedef void (* buildUserInterface) (dsp* self,UI* interface);
typedef void (* init) (dsp* self, int freq);
typedef void (* compute) (dsp* self, int len, float** inputs, float** outputs); 						
typedef void (* conclude) (dsp* self);

/*!
\brief Faust effect.
*/

class TFaustAudioEffect : public TAudioEffectInterface
{

    private:
	
		char fName[32];
		void* fHandle;
		dsp* fDsp;
		newDsp fNew;
		deleteDsp fDelete;
		getNumInputs fGetNumInputs;
		getNumOutputs fGetNumOutputs;
		buildUserInterface fBuildUserInterface;
		init fInit;
		compute fCompute;
		conclude fConclude;
		
    public:

        TFaustAudioEffect(const char* name): TAudioEffectInterface()
        {
			strcpy(fName, name);
			fHandle = dlopen(name, RTLD_LAZY);
			if (!fHandle) 
				 throw - 1;
			fNew = (newDsp)dlsym(fHandle, "newDsp");
			fDelete = (deleteDsp)dlsym(fHandle, "deleteDsp");
			fGetNumInputs = (getNumInputs)dlsym(fHandle, "getNumInputs");
			fGetNumOutputs = (getNumOutputs)dlsym(fHandle, "getNumOutputs");
			fBuildUserInterface = (buildUserInterface) dlsym(fHandle, "buildUserInterface");
			fInit = (init)dlsym(fHandle, "init");
			fCompute = (compute)dlsym(fHandle, "compute");
			fConclude = (conclude)dlsym(fHandle, "conclude");
			fDsp = fNew();
			fInit(fDsp, TAudioGlobals::fSample_Rate);
			
			//fBuildUserInterface(fDsp);
		}
        virtual ~TFaustAudioEffect()
        {
			if (fHandle) {
				fDelete(fDsp);
				dlclose(fHandle);
			}
		}

        void Process(float** input, float** output, long framesNum, long channels)
        {
			fCompute(fDsp, framesNum, input, output);
		}

        TAudioEffectInterface* Copy()
        {
            return new TFaustAudioEffect(fName);
        }
        void Reset()
        {
			fDelete(fDsp);
			fDsp = fNew();
			fInit(fDsp, TAudioGlobals::fSample_Rate);
			//fBuildUserInterface(fDsp);
		}
        long Channels()
        {
            return fGetNumInputs(fDsp);
        }
};

typedef TFaustAudioEffect * TFaustAudioEffectPtr;

#endif
