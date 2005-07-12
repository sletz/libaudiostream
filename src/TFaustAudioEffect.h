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

#ifdef __Macintosh__
#include <dlfcn.h>
#define HANDLE void* 
#define LoadModule(name) dlopen((name), RTLD_LAZY);
#define UnloadModule(handle) dlclose((handle));
#define GetProc(handle, name) dlsym((handle), (name));
#elif WIN32
#include <windows.h>
#define HANDLE HINSTANCE 
#define LoadModule(name) LoadLibrary((name));
#define UnloadModule(handle) FreeLibrary((handle));
#define GetProc(handle, name) GetProcAddress((handle), (name));
#endif

#include <vector>

//-------------------------
// Class TFaustAudioEffect
//-------------------------

class dsp;

typedef void (*uiCallback)(float val, void* data);

class UI
{
		
	public:
			
		UI() {}
		virtual ~UI() {}
		
		// -- active widgets
		
		virtual void addButton(char* label, float* zone) = 0;
		virtual void addToggleButton(char* label, float* zone) = 0;
		virtual void addCheckButton(char* label, float* zone) = 0;
		virtual void addVerticalSlider(char* label, float* zone, float init, float min, float max, float step) = 0;
		virtual void addHorizontalSlider(char* label, float* zone, float init, float min, float max, float step) = 0;
		virtual void addNumEntry(char* label, float* zone, float init, float min, float max, float step) = 0;
		
		// -- passive widgets
		
		virtual void addNumDisplay(char* label, float* zone, int precision) = 0;
		virtual void addTextDisplay(char* label, float* zone, char* names[], float min, float max) = 0;
		virtual void addHorizontalBargraph(char* label, float* zone, float min, float max) = 0;
		virtual void addVerticalBargraph(char* label, float* zone, float min, float max) = 0;
		void addCallback(float* zone, uiCallback foo, void* data);

		// -- widget's layouts
	
		virtual void openFrameBox(char* label) = 0;
		virtual void openTabBox(char* label) = 0;
		virtual void openHorizontalBox(char* label) = 0;
		virtual void openVerticalBox(char* label) = 0;
		virtual void closeBox() = 0;
};

class UIObject {

	protected:
		
		string fLabel;
		float* fZone;
		
		float range(float min, float max, float val) {return (val < min) ? min : (val > max) ? max : val;}
	
	public:
			
		UIObject(char* label, float* zone):fLabel(label),fZone(zone) {}
		virtual ~UIObject() {}
		
		virtual void SetControlValue(float f) {*fZone = range(0.0f, 1.0f, f);}
		virtual float GetControlValue() {return *fZone;}
		virtual void GetControlParam(char* label, float* min, float* max, float* init)
		{
			strcpy(label, fLabel.c_str());
			*min = 0;
			*max = 0;
			*init = 0;
		}
};

class ToggleButton : public UIObject {
	
	public:	
	
		ToggleButton(char* label, float* zone):UIObject(label,zone) {}
		virtual ~ToggleButton() {}
};

class CheckButton : public UIObject {
	
	public:
	
		CheckButton(char* label, float* zone):UIObject(label,zone) {}	
		virtual ~CheckButton() {}
};

class Button : public UIObject {
	
	public:
	
		Button(char* label, float* zone):UIObject(label,zone) {}
		virtual ~Button() {}		
};

class Slider : public UIObject {

	private:
	
		float fInit;
		float fMin;
		float fMax;
		float fStep;
	
	public:	
	
		Slider(char* label, float* zone, float init, float min, float max, float step)
			:UIObject(label,zone),fInit(init),fMin(min),fMax(max),fStep(step) {}
		virtual ~Slider() {}	
		
		void SetControlValue(float f) {*fZone = range(fMin, fMax, f);}
		
		virtual void GetControlParam(char* label, float* min, float* max, float* init)
		{
			UIObject::GetControlParam(label, min, max, init);
			*min = fMin;
			*max = fMax;
			*init = fInit;
		}
};

typedef dsp* (* newDsp) ();
typedef void (* deleteDsp) (dsp* self); 						
typedef int (* getNumInputs) (dsp* self);
typedef int (* getNumOutputs) (dsp* self);
typedef void (* buildUserInterface) (dsp* self,UI* ui);
typedef void (* init) (dsp* self, int freq);
typedef void (* compute) (dsp* self, int len, float** inputs, float** outputs); 						
typedef void (* conclude) (dsp* self);

/*!
\brief Faust effect.
*/

class TFaustAudioEffect : public TAudioEffectInterface, public UI
{

    private:
	
		char fName[32];
		HANDLE fHandle;
		dsp* fDsp;
		newDsp fNew;
		deleteDsp fDelete;
		getNumInputs fGetNumInputs;
		getNumOutputs fGetNumOutputs;
		buildUserInterface fBuildUserInterface;
		init fInit;
		compute fCompute;
		conclude fConclude;
		vector<UIObject*> fUITable;
		
    public:

        TFaustAudioEffect(const char* name): TAudioEffectInterface()
        {
			strcpy(fName, name);
			fHandle = LoadModule(name);
			if (!fHandle) 
				 throw - 1;
			fNew = (newDsp)GetProc(fHandle, "newDsp");
			fDelete = (deleteDsp)GetProc(fHandle, "deleteDsp");
			fGetNumInputs = (getNumInputs)GetProc(fHandle, "getNumInputs");
			fGetNumOutputs = (getNumOutputs)GetProc(fHandle, "getNumOutputs");
			fBuildUserInterface = (buildUserInterface) GetProc(fHandle, "buildUserInterface");
			fInit = (init)GetProc(fHandle, "init");
			fCompute = (compute)GetProc(fHandle, "compute");
			fConclude = (conclude)GetProc(fHandle, "conclude");
			fDsp = fNew();
			fInit(fDsp, TAudioGlobals::fSample_Rate);
			fBuildUserInterface(fDsp, this);
		}
        virtual ~TFaustAudioEffect()
        {
			for (vector<UIObject*>::iterator iter = fUITable.begin(); iter != fUITable.end(); iter++) 
				delete *iter;
				
			if (fHandle) {
				fDelete(fDsp);
				UnloadModule(fHandle);
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
			fBuildUserInterface(fDsp, this);
		}
        long Channels()
        {
            return fGetNumInputs(fDsp);
        }
		
		void addButton(char* label, float* zone) {fUITable.push_back(new Button(label, zone));}
		
		void addToggleButton(char* label, float* zone) {fUITable.push_back(new ToggleButton(label, zone));}
		
		void addCheckButton(char* label, float* zone) {fUITable.push_back(new CheckButton(label, zone));}
		
		void addVerticalSlider(char* label, float* zone, float init, float min, float max, float step) 
		{ 	
			fUITable.push_back(new Slider(label, zone, init, min, max, step));
		}
		
		void addHorizontalSlider(char* label, float* zone, float init, float min, float max, float step) 
		{
			fUITable.push_back(new Slider(label, zone, init, min, max, step));
		}
		
		void addNumEntry(char* label, float* zone, float init, float min, float max, float step)
		{
			fUITable.push_back(new Slider(label, zone, init, min, max, step));
		}
		
		virtual void addNumDisplay(char* label, float* zone, int precision) {}
		virtual void addTextDisplay(char* label, float* zone, char* names[], float min, float max) {}
		virtual void addHorizontalBargraph(char* label, float* zone, float min, float max) {}
		virtual void addVerticalBargraph(char* label, float* zone, float min, float max) {}
		void addCallback(float* zone, uiCallback foo, void* data);
		
		void openFrameBox(char* label) {}
		void openTabBox(char* label) {}
		void openHorizontalBox(char* label) {}
		void openVerticalBox(char* label) {}
		void closeBox() {}
		
		long GetControlCount()
		{
			return fUITable.size(); 
		}
				
		void GetControlParam(long param, char* label, float* min, float* max, float* init)
		{
			assert(param < fUITable.size()); 
			fUITable[param]->GetControlParam(label, min, max, init);
		}
		
		void SetControlValue(long param, float f) 
		{
			assert(param < fUITable.size()); 
			fUITable[param]->SetControlValue(f);
		}
		
		float GetControlValue(long param) 
		{
			assert(param < fUITable.size()); 
			return fUITable[param]->GetControlValue();
		}
};

typedef TFaustAudioEffect * TFaustAudioEffectPtr;

#endif
