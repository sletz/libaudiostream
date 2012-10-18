/*
Copyright (C) Grame 2005-2012

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

#ifndef __TFaustAudioEffect__
#define __TFaustAudioEffect__

#include "faust/audio/dsp.h"
#include "faust/gui/UI.h"
#include "faust/llvm-dsp.h"

#include "TAudioEffectInterface.h"
#include "TAudioGlobals.h"

#ifdef WIN32

#include <windows.h>
#define HANDLE HINSTANCE 
#define LoadFaustModule(name) LoadLibrary((name));
#define UnloadFaustModule(handle) FreeLibrary((handle));  
#define GetFaustProc(handle, name) GetProcAddress((handle), (name));

#else

#include <dlfcn.h>
#define HANDLE void* 
#define LoadFaustModule(name) dlopen((name), RTLD_NOW | RTLD_LOCAL);
#define UnloadFaustModule(handle) dlclose((handle));
#define GetFaustProc(handle, name) dlsym((handle), (name));

#endif

#include <vector>

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif

//-------------------------
// Class TFaustAudioEffect
//-------------------------

class UIObject {

	protected:
		
		string fLabel;
		FAUSTFLOAT* fZone;
		
		float Range(FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT val) {return (val < min) ? min : (val > max) ? max : val;}
	
	public:
			
		UIObject(const char* label, FAUSTFLOAT* zone):fLabel(label),fZone(zone) {}
		virtual ~UIObject() {}
		
		virtual void SetControlValue(float f) {*fZone = Range(0.0f, 1.0f, f);}
		virtual float GetControlValue() {return *fZone;}
		virtual void GetControlParam(char* label, FAUSTFLOAT* min, FAUSTFLOAT* max, FAUSTFLOAT* init)
		{
			strcpy(label, fLabel.c_str());
			*min = 0;
			*max = 0;
			*init = 0;
		}
};

class ToggleButton : public UIObject {
	
	public:	
	
		ToggleButton(const char* label, FAUSTFLOAT* zone):UIObject(label, zone) {}
		virtual ~ToggleButton() {}
};

class CheckButton : public UIObject {
	
	public:
	
		CheckButton(const char* label, FAUSTFLOAT* zone):UIObject(label, zone) {}	
		virtual ~CheckButton() {}
};

class Button : public UIObject {
	
	public:
	
		Button(const char* label, FAUSTFLOAT* zone):UIObject(label, zone) {}
		virtual ~Button() {}		
};

class Slider : public UIObject {

	private:
	
		FAUSTFLOAT fInit;
		FAUSTFLOAT fMin;
		FAUSTFLOAT fMax;
		FAUSTFLOAT fStep;
	
	public:	
	
		Slider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
			:UIObject(label, zone),fInit(init),fMin(min),fMax(max),fStep(step) {}
		virtual ~Slider() {}	
		
		void SetControlValue(float f) {*fZone = Range(fMin, fMax, f);}
		
		virtual void GetControlParam(char* label, FAUSTFLOAT* min, FAUSTFLOAT* max, FAUSTFLOAT* init)
		{
			UIObject::GetControlParam(label, min, max, init);
			*min = fMin;
			*max = fMax;
			*init = fInit;
		}
};

class Bargraph : public UIObject {

	private:
	
		FAUSTFLOAT fMin;
		FAUSTFLOAT fMax;
	
	public:	
	
		Bargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max)
			:UIObject(label,zone),fMin(min),fMax(max) {}
		virtual ~Bargraph() {}	
		
		void SetControlValue(FAUSTFLOAT f) {*fZone = Range(fMin, fMax, f);}
		
		virtual void GetControlParam(char* label, FAUSTFLOAT* min, FAUSTFLOAT* max, FAUSTFLOAT* init)
		{
			UIObject::GetControlParam(label, min, max, init);
			*min = fMin;
			*max = fMax;
			*init = 0.f;
		}
};

typedef dsp* (* newDsp) ();
typedef void (* deleteDsp) (dsp* self); 						
typedef int (* getNumInputs) (dsp* self);
typedef int (* getNumOutputs) (dsp* self);
typedef void (* buildUserInterface) (dsp* self, UI* ui);
typedef void (* init) (dsp* self, int freq);
typedef void (* compute) (dsp* self, int len, FAUSTFLOAT** inputs, FAUSTFLOAT** outputs); 						
typedef void (* conclude) (dsp* self);

/*!
\brief Faust effect interface.
*/

class TFaustAudioEffectBase : public TAudioEffectInterface, public UI
{
	protected:
	
		vector<UIObject*> fUITable;

    public:

        TFaustAudioEffectBase(): TAudioEffectInterface(), UI()
        {}
        virtual ~TFaustAudioEffectBase()
        {
			for (vector<UIObject*>::iterator iter = fUITable.begin(); iter != fUITable.end(); iter++) {
				delete *iter;
            }
 		}
        
        void openTabBox(const char* label) {}
		void openHorizontalBox(const char* label) {}
		void openVerticalBox(const char* label) {}
		void closeBox() {}

		void addButton(const char* label, FAUSTFLOAT* zone) {fUITable.push_back(new Button(label, zone));}
		
		void addCheckButton(const char* label, FAUSTFLOAT* zone) {fUITable.push_back(new CheckButton(label, zone));}
		
		void addVerticalSlider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) 
		{ 	
			fUITable.push_back(new Slider(label, zone, init, min, max, step));
		}
		
		void addHorizontalSlider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) 
		{
			fUITable.push_back(new Slider(label, zone, init, min, max, step));
		}
		
		void addNumEntry(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
		{
			fUITable.push_back(new Slider(label, zone, init, min, max, step));
		}
		
		virtual void addHorizontalBargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max) 
		{
			fUITable.push_back(new Bargraph(label, zone, min, max));
		}
		virtual void addVerticalBargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max)
		{
			fUITable.push_back(new Bargraph(label, zone, min, max));
		}
	
		virtual void show() {}
		virtual void run() {}
		
		long GetControlCount()
		{
			return fUITable.size(); 
		}
				
		void GetControlParam(long param, char* label, FAUSTFLOAT* min, FAUSTFLOAT* max, FAUSTFLOAT* init)
		{
			if (param < long(fUITable.size())) {
				fUITable[param]->GetControlParam(label, min, max, init);
            }
		}
		
		void SetControlValue(long param, FAUSTFLOAT f) 
		{
			if (param < long(fUITable.size())) {
				fUITable[param]->SetControlValue(f);
            }
		}
		
		FAUSTFLOAT GetControlValue(long param) 
		{
			return (param < long(fUITable.size())) ? fUITable[param]->GetControlValue() : 0.0f;
		}
};

typedef TFaustAudioEffectBase * TFaustAudioEffectBasePtr;

/*!
\brief Faust effect.
*/

class TModuleFaustAudioEffect : public TFaustAudioEffectBase
{

    private:
	
		char fName[512];
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
		
    public:

        TModuleFaustAudioEffect(const char* name): TFaustAudioEffectBase()
        {
			strcpy(fName, name);
			fHandle = LoadFaustModule(name);
			if (!fHandle) {
				 throw -1;
            }
			fNew = (newDsp)GetFaustProc(fHandle, "newDsp");
			fDelete = (deleteDsp)GetFaustProc(fHandle, "deleteDsp");
			fGetNumInputs = (getNumInputs)GetFaustProc(fHandle, "getNumInputs");
			fGetNumOutputs = (getNumOutputs)GetFaustProc(fHandle, "getNumOutputs");
			fBuildUserInterface = (buildUserInterface)GetFaustProc(fHandle, "buildUserInterface");
			fInit = (init)GetFaustProc(fHandle, "init");
			fCompute = (compute)GetFaustProc(fHandle, "compute");
			fConclude = (conclude)GetFaustProc(fHandle, "conclude");
			fDsp = fNew();
			fInit(fDsp, TAudioGlobals::fSampleRate);
			if (fGetNumInputs(fDsp) != 2 || fGetNumOutputs(fDsp) != 2) { // Temporary
				fDelete(fDsp);
				UnloadFaustModule(fHandle);
				throw -2;
			}
			fBuildUserInterface(fDsp, this);
		}
        virtual ~TModuleFaustAudioEffect()
        {
			if (fHandle) {
				fDelete(fDsp);
				UnloadFaustModule(fHandle);
			}
		}

        void Process(FAUSTFLOAT** input, FAUSTFLOAT** output, long framesNum, long channels)
        {
			fCompute(fDsp, framesNum, input, output);
		}

        TAudioEffectInterface* Copy()
        {
            return new TModuleFaustAudioEffect(fName);
        }
        void Reset()
        {
			fInit(fDsp, TAudioGlobals::fSampleRate);
		}
        long Channels()
        {
            return fGetNumInputs(fDsp);
        }
		
};

typedef TModuleFaustAudioEffect * TModuleFaustAudioEffectPtr;

class TCodeFaustAudioEffect : public TFaustAudioEffectBase
{

    private:
	
		llvm_dsp* fDsp;
        llvm_dsp_factory* fFactory;
        string fCode;
		
    public:

        TCodeFaustAudioEffect(const string& code):TFaustAudioEffectBase()
        {
            char error_msg[256];
            fCode = code;
            
            // Try filename...
            int argc = 1;
            const char* argv[argc];
            argv[0] = code.c_str();
         
            fFactory = createDSPFactory(argc, argv, "", "", "", "", "i386-apple-darwin10.6.0", error_msg, 3);
            if (fFactory) {
                goto make_instance;
            }  else {
                printf("createDSPFactory error from DSP file %s", error_msg);
            }
   
            // Try DSP code...
            fFactory = createDSPFactory(0, NULL, "", "", "in", code, "i386-apple-darwin10.6.0", error_msg, 3);
            if (fFactory) {
                goto make_instance;
            }  else {
                printf("createDSPFactory error from DSP code %s", error_msg);
            }
            
            // Try bitcode code string...
            fFactory = readDSPFactoryFromBitcode(code, "", 3);
            if (fFactory) {
                goto make_instance;
            }  else {
                printf("readDSPFactoryFromBitcode error \n");
            }
     
            // Try bitcode code file...
            fFactory = readDSPFactoryFromBitcodeFile(code, "", 3);
            if (fFactory) {
                goto make_instance;
            }  else {
                printf("readDSPFactoryFromBitcodeFile error \n");
            }
       
            // Try IR code string...
            fFactory = readDSPFactoryFromIR(code, "", 3);
            if (fFactory) {
                goto make_instance;
            }  else {
                printf("readDSPFactoryFromIR error \n");
            }
      
            // Try IR code file...
            fFactory = readDSPFactoryFromIRFile(code, "", 3);
            if (!fFactory) {
                printf("readDSPFactoryFromIRFile error \n");
                throw -1;
            } 
                        
        make_instance:
        
            assert(fFactory);
        
			fDsp = createDSPInstance(fFactory);
            if (!fDsp) {
                deleteDSPFactory(fFactory);
                throw -2;
            }
            
            fDsp->init(TAudioGlobals::fSampleRate);
			if (fDsp->getNumInputs() != 2 || fDsp->getNumOutputs() != 2) { // Temporary
                deleteDSPInstance(fDsp);
                deleteDSPFactory(fFactory);
				throw -3;
			}
            
			fDsp->buildUserInterface(this);
		}
        virtual ~TCodeFaustAudioEffect()
        {
            deleteDSPInstance(fDsp);
            deleteDSPFactory(fFactory);
        }
        void Process(FAUSTFLOAT** input, FAUSTFLOAT** output, long framesNum, long channels)
        {
			fDsp->compute(framesNum, input, output);
		}

        TAudioEffectInterface* Copy()
        {
            return new TCodeFaustAudioEffect(fCode);
        }
        void Reset()
        {
			fDsp->init(TAudioGlobals::fSampleRate);
		}
        long Channels()
        {
            return fDsp->getNumInputs();
        }
		
};

typedef TCodeFaustAudioEffect * TCodeFaustAudioEffectPtr;

#endif
