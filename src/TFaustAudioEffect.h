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
#include "faust/gui/jsonfaustui.h"

#include "TAudioEffectInterface.h"
#include "TAudioGlobals.h"
#include "TLASException.h"

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
#include <map>

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
        
        TFaustAudioEffectBase* CopyState(TFaustAudioEffectBase* src)
        {
            // Copy current control values
            for (int i = 0; i < src->GetControlCount(); i++) {
                SetControlValue(i, src->GetControlValue(i));
            }
            return this;
        }

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
        
        virtual const char* GetJson() { return ""; }
};

typedef TFaustAudioEffectBase * TFaustAudioEffectBasePtr;

/*!
\brief Faust static effect (loaded from a DLL).
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
                char error[512];
                snprintf(error, 512, "Cannot LoadFaustModule %s\n", name);
                throw TLASException(error);
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
                char error[512];
                snprintf(error, 512, "DSP instance is not stereo and has %d ins and %d outs\n", fGetNumInputs(fDsp), fGetNumOutputs(fDsp));
                throw TLASException(error);
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
             return (new TModuleFaustAudioEffect(fName))->CopyState(this);
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

/*!
\brief Faust dynamic effect generated form source using libfaust + LLVM.
*/

class TCodeFaustAudioEffect : public TFaustAudioEffectBase
{

    private:
	
		llvm_dsp* fDsp;
        string fCode;
        string fLibraryPath;
        string fDrawPath;
        
        // Global DSP factory table
        static std::map<string, llvm_dsp_factory*> fFactoryTable;
        static int fFactoryNumber;
        
        string getTarget()
        {
            int tmp;
            return (sizeof(&tmp) == 8) ? "x86_64-apple-darwin12.2.1" : "i386-apple-darwin10.6.0";
        }
		
    public:

        TCodeFaustAudioEffect(const string& code, const string& library_path, const string& draw_path):TFaustAudioEffectBase()
        {
            int argc;
            const char* argv[32];
            char error_msg[256] = {0};
            char error_lib[512] = {0};
            llvm_dsp_factory* factory = NULL;
            fCode = code;
            fLibraryPath = library_path;
            fDrawPath = draw_path;
            
            if (fFactoryTable.find(code) != fFactoryTable.end()) {
                printf("DSP factory already created...\n");
                factory = fFactoryTable[code];
                goto make_instance;
            }
            
            // Try filename...
            argv[0] = code.c_str();
            
            // Add -svg parameter if necessary
            if (draw_path != "") {
                argc = 2;
                argv[1] = "-svg";
            } else {
                argc = 1;
            }
         
            factory = createDSPFactory(argc, argv, library_path, draw_path, "", "", getTarget(), error_msg, 3);
            if (factory) {
                goto make_instance;
            }  else {
                printf("error_lib %s\n", error_msg);
                snprintf(error_lib, 512, "createDSPFactory error from DSP file %s", error_msg);
            }
            
            // Add -svg parameter if necessary
            if (draw_path != "") {
                argc = 1;
                argv[0] = "-svg";
            } else {
                argc = 0;
            }
            
            char input_name[64];
            sprintf(input_name, "LAS-faustfx-%d", fFactoryNumber);
   
            // Try DSP code...
            factory = createDSPFactory(argc, argv, library_path, draw_path, input_name, code, getTarget(), error_msg, 3);
            if (factory) {
                goto make_instance;
            }  else {
                snprintf(error_lib, 512, "createDSPFactory error from DSP code %s", error_msg);
            }
            
            // Try bitcode code string...
            factory = readDSPFactoryFromBitcode(code, getTarget(), 3);
            if (factory) {
                goto make_instance;
            }  else {
                printf("readDSPFactoryFromBitcode error");
            }
     
            // Try bitcode code file...
            factory = readDSPFactoryFromBitcodeFile(code, getTarget(), 3);
            if (factory) {
                goto make_instance;
            }  else {
                printf("readDSPFactoryFromBitcodeFile error");
            }
       
            // Try IR code string...
            factory = readDSPFactoryFromIR(code, getTarget(), 3);
            if (factory) {
                goto make_instance;
            }  else {
                printf("readDSPFactoryFromIR error");
            }
      
            // Try IR code file...
            factory = readDSPFactoryFromIRFile(code, getTarget(), 3);
            if (!factory) {
                printf("readDSPFactoryFromIR error");
                throw TLASException(error_lib);
            } 
                        
        make_instance:
        
            assert(factory);
            
            fDsp = createDSPInstance(factory);
            if (!fDsp) {
                snprintf(error_lib, 512, "DSP instance cannot be created\n");
                deleteDSPFactory(factory);
                throw TLASException(error_lib);
            }
            
            fDsp->init(TAudioGlobals::fSampleRate);
			if (fDsp->getNumInputs() != 2 || fDsp->getNumOutputs() != 2) { // Temporary
                snprintf(error_lib, 512, "DSP instance is not stereo and has %d ins and %d outs\n", fDsp->getNumInputs(), fDsp->getNumOutputs());
                deleteDSPInstance(fDsp);
                deleteDSPFactory(factory);
				throw TLASException(error_lib);
			}
            
            fFactoryTable[code] = factory;
            fFactoryNumber++;
			fDsp->buildUserInterface(this);
		}
        virtual ~TCodeFaustAudioEffect()
        {
            deleteDSPInstance(fDsp);
        }
        void Process(FAUSTFLOAT** input, FAUSTFLOAT** output, long framesNum, long channels)
        {
			fDsp->compute(framesNum, input, output);
		}

        TAudioEffectInterface* Copy()
        {
            // Allocate copy
            return (new TCodeFaustAudioEffect(fCode, fLibraryPath, fDrawPath))->CopyState(this);
        }
        void Reset()
        {
			fDsp->init(TAudioGlobals::fSampleRate);
		}
        long Channels()
        {
            return fDsp->getNumInputs();
        }
        
        const char* GetJson()
        {
            httpdfaust::jsonfaustui json("", "", 0);
            fDsp->buildUserInterface(&json);
            metadataDSPFactory(fFactoryTable[fCode], &json);
            json.numInput(fDsp->getNumInputs());
            json.numOutput(fDsp->getNumOutputs());
            return json.json();
        }
		
};

typedef TCodeFaustAudioEffect * TCodeFaustAudioEffectPtr;

#endif
