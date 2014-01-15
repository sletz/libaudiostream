/*
Copyright (C) Grame 2005-2013

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

#include "faust/llvm-dsp.h"
#include "faust/remote-dsp.h"
#include "faust/gui/jsonfaustui.h"

#include "TAudioEffectInterface.h"
#include "TAudioGlobals.h"
#include "TLASException.h"

#include <iostream>
#include <sstream>

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

using namespace std;

//-------------------------
// Class TFaustAudioEffect
//-------------------------

class UIObject {

	protected:
		
		string fLabel;
		FAUSTFLOAT* fZone;
		
		float Range(FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT val) {return (val < min) ? min : (val > max) ? max : val;}
	
	public:
			
		UIObject(const string& label, FAUSTFLOAT* zone):fLabel(label),fZone(zone) {}
		virtual ~UIObject() {}
		
		virtual void SetControlValue(float value) {*fZone = Range(0.0f, 1.0f, value);}
		virtual float GetControlValue() {return *fZone;}
		virtual void GetControlParam(char* label, FAUSTFLOAT* min, FAUSTFLOAT* max, FAUSTFLOAT* init)
		{
			strcpy(label, fLabel.c_str());
			*min = 0;
			*max = 0;
			*init = 0;
		}
        string GetLabel() { return fLabel; }
        
};

class CheckButton : public UIObject {
	
	public:
	
		CheckButton(const string& label, FAUSTFLOAT* zone):UIObject(label, zone) {}	
		virtual ~CheckButton() {}
};

class Button : public UIObject {
	
	public:
	
		Button(const string& label, FAUSTFLOAT* zone):UIObject(label, zone) {}
		virtual ~Button() {}		
};

class Slider : public UIObject {

	private:
	
		FAUSTFLOAT fInit;
		FAUSTFLOAT fMin;
		FAUSTFLOAT fMax;
		FAUSTFLOAT fStep;
	
	public:	
	
		Slider(const string& label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
			:UIObject(label, zone),fInit(init),fMin(min),fMax(max),fStep(step) {}
		virtual ~Slider() {}	
		
		void SetControlValue(float value) { *fZone = Range(fMin, fMax, value); }
		
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
	
		Bargraph(const string& label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max)
			:UIObject(label,zone),fMin(min),fMax(max) {}
		virtual ~Bargraph() {}	
		
		void SetControlValue(FAUSTFLOAT value) { *fZone = Range(fMin, fMax, value); }
		
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
        vector<string> fControlsLevel;
        
        TFaustAudioEffectBase* CopyState(TFaustAudioEffectBase* src)
        {
            // Copy current control values
            for (int i = 0; i < src->GetControlCount(); i++) {
                SetControlValue(i, src->GetControlValue(i));
            }
            return this;
        }
        
        string buildPath(const std::string& label) 
        {
            string res = "/";
            for (size_t i = 0; i < fControlsLevel.size(); i++) {
                res += fControlsLevel[i];
                res += "/";
            }
            res += label;
            return res;
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
           
        void openTabBox(const char* label) { fControlsLevel.push_back(label); }
		void openHorizontalBox(const char* label) { fControlsLevel.push_back(label); }
		void openVerticalBox(const char* label) { fControlsLevel.push_back(label); }
		void closeBox() { fControlsLevel.pop_back(); }
        
		void addButton(const char* label, FAUSTFLOAT* zone) 
        {
            fUITable.push_back(new Button(buildPath(label), zone));
        }
		
		void addCheckButton(const char* label, FAUSTFLOAT* zone) 
        {
            fUITable.push_back(new CheckButton(buildPath(label), zone));
        }
		
		void addVerticalSlider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) 
		{ 	
            fUITable.push_back(new Slider(buildPath(label), zone, init, min, max, step));
		}
		
		void addHorizontalSlider(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step) 
		{
            fUITable.push_back(new Slider(buildPath(label), zone, init, min, max, step));
		}
		
        void addNumEntry(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT init, FAUSTFLOAT min, FAUSTFLOAT max, FAUSTFLOAT step)
        {
            fUITable.push_back(new Slider(buildPath(label), zone, init, min, max, step));
        }
		
		virtual void addHorizontalBargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max) 
		{
            fUITable.push_back(new Bargraph(buildPath(label), zone, min, max));
		}
		virtual void addVerticalBargraph(const char* label, FAUSTFLOAT* zone, FAUSTFLOAT min, FAUSTFLOAT max)
		{
            fUITable.push_back(new Bargraph(buildPath(label), zone, min, max));
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
		
		void SetControlValue(long param, FAUSTFLOAT value) 
		{
			if (param < long(fUITable.size())) {
				fUITable[param]->SetControlValue(value);
            }
		}
        
        void SetControlValue(const char* label, FAUSTFLOAT value) 
        {
            for (vector<UIObject*>::iterator iter = fUITable.begin(); iter != fUITable.end(); iter++) {
          		if (strcmp((*iter)->GetLabel().c_str(), label) == 0) {
                    (*iter)->SetControlValue(value);
                    return;
                }
            }
        }
		
		FAUSTFLOAT GetControlValue(long param) 
		{
			return (param < long(fUITable.size())) ? fUITable[param]->GetControlValue() : 0.0f;
		}
        
        FAUSTFLOAT GetControlValue(const char* label) 
        {
            for (vector<UIObject*>::iterator iter = fUITable.begin(); iter != fUITable.end(); iter++) {
                if ((*iter)->GetLabel() == string(label)) {
                    return (*iter)->GetControlValue();
                }
            }
            return 0.0f;
        }
        
        virtual const char* GetJson() { return ""; }
        virtual std::string GetName() { return ""; }
        virtual void SetName(const std::string& name) {}
};

typedef TFaustAudioEffectBase * TFaustAudioEffectBasePtr;

/*!
\brief Faust static effect (loaded from a DLL).
*/
class TModuleFaustAudioEffect : public TFaustAudioEffectBase
{

    private:
	
		string fName;
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

        TModuleFaustAudioEffect(const string& name): TFaustAudioEffectBase()
        {
			fName = name;
			fHandle = LoadFaustModule(name.c_str());
			if (!fHandle) {
                stringstream error;
                error << "Cannot LoadFaustModule " << name << endl;
                throw TLASException(error.str());
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
                stringstream error;
                error << "DSP instance is not stereo and has " << fGetNumInputs(fDsp) << " ins and " << fGetNumOutputs(fDsp) << " outs" << endl;
                throw TLASException(error.str());
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

        void Process(FAUSTFLOAT** input, FAUSTFLOAT** output, long framesNum)
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
        long Inputs()
        {
            return fGetNumInputs(fDsp);
        }
        long Outputs()
        {
            return fGetNumOutputs(fDsp);
        }
		
};

typedef TModuleFaustAudioEffect * TModuleFaustAudioEffectPtr;

/*!
\brief Base class for LLVM + libfaust effect.
*/
class TCodeFaustAudioEffect : public TFaustAudioEffectBase {

    protected:
    
        string fName;
        string fJSON;
        
        struct Name_Meta : public Meta
        {
            string fName;
            
            Name_Meta():fName("")
            {}
            
            void declare(const char* key, const char* value)
            {   
                if (strcmp("name", key) == 0) {
                    fName = value;
                }
            }
        };
        
    public:
    
        TCodeFaustAudioEffect()
        {}
        virtual ~TCodeFaustAudioEffect()
        {}
        
        virtual TCodeFaustAudioEffect* CreateEffect(const string& name, const string& library_path, const string& draw_path) = 0;
  
};

/*!
\brief Base class for DSP factories.
*/
class TCodeFaustAudioEffectFactory 
{

    protected:
	
        string fLibraryPath;
        string fDrawPath;
        string fCode;
        string fName;
        
    public:
    
        TCodeFaustAudioEffectFactory()
        {}
        
        string GetLibraryPath() { return fDrawPath; }
        string GetDrawPath() { return fDrawPath; }
        virtual string GetCode() { return ""; }
        
        void SetName(const string& name) { fName = name; }
        string GetName() { return fName; }
        
        // Duplicate a Faust effect 'num' times 
        static TCodeFaustAudioEffect* DuplicateEffect(TAudioEffectInterface* effect, int num);
         
        // Split a Faust effect 'num' times 
        static TCodeFaustAudioEffect* SplitEffect(TAudioEffectInterface* effect, int num); 
        
};

/*!
\brief Base class for local DSP factories.
*/
class TLocalCodeFaustAudioEffect;

class TLocalCodeFaustAudioEffectFactory : public TCodeFaustAudioEffectFactory
{

    protected:
	
        llvm_dsp_factory* fFactory;
        
        string GetTarget()
        {
            int tmp;
            return (sizeof(&tmp) == 8) ? "x86_64-apple-darwin12.2.1" : "i386-apple-darwin10.6.0";
        }
		
    public:
    
        TLocalCodeFaustAudioEffectFactory()
        {}
        
        llvm_dsp_factory* GetFactory() { return fFactory; }
        
        static TCodeFaustAudioEffect* CreateEffect(const string& name, const string& library_path, const string& draw_path);
         
};

/*!
\brief Remote factory.
*/
class TRemoteCodeFaustAudioEffect;

class TRemoteCodeFaustAudioEffectFactory : public TLocalCodeFaustAudioEffectFactory
{

    protected:
	
        remote_dsp_factory* fFactory;
     	
    public:
    
        TRemoteCodeFaustAudioEffectFactory(const string& code, const string& library_path, const string& draw_path)
        {
            int argc = 0;
            const char* argv[32];
            std::string error_msg;
            
            fCode = code;
            fLibraryPath = library_path;
            fDrawPath = draw_path;
            
            //argv[0] = "dummy";
            //argv[1] = 0;
            
            
            /*
            // Try filename...
            argv[0] = code.c_str();
            
            // Add -svg parameter if necessary
            
            if (draw_path != "") {
                argc = 2;
                argv[1] = "-svg";
            } else {
                argc = 1;
            }
            */
            
            printf("code %s\n", code.c_str());
         
            //fFactory = createRemoteDSPFactory(argc, argv, "localhost", 7777, code, error_msg, 3);
            fFactory = NULL;
            fFactory = createRemoteDSPFactoryFromString("toto", code, argc, argv, "192.168.1.174", 7777, error_msg, 3);
            if (fFactory) {
                TAudioGlobals::fRemoteFactoryTable[code] = this;
                TAudioGlobals::fRemoteFactoryNumber++;
            }  else {
                stringstream error;
                error << "createDSPFactory error from DSP file " << error_msg << endl;
                throw TLASException(error.str());
            }
        }
        
        remote_dsp_factory* GetFactory() { return fFactory; }
        
        static TCodeFaustAudioEffect* CreateEffect(const string& name, const string& library_path, const string& draw_path);
        
        string GetCode() 
        { 
            return "environment { " + fCode  + " }.process";
        }
         
};

/*!
\brief File based local factory.
*/
class TFileCodeFaustAudioEffectFactory : public TLocalCodeFaustAudioEffectFactory {
    
    public:
    
        TFileCodeFaustAudioEffectFactory(const string& code, const string& library_path, const string& draw_path)
        {
            int argc;
            const char* argv[32];
            std::string error_msg;
            
            fCode = code;
            fLibraryPath = library_path;
            fDrawPath = draw_path;

            // Try filename...
            argv[0] = code.c_str();
            
            // Add -svg parameter if necessary
            if (draw_path != "") {
                argc = 2;
                argv[1] = "-svg";
            } else {
                argc = 1;
            }
         
            fFactory = createDSPFactory(argc, argv, library_path, draw_path, "", "", GetTarget(), error_msg, 3);
            if (fFactory) {
                TAudioGlobals::fLocalFactoryTable[code] = this;
                TAudioGlobals::fLocalFactoryNumber++;
            }  else {
                stringstream error;
                error << "createDSPFactory error from DSP file " << error_msg << endl;
                throw TLASException(error.str());
            }
        }

        string GetCode() 
        { 
            return "component(\"" + fCode  + "\")";
        }
};

/*!
\brief String based local factory.
*/
class TStringCodeFaustAudioEffectFactory : public TLocalCodeFaustAudioEffectFactory {

    public :
    
        TStringCodeFaustAudioEffectFactory(const string& code, const string& library_path, const string& draw_path)
        {
            int argc;
            const char* argv[32];
            std::string error_msg;
            char input_name[64];
            
            fCode = code;
            fLibraryPath = library_path;
            fDrawPath = draw_path;
            
            // Add -svg parameter if necessary
            if (draw_path != "") {
                argc = 1;
                argv[0] = "-svg";
            } else {
                argc = 0;
            }
            
            sprintf(input_name, "LAS-faustfx-%d", TAudioGlobals::fLocalFactoryNumber);
   
            // Try DSP code...
            fFactory = createDSPFactory(argc, argv, library_path, draw_path, input_name, code, GetTarget(), error_msg, 3);
            if (fFactory) {
                TAudioGlobals::fLocalFactoryTable[code] = this;
                TAudioGlobals::fLocalFactoryNumber++;
            } else {
                stringstream error;
                error << "createDSPFactory error from DSP code " << error_msg << endl;
                throw TLASException(error.str());
            }
        }
        
        string GetCode() 
        { 
            return "environment { " + fCode  + " }.process";
        }
 };

/*!
\brief LLVM IR based local factory.
*/
class TIRCodeFaustAudioEffectFactory : public TLocalCodeFaustAudioEffectFactory {

    public :
    
        TIRCodeFaustAudioEffectFactory(const string& code, const string& library_path, const string& draw_path)
        {
            fCode = code;
            fLibraryPath = library_path;
            fDrawPath = draw_path;
            
            // Try bitcode code string...
            fFactory = readDSPFactoryFromBitcode(code, GetTarget(), 3);
            if (fFactory) {
                goto end;
            } else {
                printf("readDSPFactoryFromBitcode error\n");
            }
     
            // Try bitcode code file...
            fFactory = readDSPFactoryFromBitcodeFile(code, GetTarget(), 3);
            if (fFactory) {
                goto end;
            } else {
                printf("readDSPFactoryFromBitcodeFile error\n");
            }
       
            // Try IR code string...
            fFactory = readDSPFactoryFromIR(code, GetTarget(), 3);
            if (fFactory) {
                goto end;
            } else {
                printf("readDSPFactoryFromIR error\n");
            }
      
            // Try IR code file...
            fFactory = readDSPFactoryFromIRFile(code, GetTarget(), 3);
            if (!fFactory) {
                printf("readDSPFactoryFromIR error\n");
                throw TLASException("cannot read IR code");
            } 
            
        end:
        
            TAudioGlobals::fLocalFactoryTable[code] = this;
            TAudioGlobals::fLocalFactoryNumber++;
        }

};

/*!
\brief Local LLVM + libfaust effect.
*/
class TLocalCodeFaustAudioEffect : public TCodeFaustAudioEffect
{

    private:
	
		llvm_dsp* fDsp;
        TLocalCodeFaustAudioEffectFactory* fFactory;
        
        string GetLibraryPath() { return fFactory->GetLibraryPath(); }
        string GetDrawPath() { return fFactory->GetDrawPath(); }
        string GetCode() { return fFactory->GetCode(); }

    public:

        TLocalCodeFaustAudioEffect(TLocalCodeFaustAudioEffectFactory* factory):TCodeFaustAudioEffect()
        {
            assert(factory);
            fFactory = factory;
            fDsp = createDSPInstance(fFactory->GetFactory());
            
            if (!fDsp) {
                throw TLASException("DSP instance cannot be created");
            }
            
            fDsp->init(TAudioGlobals::fSampleRate);
            fDsp->buildUserInterface(this);
            
            Name_Meta meta;
            metadataDSPFactory(fFactory->GetFactory(), &meta);
            fName = meta.fName;
            
            // Keep effect name in effect factory
            factory->SetName(fName);
            // Keep the effect in effect global table
            TAudioGlobals::fEffectTable[fName].push_back(this);
        }
        virtual ~TLocalCodeFaustAudioEffect()
        {
            deleteDSPInstance(fDsp);
        }
        void Process(FAUSTFLOAT** input, FAUSTFLOAT** output, long framesNum)
        {
			fDsp->compute(framesNum, input, output);
		}

        TAudioEffectInterface* Copy()
        {
            // Allocate copy
            return (new TLocalCodeFaustAudioEffect(fFactory))->CopyState(this);
        }
        void Reset()
        {
			fDsp->init(TAudioGlobals::fSampleRate);
		}
        
        long Inputs()
        {
            return fDsp->getNumInputs();
        }
        long Outputs()
        {
            return fDsp->getNumOutputs();
        }
        
        const char* GetJson()
        {
            httpdfaust::jsonfaustui json("", "", 0);
            fDsp->buildUserInterface(&json);
            metadataDSPFactory(fFactory->GetFactory(), &json);
            json.numInput(fDsp->getNumInputs());
            json.numOutput(fDsp->getNumOutputs());
            fJSON = json.json();
            return fJSON.c_str();
        }
              
        string GetName()
        {
            return fName;
        }
        
        TLocalCodeFaustAudioEffectFactory* GetFactory() { return fFactory; }
        
		TCodeFaustAudioEffect* CreateEffect(const string& name, const string& library_path, const string& draw_path)
        {
            return TLocalCodeFaustAudioEffectFactory::CreateEffect(name, library_path, draw_path);
        }
};

typedef TLocalCodeFaustAudioEffect * TLocalCodeFaustAudioEffectPtr;

/*!
\brief Remote LLVM + libfaust effect.
*/
class TRemoteCodeFaustAudioEffect : public TCodeFaustAudioEffect
{

    private:
	
		remote_dsp* fDsp;
        TRemoteCodeFaustAudioEffectFactory* fFactory;
            
        string GetLibraryPath() { return fFactory->GetLibraryPath(); }
        string GetDrawPath() { return fFactory->GetDrawPath(); }
        string GetCode() { return fFactory->GetCode(); }

    public:

        TRemoteCodeFaustAudioEffect(TRemoteCodeFaustAudioEffectFactory* factory):TCodeFaustAudioEffect()
        {
            assert(factory);
            fFactory = factory;
            string error;
            
            int argc = 4;
            const char* argv[32];
            std::string error_msg;
            
            argv[0] = "--NJ_latency";
            argv[1] = "2";
            argv[2] = "--NJ_ip";
            argv[3] = "127.0.0.1";
            
            fDsp = createRemoteDSPInstance(fFactory->GetFactory(), argc, argv, TAudioGlobals::fSampleRate, TAudioGlobals::fBufferSize, error);
            //fDsp = NULL;
            printf("TRemoteCodeFaustAudioEffect %x error.c_str() %s\n", fDsp, error.c_str());
            
            if (!fDsp) {
                throw TLASException(error.c_str());
            }
            
            fDsp->init(TAudioGlobals::fSampleRate);
            fDsp->buildUserInterface(this);
            
            /*
            Name_Meta meta;
            metadataDSPFactory(fFactory->GetFactory(), &meta);
            fName = meta.fName;
            */
            
            // Keep effect name in effect factory
            factory->SetName(fName);
            // Keep the effect in effect global table
            TAudioGlobals::fEffectTable[fName].push_back(this);
        }
        virtual ~TRemoteCodeFaustAudioEffect()
        {
            //deleteRemoteDSPInstance(fDsp);
        }
        void Process(FAUSTFLOAT** input, FAUSTFLOAT** output, long framesNum)
        {
			fDsp->compute(framesNum, input, output);
		}

        TAudioEffectInterface* Copy()
        {
            // Allocate copy
            return (new TRemoteCodeFaustAudioEffect(fFactory))->CopyState(this);
        }
        void Reset()
        {
			fDsp->init(TAudioGlobals::fSampleRate);
		}
        
        long Inputs()
        {
            return fDsp->getNumInputs();
        }
        long Outputs()
        {
            return fDsp->getNumOutputs();
        }
        
        const char* GetJson()
        {
            /*
            httpdfaust::jsonfaustui json("", "", 0);
            fDsp->buildUserInterface(&json);
            metadataDSPFactory(fFactory->GetFactory(), &json);
            json.numInput(fDsp->getNumInputs());
            json.numOutput(fDsp->getNumOutputs());
            */
            //fJSON = json.json();
            return fJSON.c_str();
        }
              
        string GetName()
        {
            return fName;
        }
        
        TRemoteCodeFaustAudioEffectFactory* GetFactory() { return fFactory; }
        
        TCodeFaustAudioEffect* CreateEffect(const string& name, const string& library_path, const string& draw_path)
        {
            return TRemoteCodeFaustAudioEffectFactory::CreateEffect(name, library_path, draw_path);
        }
		
};

typedef TLocalCodeFaustAudioEffect * TLocalCodeFaustAudioEffectPtr;
    
#endif
