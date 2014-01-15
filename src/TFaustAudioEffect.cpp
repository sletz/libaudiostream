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

#include <map>
#include "TFaustAudioEffect.h"
#include <iostream>
#include <sstream>
#include <fstream>

static bool CheckEnding(const string& name, const string& end)
{
    unsigned int match = name.rfind(end);
    return ((match != string::npos) && (name.size() - end.size() == match));
}
   
static string PathToContent(const string& path)
{
    ifstream f(path.c_str());
    string result;
    char line[4096];
    
    while (f.getline(line, 4096)) {
        result += line;
        if (!f.fail()) {
            result += "\n";
        }
    }
    
    f.close();
    return result;
}

// Duplicate a Faust effect 'num' times 
TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::DuplicateEffect(TAudioEffectInterface* effect, int num) 
{
    TCodeFaustAudioEffect* faust_effect = dynamic_cast<TCodeFaustAudioEffect*>(effect);
    assert(faust_effect);
    stringstream faust_code_stream;
    faust_code_stream << "declare name \"" << effect->GetName() << "\";" << "process = par(i,n," << effect->GetCode() << ") " << "with { n = " << num << "; };";
    return faust_effect->CreateEffect(faust_code_stream.str().c_str(), effect->GetLibraryPath(), effect->GetDrawPath());
}

// Split a stream 'num' times to connect to a Faust effect
TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::SplitEffect(TAudioEffectInterface* effect, int num) 
{
    TCodeFaustAudioEffect* faust_effect = dynamic_cast<TCodeFaustAudioEffect*>(effect);
    assert(faust_effect);
    stringstream faust_code_stream;
    faust_code_stream << "declare name \"" << effect->GetName() << "\";" << "process = par(i," << num << ",_)<:" << effect->GetCode() << ";";
    return faust_effect->CreateEffect(faust_code_stream.str().c_str(), effect->GetLibraryPath(), effect->GetDrawPath());
}

TCodeFaustAudioEffect* TLocalCodeFaustAudioEffectFactory::CreateEffect(const string& name, const string& library_path, const string& draw_path)
{
    TLocalCodeFaustAudioEffectFactory* factory = 0;
    if (TAudioGlobals::fLocalFactoryTable.find(name) != TAudioGlobals::fLocalFactoryTable.end()) {
        printf("DSP factory already created...\n");
        factory = TAudioGlobals::fLocalFactoryTable[name];
    } else if (CheckEnding(name, ".dsp")) {  // Here we assume only 'file' or 'string' are used (not IR stuff...)
        factory = new TFileCodeFaustAudioEffectFactory(name, library_path, draw_path);
    } else {
        factory = new TStringCodeFaustAudioEffectFactory(name, library_path, draw_path);
    }
    return new TLocalCodeFaustAudioEffect(factory);
}

TCodeFaustAudioEffect* TRemoteCodeFaustAudioEffectFactory::CreateEffect(const string& name, const string& library_path, const string& draw_path)
{
    TRemoteCodeFaustAudioEffectFactory* factory = 0;
    if (TAudioGlobals::fRemoteFactoryTable.find(name) != TAudioGlobals::fRemoteFactoryTable.end()) {
        printf("DSP factory already created...\n");
        factory = TAudioGlobals::fRemoteFactoryTable[name];
    } else if (CheckEnding(name, ".dsp")) { 
        factory = new TRemoteCodeFaustAudioEffectFactory(PathToContent(name), library_path, draw_path);
    } else {
        factory = new TRemoteCodeFaustAudioEffectFactory(name, library_path, draw_path);
    }
    return new TRemoteCodeFaustAudioEffect(factory);
}
