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

int TLocalCodeFaustAudioEffect::fEffectIndex = 0;
#if REMOTE_DSP
int TRemoteCodeFaustAudioEffect::fEffectIndex = 0;
#endif

static bool CheckEnding(const std::string& name, const std::string& end)
{
    const auto match = name.rfind(end);
    return ((match != std::string::npos) && (name.size() - end.size() == match));
}

static std::string PathToContent(const std::string& path)
{
    std::ifstream file(path.c_str(), std::ifstream::binary);

    file.seekg (0, file.end);
    int size = file.tellg();
    file.seekg (0, file.beg);

    // And allocate buffer to that a single line can be read...
    char* buffer = new char[size + 1];
    file.read(buffer, size);

    // Terminate the std::string
    buffer[size] = 0;
    std::string result = buffer;
    file.close();
    delete [] buffer;
    return result;
}

// Duplicate a Faust effect 'num' times
TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::DuplicateEffect(TAudioEffectInterface* effect, int num)
{
    TCodeFaustAudioEffect* faust_effect = dynamic_cast<TCodeFaustAudioEffect*>(effect);
    if(!faust_effect)
        return nullptr;
    std::stringstream faust_code_stream;
    faust_code_stream << "declare name \"" << effect->GetName() << "\";" << "process = par(i,n," << effect->GetCode() << ") " << "with { n = " << num << "; };";
    return faust_effect->CreateEffect(faust_code_stream.str().c_str(), effect->GetLibraryPath(), effect->GetDrawPath());
}

// Split a stream 'num' times to connect to a Faust effect
TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::SplitEffect(TAudioEffectInterface* effect, int num)
{
    TCodeFaustAudioEffect* faust_effect = dynamic_cast<TCodeFaustAudioEffect*>(effect);
    if(!faust_effect)
        return nullptr;
    std::stringstream faust_code_stream;
    faust_code_stream << "declare name \"" << effect->GetName() << "\";" << "process = par(i," << num << ",_)<:" << effect->GetCode() << ";";
    return faust_effect->CreateEffect(faust_code_stream.str().c_str(), effect->GetLibraryPath(), effect->GetDrawPath());
}

TCodeFaustAudioEffect* TLocalCodeFaustAudioEffectFactory::CreateEffect(const std::string& code, const std::string& library_path, const std::string& draw_path)
{
    TLocalCodeFaustAudioEffectFactory* factory = 0;
    if (TAudioGlobals::fLocalFactoryTable.find(code) != TAudioGlobals::fLocalFactoryTable.end()) {
        printf("DSP factory already created...\n");
        factory = TAudioGlobals::fLocalFactoryTable[code];
    } else if (CheckEnding(code, ".dsp")) {  // Here we assume only 'file' or 'string' are used (not IR stuff...)
        factory = new TFileCodeFaustAudioEffectFactory(code, library_path, draw_path);
    } else {
        factory = new TStringCodeFaustAudioEffectFactory(code, library_path, draw_path);
    }
    return new TLocalCodeFaustAudioEffect(factory);
}

#if REMOTE_DSP
TCodeFaustAudioEffect* TRemoteCodeFaustAudioEffectFactory::CreateEffect(const std::string& code, const std::string& library_path, const std::string& draw_path)
{
    TRemoteCodeFaustAudioEffectFactory* factory = 0;
    if (TAudioGlobals::fRemoteFactoryTable.find(code) != TAudioGlobals::fRemoteFactoryTable.end()) {
        printf("DSP factory already created...\n");
        factory = TAudioGlobals::fRemoteFactoryTable[code];
    } else if (CheckEnding(code, ".dsp")) {
        factory = new TRemoteCodeFaustAudioEffectFactory(PathToContent(code), library_path, draw_path);
    } else {
        factory = new TRemoteCodeFaustAudioEffectFactory(code, library_path, draw_path);
    }
    return new TRemoteCodeFaustAudioEffect(factory);
}
#endif
