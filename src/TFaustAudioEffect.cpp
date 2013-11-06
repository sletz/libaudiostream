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

#include "TFaustAudioEffect.h"
#include <iostream>
#include <sstream>
    
// Duplicate a Faust effect 'num' times 
TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::DuplicateEffect(TAudioEffectInterfacePtr effect, int num) 
{
    stringstream faust_code_stream;
    faust_code_stream << "process = par(i,n," << "environment { "<< effect->GetCode() << " }.process) " << "with { n = " << num << "/" << effect->Inputs() << "; };";
    return CreateEffect(faust_code_stream.str().c_str(), effect->GetLibraryPath(), effect->GetDrawPath());
}

// Split a Faust effect 'num' times 
TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::SplitEffect(TAudioEffectInterfacePtr effect, int num) 
{
    stringstream faust_code_stream;
    faust_code_stream << "process = par(i," << num << ",_)<:" << "environment { "<< effect->GetCode() << "}.process;";
    return CreateEffect(faust_code_stream.str().c_str(), effect->GetLibraryPath(), effect->GetDrawPath());
}

TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::CreateEffect(const string& name, const string& library_path, const string& draw_path)
{
    printf("CreateEffect = %s\n", name.c_str());
    TCodeFaustAudioEffectFactory* factory = new TCodeFaustAudioEffectFactory(name, library_path, draw_path);
    return new TCodeFaustAudioEffect(factory);
}
