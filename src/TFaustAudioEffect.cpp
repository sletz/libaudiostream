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

#include "TFaustAudioEffect.h"
#include <iostream>
#include <sstream>
    
// Duplicate a Faust effect 'num' times 
TAudioEffectInterfacePtr TCodeFaustAudioEffectFactory::Duplicate(TAudioEffectInterfacePtr effect, int num) 
{
    stringstream faust_code_stream;
    faust_code_stream << "process = par(i,n," << "environment { "<< effect->GetCode() << " }.process) " << "with { n = " << num << "/" << effect->Inputs() << "; };";
    string faust_code = faust_code_stream.str();
    printf("new_faust_code.str() %s\n", faust_code.c_str());
    return CreateEffect(faust_code.c_str(), "", "");
}

// Split a Faust effect 'num' times 
TAudioEffectInterfacePtr TCodeFaustAudioEffectFactory::Split(TAudioEffectInterfacePtr effect, int num) 
{
    stringstream faust_code_stream;
    faust_code_stream << "process = par(i," << num << ",_)<:" << "environment { "<< effect->GetCode() << "}.process " << ";";
    string faust_code = faust_code_stream.str();
    printf("new_faust_code.str() %s\n", faust_code.c_str());
    return CreateEffect(faust_code.c_str(), "", "");
}

TCodeFaustAudioEffect* TCodeFaustAudioEffectFactory::CreateEffect(const char* name, const char* library_path, const char* draw_path)
{
    TCodeFaustAudioEffectFactory factory(name, library_path, draw_path);
    return new TCodeFaustAudioEffect(factory.GetFactory(name));
}