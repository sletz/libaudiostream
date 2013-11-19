/*

Copyright (C) Grame 2002-2013

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

#include "TAudioStreamFactory.h"
#include <map>

#ifdef WIN32
	#define	AUDIOAPI __declspec(dllexport)
#else
	#define	AUDIOAPI __attribute__ ((visibility("default")))
#endif

struct FaustFileReader {

    TAudioStreamPtr fStream;
    int fCurIndex;
    FLOAT_BUFFER fBuffer;
    
    FaustFileReader(const char* name, int channel = 0)
    {
        fStream = TAudioStreamFactory::MakeRTRenderer(TAudioStreamFactory::MakeReadSound(name));
        fBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, fStream->Channels());
        fCurIndex = 0;
        // Read first buffer
        float* temp[fBuffer->GetChannels()];
        UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0, temp), TAudioGlobals::fBufferSize, fStream->Channels());
        fStream->Read(fBuffer, TAudioGlobals::fBufferSize, fStream->Channels());
    }
    
    virtual ~FaustFileReader()
    {
        delete fBuffer;
        delete fStream;
    }
    
    int Size()
    {
        return fStream->Length();
    }
    
    // We assume indexes are increasing...
    float Sample(int channel, int index)
    {   
        float* temp[fBuffer->GetChannels()];
        
        fCurIndex = index % TAudioGlobals::fBufferSize;
        if (channel == 0 && index > 0 && fCurIndex == 0) {
            // Read next buffer
            UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0, temp), TAudioGlobals::fBufferSize, fStream->Channels());
            fStream->Read(fBuffer, TAudioGlobals::fBufferSize, 0);
        }
        return fBuffer->GetFrame(0, temp)[channel][fCurIndex];
    }
};

// External API

std::map<const char*, FaustFileReader*> fFileTable;

#ifdef __cplusplus
extern "C"
{
#endif

AUDIOAPI int FaustFileOpen(const char* name);
AUDIOAPI int FaustFileSize(const char* name);
AUDIOAPI float FaustFileSample(const char* name, int channel, int index);

#ifdef __cplusplus
}
#endif

AUDIOAPI int FaustFileOpen(const char* name)
{
    //printf("FaustFileOpen %s\n", name);
    if (fFileTable.find(name) == fFileTable.end()) {
        try {
            TAudioGlobals::ClearLibError();
            TAudioGlobals::Init(2, 2, 2, 44100, 512, 65536 * 4, 44100 * 60 * 10, 1);
            fFileTable[name] = new FaustFileReader(name);
            return true;
        } catch (...) {
            return false;
        }
    } else {
        return true;
    }
}

AUDIOAPI int FaustFileSize(const char* name)
{
    FaustFileReader* reader = fFileTable[name];
    assert(reader);
	return reader->Size();
}

AUDIOAPI float FaustFileSample(const char* name, int channel, int index)
{
    FaustFileReader* reader = fFileTable[name];
    assert(reader);
    return reader->Sample(channel, index);
}


