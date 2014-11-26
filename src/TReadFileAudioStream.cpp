/*
Copyright (C) Grame 2002-2014

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

#include "TReadFileAudioStream.h"
#include "TLASException.h"
#include "TCmdManager.h"
#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include "UTools.h"
#include "StringTools.h"
#include <stdio.h>
#include <assert.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

TReadFileAudioStream::TReadFileAudioStream(string name, long beginFrame): TFileAudioStream(name)
{
    memset(&fInfo, 0, sizeof(fInfo));
	char utf8name[512] = {0};
	
	assert(fName.size() < 512);
	Convert2UTF8(fName.c_str(), utf8name, 512);
	fFile = sf_open(utf8name, SFM_READ, &fInfo);
	
    // Check file
    if (!fFile) {
        char error[512];
        snprintf(error, 512, "Cannot open filename \'%s\'\n", utf8name);
        throw TLASException(error);
    }
    
    fFramesNum = long(fInfo.frames);
    fChannels = long(fInfo.channels);
    
    if (SetPos(beginFrame) != NO_ERR) {
        throw TLASException("TReadFileAudioStream::SetPos : seek error");
    }
    
    if (fInfo.samplerate != TAudioGlobals::fSampleRate) {
        printf("Warning : file sample rate different from engine sample rate! lib sr = %ld file sr = %d\n", TAudioGlobals::fSampleRate, fInfo.samplerate);
    }
    
    // Dynamic allocation
    fMemoryBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, fChannels);
    fCopyBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, fChannels);
    fFileBuffer = new float[fChannels * TAudioGlobals::fStreamBufferSize];
  
    // Read first buffer directly
    TBufferedAudioStream::ReadBuffer(fMemoryBuffer, TAudioGlobals::fStreamBufferSize, 0);
    TNonInterleavedAudioBuffer<float>::Copy(fCopyBuffer, 0, fMemoryBuffer, 0, TAudioGlobals::fStreamBufferSize);

    fReady = true;
}

long TReadFileAudioStream::SetPos(long frames)
{
    if (sf_seek(fFile, frames, SEEK_SET) < 0) {
        const char* error = sf_strerror(fFile);
        sf_close(fFile);
        return SET_POS_ERR;
    }

    fBeginFrame = frames;
    return NO_ERR;
}

TReadFileAudioStream::~TReadFileAudioStream()
{
	if (fFile) {
        sf_close(fFile);
        fFile = 0;
    }

    delete fMemoryBuffer;
    delete fCopyBuffer;
    delete [] fFileBuffer;
}

TAudioStreamPtr TReadFileAudioStream::CutBegin(long frames)
{
    return new TReadFileAudioStream(fName, fBeginFrame + frames);
}

void TReadFileAudioStream::ReadEndBufferAux(TReadFileAudioStreamPtr obj, long framesNum, long framePos)
{
    obj->ReadEndBuffer(framesNum, framePos);
    obj->removeReference();
}

// Use the end of the copy buffer
void TReadFileAudioStream::ReadEndBuffer(long framesNum, long framePos)
{
    TNonInterleavedAudioBuffer<float>::Copy(fMemoryBuffer, framePos, fCopyBuffer, framePos, framesNum);
}

void TReadFileAudioStream::Reset()
{
    if (sf_seek(fFile, fBeginFrame + TAudioGlobals::fStreamBufferSize, SEEK_SET) < 0) {
        printf("TReadFileAudioStream::Reset : sf_seek error = %s\n", sf_strerror(fFile));
    }

    // Use only the beginning of the copy buffer, copy the end in the low-priority thread
    int copySize = TAudioGlobals::fBufferSize * 4;
 
    if (copySize < TAudioGlobals::fStreamBufferSize) {
        TNonInterleavedAudioBuffer<float>::Copy(fMemoryBuffer, 0, fCopyBuffer, 0, copySize);
        if (fManager == 0) {
            printf("Error : stream rendered without command manager\n");
        }
        assert(fManager);
        fManager->ExecCmd((CmdPtr)ReadEndBufferAux, (long)addReference(), TAudioGlobals::fStreamBufferSize - copySize, copySize, 0, 0);
    } else {
        TNonInterleavedAudioBuffer<float>::Copy(fMemoryBuffer, 0, fCopyBuffer, 0, TAudioGlobals::fStreamBufferSize);
    }

    TBufferedAudioStream::Reset();
}

// Called by TCmdManager
long TReadFileAudioStream::ReadImp(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert(fFile);
    float** temp = (float**)alloca(buffer->GetChannels()*sizeof(float*));
    int res = sf_readf_float(fFile, fFileBuffer, framesNum); // In frames
    UAudioTools::Deinterleave(buffer->GetFrame(framePos, temp), fFileBuffer, framesNum, fChannels);
    
	return res;
}


