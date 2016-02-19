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

#include "TWriteFileAudioStream.h"
#include "TLASException.h"
#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include "UTools.h"
#include "StringTools.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

TWriteFileAudioStream::TWriteFileAudioStream(string name, TAudioStreamPtr stream, long format)
        : TFileAudioStream(name)
{
    fChannels = stream->Channels();
    fMemoryBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fStreamBufferSize, fChannels);
    fFileBuffer = new float[fChannels * TAudioGlobals::fStreamBufferSize];
    fStream = stream;
    fFormat = format;
    fFramesNum = fStream->Length();
	fFile = 0;
    Open();
}

TWriteFileAudioStream::~TWriteFileAudioStream()
{
	Flush();
    Close();	
    delete fMemoryBuffer;  // faux a revoir (si buffer partag�)
    delete [] fFileBuffer;
}

void TWriteFileAudioStream::Open()
{
	if (fFile == 0) {
		SF_INFO info;
		info.samplerate = TAudioGlobals::fSampleRate;
		info.channels = fChannels;
		info.format = fFormat;
		char utf8name[512] = {0};
	
		assert(fName.size() < 512);
		Convert2UTF8(fName.c_str(), utf8name, 512);
		fFile = sf_open(utf8name, SFM_WRITE, &info);
	
		// Check file
		if (!fFile) {
            char error[512];
            snprintf(error, 512, "Cannot open filename \'%s\'\n", utf8name);
            throw TLASException(error);
        }
			
		sf_seek(fFile, 0, SEEK_SET);
		fReady = true;
    }
}

void TWriteFileAudioStream::Close()
{
  	if (fFile) {
		sf_close(fFile);
        printf("TWriteFileAudioStream::Close OK\n");
        fFile = 0;
    }
}

long TWriteFileAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert_stream(framesNum, framePos);
    
    long res = fStream->Read(buffer, framesNum, framePos);
    TBufferedAudioStream::Write(buffer, framesNum, framePos); // Write on disk
	if (res < framesNum) {
        if (fManager == 0) {
            printf("Error : stream rendered without command manager\n");
        }
        assert(fManager);
        fManager->ExecCmd((CmdPtr)CloseAux, (long)addReference(), 0, 0, 0, 0);
	}
    return res;
}

void TWriteFileAudioStream::Reset()
{
	Open();
    fStream->Reset();
    TBufferedAudioStream::Reset();
}

// Called by TCmdManager
long TWriteFileAudioStream::WriteImp(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    assert(fFile);
    float** temp = (float**)alloca(buffer->GetChannels()*sizeof(float*));
    UAudioTools::Interleave(fFileBuffer, buffer->GetFrame(framePos, temp), framesNum, fChannels);

    return long(sf_writef_float(fFile, fFileBuffer, framesNum));  // In frames
}

void TWriteFileAudioStream::Flush()
{
	if (fFile) {
		// Flush the current buffer
		if (fCurFrame < fMemoryBuffer->GetSize() / 2) {
			TBufferedAudioStream::WriteBuffer(fMemoryBuffer, fCurFrame, 0);  // direct write 
		} else {
			TBufferedAudioStream::WriteBuffer(fMemoryBuffer, fCurFrame - fMemoryBuffer->GetSize() / 2, fMemoryBuffer->GetSize() / 2);  // direct write 
		}

		// Start a new buffer
		fCurFrame = 0;
	}
}

// Callback called by command manager
void TWriteFileAudioStream::CloseAux(TWriteFileAudioStreamPtr obj, long u1, long u2, long u3)
{
    obj->Flush();
	obj->Close();
    obj->removeReference();
}

