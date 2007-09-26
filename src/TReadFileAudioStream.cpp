/*

Copyright © Grame 2002-2007

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
#include "TCmdManager.h"
#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include "UTools.h"
#include "StringTools.h"
#include <stdio.h>
#include <assert.h>

TReadFileAudioStream::TReadFileAudioStream(string name, long beginFrame): TFileAudioStream(name)
{
    SF_INFO info;
	memset(&info, 0, sizeof(info));
	char utf8name[512] = {0};
	
	assert(fName.size() < 512);
	Convert2UTF8(fName.c_str(), utf8name, 512);
	fFile = sf_open(utf8name, SFM_READ, &info);
	
    // Check file
    if (!fFile)
        throw - 1;

    if (sf_seek(fFile, beginFrame, SEEK_SET) < 0) {
        sf_close(fFile);
        throw - 2;
    }

    fFramesNum = long(info.frames);
    fChannels = long(info.channels);
    fBeginFrame = beginFrame;

    if (info.samplerate != TAudioGlobals::fSample_Rate)
        printf("Warning : file sample rate different from engine sample rate! %i\n", info.samplerate);

    // Dynamic allocation
    fBuffer = new TLocalAudioBuffer<short>(TAudioGlobals::fStream_Buffer_Size, fChannels);
    fCopyBuffer = new TLocalAudioBuffer<short>(TAudioGlobals::fStream_Buffer_Size, fChannels);

    // Read first buffer directly
    TBufferedAudioStream::ReadBuffer(fBuffer, TAudioGlobals::fStream_Buffer_Size, 0);
    TAudioBuffer<short>::Copy(fCopyBuffer, 0, fBuffer, 0, TAudioGlobals::fStream_Buffer_Size);

    fReady = true;
}

TReadFileAudioStream::~TReadFileAudioStream()
{
	if (fFile) {
        sf_close(fFile);
        fFile = 0;
    }

    delete fBuffer;
    delete fCopyBuffer;
}

TAudioStreamPtr TReadFileAudioStream::CutBegin(long frames)
{
    return new TReadFileAudioStream(fName, fBeginFrame + frames);
}

void TReadFileAudioStream::ReadEndBufferAux(TReadFileAudioStreamPtr obj, long framesNum, long framePos)
{
    obj->ReadEndBuffer(framesNum, framePos);
}

// Use the end of the copy buffer
void TReadFileAudioStream::ReadEndBuffer(long framesNum, long framePos)
{
    TAudioBuffer<short>::Copy(fBuffer, framePos, fCopyBuffer, framePos, framesNum);
}

void TReadFileAudioStream::Reset()
{
    sf_seek(fFile, fBeginFrame + TAudioGlobals::fStream_Buffer_Size, SEEK_SET);

    // Use only the beginning of the copy buffer, copy the end in the low-priority thread
    int copySize = TAudioGlobals::fBuffer_Size * 4;

    if (copySize < TAudioGlobals::fStream_Buffer_Size) {
        TAudioBuffer<short>::Copy(fBuffer, 0, fCopyBuffer, 0, copySize);
        if (fManager == 0)
            printf("Error : stream rendered without command manager\n");
        assert(fManager);
        fManager->ExecCmd((CmdPtr)ReadEndBufferAux, (long)this, TAudioGlobals::fStream_Buffer_Size - copySize, copySize, 0, 0);
    } else {
        TAudioBuffer<short>::Copy(fBuffer, 0, fCopyBuffer, 0, TAudioGlobals::fStream_Buffer_Size);
    }

    TBufferedAudioStream::Reset();
}

// Called by TCmdManager
long TReadFileAudioStream::Read(TAudioBuffer<short>* buffer, long framesNum, long framePos)
{
    assert(fBuffer);
    assert(fFile);
    return long(sf_readf_short(fFile, buffer->GetFrame(framePos), framesNum)); // In frames
}


