/*

Copyright © Grame 2002

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
grame@rd.grame.fr

*/

#include "TWriteFileAudioStream.h"
#include "TAudioGlobals.h"

#include "UAudioTools.h"
#include "UTools.h"
#include <stdio.h>
#include <string.h>

TWriteFileAudioStream::TWriteFileAudioStream(string name, TAudioBuffer<short>* buffer, TAudioStreamPtr stream, long format)
        : TFileAudioStream(name)
{
    SF_INFO info;

    fChannels = stream->Channels();
    fBuffer = buffer;
    fStream = stream;
    fFormat = format;

    fFramesNum = fStream->Length();

    info.samplerate = TAudioGlobals::fSample_Rate;
    info.channels = fChannels;
    info.format = fFormat;

    fFile = sf_open(fName.c_str(), SFM_WRITE, &info);

    // Check file
    if (!fFile)
        throw - 1;
    fReady = true;
}

TWriteFileAudioStream::TWriteFileAudioStream(string name, TAudioStreamPtr stream, long format)
        : TFileAudioStream(name)
{
    SF_INFO info;

    fChannels = stream->Channels();
    fBuffer = new TLocalAudioBuffer<short>(TAudioGlobals::fStream_Buffer_Size, fChannels);
    fStream = stream;
    fFormat = format;

    fFramesNum = fStream->Length();

    info.samplerate = TAudioGlobals::fSample_Rate;
    info.channels = fChannels;
    info.format = fFormat;
	
    fFile = sf_open(fName.c_str(), SFM_WRITE, &info);

    // Check file
    if (!fFile)
        throw - 1;
    fReady = true;
}

TWriteFileAudioStream::~TWriteFileAudioStream()
{
    if (fFile) {
		sf_close(fFile);
        fFile = 0;
    }

    delete fBuffer;  // faux a revoir (si buffer partagé)
}

long TWriteFileAudioStream::Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
{
    long res = fStream->Read(buffer, framesNum, framePos, channels);
    TBufferedAudioStream::Write(buffer, framesNum, framePos, channels); // Write on disk
    return res;
}

void TWriteFileAudioStream::Reset()
{
    // A AMELIORER (réutiliser les fichiers disque??)
    sf_seek (fFile, 0, SEEK_SET);
    fStream->Reset();
    TBufferedAudioStream::Reset();
}

// Called by TCmdManager
long TWriteFileAudioStream::Write(TAudioBuffer<short>* buffer, long framesNum, long framePos)
{
    assert(fBuffer);
    assert(fFile);
    return long(sf_writef_short(fFile, buffer->GetFrame(framePos), framesNum));  // In frames
}

void TWriteFileAudioStream::Stop()
{
    // Flush the current buffer
    if (fCurFrame < fBuffer->GetSize() / 2) {
		WriteBuffer(fBuffer, fCurFrame, 0);
    } else {
		WriteBuffer(fBuffer, fCurFrame - fBuffer->GetSize() / 2, fBuffer->GetSize() / 2);
    }

    // Start a new buffer
    fCurFrame = 0;
}




