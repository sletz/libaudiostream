/*

Copyright (C) Grame 2002-2012

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

#include "TBufferedAudioStream.h"
#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include "UTools.h"
#include  <string.h>

TBufferedAudioStream::TBufferedAudioStream(): TAudioStream()
{
    Init(0);
}

void TBufferedAudioStream::Init(SHORT_BUFFER buffer)
{
    fBuffer = buffer;
    fFramesNum = fCurFrame = fChannels = fTotalFrames = 0;
    fReady = false;
}

void TBufferedAudioStream::ReadBuffer(SHORT_BUFFER buffer, long framesNum, long framePos)
{
    Read(buffer, framesNum, framePos);
    fReady = true;
}

void TBufferedAudioStream::WriteBuffer(SHORT_BUFFER buffer, long framesNum, long framePos)
{
    Write(buffer, framesNum, framePos);
    fReady = true;
}

static bool EndFirst (int curframe, int framesNum, int buffersize)
{
    return ((curframe / buffersize) == 0) && (((curframe + framesNum) / buffersize) == 1);
}

static bool EndSecond (int curframe, int framesNum, int buffersize)
{
    return ((curframe / buffersize) == 1) && (((curframe + framesNum) / buffersize) == 2);
}

long TBufferedAudioStream::HandleBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels, bool read)
{
    assert(fBuffer);

    // Check length
    framesNum = UTools::Min(framesNum, fFramesNum - (fTotalFrames + fCurFrame));

    if (EndFirst(fCurFrame, framesNum, fBuffer->GetSize() / 2)) { // End of first buffer

        if (!fReady) 
            TAudioGlobals::fDiskError++;
	
        assert((fCurFrame + framesNum) <= fBuffer->GetSize());

        if (read) {
            // Read the frames from the disk buffer to the argument buffer
            UAudioTools::Short2FloatMix(fBuffer->GetFrame(fCurFrame), buffer->GetFrame(framePos), framesNum, fChannels, channels);
        } else {
            // Write the argument buffer frames to the disk buffer
            UAudioTools::Float2Short(buffer->GetFrame(framePos), fBuffer->GetFrame(fCurFrame), framesNum, channels, fChannels);
        }

        fCurFrame += framesNum;

        if (read) {
            ReadBuffer(fBuffer, fBuffer->GetSize() / 2, 0);
        } else {
            WriteBuffer(fBuffer, fBuffer->GetSize() / 2, 0);
        }

    } else if (EndSecond(fCurFrame, framesNum, fBuffer->GetSize() / 2)) { // End of second buffer

		if (!fReady) 
            TAudioGlobals::fDiskError++;

        long frames1 = fBuffer->GetSize() - fCurFrame; // Number of frames to be read at the end of the buffer
        long frames2 = framesNum - frames1;            // Number of frames to be read at the beginning of the "next" buffer

        assert((fCurFrame + frames1) <= fBuffer->GetSize());

        if (read) {
            UAudioTools::Short2FloatMix(fBuffer->GetFrame(fCurFrame), buffer->GetFrame(framePos), frames1, fChannels, channels);
            UAudioTools::Short2FloatMix(fBuffer->GetFrame(0), buffer->GetFrame(frames1 + framePos), frames2, fChannels, channels);
        } else {
            UAudioTools::Float2Short(buffer->GetFrame(framePos), fBuffer->GetFrame(fCurFrame), frames1, channels, fChannels);
            UAudioTools::Float2Short(buffer->GetFrame(frames1 + framePos), fBuffer->GetFrame(0), frames2, channels, fChannels);
        }

        fCurFrame = frames2;
        fTotalFrames += fBuffer->GetSize(); // A new file buffer has be read

        if (read) {
            ReadBuffer(fBuffer, fBuffer->GetSize() / 2, fBuffer->GetSize() / 2);
        } else {
            WriteBuffer(fBuffer, fBuffer->GetSize() / 2, fBuffer->GetSize() / 2);
        }

    } else { // General case

        assert((fCurFrame + framesNum) <= fBuffer->GetSize());

        if (read) {
            UAudioTools::Short2FloatMix(fBuffer->GetFrame(fCurFrame), buffer->GetFrame(framePos), framesNum, fChannels, channels);
        } else {
            UAudioTools::Float2Short(buffer->GetFrame(framePos), fBuffer->GetFrame(fCurFrame), framesNum, channels, fChannels);
        }

        fCurFrame += framesNum;
    }

    return framesNum;
}

long TBufferedAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
    return HandleBuffer(buffer, framesNum, framePos, channels, true);
}

long TBufferedAudioStream::Write(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
{
    return HandleBuffer(buffer, framesNum, framePos, channels, false);
}

void TBufferedAudioStream::Reset()
{
    fCurFrame = 0;
    fTotalFrames = 0;
}



