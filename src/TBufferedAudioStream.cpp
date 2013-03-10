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

#include "TBufferedAudioStream.h"
#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include "UTools.h"
#include <string.h>

TBufferedAudioStream::TBufferedAudioStream(): TAudioStream()
{
    fMemoryBuffer = NULL;
    fFramesNum = 0;
    fCurFrame = 0;
    fChannels = 0;
    fTotalFrames = 0;
    fReady = false;
}

void TBufferedAudioStream::ReadBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    Read(buffer, framesNum, framePos);
    fReady = true;
}

void TBufferedAudioStream::WriteBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos)
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
    assert(fMemoryBuffer);
  
    // Check length
    framesNum = UTools::Min(framesNum, fFramesNum - (fTotalFrames + fCurFrame));
   
    if (EndFirst(fCurFrame, framesNum, fMemoryBuffer->GetSize() / 2)) { // End of first buffer

        if (!fReady) {
            TAudioGlobals::fDiskError++;
        }
	
        assert((fCurFrame + framesNum) <= fMemoryBuffer->GetSize());

        if (read) {
            // Read the frames from the memory buffer and mix to the argument buffer
            UAudioTools::Float2FloatMix(fMemoryBuffer->GetFrame(fCurFrame), buffer->GetFrame(framePos), framesNum, fChannels, channels);
        } else {
            // Write the argument buffer frames to the memory buffer
            UAudioTools::Float2Float(buffer->GetFrame(framePos), fMemoryBuffer->GetFrame(fCurFrame), framesNum, channels, fChannels);
        }

        fCurFrame += framesNum;

        if (read) {
            ReadBuffer(fMemoryBuffer, fMemoryBuffer->GetSize() / 2, 0);
        } else {
            WriteBuffer(fMemoryBuffer, fMemoryBuffer->GetSize() / 2, 0);
        }

    } else if (EndSecond(fCurFrame, framesNum, fMemoryBuffer->GetSize() / 2)) { // End of second buffer

		if (!fReady) {
            TAudioGlobals::fDiskError++;
        }

        long frames1 = fMemoryBuffer->GetSize() - fCurFrame;    // Number of frames to be read or written at the end of the buffer
        long frames2 = framesNum - frames1;                     // Number of frames to be read or written at the beginning of the "next" buffer

        assert((fCurFrame + frames1) <= fMemoryBuffer->GetSize());

        if (read) {
            // Read the frames from the memory buffer and mix to the argument buffer
            UAudioTools::Float2FloatMix(fMemoryBuffer->GetFrame(fCurFrame), buffer->GetFrame(framePos), frames1, fChannels, channels);
            UAudioTools::Float2FloatMix(fMemoryBuffer->GetFrame(0), buffer->GetFrame(frames1 + framePos), frames2, fChannels, channels);
        } else {
            // Write the argument buffer frames to the memory buffer
            UAudioTools::Float2Float(buffer->GetFrame(framePos), fMemoryBuffer->GetFrame(fCurFrame), frames1, channels, fChannels);
            UAudioTools::Float2Float(buffer->GetFrame(frames1 + framePos), fMemoryBuffer->GetFrame(0), frames2, channels, fChannels);
        }

        fCurFrame = frames2;
        fTotalFrames += fMemoryBuffer->GetSize(); // A new file buffer has be read or written

        if (read) {
            ReadBuffer(fMemoryBuffer, fMemoryBuffer->GetSize() / 2, fMemoryBuffer->GetSize() / 2);
        } else {
            WriteBuffer(fMemoryBuffer, fMemoryBuffer->GetSize() / 2, fMemoryBuffer->GetSize() / 2);
        }

    } else { // General case

        assert((fCurFrame + framesNum) <= fMemoryBuffer->GetSize());

        if (read) {
            // Read the frames from the memory buffer and mix to the argument buffer
            UAudioTools::Float2FloatMix(fMemoryBuffer->GetFrame(fCurFrame), buffer->GetFrame(framePos), framesNum, fChannels, channels);
        } else {
            // Write the argument buffer frames to the memory buffer
            UAudioTools::Float2Float(buffer->GetFrame(framePos), fMemoryBuffer->GetFrame(fCurFrame), framesNum, channels, fChannels);
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



