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

#ifndef __TNullAudioStream__
#define __TNullAudioStream__

#include "TAudioStream.h"
#include "UAudioTools.h"
#include "UTools.h"

//------------------------
// Class TNullAudioStream
//------------------------
/*!
\brief A TNullAudioStream generates "silence".
*/

class TNullAudioStream : public TAudioStream
{

    private:

        long fFramesNum;
        long fCurFrame;
        long fChannels;

    public:

        TNullAudioStream(long lengthFrame): fFramesNum(lengthFrame), fCurFrame(0), fChannels(1)
        {}
        TNullAudioStream(long channels, long lengthFrame): fFramesNum(lengthFrame), fCurFrame(0), fChannels(channels)
        {}
        virtual ~TNullAudioStream()
        {}

        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            framesNum = UTools::Min(framesNum, fFramesNum - fCurFrame);
            fCurFrame += framesNum;
            return framesNum;
        }

        TAudioStreamPtr CutBegin(long frames)
        {
            return new TNullAudioStream(fChannels, UTools::Max(0, fFramesNum - frames));
        }
        
        long Length()
        {
            return fFramesNum;
        }
        
        long Channels()
        {
            return fChannels;
        }
        
        void Reset()
        {
            fCurFrame = 0;
        }
        
        TAudioStreamPtr Copy()
        {
            return new TNullAudioStream(fChannels, fFramesNum);
        }
};

typedef TNullAudioStream * TNullAudioStreamPtr;

//----------------------------
// Class TConstantAudioStream
//----------------------------
/*!
\brief A TConstantAudioStream generates a constant value.
*/

class TConstantAudioStream : public TAudioStream
{

    private:

        long fFramesNum;
        long fCurFrame;
        long fChannels;
        float fValue;

    public:

        TConstantAudioStream(long lengthFrame, float value): fFramesNum(lengthFrame), fCurFrame(0), fChannels(1), fValue(value)
        {}
        TConstantAudioStream(long channels, long lengthFrame, float value): fFramesNum(lengthFrame), fCurFrame(0), fChannels(channels), fValue(value)
        {}
        virtual ~TConstantAudioStream()
        {}

        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            framesNum = UTools::Min(framesNum, fFramesNum - fCurFrame);
            fCurFrame += framesNum;
            
            float* temp[buffer->GetChannels()];
            for (int i = 0; i < framesNum; i++) {
                UAudioTools::AddFrame(buffer->GetFrame(i + framePos, temp), fValue, buffer->GetChannels());
            }
            
            return framesNum;
        }

        TAudioStreamPtr CutBegin(long frames)
        {
            return new TConstantAudioStream(fChannels, UTools::Max(0, fFramesNum - frames), fValue);
        }
        
        long Length()
        {
            return fFramesNum;
        }
        
        long Channels()
        {
            return fChannels;
        }
        
        void Reset()
        {
            fCurFrame = 0;
        }
        
        TAudioStreamPtr Copy()
        {
            return new TConstantAudioStream(fChannels, fFramesNum, fValue);
        }
};

typedef TConstantAudioStream * TConstantAudioStreamPtr;

#endif
