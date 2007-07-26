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

#ifndef __TFadeAudioStream__
#define __TFadeAudioStream__

#include "TAudioStream.h"
#include "Envelope.h"

//------------------------
// Class TFadeAudioStream
//------------------------
/*!
\brief  A TFadeAudioStream applies FadeIn and FadeOut on a decorated stream.
*/

class TFadeAudioStream : public TDecoratedAudioStream
{

    protected:

        Envelope fFadeIn;    	// FadeIn object
        Envelope fFadeOut;   	// FadeOut object
        long fFadeInFrames;		// Number of frames for FadeIn
        long fFadeOutFrames;	// Number of frames for FadeOut
        long fStatus;			// Channel state
        long fCurFrame;			// Current frame
        long fFramesNum;		// Frames number
        TAudioBuffer<float>* fMixBuffer;     // Used for mixing

        long ReadAux(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);
        long FadeIn(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);
        long FadeOut(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);

        void Init(float v1, float f1, float v2, float f2);

    public:

        enum {kIdle = 0, kFadeIn, kPlaying, kFadeOut};

        TFadeAudioStream();
        TFadeAudioStream(TAudioStreamPtr stream, long fadeIn, long fadeOut);
        virtual ~TFadeAudioStream()
        {
            delete fMixBuffer;
        }

        long Write(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
        {
            return 0;
        }
        long Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);

        void Reset();
        TAudioStreamPtr CutBegin(long frames);
        TAudioStreamPtr Copy()
        {
            return new TFadeAudioStream(fStream->Copy(), fFadeInFrames, fFadeOutFrames);
        }
};

typedef TFadeAudioStream * TFadeAudioStreamPtr;

//-------------------------------
// Class TChannelFadeAudioStream
//-------------------------------
/*!
\brief A special Fade stream used inside a channel.
*/

class TChannelFadeAudioStream : public TFadeAudioStream
{

    public:

        TChannelFadeAudioStream(): TFadeAudioStream()
        {}
        virtual ~TChannelFadeAudioStream()
        {}

        // Additional interface
        void FadeIn();
        void FadeOut();
        void SetStream(TAudioStreamPtr stream, long fadein, long fadeout);
        void ClearStream()
        {
            fStream = 0;
        }
        long GetStatus()
        {
            return fStatus;
        }
        long GetFrame()
        {
            return fCurFrame;
        }

        bool IsPlaying()
        {
            return (fStatus == kPlaying);
        }
        bool IsIdle()
        {
            return (fStatus == kIdle);
        }
};

typedef TChannelFadeAudioStream * TChannelFadeAudioStreamPtr;

#endif
