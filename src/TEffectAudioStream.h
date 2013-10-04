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

#ifndef __TEffectAudioStream__
#define __TEffectAudioStream__

#include "TFadeAudioStream.h"
#include "TAudioEffect.h"

//-----------------------------
// Class TEffectAudioStream
//-----------------------------
/*!
\brief  A TEffectAudioStream object will apply a list of sound effects on a stream.
*/

class TEffectAudioStream : public TDecoratedAudioStream
{

    private:

        TAudioEffectInterfacePtr fEffect;  	// Effect
		long fFadeIn;	// FadeIn time
        long fFadeOut;	// FadeOut time
		FLOAT_BUFFER fBuffer;

    public:

        TEffectAudioStream(TAudioStreamPtr stream, TAudioEffectInterfacePtr effect, long fadeIn, long fadeOut);
        virtual ~TEffectAudioStream()
        {
    		delete fBuffer;
        }

        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return 0;
        }
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);

        void Reset();
        TAudioStreamPtr CutBegin(long frames);    // Length in frames
        TAudioStreamPtr Copy();
};

typedef TEffectAudioStream * TEffectAudioStreamPtr;

#endif
