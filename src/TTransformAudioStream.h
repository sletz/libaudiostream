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

#ifndef __TTransformAudioStream__
#define __TTransformAudioStream__

#include "TFadeAudioStream.h"
#include "TAudioEffect.h"

//-----------------------------
// Class TTransformAudioStream
//-----------------------------
/*!
\brief  A TTransformAudioStream object will apply a list of sound effects on a stream.
*/

class TTransformAudioStream : public TDecoratedAudioStream
{

    private:

        TAudioEffectPtr fCurEffectList;  	// Effect list
		TAudioEffectPtr fNextEffectList;  	// Effect list
        long fFadeIn;	// FadeIn time
        long fFadeOut;	// FadeOut time
		TAudioBuffer<float>* fBuffer;

    public:

        TTransformAudioStream(TAudioStreamPtr stream, TAudioEffectPtr effect, long fadeIn, long fadeOut);
        virtual ~TTransformAudioStream()
        {
            delete fCurEffectList;
			delete fBuffer;
        }

        virtual long Write(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
        {
            return 0;
        }
        long Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);

        void Reset();
        TAudioStreamPtr CutBegin(long frames);    // Length in frames
        TAudioStreamPtr Copy();
		
		void SetEffectList(TAudioEffectPtr effectList) 
		{
			fNextEffectList = effectList;
		}
		TAudioEffectPtr GetEffectList() 
		{
			return fCurEffectList;
		}
};

typedef TTransformAudioStream * TTransformAudioStreamPtr;

#endif
