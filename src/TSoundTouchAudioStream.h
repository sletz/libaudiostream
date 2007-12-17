/*
Copyright � Grame 2002-2007

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

#ifndef __TSoundTouchAudioStream__
#define __TSoundTouchAudioStream__

#include "TAudioStream.h"
#include "soundtouch/SoundTouch.h"

//------------------------------
// Class TSoundTouchAudioStream
//------------------------------
/*!
\brief  A TSoundTouchAudioStream object using SoundTouch library.
*/

class TSoundTouchAudioStream : public TDecoratedAudioStream
{

    private:

		double* fPitchShift;
		double* fTimeStretch;
		double fPitchShiftVal;
		double fTimeStretchVal;
		soundtouch::SoundTouch* fSoundTouch;
		TAudioBuffer<float>* fBuffer;
	
    public:

        TSoundTouchAudioStream(TAudioStreamPtr stream, double* pitch_shift, double* time_strech);
        virtual ~TSoundTouchAudioStream();
   
        virtual long Write(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels)
        {
            return 0;
        }
        long Read(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);

        void Reset();
        TAudioStreamPtr CutBegin(long frames);    // Length in frames
        TAudioStreamPtr Copy();
};

typedef TSoundTouchAudioStream * TSoundTouchAudioStreamPtr;

#endif
