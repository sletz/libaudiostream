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
research@grame.fr

*/

#ifndef __TCoreAudioRenderer__
#define __TCoreAudioRenderer__

#include "TAudioRenderer.h"
#include <AudioToolbox/AudioConverter.h>
#include <CoreAudio/CoreAudio.h>
#include <AudioUnit/AudioUnit.h>

//--------------------------
// Class TCoreAudioRenderer
//--------------------------
/*!
\brief Use the CoreAudio API to access sound drivers.
*/

class TCoreAudioRenderer : public TAudioRenderer
{

    private:

		AudioBufferList* fInputData;
		AudioDeviceID fDeviceID;
		AudioUnit fAUHAL;
		
		OSStatus GetDefaultDevice(int inChan, int outChan, AudioDeviceID* id);

		static	OSStatus Render(void *inRefCon,
                               AudioUnitRenderActionFlags *ioActionFlags,
                               const AudioTimeStamp *inTimeStamp,
                               UInt32 inBusNumber,
                               UInt32 inNumberFrames,
                               AudioBufferList *ioData);

    public:

        TCoreAudioRenderer(): TAudioRenderer()
        {}
        virtual ~TCoreAudioRenderer()
        {}

        long Open(long* inChan, long* outChan, long* bufferSize, long* sampleRate);
        long Close();

        long Start();
        long Stop();

        void GetInfo(RendererInfoPtr info);
};

typedef TCoreAudioRenderer * TCoreAudioRendererPtr;

#endif



