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

#ifndef __TPortAudioV19Renderer__
#define __TPortAudioV19Renderer__

#include "TAudioRenderer.h"
#include "portaudioV19.h"

//-----------------------------
// Class TPortAudioV19Renderer
//-----------------------------
/*!
\brief Use the <A HREF=http://www.portaudio.com> PortAudio V19 API </A> to access sound devices.
*/

class TPortAudioV19Renderer : public TAudioRenderer
{

    private:

        PaStream* fStream;
        
        PaTime fAnchorFrameTime;

        static int Process(const void* inputBuffer,
							void* outputBuffer,
							unsigned long framesPerBuffer,
							const PaStreamCallbackTimeInfo* timeInfo,
							PaStreamCallbackFlags statusFlags,
							void* userData);

        void DisplayDevices();
        int GetFirstValidInputDevice();
        int GetFirstValidOutputDevice();
        
        long OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
     

    public:

        TPortAudioV19Renderer();        
		virtual ~TPortAudioV19Renderer();

        long Open(long inChan, long outChan, long bufferSize, long sampleRate);
        long Close();

        long Start();
        long Stop();
    
        long Pause();
        long Cont();

        void GetInfo(RendererInfoPtr info);
		
		long GetDeviceCount();
		void GetDeviceInfo(long deviceNum, DeviceInfoPtr info);
		long GetDefaultInputDevice();
		long GetDefaultOutputDevice();
};

typedef TPortAudioV19Renderer * TPortAudioV19RendererPtr;

#endif



