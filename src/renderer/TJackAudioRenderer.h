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

#ifndef __TJackAudioRenderer__
#define __TJackAudioRenderer__

#include "TAudioRenderer.h"

#ifdef WIN32
	#include "jack.h"
	#include "types.h"
#else
	#include <jack/jack.h>
	#include <jack/types.h>
#endif

#define MAX_PORTS 32

//--------------------------
// Class TJackAudioRenderer
//--------------------------
/*!
\brief Use the <A HREF=http://jackit.sourceforge.net> Jack API </A> to access sound devices.
*/

class TJackAudioRenderer : public TAudioRenderer
{

    private:

		jack_client_t* fClient;

        jack_port_t** fInput_ports;
        jack_port_t** fOutput_ports;

        float* fInputBuffer;
        float* fOutputBuffer;
		
		long fInput;
		long fOutput;
        
        jack_nframes_t fAnchorFrameTime;    // Time stamp of the begining of rendering
        jack_time_t fAnchorUsecTime;        // Time stamp of the begining of rendering


        static int Process(jack_nframes_t nframes, void *arg);

    public:

        TJackAudioRenderer();
        virtual ~TJackAudioRenderer();

      	long OpenDefault(long inChan, long outChan, long bufferSize, long sampleRate);
		long Open(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
        long Close();

        long Start();
        long Stop();
	
        void GetInfo(RendererInfoPtr info);
		
		long GetDeviceCount();
		void GetDeviceInfo(long deviceNum, DeviceInfoPtr info);
		long GetDefaultInputDevice();
		long GetDefaultOutputDevice();
};

typedef TJackAudioRenderer * TJackAudioRendererPtr;

#endif



