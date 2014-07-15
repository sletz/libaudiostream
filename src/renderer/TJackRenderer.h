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

#ifndef __TJackRenderer__
#define __TJackRenderer__

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
// Class TJackRenderer
//--------------------------
/*!
\brief Use the <A HREF=http://jackit.sourceforge.net> Jack API </A> to access sound devices.
*/

class TJackRenderer : public TAudioRenderer
{

    private:

		jack_client_t* fClient;

        jack_port_t** fInput_ports;
        jack_port_t** fOutput_ports;

        float** fInputBuffer;
        float** fOutputBuffer;
		
		long fInput;
		long fOutput;
    
        
        RendererInfo fInfo;
        
        jack_nframes_t fAnchorFrameTime;    // Time stamp of the begining of rendering
        jack_time_t fAnchorUsecTime;        // Time stamp of the begining of rendering

        static int Process(jack_nframes_t nframes, void *arg);
        int ProcessAux(jack_nframes_t nframes);
        
        long OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);

    public:

        TJackRenderer();
        virtual ~TJackRenderer();

      	long Open(long inChan, long outChan, long bufferSize, long sampleRate);
        long Close();

        long Start();
        long Stop();
    
        long Pause();
        long Cont();
	
        void GetInfo(RendererInfoPtr info);
		
		static long GetDeviceCount();
		static void GetDeviceInfo(long deviceNum, DeviceInfoPtr info);
		static long GetDefaultInputDevice();
		static long GetDefaultOutputDevice();
};

typedef TJackRenderer * TJackRendererPtr;

#endif



