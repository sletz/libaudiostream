/*

Copyright (C) Grame 2014

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

#ifndef __TOfflineRenderer__
#define __TOfflineRenderer__

#include "TAudioRenderer.h"
//#include <pthread.h>

//-----------------------------
// Class TOfflineRenderer
//-----------------------------

class TOfflineRenderer : public TAudioRenderer
{

    private:
    
        float** fInputBuffer;
        float** fOutputBuffer;
        
        //pthread_t fThread;
    
        long OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
        
        void ProcessAux();
    
    public:

        TOfflineRenderer();
        virtual ~TOfflineRenderer();
  
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
        
        static void* Process(void* arg);
};

typedef TOfflineRenderer * TOfflineRendererPtr;

#endif



