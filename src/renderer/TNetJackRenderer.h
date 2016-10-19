/*

Copyright (C) Grame 2015

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

#ifndef __TNetJackRenderer__
#define __TNetJackRenderer__

#include "TAudioRenderer.h"
#include <string>
#include <cstdio>
#include <jack/net.h>

//-------------------------
// Class TNetJackRenderer
//-------------------------

class TNetJackRenderer : public TAudioRenderer
{

    private:
        
        jack_net_slave_t* fNet;
        int fNetFormat;
        std::string fMasterIP;
        int fMasterPort;
        int fMTU;
        int fLatency;
        jack_master_t fResult;
    
        long OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
        
        virtual void error_cb(int error_code)
        {}
        
        virtual int restart_cb()
        {
            return 0;
        }
        
    #ifdef RESTART_CB_API
        static int net_restart(void* arg) 
        {
            printf("Network failure, restart...\n");
            return static_cast<TNetJackRenderer*>(arg)->restart_cb();
        }
    #else 
        static void net_shutdown(void* arg) 
        {
            printf("Network failure, shutdown...\n");
            static_cast<TNetJackRenderer*>(arg)->shutdown_cb();
        }
    #endif
        
        static int net_sample_rate(jack_nframes_t nframes, void* arg) 
        {
            //return static_cast<TNetJackRenderer*>(arg)->set_sample_rate(nframes);
            return -1;
        }
        
        static int net_buffer_size(jack_nframes_t nframes, void* arg) 
        {
            //return static_cast<TNetJackRenderer*>(arg)->set_buffer_size(nframes);
            return -1;
        }
    
        static void net_error(int error_code, void* arg)
        {
            return static_cast<TNetJackRenderer*>(arg)->error_cb(error_code);
        }
        
        static int net_process(jack_nframes_t buffer_size,
                               int,
                               float** audio_inputs,
                               int,
                               void** midi_inputs,
                               int,
                               float** audio_outputs,
                               int,
                               void** midi_outputs,
                               void* arg) 
        {
            static_cast<TNetJackRenderer*>(arg)->Process(buffer_size, audio_inputs, audio_outputs, midi_inputs, midi_outputs);
            return 0;
        }
        
        virtual void Process(int count, float** audio_inputs, float** audio_outputs, void** midi_inputs, void** midi_outputs);
         
    public:

        TNetJackRenderer(int net_format, const std::string& master_ip, int master_port, int mtu, int latency = 2);
        virtual ~TNetJackRenderer();
  
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

typedef TNetJackRenderer * TNetJackRendererPtr;

#endif



