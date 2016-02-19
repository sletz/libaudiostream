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
#include <cstring>

#include "TAudioEffect.h"
#include "TJackRenderer.h"
#include "TAudioGlobals.h"
#include "TSharedBuffers.h"
#include "UTools.h"

#ifdef WIN32
	#define vsnprintf _vsnprintf
	#define snprintf _snprintf
#endif

int TJackRenderer::Process(jack_nframes_t nframes, void *arg)
{
    TJackRendererPtr renderer = (TJackRendererPtr)arg;
    return renderer->ProcessAux(nframes);
}

int TJackRenderer::ProcessAux(jack_nframes_t nframes)
{
	int i;
    
    // Take time stamp of first call to Process 
    if (fAnchorFrameTime == 0) {
        jack_time_t    next_usecs;
        float          period_usecs;
        jack_get_cycle_times(fClient, &fAnchorFrameTime, &fAnchorUsecTime, &next_usecs, &period_usecs);
    
        //printf("fAnchorFrameTime %ld \n", int(fAnchorFrameTime));
        //printf("fAnchorUsecTime %ld \n", int(fAnchorUsecTime));
    }

    // Copy input buffers
	for (i = 0; i < fInput; i++) {
		fInputBuffer[i] = (float*)jack_port_get_buffer(fInput_ports[i], nframes);
    }
    
    // Copy output buffers
    for (i = 0; i < fOutput; i++) {
		fOutputBuffer[i] = (float*)jack_port_get_buffer(fOutput_ports[i], nframes);
    }
	
    Run(fInputBuffer, fOutputBuffer, nframes);
    return 0;
}

TJackRenderer::TJackRenderer(): TAudioRenderer()
{
	fInput = fOutput = MAX_PORTS;
    fInputBuffer = new float*[TAudioGlobals::fInput];
    fOutputBuffer = new float*[TAudioGlobals::fOutput];
	fInput_ports = (jack_port_t**)calloc(fInput, sizeof(jack_port_t*));
	fOutput_ports = (jack_port_t**)calloc(fOutput, sizeof(jack_port_t*));
    fAnchorFrameTime = 0;
    fAnchorUsecTime = 0;
}

TJackRenderer::~TJackRenderer()
{
    delete[] fInputBuffer;
    delete[] fOutputBuffer;
	free(fInput_ports);
	free(fOutput_ports);
}

long TJackRenderer::Open(long inChan, long outChan, long bufferSize, long sampleRate)
{
	int i;
 
    if ((fClient = jack_client_open("AudioPlayer", JackNullOption, NULL)) == 0) {
        printf("Jack server not running?\n");
        goto error;
    }
	
	if (sampleRate != long(jack_get_sample_rate(fClient))) {
		printf("Warning: requested sample rate = %ld different from driver sample rate = %d \n", sampleRate, jack_get_sample_rate(fClient));
	}
	
	if (bufferSize != long(jack_get_buffer_size(fClient))) { 
		printf("Warning: requested buffer size = %ld different from driver buffer size = %d \n", bufferSize, jack_get_buffer_size(fClient));
	}

	sampleRate = jack_get_sample_rate(fClient);
    bufferSize = jack_get_buffer_size(fClient);
    jack_set_process_callback(fClient, Process, this);

	assert(inChan < MAX_PORTS);
	assert(outChan < MAX_PORTS);
		
	char buf[256];
	
	for (i = 0; i < inChan; i++) {
		snprintf(buf, sizeof(buf) - 1, "in%d", i + 1);
		fInput_ports[i] = jack_port_register(fClient, buf, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
		if (!fInput_ports[i]) {
			goto error;
        }
	}
	for (i = 0; i < outChan; i++) {
		snprintf(buf, sizeof(buf) - 1, "out%d", i + 1);
		fOutput_ports[i] = jack_port_register(fClient, buf, JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
		if (!fOutput_ports[i]) {
			goto error;
        }
	}

	return TAudioRenderer::Open(inChan, outChan, bufferSize, sampleRate);

error:
    printf("Error while opening jack client\n");
    if (fClient) {
        jack_client_close(fClient);
    }
    fClient = 0;
    return OPEN_ERR;
}

long TJackRenderer::OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long samplerate)
{
	return Open(inChan, outChan, bufferSize, samplerate);
}

long TJackRenderer::Close()
{
	int i;

	if (fClient) {
		for (i = 0; i < fInput; i++) {
			jack_port_unregister(fClient, fInput_ports[i]);
		}
		for (i = 0; i < fOutput; i++) {
			jack_port_unregister(fClient, fOutput_ports[i]);
		}
	}
	
	if (fClient && jack_client_close(fClient)) {
        printf("Cannot close client");
        return CLOSE_ERR;
    }
	
    return NO_ERR;
}

long TJackRenderer::Pause()
{
    return Stop();
}

long TJackRenderer::Start()
{
    // Init timing here
    memset(&fInfo, 0, sizeof(RendererInfo));
    
    fAnchorFrameTime = 0;
    fAnchorUsecTime = 0;
    
    return Cont();
}

long TJackRenderer::Cont()
{
    const char** ports = 0;

    if (jack_activate(fClient)) {
        printf("Cannot activate client");
        goto error;
    }

    if ((ports = jack_get_ports(fClient, NULL, JACK_DEFAULT_AUDIO_TYPE, JackPortIsPhysical | JackPortIsOutput)) == NULL) {
        printf("Cannot find any physical capture ports\n");
    } else {
        for (int i = 0; i < fInput && ports[i]; i++) {
            if (jack_connect(fClient, ports[i], jack_port_name(fInput_ports[i])) != 0) {
				printf("Cannot connect input ports\n");
			}
		}
        jack_free(ports);
    }

    if ((ports = jack_get_ports(fClient, NULL, JACK_DEFAULT_AUDIO_TYPE, JackPortIsPhysical | JackPortIsInput)) == NULL) {
        printf("Cannot find any physical playback ports");
    } else {
		for (int i = 0; i < fOutput && ports[i]; i++) {
			if (jack_connect(fClient, jack_port_name(fOutput_ports[i]), ports[i]) != 0) {
				printf("Cannot connect output ports\n");
			}
		}
		jack_free(ports);
    }

    return NO_ERR;

error:
    printf("Error while activating client\n");
    return OPEN_ERR;
}

long TJackRenderer::Stop()
{
    if (jack_deactivate(fClient)) {
        printf("Cannot deactivate client");
        return OPEN_ERR;
    } else {
        
        // Keep current time if really started...
        if (fAnchorFrameTime > 0) {
            fInfo.fCurFrame = jack_frame_time(fClient) - fAnchorFrameTime;
            fInfo.fCurUsec = jack_get_time() - fAnchorUsecTime;
        }
        
        // Renderer is stopped...
        fAnchorFrameTime = 0;
        fAnchorUsecTime = 0;
        
		return NO_ERR;
	}
}

void TJackRenderer::GetInfo(RendererInfoPtr info)
{
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    
    if (fAnchorFrameTime == 0) { // Renderer is stopped...
        info->fCurFrame = fInfo.fCurFrame;
        info->fCurUsec = fInfo.fCurUsec;
    } else {
        info->fCurFrame = jack_frame_time(fClient) - fAnchorFrameTime;
        info->fCurUsec = jack_get_time() - fAnchorUsecTime;
    }
}

long TJackRenderer::GetDeviceCount()
{
    jack_client_t* client;
	if ((client = jack_client_open("Dummy", JackNullOption, NULL)) == 0) {
        printf("Jack server not running?\n");
  		return 0;
    } else {
		jack_client_close(client);
		return 1;
	}
}

void TJackRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
    const char** ports = 0;
    jack_client_t* client;
    int i;
	
    if ((client = jack_client_open("Dummy", JackNullOption, NULL)) == 0) {
        printf("Jack server not running?\n");
        info->fMaxInputChannels = 0;
		info->fMaxOutputChannels = 0;
		info->fDefaultSampleRate = 0.0;	
		info->fDefaultBufferSize = 0;
		return;
    }
	
    if ((ports = jack_get_ports(client, NULL, NULL, JackPortIsPhysical | JackPortIsOutput)) == NULL) {
        printf("Cannot find any physical capture ports\n");
    } else {
		for (i = 0; ports[i]; i++) {}
		info->fMaxInputChannels = i;
        jack_free(ports);
    }
	
	if ((ports = jack_get_ports(client, NULL, NULL, JackPortIsPhysical | JackPortIsInput)) == NULL) {
        printf("Cannot find any physical playback ports\n");
    } else {
		for (i = 0; ports[i]; i++) {}
		info->fMaxOutputChannels = i;
        jack_free(ports);
    }
	
	strcpy(info->fName, "Jack backend");
	info->fDefaultSampleRate = double(jack_get_sample_rate(client));	
	info->fDefaultBufferSize = jack_get_buffer_size(client);
	
	jack_client_close(client);
}

long TJackRenderer::GetDefaultInputDevice()
{
	return 0; // Only 1 device available...
}

long TJackRenderer::GetDefaultOutputDevice()
{
	return 0; // Only 1 device available...
}
