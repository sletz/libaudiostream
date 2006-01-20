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

#include "TJackAudioRenderer.h"
#include "TAudioGlobals.h"
#include "TSharedBuffers.h"
#include "UTools.h"

int TJackAudioRenderer::Process(jack_nframes_t nframes, void *arg)
{
    TJackAudioRendererPtr renderer = (TJackAudioRendererPtr)arg;

    // Copy input and interleaving
	for (int i = 0; i < renderer->fInput; i++) {
		float* input = (float*)jack_port_get_buffer(renderer->fInput_ports[i], nframes);
		for (jack_nframes_t j = 0; j < nframes; j++) {
			renderer->fInputBuffer[renderer->fInput * j + i] = input[j];
		}
    }
	
    renderer->Run(renderer->fInputBuffer, renderer->fOutputBuffer, nframes);

    // Copy output and de-interleaving
	for (int i = 0; i < renderer->fOutput; i++) {
		float* output = (float*)jack_port_get_buffer(renderer->fOutput_ports[i], nframes);
		for (jack_nframes_t j = 0; j < nframes; j++) {
			 output[j] = renderer->fOutputBuffer[renderer->fOutput * j + i];
		}
    }

    return 0;
}

TJackAudioRenderer::TJackAudioRenderer(): TAudioRenderer()
{
    fInputBuffer = new float[TAudioGlobals::fBuffer_Size * TAudioGlobals::fChannels];
    fOutputBuffer = new float[TAudioGlobals::fBuffer_Size * TAudioGlobals::fChannels];
	fInput_ports = (jack_port_t**)calloc(fInput, sizeof(jack_port_t*));
	fOutput_ports = (jack_port_t**)calloc(fOutput, sizeof(jack_port_t*));
}

TJackAudioRenderer::~TJackAudioRenderer()
{
    delete[]fInputBuffer;
    delete[]fOutputBuffer;
	free(fInput_ports);
	free(fOutput_ports);
}

long TJackAudioRenderer::Open(long* inChan, long* outChan, long* bufferSize, long* sampleRate)
{
    if ((fClient = jack_client_new("AudioPlayer")) == 0) {
        printf("Jack server not running?\n");
        goto error;
    }
	
	if (*sampleRate != jack_get_sample_rate(fClient)) {
		printf("Warning: requested sample rate = %ld different from driver sample rate = %d \n", *sampleRate, jack_get_sample_rate(fClient));
	}
	
	if (*bufferSize != jack_get_buffer_size(fClient)) { 
		printf("Error: requested buffer size = %ld different from driver bufffer size = %d \n", *bufferSize, jack_get_buffer_size(fClient));
		goto error;
	}

	*sampleRate = jack_get_sample_rate(fClient);
    *bufferSize = jack_get_buffer_size(fClient);
    jack_set_process_callback(fClient, Process, this);
	
	fInput = *inChan;
	fOutput = *outChan;
		
	char buf[256];
	
	for (int i = 0; i < fInput; i++) {
		snprintf(buf, sizeof(buf) - 1, "in%d", i + 1);
		fInput_ports[i] = jack_port_register(fClient, buf, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
		if (!fInput_ports[i])
			goto error;
	}
	for (int i = 0; i < fOutput; i++) {
		snprintf(buf, sizeof(buf) - 1, "out%d", i + 1);
		fOutput_ports[i] = jack_port_register(fClient, buf, JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
		if (!fOutput_ports[i])
			goto error;
	}

	return TAudioRenderer::Open(inChan, outChan, bufferSize, sampleRate);

error:
    printf("Error while opening jack client\n");
    if (fClient)
        jack_client_close(fClient);
    fClient = 0;
    return OPEN_ERR;
}

long TJackAudioRenderer::Close()
{
	for (int i = 0; i < fInput; i++) {
		jack_port_unregister(fClient, fInput_ports[i]);
	}
	for (int i = 0; i < fOutput; i++) {
		jack_port_unregister(fClient, fOutput_ports[i]);
	}
	
	if (fClient && jack_client_close(fClient)) {
        printf ("Cannot close client");
        return CLOSE_ERR;
    }
	
    return NO_ERR;
}

long TJackAudioRenderer::Start()
{
    const char** ports;

    if (jack_activate(fClient)) {
        printf("Cannot activate client");
        goto error;
    }

    if ((ports = jack_get_ports(fClient, NULL, NULL, JackPortIsPhysical | JackPortIsOutput)) == NULL) {
        printf("Cannot find any physical capture ports\n");
    } else {
		for (int i = 0; i < fInput; i++) {
			if (jack_connect(fClient, ports[i], jack_port_name(fInput_ports[i])) != 0) {
				printf("Cannot connect input ports\n");
			}
		}
        free(ports);
    }

    if ((ports = jack_get_ports(fClient, NULL, NULL, JackPortIsPhysical | JackPortIsInput)) == NULL) {
        printf("Cannot find any physical playback ports");
    } else {
		for (int i = 0; i < fOutput; i++) {
			if (jack_connect(fClient, jack_port_name(fOutput_ports[i]), ports[i]) != 0) {
				printf("Cannot connect output ports\n");
			}
		}
		free(ports);
    }

    return NO_ERR;

error:
    printf("Error while activating client\n");
    return OPEN_ERR;
}

long TJackAudioRenderer::Stop()
{
    if (jack_deactivate(fClient)) {
        printf("Cannot deactivate client");
        return OPEN_ERR;
    } else {
		return NO_ERR;
	}
}

void TJackAudioRenderer::GetInfo(RendererInfoPtr info)
{
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    info->fCurFrame = jack_frame_time(fClient);
    info->fCurMs = ConvertSample2Ms(info->fCurFrame);
}
