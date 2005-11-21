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

    float* input1 = (float*)jack_port_get_buffer(renderer->fInput_port1, nframes);
    float* input2 = (float*)jack_port_get_buffer(renderer->fInput_port2, nframes);

    float* output1 = (float*)jack_port_get_buffer(renderer->fOutput_port1, nframes);
    float* output2 = (float*)jack_port_get_buffer(renderer->fOutput_port2, nframes);

    // Copy input and interleaving
    for (jack_nframes_t i = 0; i < nframes; i++) {
        renderer->fInputBuffer[2 * i] = input1[i];
        renderer->fInputBuffer[2 * i + 1] = input2[i];
    }

    renderer->Run(renderer->fInputBuffer, renderer->fOutputBuffer, nframes);

    // Copy output and de-interleaving
    for (jack_nframes_t i = 0; i < nframes; i++) {
        output1[i] = renderer->fOutputBuffer[2 * i];
        output2[i] = renderer->fOutputBuffer[2 * i + 1];
    }

    return 0;
}

TJackAudioRenderer::TJackAudioRenderer(): TAudioRenderer()
{
    // Allocates objects
    fInputBuffer = new float[TAudioGlobals::fBuffer_Size * TAudioGlobals::fChannels];
    fOutputBuffer = new float[TAudioGlobals::fBuffer_Size * TAudioGlobals::fChannels];
}

TJackAudioRenderer::~TJackAudioRenderer()
{
    delete[]fInputBuffer;
    delete[]fOutputBuffer;
}

long TJackAudioRenderer::Open(long* inChan, long* outChan, long* bufferSize, long* sampleRate)
{
    if ((fClient = jack_client_new("AudioPlayer")) == 0) {
        printf("Jack server not running?\n");
        goto error;
    }

    jack_set_process_callback(fClient, Process, this);

    // To be finished
    fInput_port1 = jack_port_register(fClient, "in1", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
	if (!fInput_port1)
		goto error;
    fInput_port2 = jack_port_register(fClient, "in2", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
	if (!fInput_port2)
		goto error;

    fOutput_port1 = jack_port_register(fClient, "out1", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
	if (!fOutput_port1)
		goto error;
    fOutput_port2 = jack_port_register(fClient, "out2", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
	if (!fOutput_port2)
		goto error;

    *sampleRate = jack_get_sample_rate(fClient);
    *bufferSize = jack_get_buffer_size(fClient);

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
	// To be finished
    jack_port_unregister(fClient, fInput_port1);
	jack_port_unregister(fClient, fInput_port2);
	jack_port_unregister(fClient, fOutput_port1);
	jack_port_unregister(fClient, fOutput_port2);
		
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

        if (jack_connect(fClient, ports[0], jack_port_name(fInput_port1))) {
            printf("Cannot connect input ports\n");
        }

        if (jack_connect(fClient, ports[1], jack_port_name(fInput_port2))) {
            printf("Cannot connect input ports\n");
        }

        free (ports);
    }

    if ((ports = jack_get_ports(fClient, NULL, NULL, JackPortIsPhysical | JackPortIsInput)) == NULL) {
        printf("Cannot find any physical playback ports");
    } else {

        if (jack_connect(fClient, jack_port_name(fOutput_port1), ports[0])) {
            printf("Cannot connect output ports\n");
        }

        if (jack_connect(fClient, jack_port_name(fOutput_port2), ports[1])) {
            printf("Cannot connect output ports\n");
        }

        free (ports);
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
    }
    return NO_ERR;
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
