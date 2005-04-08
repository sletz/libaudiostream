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
grame@rd.grame.fr

*/

#include "TJackAudioRenderer.h"
#include "TAudioGlobals.h"
#include "TSharedBuffers.h"
#include "UTools.h"

int TJackAudioRenderer::Process(jack_nframes_t nframes, void *arg)
{
    TJackAudioRendererPtr engine = (TJackAudioRendererPtr) arg;

    float * input1 = (float *)jack_port_get_buffer(engine->fInput_port1, nframes);
    float * input2 = (float *)jack_port_get_buffer(engine->fInput_port2, nframes);

    float * output1 = (float *)jack_port_get_buffer(engine->fOutput_port1, nframes);
    float * output2 = (float *)jack_port_get_buffer(engine->fOutput_port2, nframes);

    // Copy input and interleaving
    for (jack_nframes_t i = 0; i < nframes; i++) {
        engine->fInputBuffer[2*i] = input1[i];
        engine->fInputBuffer[2*i + 1] = input2[i];
    }

    engine->Run(engine->fInputBuffer, engine->fOutputBuffer, nframes);

    // Copy output and de-interleaving
    for (jack_nframes_t i = 0; i < nframes; i++) {
        output1[i] = engine->fOutputBuffer[2 * i];
        output2[i] = engine->fOutputBuffer[2 * i + 1];
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
        printf("jack server not running?\n");
        goto error;
    }

    jack_set_process_callback(fClient, Process, this);

    // To be finished
    fInput_port1 = jack_port_register(fClient, "in1", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
    fInput_port2 = jack_port_register(fClient, "in2", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);

    fOutput_port1 = jack_port_register(fClient, "out1", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
    fOutput_port2 = jack_port_register(fClient, "out2", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);

    *sampleRate = jack_get_sample_rate(fClient);
    *bufferSize = jack_get_buffer_size(fClient);

    TAudioRenderer::Open(inChan, outChan, bufferSize, sampleRate);
    return NO_ERR;

error:
    printf("Error while opening jack client\n");
    if (fClient)
        jack_client_close(fClient);
    fClient = 0;
    return OPEN_ERR;
}

long TJackAudioRenderer::Close()
{
    if (fClient && jack_client_close(fClient)) {
        printf ("cannot close client");
        return CLOSE_ERR;
    }
    return NO_ERR;
}

long TJackAudioRenderer::Start()
{
    const char** ports;

    if (jack_activate(fClient)) {
        printf("cannot activate client");
        goto error;
    }

    if ((ports = jack_get_ports(fClient, NULL, NULL, JackPortIsPhysical | JackPortIsOutput)) == NULL) {
        printf("Cannot find any physical capture ports\n");
    } else {

        if (jack_connect(fClient, ports[0], jack_port_name(fInput_port1))) {
            printf("cannot connect input ports\n");
        }

        if (jack_connect(fClient, ports[1], jack_port_name(fInput_port2))) {
            printf("cannot connect input ports\n");
        }

        free (ports);
    }

    if ((ports = jack_get_ports(fClient, NULL, NULL, JackPortIsPhysical | JackPortIsInput)) == NULL) {
        printf("Cannot find any physical playback ports");
    } else {

        if (jack_connect(fClient, jack_port_name(fOutput_port1), ports[0])) {
            printf("cannot connect output ports\n");
        }

        if (jack_connect(fClient, jack_port_name(fOutput_port2), ports[1])) {
            printf("cannot connect output ports\n");
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
        printf("cannot deactivate client");
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
