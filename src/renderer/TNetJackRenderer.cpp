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

#include "TNetJackRenderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include <cstring>

TNetJackRenderer::TNetJackRenderer(int net_format, const std::string& master_ip, int master_port, int mtu, int latency)
    :TAudioRenderer(), fNet(0), fNetFormat(net_format), fMasterIP(master_ip), fMasterPort(master_port), fMTU(mtu), fLatency(latency)
{}

TNetJackRenderer::~TNetJackRenderer()
{
    Close();
}

void TNetJackRenderer::Process(int count, float** audio_inputs, float** audio_outputs, void** midi_inputs, void** midi_outputs)
{
    Run(audio_inputs, audio_outputs, count);
}

long TNetJackRenderer::Open(long inChan, long outChan, long bufferSize, long sampleRate)
{
    int inDevice = 0;
    int outDevice = 0;
    return OpenImp(inDevice, outDevice, inChan, outChan, bufferSize, sampleRate);
}

long TNetJackRenderer::OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
    jack_slave_t request = {
                int(inChan),
                int(outChan),
                0,
                0,
                fMTU,
                -1,
                (fNetFormat > 0) ? JackOpusEncoder : ((fNetFormat == -1) ? JackFloatEncoder : JackIntEncoder),
                (fNetFormat > 0) ? fNetFormat : 0,
                fLatency
            };

    if ((fNet = jack_net_slave_open(fMasterIP.c_str(), fMasterPort, "net_slave", &request, &fResult)) == 0) {
        printf("JACK remote server not running ?\n");
        return OPEN_ERR;
    }

    if (sampleRate != fResult.sample_rate) {
        printf("Warning: requested sample rate = %ld different from driver sample rate = %d \n", sampleRate, fResult.sample_rate);
    }

    if (bufferSize != fResult.buffer_size) {
        printf("Warning: requested buffer size = %ld different from driver buffer size = %d \n", bufferSize, fResult.buffer_size);
    }

    sampleRate = fResult.sample_rate;
    bufferSize = fResult.buffer_size;

    jack_set_net_slave_process_callback(fNet, net_process, this);
#ifdef RESTART_CB_API
    jack_set_net_slave_restart_callback(fNet, net_restart, this);
#else
    jack_set_net_slave_shutdown_callback(fNet, net_shutdown, this);
#endif
    jack_set_net_slave_sample_rate_callback(fNet, net_sample_rate, this);

    jack_set_net_slave_buffer_size_callback(fNet, net_buffer_size, this);

    jack_set_net_slave_error_callback(fNet, net_error, this);

    return TAudioRenderer::Open(inChan, outChan, bufferSize, sampleRate);
}

long TNetJackRenderer::Close()
{
    if (fNet) {
        Stop();
        jack_net_slave_close(fNet);
        fNet = 0;
    }
    return NO_ERR;
}

long TNetJackRenderer::Start()
{
    if (jack_net_slave_activate(fNet)) {
        printf("cannot activate net");
        return OPEN_ERR;
    } else {
        return NO_ERR;
    }
}

long TNetJackRenderer::Stop()
{
    if (fNet) {
        return (jack_net_slave_deactivate(fNet) == 0) ? NO_ERR : OPEN_ERR;
    } else {
        return OPEN_ERR;
    }
}

long TNetJackRenderer::Pause()
{
    return Stop();
}

long TNetJackRenderer::Cont()
{
    return Start();
}

void TNetJackRenderer::GetInfo(RendererInfoPtr info)
{
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    // TODO : Zero for now...
    info->fCurFrame = 0;
    info->fCurUsec = 0;
    info->fOutputLatencyFrame = 0;
    info->fOutputLatencyUsec = 0;
}

long TNetJackRenderer::GetDeviceCount()
{
    return 1;
}

void TNetJackRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
    strcpy(info->fName, "NetJack backend");
    // TODO
}

long TNetJackRenderer::GetDefaultInputDevice()
{
    return 0;
}

long TNetJackRenderer::GetDefaultOutputDevice()
{
    return 0;
}
