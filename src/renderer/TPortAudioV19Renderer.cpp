/*

Copyright © Grame 2002-2007

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

#include "TPortAudioV19Renderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include "UTools.h"

#define PA_SAMPLE_TYPE paFloat32

int TPortAudioV19Renderer::Process(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
									const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags, void* userData)
{
    TPortAudioV19RendererPtr renderer = (TPortAudioV19RendererPtr)userData;
    renderer->Run((float*)inputBuffer, (float*)outputBuffer, framesPerBuffer);
    return 0;
}

void TPortAudioV19Renderer::DisplayDevices()
{
    const PaDeviceInfo *pdi;
    int i;

	PaDeviceIndex numDevices = Pa_GetDeviceCount();

    printf("Number of devices = %d\n", numDevices );
    for (i = 0; i < numDevices; i++) {
        pdi = Pa_GetDeviceInfo(i);
        printf("---------------------------------------------- #%d", i );
        if (i == Pa_GetDefaultInputDevice())
            printf(" DefaultInput");
        if (i == Pa_GetDefaultOutputDevice())
            printf(" DefaultOutput");
        printf("\nName         = %s\n", pdi->name);
        printf("Max Inputs   = %d", pdi->maxInputChannels);
        printf(", Max Outputs = %d\n", pdi->maxOutputChannels);
		printf("Sample rate = %f\n", pdi->defaultSampleRate);
        printf("\n");
    }
}

int TPortAudioV19Renderer::GetFirstValidInputDevice()
{
    const PaDeviceInfo* pdi;
    int i, numDevices;

    numDevices = Pa_GetDeviceCount();
    for (i = 0; i < numDevices; i++) {
        pdi = Pa_GetDeviceInfo(i);
        if (pdi->maxInputChannels > 0) {
            printf("GetFirstValidInputDevice: %d\n", i);
            return i;
        }
    }
    return paNoDevice;
}

int TPortAudioV19Renderer::GetFirstValidOutputDevice()
{
    const PaDeviceInfo* pdi;
    int i, numDevices;

    numDevices = Pa_GetDeviceCount();
    for (i = 0; i < numDevices; i++) {
        pdi = Pa_GetDeviceInfo(i);
        if (pdi->maxOutputChannels > 0) {
            printf("GetFirstValidOutputDevice: %d\n", i);
            return i;
        }
    }
    return paNoDevice;
}

TPortAudioV19Renderer::TPortAudioV19Renderer(): TAudioRenderer()
{
	PaError err;
	
    if ((err = Pa_Initialize()) != paNoError) {
		printf("Pa_Initialize error: %s\n", Pa_GetErrorText(err));
        throw new std::bad_alloc;
	}
}

TPortAudioV19Renderer::~TPortAudioV19Renderer()
{
	Pa_Terminate();
}

long TPortAudioV19Renderer::OpenDefault(long inChan, long outChan, long bufferSize, long sampleRate)
{
    PaError err;
    const PaDeviceInfo* pdi;
    int numDevices;
    int inDevice;
    int outDevice;

	printf("Opening default device : inChan: %ld outChan: %ld bufferSize: %ld sampleRate: %ld\n", inChan, outChan, bufferSize, sampleRate);

    numDevices = Pa_GetDeviceCount();
    if (numDevices < 0) {
        printf("ERROR: Pa_GetDeviceCount returned 0x%x\n", numDevices);
        fStream = 0;
		err = numDevices;
        printf("Error while opening device: device open error %s\n", Pa_GetErrorText(err));
		return OPEN_ERR;
    } else {
        DisplayDevices();
    }

    // Compute input and output number : to be checked
    inDevice = Pa_GetDefaultInputDevice();
    pdi = Pa_GetDeviceInfo(inDevice);

    if (pdi != 0) {
        if (pdi->maxInputChannels == 0) {
            inDevice = GetFirstValidInputDevice();
            pdi = Pa_GetDeviceInfo(inDevice);
        }
        inChan = (inChan < pdi->maxInputChannels) ? inChan : pdi->maxInputChannels;
        printf("Input channel number %ld\n", inChan);
    }

    outDevice = Pa_GetDefaultOutputDevice();
    pdi = Pa_GetDeviceInfo(outDevice);

    if (pdi != 0) {
        if (pdi->maxOutputChannels == 0) {
            outDevice = GetFirstValidOutputDevice();
            pdi = Pa_GetDeviceInfo(outDevice);
        }
        outChan = (outChan < pdi->maxOutputChannels) ? outChan : pdi->maxOutputChannels;
        printf("Ouput channel number %ld\n", outChan);
    }
	
	return Open(inDevice, outDevice, inChan, outChan, bufferSize, sampleRate);
}

long TPortAudioV19Renderer::Open(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
    printf("Opening device : inputDevice: %ld outputDevice: %ld inChan: %ld outChan: %ld bufferSize: %ld sampleRate: %ld\n", 
		inputDevice, outputDevice, inChan, outChan, bufferSize, sampleRate);
	
	PaStreamParameters inputParameters;
    PaStreamParameters outputParameters;
	
	inputParameters.device = inputDevice;
    inputParameters.channelCount = inChan;
    inputParameters.sampleFormat = paFloat32;		// 32 bit floating point output
    inputParameters.suggestedLatency = (inputDevice != paNoDevice)		// TODO: check how to setup this on ASIO
                                       ? Pa_GetDeviceInfo(inputParameters.device)->defaultLowInputLatency
                                       : 0;
    inputParameters.hostApiSpecificStreamInfo = NULL;

    outputParameters.device = outputDevice;
    outputParameters.channelCount = outChan;
    outputParameters.sampleFormat = paFloat32;		// 32 bit floating point output
    outputParameters.suggestedLatency = (outputDevice != paNoDevice)	// TODO: check how to setup this on ASIO
                                        ? Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency
                                        : 0;
    outputParameters.hostApiSpecificStreamInfo = NULL;

    PaError err = Pa_OpenStream(&fStream,
								(inputDevice == paNoDevice) ? 0 : &inputParameters,
								(outputDevice == paNoDevice) ? 0 : &outputParameters,
								sampleRate,
								bufferSize,
								paNoFlag,  // Clipping is on...
								Process,
								this);

    if (err != paNoError)
        goto error;
    return TAudioRenderer::OpenDefault(inChan, outChan, bufferSize, sampleRate);

error:
    printf("Error while opening device : device open error %s\n", Pa_GetErrorText(err));
    fStream = 0;
    return OPEN_ERR;
}

long TPortAudioV19Renderer::Close()
{
    if (fStream) 
        Pa_CloseStream(fStream);
    return NO_ERR;
}

long TPortAudioV19Renderer::Start()
{
    PaError err = Pa_StartStream(fStream);

    if (err != paNoError) {
        printf("Error while opening device : device open error %s\n", Pa_GetErrorText(err));
        return OPEN_ERR;
    } else {
        return NO_ERR;
	}
}

long TPortAudioV19Renderer::Stop()
{
    PaError err = Pa_StopStream(fStream);

    if (err != paNoError) {
        printf("Error while closing device : device close error %s\n", Pa_GetErrorText(err));
        return OPEN_ERR;
    } else {
        return NO_ERR;
	}
}

void TPortAudioV19Renderer::GetInfo(RendererInfoPtr info)
{
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    info->fCurFrame = long(Pa_GetStreamTime(fStream));
    info->fCurMs = ConvertSample2Ms(info->fCurFrame);
#if defined(WIN32) && defined(IMUTUS)
    info->fOutputLatencyFrame = Pa_GetOutputLatency(fStream);
    info->fOutputLatencyMs = ConvertSample2Ms(info->fOutputLatencyFrame);
#else
	info->fOutputLatencyFrame = 0;
    info->fOutputLatencyMs = 0;
#endif
}

long TPortAudioV19Renderer::GetDeviceCount()
{
	return Pa_GetDeviceCount();
}

void TPortAudioV19Renderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
	const PaDeviceInfo* pdi;
	pdi = Pa_GetDeviceInfo(deviceNum);
	
	info->fMaxInputChannels = pdi->maxInputChannels;
	info->fMaxOutputChannels = pdi->maxOutputChannels;
	strcpy(info->fName, pdi->name);
	info->fDefaultBufferSize = 512;
	info->fDefaultSampleRate = pdi->defaultSampleRate; // init value
}

long TPortAudioV19Renderer::GetDefaultInputDevice()
{
	return Pa_GetDefaultInputDevice();
}

long TPortAudioV19Renderer::GetDefaultOutputDevice()
{
	return Pa_GetDefaultOutputDevice();
}
