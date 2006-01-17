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

#include "TPortAudioRenderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include "UTools.h"

#define PA_SAMPLE_TYPE paFloat32

int TPortAudioRenderer::Process(void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
                                PaTimestamp outTime, void* userData)
{
    TPortAudioRendererPtr renderer = (TPortAudioRendererPtr)userData;
    renderer->Run((float*)inputBuffer, (float*)outputBuffer, framesPerBuffer);
    return 0;
}

void TPortAudioRenderer::DisplayDevices()
{
    const PaDeviceInfo *pdi;
    int i, j, numDevices;

    numDevices = Pa_CountDevices();

    printf("Number of devices = %d\n", numDevices );
    for ( i = 0; i < numDevices; i++ ) {
        pdi = Pa_GetDeviceInfo( i );
        printf("---------------------------------------------- #%d", i );
        if (i == Pa_GetDefaultInputDeviceID())
            printf(" DefaultInput");
        if (i == Pa_GetDefaultOutputDeviceID())
            printf(" DefaultOutput");
        printf("\nName         = %s\n", pdi->name);
        printf("Max Inputs   = %d", pdi->maxInputChannels);
        printf(", Max Outputs = %d\n", pdi->maxOutputChannels);
        if ( pdi->numSampleRates == -1 ) {
            printf("Sample Rate Range = %f to %f\n", pdi->sampleRates[0], pdi->sampleRates[1]);
        } else {
            printf("Sample Rates =");
            for (j = 0; j < pdi->numSampleRates; j++) {
                printf(" %8.2f,", pdi->sampleRates[j]);
            }
            printf("\n");
        }
        printf("Native Sample Formats = ");
        if (pdi->nativeSampleFormats & paInt8)
            printf("paInt8, ");
        if (pdi->nativeSampleFormats & paUInt8)
            printf("paUInt8, ");
        if (pdi->nativeSampleFormats & paInt16)
            printf("paInt16, ");
        if (pdi->nativeSampleFormats & paInt32)
            printf("paInt32, ");
        if (pdi->nativeSampleFormats & paFloat32)
            printf("paFloat32, ");
        if (pdi->nativeSampleFormats & paInt24)
            printf("paInt24, ");
        if (pdi->nativeSampleFormats & paPackedInt24)
            printf("paPackedInt24, ");
        printf("\n");
    }
}

int TPortAudioRenderer::GetFirstValidInputDevice()
{
    const PaDeviceInfo* pdi;
    int i, numDevices;

    numDevices = Pa_CountDevices();
    for (i = 0; i < numDevices; i++) {
        pdi = Pa_GetDeviceInfo(i);
        if (pdi->maxInputChannels > 0) {
            printf("GetFirstValidInputDevice: %d\n", i);
            return i;
        }
    }
    return paNoDevice;
}

int TPortAudioRenderer::GetFirstValidOutputDevice()
{
    const PaDeviceInfo* pdi;
    int i, numDevices;

    numDevices = Pa_CountDevices();
    for (i = 0; i < numDevices; i++) {
        pdi = Pa_GetDeviceInfo(i);
        if (pdi->maxOutputChannels > 0) {
            printf("GetFirstValidOutputDevice: %d\n", i);
            return i;
        }
    }
    return paNoDevice;
}

long TPortAudioRenderer::Open(long* inChan, long* outChan, long* bufferSize, long* sampleRate)
{
    PaError err;
    const PaDeviceInfo* pdi;
    int numDevices;
    int inDevice;
    int outDevice;

    printf("Opening device : inChan: %ld outChan: %ld bufferSize: %ld sampleRate: %ld\n",
           *inChan, *outChan, *bufferSize, *sampleRate);

    err = Pa_Initialize();
    if (err != paNoError)
        goto error;

    numDevices = Pa_CountDevices();
    if (numDevices < 0) {
        printf("ERROR: Pa_CountDevices returned 0x%x\n", numDevices );
        err = numDevices;
        goto error;
    } else {
        DisplayDevices();
    }

    // Compute input and output number : to be checked
    inDevice = Pa_GetDefaultInputDeviceID();
    pdi = Pa_GetDeviceInfo(inDevice);

    if (pdi != 0) {
        if (pdi->maxInputChannels == 0) {
            inDevice = GetFirstValidInputDevice();
            pdi = Pa_GetDeviceInfo(inDevice);
        }
        *inChan = (*inChan < pdi->maxInputChannels) ? *inChan : pdi->maxInputChannels;
        printf("Input channel number %ld\n", *inChan);
    }

    outDevice = Pa_GetDefaultOutputDeviceID();
    pdi = Pa_GetDeviceInfo(outDevice);

    if (pdi != 0) {
        if (pdi->maxOutputChannels == 0) {
            outDevice = GetFirstValidOutputDevice();
            pdi = Pa_GetDeviceInfo(outDevice);
        }
        *outChan = (*outChan < pdi->maxOutputChannels) ? *outChan : pdi->maxOutputChannels;
        printf("Ouput channel number %ld\n", *outChan);
    }

    err = Pa_OpenStream(&fStream,
                        (*inChan > 0) ? inDevice : paNoDevice,
                        *inChan,
                        PA_SAMPLE_TYPE,
                        0,
                        (*outChan > 0) ? outDevice : paNoDevice,
                        *outChan,
                        PA_SAMPLE_TYPE,
                        0,
                        *sampleRate,
                        *bufferSize,
                        0,                  //  number of buffers, if zero then use default minimum
                        paClipOff,
                        Process,
                        this);

    if (err != paNoError)
        goto error;
    return TAudioRenderer::Open(inChan, outChan, bufferSize, sampleRate);

error:
    printf("Error while opening device : device open error %s\n", Pa_GetErrorText(err));
    Pa_Terminate();
    fStream = 0;
    return OPEN_ERR;
}

long TPortAudioRenderer::Close()
{
    if (fStream) {
        Pa_CloseStream(fStream);
        Pa_Terminate();
    }
    return NO_ERR;
}

long TPortAudioRenderer::Start()
{
    PaError err = Pa_StartStream(fStream);

    if (err != paNoError) {
        printf("Error while opening device : device open error %s\n", Pa_GetErrorText(err));
        return OPEN_ERR;
    } else
        return NO_ERR;
}

long TPortAudioRenderer::Stop()
{
    PaError err = Pa_StopStream(fStream);

    if (err != paNoError) {
        printf("Error while closing device : device close error %s\n", Pa_GetErrorText(err));
        return OPEN_ERR;
    } else
        return NO_ERR;
}

void TPortAudioRenderer::GetInfo(RendererInfoPtr info)
{
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    info->fCurFrame = long(Pa_StreamTime(fStream));
    info->fCurMs = ConvertSample2Ms(info->fCurFrame);
#if defined(WIN32) && defined(IMUTUS)
    info->fOutputLatencyFrame = Pa_GetOutputLatency(fStream);
    info->fOutputLatencyMs = ConvertSample2Ms(info->fOutputLatencyFrame);
#else
	info->fOutputLatencyFrame = 0;
    info->fOutputLatencyMs = 0;
#endif
}

