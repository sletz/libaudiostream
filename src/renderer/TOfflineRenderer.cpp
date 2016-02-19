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

#include <cstring>
#include "TOfflineRenderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include "UTools.h"

void* TOfflineRenderer::Process(void* arg)
{
    TOfflineRenderer* renderer = static_cast<TOfflineRenderer*>(arg);
    renderer->ProcessAux();
	return NULL;
 }

void TOfflineRenderer::ProcessAux()
{
    while (true) {
        Run(fInputBuffer, fOutputBuffer, TAudioGlobals::fBufferSize);
    }
}

TOfflineRenderer::TOfflineRenderer(): TAudioRenderer()
{
    fInputBuffer = new float*[TAudioGlobals::fInput];
    fOutputBuffer = new float*[TAudioGlobals::fOutput];
    
    for (int i = 0; i < TAudioGlobals::fInput; i++) {
        fInputBuffer[i] = new float[TAudioGlobals::fBufferSize];
    }
    for (int i = 0; i < TAudioGlobals::fInput; i++) {
        fOutputBuffer[i] = new float[TAudioGlobals::fBufferSize];
    }
}

TOfflineRenderer::~TOfflineRenderer()
{
    for (int i = 0; i < TAudioGlobals::fInput; i++) {
        delete [] fInputBuffer[i];
    }
    for (int i = 0; i < TAudioGlobals::fInput; i++) {
        delete [] fOutputBuffer[i];;
    }
    
    delete [] fInputBuffer;
    delete [] fOutputBuffer;
}

long TOfflineRenderer::Open(long inChan, long outChan, long bufferSize, long sampleRate)
{
    int inDevice = 0;
    int outDevice = 0;
    return OpenImp(inDevice, outDevice, inChan, outChan, bufferSize, sampleRate);
}

long TOfflineRenderer::OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
    return 0;
}

long TOfflineRenderer::Close()
{
    return 0;
}

long TOfflineRenderer::Start()
{
    //return pthread_create(&fThread, NULL, Process, this);
	return false;
}

long TOfflineRenderer::Stop()
{
    //pthread_cancel(fThread);
    //pthread_join(fThread, NULL); 
    return 0;
}

long TOfflineRenderer::Pause()
{
    return Stop();
}

long TOfflineRenderer::Cont()
{
    return Start();
}

void TOfflineRenderer::GetInfo(RendererInfoPtr info)
{
    // TODO
}

long TOfflineRenderer::GetDeviceCount()
{
	// TODO
    return 0;
}

void TOfflineRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
	// TODO
}

long TOfflineRenderer::GetDefaultInputDevice()
{
	return 1;
}

long TOfflineRenderer::GetDefaultOutputDevice()
{
	return 1;
}

