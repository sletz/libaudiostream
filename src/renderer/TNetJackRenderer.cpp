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
#include "TNetJackRenderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include "UTools.h"

void* TNetJackRenderer::Process(void* arg)
{
    TNetJackRenderer* renderer = static_cast<TNetJackRenderer*>(arg);
  	return NULL;
 }

TNetJackRenderer::TNetJackRenderer(): TAudioRenderer()
{
    
}

TNetJackRenderer::~TNetJackRenderer()
{
    
}

long TNetJackRenderer::Open(long inChan, long outChan, long bufferSize, long sampleRate)
{
    int inDevice = 0;
    int outDevice = 0;
    return OpenImp(inDevice, outDevice, inChan, outChan, bufferSize, sampleRate);
}

long TNetJackRenderer::OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
    return 0;
}

long TNetJackRenderer::Close()
{
    return 0;
}

long TNetJackRenderer::Start()
{
 	return false;
}

long TNetJackRenderer::Stop()
{
    return 0;
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
    // TODO
}

long TNetJackRenderer::GetDeviceCount()
{
	// TODO
    return 0;
}

void TNetJackRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
	// TODO
}

long TNetJackRenderer::GetDefaultInputDevice()
{
	return 1;
}

long TNetJackRenderer::GetDefaultOutputDevice()
{
	return 1;
}

