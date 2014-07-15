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

TOfflineRenderer::TOfflineRenderer(): TAudioRenderer()
{
	// TODO
}

TOfflineRenderer::~TOfflineRenderer()
{
	// TODO
}

long TOfflineRenderer::Open(long inChan, long outChan, long bufferSize, long sampleRate)
{
    // TODO
    int inDevice = 0;
    int outDevice = 0;
    return OpenImp(inDevice, outDevice, inChan, outChan, bufferSize, sampleRate);
}

long TOfflineRenderer::OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
    // TODO
    return -1;
}

long TOfflineRenderer::Close()
{
    // TODO
    return -1;
}

long TOfflineRenderer::Start()
{
    // TODO
    return -1;
}

long TOfflineRenderer::Stop()
{
    // TODO 
    return -1;
}

long TOfflineRenderer::Pause()
{
    // TODO
    return -1;
}

long TOfflineRenderer::Cont()
{
    // TODO 
    return -1;
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

