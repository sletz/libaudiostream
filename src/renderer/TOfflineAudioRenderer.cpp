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
#include "TOfflineAudioRenderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include "UTools.h"

TOfflineAudioRenderer::TOfflineAudioRenderer(): TAudioRenderer()
{
	// TODO
}

TOfflineAudioRenderer::~TOfflineAudioRenderer()
{
	// TODO
}

long TOfflineAudioRenderer::OpenDefault(long inChan, long outChan, long bufferSize, long sampleRate)
{
    // TODO
	return Open(inDevice, outDevice, inChan, outChan, bufferSize, sampleRate);
}

long TOfflineAudioRenderer::Open(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
    // TODO
    return -1;
}

long TOfflineAudioRenderer::Close()
{
    // TODO
    return -1;
}

long TOfflineAudioRenderer::Start()
{
    // TODO
    return -1;
}

long TOfflineAudioRenderer::Stop()
{
    // TODO 
    return -1;
}

void TOfflineAudioRenderer::GetInfo(RendererInfoPtr info)
{
    // TODO
}

long TOfflineAudioRenderer::GetDeviceCount()
{
	// TODO
    return 0;
}

void TOfflineAudioRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
	// TODO
}

long TOfflineAudioRenderer::GetDefaultInputDevice()
{
	return 1;
}

long TOfflineAudioRenderer::GetDefaultOutputDevice()
{
	return 1;
}

