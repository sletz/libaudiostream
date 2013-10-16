/*

Copyright (C) Grame 2002-2013

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

#include "TAudioRenderer.h"
#include "UAudioTools.h"
#include "TSharedBuffers.h"

long TAudioRenderer::Open(long inChan, long outChan, long bufferSize, long sampleRate)
{
    fInput = inChan;
    fOutput = outChan;
    fBufferSize = bufferSize;
    fSampleRate = sampleRate;
    return NO_ERR;
}

void TAudioRenderer::Run(float** inputs, float** outputs, long frames)
{
    // Clear output buffer
    UAudioTools::ZeroFloatBlk(outputs, frames, fOutput);
    
    // Setup in/out real-time buffers
    TSharedBuffers::SetInBuffer(inputs);
    TSharedBuffers::SetOutBuffer(outputs);

	// Client callback are supposed to *mix* their result in outputs 
	list<TRTAudioClient>::iterator iter = fClientList.begin();
	while (iter != fClientList.end()) {
		TAudioClientPtr client = (*iter).fRTClient;
		if (client) {
            client->AudioCallback(inputs, outputs, frames);
			iter++;
		} else {  // Client was removed
			iter = fClientList.erase(iter);
		}
	}
}
