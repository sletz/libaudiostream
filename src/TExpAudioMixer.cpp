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

#ifdef WIN32 
#pragma warning (disable : 4786)
#endif

#include "TExpAudioMixer.h"

/*--------------------------------------------------------------------------*/
// Internal API
/*--------------------------------------------------------------------------*/

TExpAudioMixer::TExpAudioMixer ()
{
    fMixBuffer = new TLocalNonInterleavedAudioBuffer<float>(TAudioGlobals::fBufferSize, MAX_OUTPUT_CHAN);
}

TExpAudioMixer::~TExpAudioMixer()
{
    delete fMixBuffer;
}

bool TExpAudioMixer::AudioCallback(float** inputBuffer, float** outputBuffer, long frames)
{
    // Init buffer
    UAudioTools::ZeroFloatBlk(fMixBuffer->GetFrame(0), TAudioGlobals::fBufferSize, MAX_OUTPUT_CHAN);
   
    // Real-time input
    //TAudioGlobals::fSharedInput->Read(fMixBuffer, frames, 0, TAudioGlobals::fOutput);
 
    // Mix all Streams
	list<TAudioStreamPtr>::iterator iter = fStreamSeq.begin();
	while (iter != fStreamSeq.end()) {
		TAudioStreamPtr stream = *iter;
        
        //printf("TExpAudioMixer::AudioCallback stream->Read %d\n", frames);
    	if (stream->Read(fMixBuffer, TAudioGlobals::fBufferSize, 0) < TAudioGlobals::fBufferSize) { // End of stream
            iter = fStreamSeq.erase(iter);
		} else {
			iter++;
		}
	}
    
    // Mix in outputBuffer
	UAudioTools::MixFrameToFrameBlk(outputBuffer,
									fMixBuffer->GetFrame(0),
									TAudioGlobals::fBufferSize,
									TAudioGlobals::fOutput);
 
    return true;
}



