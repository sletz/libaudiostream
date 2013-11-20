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
#include "TBufferedInputAudioStream.h"

/*--------------------------------------------------------------------------*/
// Internal API
/*--------------------------------------------------------------------------*/

bool TExpAudioMixer::AudioCallback(float** inputs, float** outputs, long frames)
{
    TSharedNonInterleavedAudioBuffer<float> shared_buffer(outputs, frames, TAudioGlobals::fOutput);
  
    // Real-time input
    TAudioGlobals::fSharedInput->Read(&shared_buffer, frames, 0);
   
    // Mix all streams
     list<ScheduledStream>::iterator iter = fRunningStreamSeq.begin();
    
	while (iter != fRunningStreamSeq.end()) {
		ScheduledStream sc_stream = *iter;
        audio_frames_t date = sc_stream.fDate;
        TRTRendererAudioStreamPtr stream = sc_stream.fStream;
        
        long offset_in_buffer = 0;
        bool to_play = false;
        
        if (date >= fCurDate && date <= fCurDate + frames) {
            // New stream to play
            offset_in_buffer = date - fCurDate;
            to_play = true;
            printf("Start stream offet = %d\n", offset_in_buffer);
        } else if (fCurDate > date) {
            // Stream currently playing...
            to_play = true;
        }
        
        // Play it...
        if (to_play && stream->Read(&shared_buffer, TAudioGlobals::fBufferSize, offset_in_buffer) < TAudioGlobals::fBufferSize) {
            // End of stream
            printf("Stop stream\n");
            iter = fRunningStreamSeq.erase(iter);
        } else {
            iter++;
        }
    }
    
    // Update curdate
    fCurDate += frames;
    return true;
}




