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
   
    // Excute all commands
    list<SCommand>::iterator iter = fRunningCommands.begin();
    map<SymbolicDate, audio_frames_t> date_map;
    
	while (iter != fRunningCommands.end()) {
        SCommand command = *iter;
        if (command->Execute(shared_buffer, date_map, fCurFrame, frames)) {
            iter++;
        } else {
            iter = fRunningCommands.erase(iter);
        }
    }
    
    // Update date
    fCurFrame += frames;
    return true;
}

SStreamCommand TExpAudioMixer::GetStreamCommand(TAudioStreamPtr stream)
{
    list<SCommand>::iterator it;
    for (it = fRunningCommands.begin(); it != fRunningCommands.end(); it++) {
        TCommand* command = (*it);
        TStreamCommand* stream_command = dynamic_cast<TStreamCommand*>(command);
        if (stream_command) {
            TRTRendererAudioStreamPtr stream1 = static_cast<TRTRendererAudioStreamPtr>(stream_command->fStream);
            if (stream1->GetBranch1() == stream) {
                return stream_command;
            }
        }
    }
    return 0;
}


