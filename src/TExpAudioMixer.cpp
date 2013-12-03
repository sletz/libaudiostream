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

static bool compare_command_date (TCommandPtr first, TCommandPtr second)
{
    return first->GetDate() < second->GetDate();
}

bool TExpAudioMixer::AudioCallback(float** inputs, float** outputs, long frames)
{
    TSharedNonInterleavedAudioBuffer<float> shared_buffer(outputs, frames, TAudioGlobals::fOutput);
  
    // Real-time input
    TAudioGlobals::fSharedInput->Read(&shared_buffer, frames, 0);
   
    // Excute all commands
    COMMANDS_ITERATOR it = fRunningCommands.begin();
    map<SymbolicDate, audio_frames_t> date_map;
   
    /*
    vector<COMMANDS_ITERATOR> to_erase;
    for (it = fRunningCommands.begin(); it != fRunningCommands.end(); it++) {
        TCommandPtr command = *it;
        if (!command->Execute(shared_buffer, date_map, fCurFrame, frames)) {
            to_erase.push_back(it);
        } 
    }
    
    // Erase 
    for (int i = 0; i < to_erase.size(); i++) {
        fRunningCommands.erase(to_erase[i]);
    }
    */

    fRunningCommands.sort(compare_command_date); 
	while (it != fRunningCommands.end()) {
        TCommandPtr command = *it;
        if (command->Execute(shared_buffer, date_map, fCurFrame, frames)) {
            it++;
        } else {
            it = fRunningCommands.erase(it);
        }
    }
    
    // Update date
    fCurFrame += frames;
    return true;
}

TStreamCommandPtr TExpAudioMixer::GetStreamCommand(TAudioStreamPtr stream)
{
    COMMANDS_ITERATOR it;
    
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


