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

/*
bool TExpAudioMixer::AudioCallback(float** inputs, float** outputs, long frames)
{
    TSharedNonInterleavedAudioBuffer<float> shared_buffer(outputs, frames, TAudioGlobals::fOutput);
  
    // Real-time input
    TAudioGlobals::fSharedInput->Read(&shared_buffer, frames, 0);
   
    // Excute all commands
    COMMANDS_ITERATOR it;
    map<SymbolicDate, audio_frames_t> date_map;
  
    fControlCommands.PossiblySort();
    fStreamCommands.PossiblySort();
 
    it = fStreamCommands.begin();
    while (it != fStreamCommands.end()) {
        TCommandPtr command = *it;
        if (command->Execute(shared_buffer, date_map, fCurFrame, frames)) {
            it++;
        } else {
            it = fStreamCommands.erase(it);
            //printf("fStreamCommands size %d\n", fStreamCommands.size());
        }
    }
    
    // Update date
    fCurFrame += frames;
    return true;
}
*/

/*
Execute all control commands that have the same date, and return the date of those controls as on "offset" in the buffer.
*/
COMMANDS_ITERATOR TExpAudioMixer::ExecuteControlSlice(COMMANDS_ITERATOR it, 
                                        TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                                        map<SymbolicDate, audio_frames_t>& date_map, 
                                        audio_frames_t cur_frame, 
                                        long frames,
                                        long& offset)
{
    while (it != fControlCommands.end()) {
    
        TCommandPtr command = *it;
        long command_offset = command->GetOffset(cur_frame, frames);
        
        if (offset == 0) {
            // Keep current offset
            offset = command_offset;
        } else if (command_offset != offset)  {
            // New offset, so break here
            break;
        }
        
        // Execute comand at the same offset
        if (command->Execute(shared_buffer, date_map, cur_frame, frames)) {
            it++;
        } else {
            printf("fControlCommands.erase \n");
            it = fControlCommands.erase(it);
        }
        
    } 
    
    printf("ExecuteControlSlice offset %d\n", offset);
    return it;
}

/*


*/
void TExpAudioMixer::ExecuteStreamsSlice(TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                                        map<SymbolicDate, audio_frames_t>& date_map, 
                                        audio_frames_t cur_frame, 
                                        long frames)
{
    
    printf("ExecuteStreamsSlice cur_frame %lld  frames %d\n", cur_frame, frames);
    
    COMMANDS_ITERATOR it = fStreamCommands.begin();
    while (it != fStreamCommands.end()) {
        TCommandPtr command = *it;
        if (command->Execute(shared_buffer, date_map, cur_frame, frames)) {
            it++;
        } else {
            it = fStreamCommands.erase(it);
        }
    }
}

bool TExpAudioMixer::AudioCallback(float** inputs, float** outputs, long frames)
{
    TSharedNonInterleavedAudioBuffer<float> shared_buffer(outputs, frames, TAudioGlobals::fOutput);
  
    // Real-time input
    TAudioGlobals::fSharedInput->Read(&shared_buffer, frames, 0);
   
    map<SymbolicDate, audio_frames_t> date_map;
  
    fControlCommands.PossiblySort();
    fStreamCommands.PossiblySort();
    
    printf("AudioCallback %ld \n", frames);
    //ExecuteStreamsSlice(0, shared_buffer, date_map, fCurFrame, frames);
    
    COMMANDS_ITERATOR it = fControlCommands.begin();
    
    long previous_offset = 0;
    
    do {
    
        long offset = 0;
        
        // Execute all controls at the same date, return the offset in buffer
        it = ExecuteControlSlice(it, shared_buffer, date_map, fCurFrame, frames, offset);
        
        // Render all streams inside this slice
        float* temp[shared_buffer.GetChannels()];
        shared_buffer.GetFrame(offset, temp);
        TSharedNonInterleavedAudioBuffer<float> shared_buffer_imp(temp, frames - offset, TAudioGlobals::fOutput);
        ExecuteStreamsSlice(shared_buffer_imp, date_map, fCurFrame + offset, (offset - previous_offset));
        
        previous_offset += offset;
    
    } while (it != fControlCommands.end());
       
    // Update date
    fCurFrame += frames;
    return true;
}


TStreamCommandPtr TExpAudioMixer::GetStreamCommand(TAudioStreamPtr stream)
{
    COMMANDS_ITERATOR it;
    
    for (it = fStreamCommands.begin(); it != fStreamCommands.end(); it++) {
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


