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

void TExpAudioMixer::ExecuteControlSlice(TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                                        map<SymbolicDate, audio_frames_t>& date_map, 
                                        audio_frames_t cur_frame, 
                                        long offset,
                                        long slice)
{
    COMMANDS_ITERATOR it = fControlCommands.begin();
    while (it != fControlCommands.end()) {
     
        TCommandPtr command = *it;
        long command_offset = command->GetOffset(cur_frame, slice);
        
        if (command_offset == offset) {
            if (command->Execute(shared_buffer, date_map, cur_frame, slice)) {
                it++;
            } else {
                //printf("fControlCommands.erase offset = %ld\n", command_offset);
                it = fControlCommands.erase(it);
            }
        } else if (command_offset > offset || command_offset == -1) {
            break;
        }
    }
}

/* 
    Returns the offset if next control if in buffer, or frames if not in buffer.
*/
long TExpAudioMixer::GetNextControlOffset(audio_frames_t cur_frame, long frames)
{
    if (fControlCommands.begin() != fControlCommands.end()) {
        return (*fControlCommands.begin())->GetOffset(cur_frame, frames);
    } else {
        return frames;
    }
}

/*
Render all stream inside a "slice" inside a buffer.
*/
void TExpAudioMixer::ExecuteStreamsSlice(TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                                        map<SymbolicDate, audio_frames_t>& date_map, 
                                        audio_frames_t cur_frame, 
                                        long offset,
                                        long slice)
{
    //printf("ExecuteStreamsSlice cur_frame %lld offset %ld slice %ld\n", cur_frame, offset, slice);
    
    float* temp[shared_buffer.GetChannels()];
    shared_buffer.GetFrame(offset, temp);
    TSharedNonInterleavedAudioBuffer<float> shared_buffer_imp(temp, slice, TAudioGlobals::fOutput);
    
    COMMANDS_ITERATOR it = fStreamCommands.begin();
    while (it != fStreamCommands.end()) {
        TCommandPtr command = *it;
        if (command->Execute(shared_buffer_imp, date_map, cur_frame + offset, slice)) {
            it++;
        } else {
            //printf("fStreamCommands.erase\n");
            it = fStreamCommands.erase(it);
        }
    }
}

bool TExpAudioMixer::AudioCallback(float** inputs, float** outputs, long frames)
{
    TSharedNonInterleavedAudioBuffer<float> shared_buffer(outputs, frames, TAudioGlobals::fOutput);
  
    // Real-time input
    TAudioGlobals::fSharedInput->Read(&shared_buffer, frames, 0);
   
    // Possibly sort commands and streams sequences
    fControlCommands.PossiblySort();
    fStreamCommands.PossiblySort();
     
    long offset_in_control = 0;
    long offset_in_stream = 0;
    long slice = 0;
    map<SymbolicDate, audio_frames_t> date_map;
    
    
    // Possibly render slices in presence of control
    while ((offset_in_control = GetNextControlOffset(fCurFrame, frames)) < frames) {
    
        // Render all streams inside this slice
        slice = offset_in_control - offset_in_stream;
        ExecuteStreamsSlice(shared_buffer, date_map, fCurFrame, offset_in_stream, slice);
        
        // Execute all controls at the same date
        ExecuteControlSlice(shared_buffer, date_map, fCurFrame, offset_in_control, frames);
        
        // Move to next slice
        offset_in_stream += slice;
    }
     
    // Render all streams inside last slice, which can be global buffer is no control
    slice = offset_in_control - offset_in_stream;
    ExecuteStreamsSlice(shared_buffer, date_map, fCurFrame, offset_in_stream, slice);
         
    // Update date in frames
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


