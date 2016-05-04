/*

Copyright (C) Grame 2002-2014

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

/*#ifdef WIN32 
#pragma warning (disable : 4786)
#endif*/

#include "TExpAudioMixer.h"
#include "TBufferedInputAudioStream.h"

/*--------------------------------------------------------------------------*/
// Internal API
/*--------------------------------------------------------------------------*/

void TExpAudioMixer::ExecuteControlSlice(TNonInterleavedAudioBuffer<float>* buffer, 
                                        map<SymbolicDate, audio_frame_t>& date_map, 
                                        audio_frame_t cur_frame, 
                                        long offset_in_control,
                                        long control_slice)
{
    COMMANDS_ITERATOR it = fControlCommands.begin();
    while (it != fControlCommands.end()) {
     
        TCommandPtr command = *it;
        long command_offset = command->GetOffset(cur_frame, control_slice);
        
        if (command_offset == offset_in_control) {
            if (command->Execute(buffer, date_map, cur_frame, control_slice)) {
                it++;
            } else {
                //printf("fControlCommands.erase offset = %ld\n", command_offset);
                it = fControlCommands.erase(it);
            }
        } else if (command_offset > offset_in_control || command_offset == -1) {
            break;
        }
    }
}

/* 
    Returns the offset if next control if in buffer, or frames if not in buffer.
*/
long TExpAudioMixer::GetNextControlOffset(audio_frame_t cur_frame, long frames)
{
    if (fControlCommands.begin() != fControlCommands.end()) {
        return (*fControlCommands.begin())->GetOffset(cur_frame, frames);
    } else {
        return frames;
    }
}

/*
Render all streams inside a "slice" in a buffer.
*/
void TExpAudioMixer::ExecuteStreamsSlice(TNonInterleavedAudioBuffer<float>* buffer, 
                                        map<SymbolicDate, audio_frame_t>& date_map, 
                                        audio_frame_t cur_frame, 
                                        long offset_in_stream,
                                        long stream_slice)
{
    float** temp = (float**)alloca(buffer->GetChannels()*sizeof(float*));
    buffer->GetFrame(offset_in_stream, temp);
    TSharedNonInterleavedAudioBuffer<float> buffer_imp(temp, stream_slice, TAudioGlobals::fOutput);

    COMMANDS_ITERATOR it = fStreamCommands.begin();
    while (it != fStreamCommands.end()) {
        TCommandPtr command = *it;
        //printf("ExecuteStreamsSlice cur_frame %lld offset_in_stream %ld stream_slice %ld\n", cur_frame, offset_in_stream, stream_slice);
        if (command->Execute(&buffer_imp, date_map, cur_frame + offset_in_stream, stream_slice)) {
            it++;
        } else {
            //printf("fStreamCommands.erase\n");
            it = fStreamCommands.erase(it);
        }
    }
}

/*
    [       control               control           ]
    [ slice    |         slice       |      slice   ]

*/

bool TExpAudioMixer::AudioCallback(float** inputs, float** outputs, long frames)
{
    TNonInterleavedAudioBuffer<float>* buffer;
    
    TSharedNonInterleavedAudioBuffer<float> shared_buffer(outputs, frames, TAudioGlobals::fOutput);
    
    // Real-time input
    if (TAudioGlobals::fSharedInput) {
        TAudioGlobals::fSharedInput->Read(&shared_buffer, frames, 0);
    }

    // Prepare buffer for master effect
    if (fMasterEffect) {
        buffer = fBuffer;
        float** temp = (float**)alloca(buffer->GetChannels()*sizeof(float*));
        UAudioTools::ZeroFloatBlk(buffer->GetFrame(0, temp), frames, TAudioGlobals::fOutput);
    } else {
        buffer = &shared_buffer;
    }
 
    // Possibly sort commands and streams sequences
    fControlCommands.PossiblySort();
    fStreamCommands.PossiblySort();
     
    long offset_in_control = 0;
    long offset_in_stream = 0;
    long stream_slice = 0;
    map<SymbolicDate, audio_frame_t> date_map;

    // Possibly render slices in presence of control
    while ((offset_in_control = GetNextControlOffset(fCurFrame, frames)) < frames) {

        // Render all streams inside this slice
        stream_slice = offset_in_control - offset_in_stream;
        ExecuteStreamsSlice(buffer, date_map, fCurFrame, offset_in_stream, stream_slice);
        
        // Execute all controls at the same date
        ExecuteControlSlice(buffer, date_map, fCurFrame, offset_in_control, frames);
        
        // Move to next slice
        offset_in_stream += stream_slice;
    }
     
    // Render all streams inside last slice, which can be entire buffer if no control in it
    stream_slice = offset_in_control - offset_in_stream;
    ExecuteStreamsSlice(buffer, date_map, fCurFrame, offset_in_stream, stream_slice);
    
    // Apply master effect
    if (fMasterEffect) {
        float** temp = (float**)alloca(buffer->GetChannels()*sizeof(float*));
        fMasterEffect->Process(buffer->GetFrame(0, temp), outputs, frames);
    }
         
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

/*
    Semantic of SetPos(frames):
        - erase all commands which command_end_date are before "frames"
        - call SetPos (frames - command_start_date) for all commands where command_start_date < frames < command_end_date, set command_start_date to 0
        - change command_start_date to command_start_date - frames for all commands where frames < command_start_date
*/

long TExpAudioMixer::SetPos(audio_frame_t frames)
{
    // TODO
}
