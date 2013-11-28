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

#ifndef __TExpAudioMixer__
#define __TExpAudioMixer__

#include "TAudioClient.h"
#include "TAudioChannel.h"
#include "TAudioGlobals.h"
#include "TAudioDate.h"
#include "la_smartpointer.h"

#include <list>
#include <map>


//----------------
// Class TCommand
//----------------

struct TCommand : public la_smartable1 {
  
        inline audio_frames_t GetDate(map<SymbolicDate, audio_frames_t>& date_map, SymbolicDate date)
        {
            if (date_map.find(date) == date_map.end()) {
                date_map[date] = date->getDate();
            }
            return date_map[date];
        }
        
        TCommand() 
        {}
        virtual ~TCommand() 
        {}
         
        virtual bool Execute(TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                            map<SymbolicDate, audio_frames_t>& date_map, 
                            audio_frames_t cur_frame, 
                            long frames) = 0;
        
};

typedef class LA_SMARTP<TCommand> SCommand;

struct TControlCommand : public TCommand {
    
        string fEffectName;
        string fPath;
        float fValue;
        SymbolicDate fDate;
    
        TControlCommand() 
        {}
        TControlCommand(SymbolicDate date) : fDate(date)
        {}
        virtual ~TControlCommand() 
        {}
         
        virtual bool Execute(TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                            map<SymbolicDate, audio_frames_t>& date_map, 
                            audio_frames_t cur_frame, 
                            long frames)
        {
            if (cur_frame > fDate->getDate()) {
                printf("TControlCommand OK\n");
            }
            return true;
        }
};

typedef class LA_SMARTP<TRTRendererAudioStream> SAudioStream;
    
struct TStreamCommand : public TCommand {
        
        SAudioStream fStream;  // SmartPtr here...
            
        SymbolicDate fStartDate;
        SymbolicDate fStopDate;
 
        TStreamCommand(TRTRendererAudioStreamPtr stream, SymbolicDate start_date, SymbolicDate stop_date)
                :fStream(stream), fStartDate(start_date), fStopDate(stop_date)
        {}
        virtual ~TStreamCommand() 
        {}
        
        void SetSartDate(SymbolicDate start_date) { fStartDate = start_date; }
        void SetStopDate(SymbolicDate stop_date) { fStopDate = stop_date; }
         
        virtual bool Execute(TSharedNonInterleavedAudioBuffer<float>& shared_buffer, 
                            map<SymbolicDate, audio_frames_t>& date_map, 
                            audio_frames_t cur_frame, 
                            long frames)
        {
            // Keeps the same value for the entire audio cycle
            audio_frames_t start_date = GetDate(date_map, fStartDate);
            audio_frames_t stop_date = GetDate(date_map, fStopDate);
            
            long buffer_offset = 0;
            long frame_num = std::min((unsigned long)TAudioGlobals::fBufferSize, (unsigned long)(stop_date - cur_frame));
            bool to_play = false;
            long res = 0;
            
            if (start_date >= cur_frame && start_date < cur_frame + frames) {
                // New stream to play
                buffer_offset = start_date - cur_frame;
                to_play = true;
                printf("Start stream fCurFrame = %lld offset = %d\n", cur_frame, buffer_offset);
            } else if (cur_frame > start_date) {
                // Stream currently playing...
                to_play = true;
            }
            
            // Play it...
            if (to_play && ((res = fStream->Read(&shared_buffer, frame_num, buffer_offset)) < TAudioGlobals::fBufferSize)) {
                // End of stream
                printf("Stop stream frame_num = %d res = %d\n", frame_num, res);
                return false;
            } else {
                return true;
            }
        }

};

typedef class LA_SMARTP<TStreamCommand> SStreamCommand;

//----------------------
// Class TExpAudioMixer
//----------------------

class TExpAudioMixer : public TAudioClient
{

    private:
    
        list<SCommand> fRunningCommands;   // List of running sound streams
        audio_frames_t fCurFrame;
   
        bool AudioCallback(float** inputs, float** outputs, long frames);
      
    public:

        TExpAudioMixer():fCurFrame(0) {}
        virtual ~TExpAudioMixer() {}
        
        void AddCommand(SCommand command) { fRunningCommands.push_back(command); }
        void RemoveCommand(SCommand command) { fRunningCommands.remove(command); }
      
        SStreamCommand GetStreamCommand(TAudioStreamPtr stream);
    
};

typedef TExpAudioMixer * TExpAudioMixerPtr;

#endif


