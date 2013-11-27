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
#include <list>
#include <map>

//----------------------
// Class TExpAudioMixer
//----------------------
/*!
\brief A mixer contains several TAudioStream objects.
*/

class TExpAudioMixer : public TAudioClient
{

    private:
    
        typedef class LA_SMARTP<TRTRendererAudioStream> SAudioStream;
    
        struct ScheduledStream {
            
            SAudioStream fStream;  // SmartPtr here...
            
            SymbolicDate fStartDate;
            SymbolicDate fStopDate;
            
            ScheduledStream(TRTRendererAudioStreamPtr stream, SymbolicDate start_date, SymbolicDate stop_date)
                :fStream(stream), fStartDate(start_date), fStopDate(stop_date)
            {}
    
            bool operator< (ScheduledStream stream) 
            { 
                return fStartDate < stream.fStartDate; 
            }
            
            audio_frames_t getStartDate() { return fStartDate->getDate(); }
            audio_frames_t getStopDate() { return fStopDate->getDate(); }
            
        }; 
  
        list<ScheduledStream> fRunningStreamSeq;   // List of running sound streams
        audio_frames_t fCurFrame;
   
        bool AudioCallback(float** inputs, float** outputs, long frames);
        
        inline audio_frames_t GetDate(map<SymbolicDate, audio_frames_t>& date_map, SymbolicDate date);
      
    public:

        TExpAudioMixer():fCurFrame(0) {}
        virtual ~TExpAudioMixer() {}
      
        void StartStream(TAudioStreamPtr stream, SymbolicDate date)
        {
            TRTRendererAudioStreamPtr renderer_stream = new TRTRendererAudioStream(stream);
            renderer_stream->Reset();
            fRunningStreamSeq.push_back(ScheduledStream(renderer_stream, date, new TSymbolicDate()));
        }
        
        bool StopStream(TAudioStreamPtr stream2, SymbolicDate date)
        {
            list<ScheduledStream>::iterator it;
            for (it = fRunningStreamSeq.begin(); it != fRunningStreamSeq.end(); it++) {
                TRTRendererAudioStreamPtr stream1 = static_cast<TRTRendererAudioStreamPtr>((*it).fStream);
                if (stream1->GetBranch1() == stream2) {
                    (*it).fStopDate = date;
                    return true;
                }
            }
            return false;
        }
            
};

typedef TExpAudioMixer * TExpAudioMixerPtr;

#endif


