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

//----------------------
// Class TExpAudioMixer
//----------------------
/*!
\brief A mixer contains several TAudioStream objects.
*/

typedef class LA_SMARTP<TAudioStream> SAudioStream;

class TExpAudioMixer : public TAudioClient
{

    private:
    
        struct ScheduledStream {
            
            SAudioStream fStream;  // SmartPtr here...
            audio_frames_t fDate;
            
            ScheduledStream(TRTRendererAudioStreamPtr stream, audio_frames_t date)
                :fStream(stream), fDate(date)
            {}
            
            bool operator< (ScheduledStream stream) 
            { 
                return fDate < stream.fDate; 
            }
            
        }; 
        
        struct IsStream {
        
            TAudioStreamPtr fStream;
            bool fFound;
            
            IsStream(TAudioStreamPtr stream):fStream(stream),fFound(false) {}
    
            bool operator() (ScheduledStream stream) 
            { 
                TRTRendererAudioStreamPtr s1 = dynamic_cast<TRTRendererAudioStreamPtr>(static_cast<TAudioStream*>(stream.fStream));
                fFound = (fStream == s1->GetBranch1());
                return fFound; 
            }
        };

        list<ScheduledStream>   fRunningStreamSeq;      // List of running sound streams
        audio_frames_t          fCurDate;
   
        bool AudioCallback(float** inputs, float** outputs, long frames);
      
    public:

        TExpAudioMixer():fCurDate(0) {}
        virtual ~TExpAudioMixer() {}
      
        void AddStream(TAudioStreamPtr stream)
        {
            ScheduleStream(stream, fCurDate);
        }
        
        bool RemoveStream(TAudioStreamPtr stream) 
        {
            IsStream comparator(stream);
            fRunningStreamSeq.remove_if(comparator); 
            return comparator.fFound;
        }
        
        void ScheduleStream(TAudioStreamPtr stream, audio_frames_t date)
        {
            TRTRendererAudioStreamPtr renderer_stream = new TRTRendererAudioStream(stream);
            renderer_stream->Reset();
            fRunningStreamSeq.push_back(ScheduledStream(renderer_stream, date));
            //fRunningStreamSeq.sort();
        }
        
        bool UnScheduleStream(TAudioStreamPtr stream, audio_frames_t date)
        {
            return false;
        }
    
};

typedef TExpAudioMixer * TExpAudioMixerPtr;

#endif




