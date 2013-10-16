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
#include <list>

//----------------------
// Class TExpAudioMixer
//----------------------
/*!
\brief A mixer contains several TAudioStream objects.
*/

class TExpAudioMixer : public TAudioClient
{

    private:

        list<TRTRendererAudioStreamPtr> fStreamSeq;	// List of running sound streams
   
        bool AudioCallback(float** inputs, float** outputs, long frames);
        
        // a predicate implemented as a function:
        struct is_stream {
            TAudioStreamPtr fStream;
            is_stream(TAudioStreamPtr stream):fStream(stream) {}
            bool operator() (TRTRendererAudioStreamPtr stream) { return (fStream == stream->GetBranch1()); }
        };

    public:

        TExpAudioMixer() {}
        virtual ~TExpAudioMixer() {}
      
        void AddStream(TAudioStreamPtr stream)      
        {   
            // We assume stream->Channels() < MAX_OUTPUT_CHAN here
            TRTRendererAudioStreamPtr renderer_stream = new TRTRendererAudioStream(stream);
            renderer_stream->Reset();
            fStreamSeq.push_back(renderer_stream); 
        }
        void RemoveStream(TAudioStreamPtr stream)   
        { 
            fStreamSeq.remove_if(is_stream(stream)); 
        }
    
};

typedef TExpAudioMixer * TExpAudioMixerPtr;

#endif




