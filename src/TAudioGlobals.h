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

#ifndef __TAudioGlobals__
#define __TAudioGlobals__

#include "AudioExports.h"
#include "TAudioBuffer.h"
#include "TAudioEffectInterface.h"
#include <string>
#include <map>
#include <list>

#define MAX_OUTPUT_CHAN 64 // Used in TExpAudioMixer

//---------------------
// Class TAudioGlobals
//---------------------
/*!
\brief Global state.
*/

class TBufferedAudioStream;

class TLocalCodeFaustAudioEffectFactory;
class TRemoteCodeFaustAudioEffectFactory;

class AUDIO_EXPORTS TAudioGlobals
{

    private:

        static TAudioGlobals* fInstance;   	// Unique instance`
		static long fClientCount;

    public:

        /* static long fChannels;                     // ??? */
        static long fInput;                     // Number of input channels
        static long fOutput;                    // Number of output channels
        static long fSampleRate;                // Sampling Rate
        static long fBufferSize;                // I/O Buffer size
        static long fStreamBufferSize;          // Stream Buffer size
        static long fDiskError;                 // Counter of disk streaming errors
		static long fFileMax;

		static long fInputLatency;				// Suggested input latency (when used with PortAudio)
		static long fOutputLatency;				// Suggested output latency (when used with PortAudio)
        
        static char* fLastLibError;
    
        static TBufferedAudioStream* fSharedInput;  // Shared real-time input
        
        static std::map<std::string, TLocalCodeFaustAudioEffectFactory*> fLocalFactoryTable;     // Local effect factory 
        static int fLocalFactoryNumber;
         
        static std::map<std::string, TRemoteCodeFaustAudioEffectFactory*> fRemoteFactoryTable;   // Remote effect factory 
        static int fRemoteFactoryNumber;
       
        static std::map<std::string, std::list<TAudioEffectInterfacePtr> > fEffectTable;    // Effect table 
   
        TAudioGlobals(long inChan, long outChan, long sample_rate,
                      long buffer_size, long stream_buffer_size, long rtstream_buffer_size);
        virtual ~TAudioGlobals();

        static void Init(long inChan, long outChan,
                         long sample_rate, long buffer_size, long stream_buffer_size, 
                         long rtstream_buffer_size, long thread_num);

        static void LogError();
        static void Destroy();
        
        static void ClearLibError();
        static void AddLibError(char* error);
        static void AddLibError(const char* error);
        static void AddLibError(const std::string& error);
      
};

typedef TAudioGlobals * TAudioGlobalsPtr;

#endif





