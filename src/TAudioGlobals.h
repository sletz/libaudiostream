/*

Copyright © Grame 2002

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

//---------------------
// Class TAudioGlobals
//---------------------
/*!
\brief Global state.
*/

class AUDIO_EXPORTS TAudioGlobals
{

    private:

        static TAudioGlobals* fInstance;   	// Unique instance`
		static long fClientCount;

    public:

        static TAudioBuffer<short>* fInBuffer; 	// Shared buffer for Real-Time stream
        static long fInput;                     // Number of input channels
        static long fOutput;                    // Number of output channels
        static long fChannels;                  // Number of sound channels
        static long fSample_Rate;               // Sampling Rate
        static long fBuffer_Size;               // I/O Buffer size
        static long fStream_Buffer_Size;        // Stream Buffer size
        static long fRTStream_Buffer_Size;      // Real-Time Stream Buffer size
        static long fDiskError;                 // Counter of disk streaming errors

        TAudioGlobals(long inChan, long outChan, long channels, long sample_rate,
                      long buffer_size, long stream_buffer_size, long rtstream_buffer_size);
        virtual ~TAudioGlobals();

        static void Init(long inChan, long outChan, long channels,
                         long sample_rate, long buffer_size, long stream_buffer_size, 
                         long rtstream_buffer_size, long thread_num);

        static void LogError();
        static void Destroy();
};

typedef TAudioGlobals * TAudioGlobalsPtr;

#endif





