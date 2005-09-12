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

#include "TAudioGlobals.h"
#include "TAudioConstants.h"
#include "TPanTable.h"
#include "TRendererAudioStream.h"
#include "TSharedBuffers.h"

// Globals
float* TSharedBuffers::fInBuffer = NULL;
float* TSharedBuffers::fOutBuffer = NULL;

float TPanTable::fPanTable[128];
float TPanTable::fVolTable[128];

TCmdManagerPtr smartable1::fManager = NULL;
TCmdManagerPtr TCmdManager::fInstance = NULL;
TAudioGlobalsPtr TAudioGlobals::fInstance = NULL;
long TAudioGlobals::fClientCount = 0;
TAudioBuffer<short>* TAudioGlobals::fInBuffer = 0;

long TAudioGlobals::fInput = 0;
long TAudioGlobals::fOutput = 0;
long TAudioGlobals::fChannels = 0;

long TAudioGlobals::fBuffer_Size = 0;
long TAudioGlobals::fStream_Buffer_Size = 0;
long TAudioGlobals::fRTStream_Buffer_Size = 0;

long TAudioGlobals::fSample_Rate = 0;
long TAudioGlobals::fDiskError = 0;

TCmdManagerPtr TDTRendererAudioStream::fManager = 0;
TCmdManagerPtr TRTRendererAudioStream::fManager = 0;

void TAudioGlobals::Init(long inChan, long outChan, long channels, long sample_rate,
                         long buffer_size, long stream_buffer_size, long rtstream_buffer_size, long thread_num)
{
	if (fClientCount++ == 0 && !fInstance) {
		fInstance = new TAudioGlobals(inChan, outChan, channels, sample_rate,
									  buffer_size, stream_buffer_size, rtstream_buffer_size);
		TDTRendererAudioStream::Init();
		TRTRendererAudioStream::Init(thread_num);
		smartable1::Init();
		TPanTable::FillTable();
	}
}

void TAudioGlobals::Destroy()
{
	if (--fClientCount == 0 && fInstance) {
		TDTRendererAudioStream::Destroy();
		TRTRendererAudioStream::Destroy();
		smartable1::Destroy();
		delete fInstance;
		fInstance = NULL;
	}
}

TAudioGlobals::TAudioGlobals(long inChan, long outChan, long channels, long sample_rate,
                             long buffer_size, long stream_buffer_size, long rtstream_buffer_size)
{
    fInBuffer = new TLocalAudioBuffer<short>(rtstream_buffer_size, inChan);
    assert(fInBuffer);
    fInput = inChan;
    fOutput = outChan;
    fChannels = channels;
    fBuffer_Size = buffer_size;
    fStream_Buffer_Size = stream_buffer_size;
    fRTStream_Buffer_Size = rtstream_buffer_size;
    fSample_Rate = sample_rate;
    fDiskError = 0;
}

TAudioGlobals::~TAudioGlobals()
{
    delete fInBuffer;
}

void TAudioGlobals::LogError()
{
    printf("Disk Streaming errors : %ld\n", fDiskError);
}
