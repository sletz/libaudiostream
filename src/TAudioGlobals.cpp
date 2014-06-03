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

#include "TAudioGlobals.h"
#include "TAudioConstants.h"
#include "TRendererAudioStream.h"
#include "TBufferedInputAudioStream.h"
#include "TSharedBuffers.h"
#include "TFaustAudioEffect.h"

#ifndef WIN32
	#include <sys/errno.h>
	#include <sys/resource.h>
#endif

// Globals
float** TSharedBuffers::fInBuffer = NULL;
float** TSharedBuffers::fOutBuffer = NULL;
long TSharedBuffers::fInputOffset = 0;
long TSharedBuffers::fOutputOffset = 0;

TCmdManagerPtr la_smartable1::fDeleteManager = NULL;
TCmdManagerPtr TCmdManager::fInstance = NULL;

TAudioGlobalsPtr TAudioGlobals::fInstance = NULL;
long TAudioGlobals::fClientCount = 0;

long TAudioGlobals::fInput = 0;
long TAudioGlobals::fOutput = 0;

long TAudioGlobals::fBufferSize = 0;
long TAudioGlobals::fStreamBufferSize = 0;

long TAudioGlobals::fSampleRate = 0;
long TAudioGlobals::fDiskError = 0;
long TAudioGlobals::fFileMax = 0;

long TAudioGlobals::fInputLatency = -1;
long TAudioGlobals::fOutputLatency = -1;

TBufferedAudioStream* TAudioGlobals::fSharedInput = NULL;

char* TAudioGlobals::fLastLibError = NULL;

TCmdManagerPtr TDTRendererAudioStream::fManager = 0;
TCmdManagerPtr TRTRendererAudioStream::fManager = 0;

// Local effect factory
std::map<string, TLocalCodeFaustAudioEffectFactory*> TAudioGlobals::fLocalFactoryTable;
int TAudioGlobals::fLocalFactoryNumber = 0;

// Remote effect factory
std::map<string, TRemoteCodeFaustAudioEffectFactory*> TAudioGlobals::fRemoteFactoryTable;
int TAudioGlobals::fRemoteFactoryNumber = 0;

// Effect table
std::map<std::string, list <TAudioEffectInterfacePtr> > TAudioGlobals::fEffectTable;

#ifdef WIN32
static int SetMaximumFiles(long filecount)
{
    return 0;
}
static int GetMaximumFiles(long* filecount) 
{
    return 0;
}

#else
static bool SetMaximumFiles(long filecount)
{
    struct rlimit lim;
    lim.rlim_cur = lim.rlim_max = (rlim_t)filecount;
    if (setrlimit(RLIMIT_NOFILE, &lim) == 0) {
        return true;
    } else {
        return false;
    }
}

static bool GetMaximumFiles(long* filecount) 
{
    struct rlimit lim;
    if (getrlimit(RLIMIT_NOFILE, &lim) == 0) {
        *filecount = (long)lim.rlim_max;
        return true;
    } else {
        return false;
	}
}
#endif

void TAudioGlobals::Init(long inChan, long outChan, long sample_rate,
                         long buffer_size, long stream_buffer_size, long rtstream_buffer_size, long thread_num)
{
	if (fClientCount++ == 0 && !fInstance) {
		fInstance = new TAudioGlobals(inChan, outChan, sample_rate,
									  buffer_size, stream_buffer_size, rtstream_buffer_size);
		TDTRendererAudioStream::Init();
		TRTRendererAudioStream::Init(thread_num);
		la_smartable1::Init();
		GetMaximumFiles(&fFileMax);
		SetMaximumFiles(1024);
	}
}

void TAudioGlobals::Destroy()
{
	if (--fClientCount == 0 && fInstance) {
		TDTRendererAudioStream::Destroy();
		TRTRendererAudioStream::Destroy();
		la_smartable1::Destroy();
		delete fInstance;
		fInstance = NULL;
		SetMaximumFiles(fFileMax);
	}
}

TAudioGlobals::TAudioGlobals(long inChan, long outChan, long sample_rate,
                             long buffer_size, long stream_buffer_size, long rtstream_duration)
{
    fInput = inChan;
    fOutput = outChan;
    fBufferSize = buffer_size;
    fStreamBufferSize = stream_buffer_size;
    fSampleRate = sample_rate;
    fDiskError = 0;
    // Allocate shared real-time input
    fSharedInput = new TBufferedInputAudioStream(rtstream_duration);  
    fLastLibError = new char[512];
}

TAudioGlobals::~TAudioGlobals()
{
    delete fSharedInput;
    delete [] fLastLibError;
}

void TAudioGlobals::LogError()
{
    printf("Disk Streaming errors : %ld\n", fDiskError);
}

void TAudioGlobals::ClearLibError()
{
    // First clear error message
    if (fLastLibError) {
        strncpy(fLastLibError, "", 512);
    }
}

void TAudioGlobals::AddLibError(char* error)
{
    strncpy(TAudioGlobals::fLastLibError, error, 512);
}

void TAudioGlobals::AddLibError(const char* error)
{
    strncpy(TAudioGlobals::fLastLibError, error, 512);
}

void TAudioGlobals::AddLibError(const std::string& error)
{
    strncpy(fLastLibError, error.c_str(), 512);
}
