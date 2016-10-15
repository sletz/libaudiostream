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

#ifndef __TAudioRenderer__
#define __TAudioRenderer__

#include "TAudioConstants.h"
#include "TAudioClient.h"
#include "AudioExports.h"
#include <list>
#include <stdint.h>

/*!
\brief To get renderer state.
*/
typedef struct RendererInfo * RendererInfoPtr;

typedef struct RendererInfo {
    long fInput;   				// Number of input channels
    long fOutput;   			// Number of output channels
    long fSampleRate; 			// Sampling Rate
    long fBufferSize;			// I/O Buffer size
    uint64_t fCurFrame;         // Currrent date in frames
    uint64_t fCurUsec;          // Current date in microsecond
    long fOutputLatencyFrame;   // Output latency in frames
    long fOutputLatencyUsec;    // Output latency in microsecond
    long fInputLatencyFrame;    // Input latency in frames
    long fInputLatencyUsec;     // Input latency in microsecond
} RendererInfo;

/*!
\brief Audio device info.
*/
typedef struct DeviceInfo* DeviceInfoPtr;

typedef struct DeviceInfo {
	char fName[64];      
	long fMaxInputChannels;
	long fMaxOutputChannels; 
	long fDefaultBufferSize; 
	double fDefaultSampleRate;
} DeviceInfo;

//----------------------
// Class TRTAudioClient
//----------------------
/*!
\brief For RT safe add/remove of clients.
*/

struct TRTAudioClient : public TAudioClient
{

	TAudioClient* fRTClient;

	TRTAudioClient(TAudioClient* client):fRTClient(client)
	{}
	virtual ~TRTAudioClient()
	{}

	virtual bool AudioCallback(float** inputs, float** outputs, long frames)
	{
		return true;
	}

};

//----------------------
// Class TAudioRenderer
//----------------------
/*!
\brief The base class for audio renderer.
*/

class AUDIO_EXPORTS TAudioRenderer
{

    protected:

        std::list<TRTAudioClient> fClientList;
        long fSampleRate;
        long fInput;
        long fOutput;
        long fBufferSize;

        void Run(float** inputs, float** outputs, long frames);
        
        virtual long OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate) = 0;

    public:

        TAudioRenderer():fSampleRate(0)
        {}
        virtual ~TAudioRenderer()
        {}

        void AddClient(TAudioClientPtr client)
        {
            fClientList.push_back(TRTAudioClient(client));
        }

        void RemoveClient(TAudioClientPtr client)
        {
			for (std::list<TRTAudioClient>::iterator iter = fClientList.begin(); iter != fClientList.end(); iter++) {
				if ((*iter).fRTClient == client) {
					(*iter).fRTClient = 0; // Mark client to be removed
                }
			}
	   }

        virtual long Open(long inChan, long outChan, long bufferSize, long sampleRate) = 0;
        virtual long Close() = 0;

        virtual long Start() = 0;
        virtual long Stop() = 0;
    
        virtual long Pause() = 0;
        virtual long Cont() = 0;

        virtual void GetInfo(RendererInfoPtr info) = 0;
		
        long GetInputs() { return fInput; }
        long GetOutputs() { return fOutput; }

        double ConvertSample2Ms(double sample)
        {
            return (double(sample) * 1000.0) / double(fSampleRate);
        }
        double ConvertMs2Sample(double ms)
        {
            return (double(ms) * double(fSampleRate)) / 1000.0;
        }
        
        double ConvertSample2Usec(double sample)
        {
            return (double(sample) * 1000000.0) / double(fSampleRate);
        }
        double ConvertUsec2Sample(double usec)
        {
            return (double(usec) * double(fSampleRate)) / 1000000.0;
        }

};

typedef TAudioRenderer * TAudioRendererPtr;

#endif



