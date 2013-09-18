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

#ifndef __TAudioRenderer__
#define __TAudioRenderer__

#include "TAudioClient.h"
#include "AudioExports.h"
#include <list>
#include <stdint.h>

using namespace std;

#define NO_ERR               0
#define OPEN_ERR            -1
#define CLOSE_ERR           -2
#define LOAD_ERR            -3
#define FILE_NOT_FOUND_ERR  -4

typedef struct RendererInfo * RendererInfoPtr;
/*!
\brief To get renderer state.
*/
typedef struct RendererInfo {
    long fInput;   				// Number of input channels
    long fOutput;   			// Number of output channels
    long fSampleRate; 			// Sampling Rate
    long fBufferSize;			// I/O Buffer size
    uint64_t fCurFrame;			// Currrent sample
    uint64_t fCurUsec;			// Current microsecond
    long fOutputLatencyFrame;	// Output latency in frames
    long fOutputLatencyUsec;	// Output latency in microsecond
    long fInputLatencyFrame;	// Input latency in frames
    long fInputLatencyUsec;		// Input latency in microsecond
} RendererInfo;

typedef struct DeviceInfo* DeviceInfoPtr;
/*!
\brief Audio device info.
*/
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

	virtual bool AudioCallback(float** inputBuffer, float** outputBuffer, long frames)
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

        list<TRTAudioClient> fClientList;
        long fSampleRate;
        long fInput;
        long fOutput;
        long fBufferSize;

        void Run(float** inputBuffer, float** outputBuffer, long frames);

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
			for (list<TRTAudioClient>::iterator iter = fClientList.begin(); iter != fClientList.end(); iter++) {
				if ((*iter).fRTClient == client) {
					(*iter).fRTClient = 0; // Mark client to be removed
                }
			}
	   }

        virtual long OpenDefault(long inChan, long outChan, long bufferSize, long sampleRate) = 0;
		virtual long Open(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate) = 0;
        virtual long Close() = 0;

        virtual long Start() = 0;
        virtual long Stop() = 0;

        virtual void GetInfo(RendererInfoPtr info) = 0;
		
		virtual long GetDeviceCount() = 0;
		virtual void GetDeviceInfo(long deviceNum, DeviceInfoPtr info) = 0;
		virtual long GetDefaultInputDevice() = 0;
		virtual long GetDefaultOutputDevice() = 0;
        
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



