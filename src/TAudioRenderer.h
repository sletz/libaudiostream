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

#ifndef __TAudioRenderer__
#define __TAudioRenderer__

#include "TAudioClient.h"
#include "AudioExports.h"
#include <list>

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
    long fCurFrame;				// Currrent sample
    long fCurMs;				// Current millisecond
    long fOutputLatencyFrame;	// Ouput latency in frames
    long fOutputLatencyMs;		// Ouput latency in millisecond
    long fInputLatencyFrame;	// Input latency in frames
    long fInputLatencyMs;		// Input latency in millisecond
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
// Class TAudioRenderer
//----------------------
/*!
\brief The base class for audio renderer.
*/

class AUDIO_EXPORTS TAudioRenderer
{

    protected:

        list<TAudioClientPtr> fClientList;
        long fSampleRate;
        long fInput;
        long fOutput;
        long fBufferSize;

        void Run(float* inputBuffer, float* outputBuffer, long frames);

    public:

        TAudioRenderer():fSampleRate(0)
        {}
        virtual ~TAudioRenderer()
        {}

        void AddClient(TAudioClientPtr client)
        {
            fClientList.push_back(client);
        }
        void RemoveClient(TAudioClientPtr client)
        {
            fClientList.remove(client);
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

        long ConvertSample2Ms(long sample)
        {
            return long((float(sample) * 1000.0f) / fSampleRate);
        }
        long ConvertMs2Sample(long ms)
        {
            return long((float(ms) * fSampleRate) / 1000.0f);
        }
};

typedef TAudioRenderer * TAudioRendererPtr;

#endif



