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

#ifndef __TCoreAudioRenderer__
#define __TCoreAudioRenderer__

#include <vector>
#include "TAudioRenderer.h"
#include <AudioToolbox/AudioConverter.h>
#include <CoreAudio/CoreAudio.h>
#include <AudioUnit/AudioUnit.h>
#include <CoreServices/CoreServices.h>

//--------------------------
// Class TCoreAudioRenderer
//--------------------------
/*!
\brief Use the CoreAudio API to access sound devices.
*/

#define WAIT_COUNTER 60
#define WAIT_NOTIFICATION_COUNTER 30

class TCoreAudioRenderer : public TAudioRenderer
{

    private:

		AudioBufferList* fInputData;
		AudioDeviceID fDeviceID;
		AudioUnit fAUHAL;
        static AudioObjectID gPluginID;             // Used for aggregate device
        static AudioDeviceID gAggregateDeviceID;    // Used for aggregate device
        static bool fState;
    
        AudioTimeStamp fCallbackTime;
        UInt64 fCallbackHostTime;
    
        RendererInfo fInfo;
    
        Float64 fAnchorFrameTime;   // Time stamp of the begining of rendering
        UInt64 fAnchorHostTime;     // Time stamp of the begining of rendering
   	
		static OSStatus GetDefaultDevice(int inChan, int outChan, int samplerate, AudioDeviceID* id);
        static int SetupSampleRateAux(AudioDeviceID inDevice, long samplerate);
        int SetupBufferSize(long buffer_size);
        
        int Render(AudioUnitRenderActionFlags *ioActionFlags,
                 const AudioTimeStamp *inTimeStamp,
                 UInt32 inBusNumber,
                 UInt32 inNumberFrames,
                 AudioBufferList *ioData);

		static OSStatus Render(void *inRefCon,
                               AudioUnitRenderActionFlags *ioActionFlags,
                               const AudioTimeStamp *inTimeStamp,
                               UInt32 inBusNumber,
                               UInt32 inNumberFrames,
                               AudioBufferList *ioData);
    
        static OSStatus SRNotificationCallback(AudioDeviceID inDevice,
                                                UInt32 inChannel,
                                                Boolean isInput,
                                                AudioDevicePropertyID inPropertyID,
                                                void* inClientData);
                                                
        static OSStatus BSNotificationCallback(AudioDeviceID inDevice,
                                                UInt32 inChannel,
                                                Boolean	isInput,
                                                AudioDevicePropertyID inPropertyID,
                                                void* inClientData);
                                       
        static OSStatus GetDeviceNameFromID(AudioDeviceID id, char* name);
                                            
        static OSStatus CreateAggregateDevice(AudioDeviceID captureDeviceID, AudioDeviceID playbackDeviceID, int samplerate, AudioDeviceID* outAggregateDevice);
		static OSStatus CreateAggregateDeviceAux(std::vector<AudioDeviceID> captureDeviceID, std::vector<AudioDeviceID> playbackDeviceID, int samplerate, AudioDeviceID* outAggregateDevice);
        static OSStatus DestroyAggregateDevice(AudioDeviceID id);
        
        static void InitTime();
        static double GetMicroSeconds();
        
        long OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
  
    public:

        TCoreAudioRenderer(): TAudioRenderer(),fInputData(0),fDeviceID(0),
            fAUHAL(0), fCallbackHostTime(0),
            fAnchorFrameTime(0.),fAnchorHostTime(0)
        {}
        virtual ~TCoreAudioRenderer()
        {}

        long Open(long inChan, long outChan, long bufferSize, long sampleRate);
        long Close();

        long Start();
        long Stop();
    
        long Pause();
        long Cont();
	
        void GetInfo(RendererInfoPtr info);
		
		static long GetDeviceCount();
		static void GetDeviceInfo(long deviceNum, DeviceInfoPtr info);
		static long GetDefaultInputDevice();
		static long GetDefaultOutputDevice();
};

typedef TCoreAudioRenderer * TCoreAudioRendererPtr;

#endif



