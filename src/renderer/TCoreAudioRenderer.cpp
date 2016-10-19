/*

Copyright (C) Grame 2002-2014

This library is free software; you can redistribute it and modify it under 
the terms of the GNU Library General Public License as published by the 
Free Software Foundation version 2 of the License, or any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License printf
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
research@grame.fr

*/

#include "TCoreAudioRenderer.h"
#include "TSharedBuffers.h"
#include "TAudioGlobals.h"
#include "UTools.h"
#include <mach/mach_time.h>
#include <sys/time.h>

#define WAIT_COUNTER 60

typedef	UInt8 CAAudioHardwareDeviceSectionID;

#define	kAudioDeviceSectionInput	((CAAudioHardwareDeviceSectionID)0x01)
#define	kAudioDeviceSectionOutput	((CAAudioHardwareDeviceSectionID)0x00)
#define	kAudioDeviceSectionGlobal	((CAAudioHardwareDeviceSectionID)0x00)
#define	kAudioDeviceSectionWildcard	((CAAudioHardwareDeviceSectionID)0xFF)

#define DEBUG 1

AudioObjectID TCoreAudioRenderer::gPluginID = 0;
bool TCoreAudioRenderer::fState = false;
AudioDeviceID TCoreAudioRenderer::gAggregateDeviceID = 0;

static void PrintStreamDesc(AudioStreamBasicDescription *inDesc)
{
    printf("- - - - - - - - - - - - - - - - - - - -\n");
    printf("  Sample Rate:%f\n", inDesc->mSampleRate);
    printf("  Format ID:%.*s\n", (int) sizeof(inDesc->mFormatID), (char*)&inDesc->mFormatID);
    printf("  Format Flags:%lX\n", (unsigned long)inDesc->mFormatFlags);
    printf("  Bytes per Packet:%ld\n", (unsigned long)inDesc->mBytesPerPacket);
    printf("  Frames per Packet:%ld\n", (unsigned long)inDesc->mFramesPerPacket);
    printf("  Bytes per Frame:%ld\n", (unsigned long)inDesc->mBytesPerFrame);
    printf("  Channels per Frame:%ld\n", (unsigned long)inDesc->mChannelsPerFrame);
    printf("  Bits per Channel:%ld\n", (unsigned long)inDesc->mBitsPerChannel);
    printf("- - - - - - - - - - - - - - - - - - - -\n");
}

static void printError(OSStatus err)
{
#ifdef DEBUG
    switch (err) {
        case kAudioHardwareNoError:
            printf("error code : kAudioHardwareNoError\n");
            break;
		case kAudioConverterErr_FormatNotSupported:
            printf("error code : kAudioConverterErr_FormatNotSupported\n");
            break;
        case kAudioConverterErr_OperationNotSupported:
            printf("error code : kAudioConverterErr_OperationNotSupported\n");
            break;
        case kAudioConverterErr_PropertyNotSupported:
            printf("error code : kAudioConverterErr_PropertyNotSupported\n");
            break;
        case kAudioConverterErr_InvalidInputSize:
            printf("error code : kAudioConverterErr_InvalidInputSize\n");
            break;
        case kAudioConverterErr_InvalidOutputSize:
            printf("error code : kAudioConverterErr_InvalidOutputSize\n");
            break;
        case kAudioConverterErr_UnspecifiedError:
            printf("error code : kAudioConverterErr_UnspecifiedError\n");
            break;
        case kAudioConverterErr_BadPropertySizeError:
            printf("error code : kAudioConverterErr_BadPropertySizeError\n");
            break;
        case kAudioConverterErr_RequiresPacketDescriptionsError:
            printf("error code : kAudioConverterErr_RequiresPacketDescriptionsError\n");
            break;
        case kAudioConverterErr_InputSampleRateOutOfRange:
            printf("error code : kAudioConverterErr_InputSampleRateOutOfRange\n");
            break;
        case kAudioConverterErr_OutputSampleRateOutOfRange:
            printf("error code : kAudioConverterErr_OutputSampleRateOutOfRange\n");
            break;
		case kAudioHardwareNotRunningError:
            printf("error code : kAudioHardwareNotRunningError\n");
            break;
        case kAudioHardwareUnknownPropertyError:
            printf("error code : kAudioHardwareUnknownPropertyError\n");
            break;
        case kAudioHardwareIllegalOperationError:
            printf("error code : kAudioHardwareIllegalOperationError\n");
            break;
        case kAudioHardwareBadDeviceError:
            printf("error code : kAudioHardwareBadDeviceError\n");
            break;
        case kAudioHardwareBadStreamError:
            printf("error code : kAudioHardwareBadStreamError\n");
            break;
        case kAudioDeviceUnsupportedFormatError:
            printf("error code : kAudioDeviceUnsupportedFormatError\n");
            break;
        case kAudioDevicePermissionsError:
            printf("error code : kAudioDevicePermissionsError\n");
            break;
        default:
            printf("error code : unknown\n");
            break;
    }
#endif
}

OSStatus TCoreAudioRenderer::Render(void *inRefCon,
                                     AudioUnitRenderActionFlags *ioActionFlags,
                                     const AudioTimeStamp *inTimeStamp,
                                     UInt32 inBusNumber,
                                     UInt32 inNumberFrames,
                                     AudioBufferList *ioData)
{
    TCoreAudioRendererPtr renderer = (TCoreAudioRendererPtr)inRefCon;
    return renderer->Render(ioActionFlags, inTimeStamp, inBusNumber, inNumberFrames, ioData);
}

int TCoreAudioRenderer::Render(AudioUnitRenderActionFlags *ioActionFlags,
                             const AudioTimeStamp *inTimeStamp,
                             UInt32 inBusNumber,
                             UInt32 inNumberFrames,
                             AudioBufferList *ioData)
{

    // Signal waiting start function...
    fState = true;
    
    if (fInput > 0) {
        AudioUnitRender(fAUHAL, ioActionFlags, inTimeStamp, 1, inNumberFrames, fInputData);
    }
    
    // Keep time
    fCallbackTime = *inTimeStamp;
    fCallbackHostTime = AudioGetCurrentHostTime();
    
    // Take time stamp of first call to Process 
    if (fAnchorHostTime == 0) {
        fAnchorFrameTime = fCallbackTime.mSampleTime;
        fAnchorHostTime = fCallbackHostTime;
    }
    
    float* inputBuffers[fInput];
    float* outputBuffers[fOutput];
    
    for (int i = 0; i < fInput; i++) {
        inputBuffers[i] = (float*)fInputData->mBuffers[i].mData;
    }
    
    for (int i = 0; i < fOutput; i++) {
        memset((float*)ioData->mBuffers[i].mData, 0, ioData->mBuffers[i].mDataByteSize); // Necessary since renderer does a *mix*
        outputBuffers[i] = (float*)ioData->mBuffers[i].mData;
    }
  
    Run(inputBuffers, outputBuffers, inNumberFrames);
	return 0;
}

static CFStringRef GetDeviceName(AudioDeviceID id)
{
    UInt32 size = sizeof(CFStringRef);
    CFStringRef UIname;
    OSStatus err = AudioDeviceGetProperty(id, 0, false, kAudioDevicePropertyDeviceUID, &size, &UIname);
    return (err == noErr) ? UIname : NULL;
}

OSStatus TCoreAudioRenderer::GetDeviceNameFromID(AudioDeviceID id, char* name)
{
    UInt32 size = 256;
    return AudioDeviceGetProperty(id, 0, false, kAudioDevicePropertyDeviceName, &size, name);
}

OSStatus TCoreAudioRenderer::GetDefaultDevice(int inChan, int outChan, int samplerate, AudioDeviceID* id)
{
    UInt32 theSize = sizeof(UInt32);
    AudioDeviceID inDefault;
    AudioDeviceID outDefault;
	OSStatus res;

    if ((res = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultInputDevice,
                                        &theSize, &inDefault)) != noErr) {
        return res;
    }

    if ((res = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice,
                                        &theSize, &outDefault)) != noErr) {
        return res;
    }
	
	// Duplex mode
	if (inChan > 0 && outChan > 0) {
		// Get the device only if default input and output are the same
		if (inDefault == outDefault) {
			*id = inDefault;
			return noErr;
		} else {
			printf("GetDefaultDevice : input = %ld and output = %ld are not the same \n", (unsigned long)inDefault, (unsigned long)outDefault);
            if (gAggregateDeviceID == 0) { // Create aggregate device the first time
                if (CreateAggregateDevice(inDefault, outDefault, samplerate, id) != noErr) {
                    return kAudioHardwareBadDeviceError;
                }
                gAggregateDeviceID = *id;
                printf("GetDefaultDevice : new fAgregateDeviceID = %d\n", gAggregateDeviceID);
            } else {
                printf("GetDefaultDevice : fAgregateDeviceID = %d already created \n", gAggregateDeviceID);
                *id = gAggregateDeviceID;
            }
		}
	} else if (inChan > 0) {
		*id = inDefault;
		return noErr;
	} else if (outChan > 0) {
		*id = outDefault;
		return noErr;
	} else {
		return kAudioHardwareBadDeviceError;
	}
	
	return noErr;
}

OSStatus TCoreAudioRenderer::CreateAggregateDevice(AudioDeviceID captureDeviceID, AudioDeviceID playbackDeviceID, int samplerate, AudioDeviceID* outAggregateDevice)
{
    OSStatus err = noErr;
    AudioObjectID sub_device[32];
    UInt32 outSize = sizeof(sub_device);

    err = AudioDeviceGetProperty(captureDeviceID, 0, kAudioDeviceSectionGlobal, kAudioAggregateDevicePropertyActiveSubDeviceList, &outSize, sub_device);
    std::vector<AudioDeviceID> captureDeviceIDArray;

    if (err != noErr) {
        //printf("Input device does not have subdevices\n");
        captureDeviceIDArray.push_back(captureDeviceID);
    } else {
        int num_devices = outSize / sizeof(AudioObjectID);
        //printf("Input device has %d subdevices\n", num_devices);
        for (int i = 0; i < num_devices; i++) {
            captureDeviceIDArray.push_back(sub_device[i]);
        }
    }

    outSize = sizeof(sub_device);
    err = AudioDeviceGetProperty(playbackDeviceID, 0, kAudioDeviceSectionGlobal, kAudioAggregateDevicePropertyActiveSubDeviceList, &outSize, sub_device);
    std::vector<AudioDeviceID> playbackDeviceIDArray;

    if (err != noErr) {
        //printf("Output device does not have subdevices\n");
        playbackDeviceIDArray.push_back(playbackDeviceID);
    } else {
        int num_devices = outSize / sizeof(AudioObjectID);
        //printf("Output device has %d subdevices\n", num_devices);
        for (int i = 0; i < num_devices; i++) {
            playbackDeviceIDArray.push_back(sub_device[i]);
        }
    }

    return CreateAggregateDeviceAux(captureDeviceIDArray, playbackDeviceIDArray, samplerate, outAggregateDevice);
}

OSStatus TCoreAudioRenderer::CreateAggregateDeviceAux(std::vector<AudioDeviceID> captureDeviceID, std::vector<AudioDeviceID> playbackDeviceID, int samplerate, AudioDeviceID* outAggregateDevice)
{
    OSStatus osErr = noErr;
    UInt32 outSize;
    Boolean outWritable;

    bool fClockDriftCompensate = true;

    // Prepare sub-devices for clock drift compensation
    // Workaround for bug in the HAL : until 10.6.2
    AudioObjectPropertyAddress theAddressOwned = { kAudioObjectPropertyOwnedObjects, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster };
    AudioObjectPropertyAddress theAddressDrift = { kAudioSubDevicePropertyDriftCompensation, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster };
    UInt32 theQualifierDataSize = sizeof(AudioObjectID);
    AudioClassID inClass = kAudioSubDeviceClassID;
    void* theQualifierData = &inClass;
    UInt32 subDevicesNum = 0;

    //---------------------------------------------------------------------------
    // Setup SR of both devices otherwise creating AD may fail...
    //---------------------------------------------------------------------------
    UInt32 keptclockdomain = 0;
    UInt32 clockdomain = 0;
    outSize = sizeof(UInt32);
    bool need_clock_drift_compensation = false;

    for (UInt32 i = 0; i < captureDeviceID.size(); i++) {
        if (SetupSampleRateAux(captureDeviceID[i], samplerate) < 0) {
            printf("TCoreAudioRenderer::CreateAggregateDevice : cannot set SR of input device\n");
        } else  {
            // Check clock domain
            osErr = AudioDeviceGetProperty(captureDeviceID[i], 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyClockDomain, &outSize, &clockdomain);
            if (osErr != 0) {
                printf("TCoreAudioRenderer::CreateAggregateDevice : kAudioDevicePropertyClockDomain error\n");
                printError(osErr);
            } else {
                keptclockdomain = (keptclockdomain == 0) ? clockdomain : keptclockdomain;
                //printf("TCoreAudioRenderer::CreateAggregateDevice : input clockdomain = %d\n", clockdomain);
                if (clockdomain != 0 && clockdomain != keptclockdomain) {
                    //printf("TCoreAudioRenderer::CreateAggregateDevice : devices do not share the same clock!! clock drift compensation would be needed...\n");
                    need_clock_drift_compensation = true;
                }
            }
        }
    }

    for (UInt32 i = 0; i < playbackDeviceID.size(); i++) {
        if (SetupSampleRateAux(playbackDeviceID[i], samplerate) < 0) {
            printf("TCoreAudioRenderer::CreateAggregateDevice : cannot set SR of output device\n");
        } else {
            // Check clock domain
            osErr = AudioDeviceGetProperty(playbackDeviceID[i], 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyClockDomain, &outSize, &clockdomain);
            if (osErr != 0) {
                printf("TCoreAudioRenderer::CreateAggregateDevice : kAudioDevicePropertyClockDomain error\n");
                printError(osErr);
            } else {
                keptclockdomain = (keptclockdomain == 0) ? clockdomain : keptclockdomain;
                //printf("TCoreAudioRenderer::CreateAggregateDevice : output clockdomain = %d", clockdomain);
                if (clockdomain != 0 && clockdomain != keptclockdomain) {
                    //printf("TCoreAudioRenderer::CreateAggregateDevice : devices do not share the same clock!! clock drift compensation would be needed...\n");
                    need_clock_drift_compensation = true;
                }
            }
        }
    }

    // If no valid clock domain was found, then assume we have to compensate...
    if (keptclockdomain == 0) {
        need_clock_drift_compensation = true;
    }

    //---------------------------------------------------------------------------
    // Start to create a new aggregate by getting the base audio hardware plugin
    //---------------------------------------------------------------------------

    char device_name[256];
    for (UInt32 i = 0; i < captureDeviceID.size(); i++) {
        GetDeviceNameFromID(captureDeviceID[i], device_name);
        printf("Separated input = '%s' \n", device_name);
    }

    for (UInt32 i = 0; i < playbackDeviceID.size(); i++) {
        GetDeviceNameFromID(playbackDeviceID[i], device_name);
        printf("Separated output = '%s' \n", device_name);
    }

    osErr = AudioHardwareGetPropertyInfo(kAudioHardwarePropertyPlugInForBundleID, &outSize, &outWritable);
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioHardwareGetPropertyInfo kAudioHardwarePropertyPlugInForBundleID error\n");
        printError(osErr);
        return osErr;
    }

    AudioValueTranslation pluginAVT;

    CFStringRef inBundleRef = CFSTR("com.apple.audio.CoreAudio");

    pluginAVT.mInputData = &inBundleRef;
    pluginAVT.mInputDataSize = sizeof(inBundleRef);
    pluginAVT.mOutputData = &gPluginID;
    pluginAVT.mOutputDataSize = sizeof(gPluginID);

    osErr = AudioHardwareGetProperty(kAudioHardwarePropertyPlugInForBundleID, &outSize, &pluginAVT);
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioHardwareGetProperty kAudioHardwarePropertyPlugInForBundleID error\n");
        printError(osErr);
        return osErr;
    }

    //-------------------------------------------------
    // Create a CFDictionary for our aggregate device
    //-------------------------------------------------

    CFMutableDictionaryRef aggDeviceDict = CFDictionaryCreateMutable(NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);

    char buffer1[64];
    char buffer2[64];
    
    // generate "random" name
    struct timeval fTv1;
    struct timezone tz;
    gettimeofday(&fTv1, &tz);
    
    sprintf(buffer1, "com.grame.%d", fTv1.tv_sec + fTv1.tv_usec);
    sprintf(buffer2, "%d", fTv1.tv_sec + fTv1.tv_usec);
    
    CFStringRef AggregateDeviceNameRef = CFStringCreateWithCString(kCFAllocatorDefault, buffer1, CFStringGetSystemEncoding());
    CFStringRef AggregateDeviceUIDRef = CFStringCreateWithCString(kCFAllocatorDefault, buffer2, CFStringGetSystemEncoding());
  

    // add the name of the device to the dictionary
    CFDictionaryAddValue(aggDeviceDict, CFSTR(kAudioAggregateDeviceNameKey), AggregateDeviceNameRef);

    // add our choice of UID for the aggregate device to the dictionary
    CFDictionaryAddValue(aggDeviceDict, CFSTR(kAudioAggregateDeviceUIDKey), AggregateDeviceUIDRef);

    // add a "private aggregate key" to the dictionary
    int value = 1;
    CFNumberRef AggregateDeviceNumberRef = CFNumberCreate(NULL, kCFNumberIntType, &value);

    SInt32 system;
    Gestalt(gestaltSystemVersion, &system);

    //printf("TCoreAudioRenderer::CreateAggregateDevice : system version = %x limit = %x\n", system, 0x00001054);

    // Starting with 10.5.4 systems, the AD can be internal... (better)
    if (system < 0x00001054) {
        //printf("TCoreAudioRenderer::CreateAggregateDevice : public aggregate device....\n");
    } else {
        //printf("TCoreAudioRenderer::CreateAggregateDevice : private aggregate device....\n");
        CFDictionaryAddValue(aggDeviceDict, CFSTR(kAudioAggregateDeviceIsPrivateKey), AggregateDeviceNumberRef);
    }

    // Prepare sub-devices for clock drift compensation
    CFMutableArrayRef subDevicesArrayClock = NULL;

    /*
    if (fClockDriftCompensate) {
        if (need_clock_drift_compensation) {
            jack_info("Clock drift compensation activated...");
            subDevicesArrayClock = CFArrayCreateMutable(NULL, 0, &kCFTypeArrayCallBacks);

            for (UInt32 i = 0; i < captureDeviceID.size(); i++) {
                CFStringRef UID = GetDeviceName(captureDeviceID[i]);
                if (UID) {
                    CFMutableDictionaryRef subdeviceAggDeviceDict = CFDictionaryCreateMutable(NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
                    CFDictionaryAddValue(subdeviceAggDeviceDict, CFSTR(kAudioSubDeviceUIDKey), UID);
                    CFDictionaryAddValue(subdeviceAggDeviceDict, CFSTR(kAudioSubDeviceDriftCompensationKey), AggregateDeviceNumberRef);
                    //CFRelease(UID);
                    CFArrayAppendValue(subDevicesArrayClock, subdeviceAggDeviceDict);
                }
            }

            for (UInt32 i = 0; i < playbackDeviceID.size(); i++) {
                CFStringRef UID = GetDeviceName(playbackDeviceID[i]);
                if (UID) {
                    CFMutableDictionaryRef subdeviceAggDeviceDict = CFDictionaryCreateMutable(NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
                    CFDictionaryAddValue(subdeviceAggDeviceDict, CFSTR(kAudioSubDeviceUIDKey), UID);
                    CFDictionaryAddValue(subdeviceAggDeviceDict, CFSTR(kAudioSubDeviceDriftCompensationKey), AggregateDeviceNumberRef);
                    //CFRelease(UID);
                    CFArrayAppendValue(subDevicesArrayClock, subdeviceAggDeviceDict);
                }
            }

            // add sub-device clock array for the aggregate device to the dictionary
            CFDictionaryAddValue(aggDeviceDict, CFSTR(kAudioAggregateDeviceSubDeviceListKey), subDevicesArrayClock);
        } else {
            jack_info("Clock drift compensation was asked but is not needed (devices use the same clock domain)");
        }
    }
    */

    //-------------------------------------------------
    // Create a CFMutableArray for our sub-device list
    //-------------------------------------------------

    // we need to append the UID for each device to a CFMutableArray, so create one here
    CFMutableArrayRef subDevicesArray = CFArrayCreateMutable(NULL, 0, &kCFTypeArrayCallBacks);

    std::vector<CFStringRef> captureDeviceUID;
    for (UInt32 i = 0; i < captureDeviceID.size(); i++) {
        CFStringRef ref = GetDeviceName(captureDeviceID[i]);
        if (ref == NULL) {
            return -1;
        }
        captureDeviceUID.push_back(ref);
        // input sub-devices in this example, so append the sub-device's UID to the CFArray
        CFArrayAppendValue(subDevicesArray, ref);
   }

    std::vector<CFStringRef> playbackDeviceUID;
    for (UInt32 i = 0; i < playbackDeviceID.size(); i++) {
        CFStringRef ref = GetDeviceName(playbackDeviceID[i]);
        if (ref == NULL) {
            return -1;
        }
        playbackDeviceUID.push_back(ref);
        // output sub-devices in this example, so append the sub-device's UID to the CFArray
        CFArrayAppendValue(subDevicesArray, ref);
    }

    //-----------------------------------------------------------------------
    // Feed the dictionary to the plugin, to create a blank aggregate device
    //-----------------------------------------------------------------------

    AudioObjectPropertyAddress pluginAOPA;
    pluginAOPA.mSelector = kAudioPlugInCreateAggregateDevice;
    pluginAOPA.mScope = kAudioObjectPropertyScopeGlobal;
    pluginAOPA.mElement = kAudioObjectPropertyElementMaster;
    UInt32 outDataSize;

    osErr = AudioObjectGetPropertyDataSize(gPluginID, &pluginAOPA, 0, NULL, &outDataSize);
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioObjectGetPropertyDataSize error\n");
        printError(osErr);
        goto error;
    }

    osErr = AudioObjectGetPropertyData(gPluginID, &pluginAOPA, sizeof(aggDeviceDict), &aggDeviceDict, &outDataSize, outAggregateDevice);
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioObjectGetPropertyData error\n");
        printError(osErr);
        goto error;
    }

    // pause for a bit to make sure that everything completed correctly
    // this is to work around a bug in the HAL where a new aggregate device seems to disappear briefly after it is created
    CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, false);

    //-------------------------
    // Set the sub-device list
    //-------------------------

    pluginAOPA.mSelector = kAudioAggregateDevicePropertyFullSubDeviceList;
    pluginAOPA.mScope = kAudioObjectPropertyScopeGlobal;
    pluginAOPA.mElement = kAudioObjectPropertyElementMaster;
    outDataSize = sizeof(CFMutableArrayRef);
    osErr = AudioObjectSetPropertyData(*outAggregateDevice, &pluginAOPA, 0, NULL, outDataSize, &subDevicesArray);
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioObjectSetPropertyData for sub-device list error\n");
        printError(osErr);
        goto error;
    }

    // pause again to give the changes time to take effect
    CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, false);

    //-----------------------
    // Set the master device
    //-----------------------

    // set the master device manually (this is the device which will act as the master clock for the aggregate device)
    // pass in the UID of the device you want to use
    pluginAOPA.mSelector = kAudioAggregateDevicePropertyMasterSubDevice;
    pluginAOPA.mScope = kAudioObjectPropertyScopeGlobal;
    pluginAOPA.mElement = kAudioObjectPropertyElementMaster;
    outDataSize = sizeof(CFStringRef);
    osErr = AudioObjectSetPropertyData(*outAggregateDevice, &pluginAOPA, 0, NULL, outDataSize, &captureDeviceUID[0]);  // First capture is master...
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioObjectSetPropertyData for master device error\n");
        printError(osErr);
        goto error;
    }

    // pause again to give the changes time to take effect
    CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, false);

    // Prepare sub-devices for clock drift compensation
    // Workaround for bug in the HAL : until 10.6.2

    if (fClockDriftCompensate) {
        if (need_clock_drift_compensation) {
            //printf("Clock drift compensation activated...\n");

            // Get the property data size
            osErr = AudioObjectGetPropertyDataSize(*outAggregateDevice, &theAddressOwned, theQualifierDataSize, theQualifierData, &outSize);
            if (osErr != noErr) {
                printf("TCoreAudioRenderer::CreateAggregateDevice kAudioObjectPropertyOwnedObjects error\n");
                printError(osErr);
            }

            //	Calculate the number of object IDs
            subDevicesNum = outSize / sizeof(AudioObjectID);
            //printf("TCoreAudioRenderer::CreateAggregateDevice clock drift compensation, number of sub-devices = %d\n", subDevicesNum);
            AudioObjectID subDevices[subDevicesNum];
            outSize = sizeof(subDevices);

            osErr = AudioObjectGetPropertyData(*outAggregateDevice, &theAddressOwned, theQualifierDataSize, theQualifierData, &outSize, subDevices);
            if (osErr != noErr) {
                printf("TCoreAudioRenderer::CreateAggregateDevice kAudioObjectPropertyOwnedObjects error\n");
                printError(osErr);
            }

            // Set kAudioSubDevicePropertyDriftCompensation property...
            for (UInt32 index = 0; index < subDevicesNum; ++index) {
                UInt32 theDriftCompensationValue = 1;
                osErr = AudioObjectSetPropertyData(subDevices[index], &theAddressDrift, 0, NULL, sizeof(UInt32), &theDriftCompensationValue);
                if (osErr != noErr) {
                    printf("TCoreAudioRenderer::CreateAggregateDevice kAudioSubDevicePropertyDriftCompensation error\n");
                    printError(osErr);
                }
            }
        } else {
            //printf("Clock drift compensation was asked but is not needed (devices use the same clock domain)\n");
        }
    }

    // pause again to give the changes time to take effect
    CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, false);

    //----------
    // Clean up
    //----------

    // release the private AD key
    CFRelease(AggregateDeviceNumberRef);

    // release the CF objects we have created - we don't need them any more
    CFRelease(aggDeviceDict);
    CFRelease(subDevicesArray);

    if (subDevicesArrayClock) {
        CFRelease(subDevicesArrayClock);
    }

    // release the device UID
    for (UInt32 i = 0; i < captureDeviceUID.size(); i++) {
        CFRelease(captureDeviceUID[i]);
    }

    for (UInt32 i = 0; i < playbackDeviceUID.size(); i++) {
        CFRelease(playbackDeviceUID[i]);
    }

    //printf("New aggregate device %d\n", *outAggregateDevice);
    return noErr;

error:
    printf("Cannot create aggregate device : modify your audio setup so that default audio input and output devices can be aggregated\n");
    DestroyAggregateDevice(*outAggregateDevice);
    return -1;
}

OSStatus TCoreAudioRenderer::DestroyAggregateDevice(AudioDeviceID id)
{
    // No more needed : will be done when process quits...
    return noErr;
}

OSStatus TCoreAudioRenderer::SRNotificationCallback(AudioDeviceID inDevice,
                                                     UInt32 inChannel,
                                                     Boolean	isInput,
                                                     AudioDevicePropertyID inPropertyID,
                                                     void* inClientData)
{
    switch (inPropertyID) {
            
        case kAudioDevicePropertyNominalSampleRate: {
            //printf("TCoreAudioRenderer::SRNotificationCallback kAudioDevicePropertyNominalSampleRate\n");
            fState = true;
            // Check new sample rate
            Float64 sampleRate;
            UInt32 outSize =  sizeof(Float64);
            OSStatus err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, &outSize, &sampleRate);
            if (err != noErr) {
                printf("Cannot get current sample rate\n");
                printError(err);
            } else {
                //printf("SRNotificationCallback : checked sample rate = %f\n", sampleRate);
            }
            break;
        }
    }
    
    return noErr;
}

OSStatus TCoreAudioRenderer::BSNotificationCallback(AudioDeviceID inDevice,
                                                     UInt32 inChannel,
                                                     Boolean isInput,
                                                     AudioDevicePropertyID inPropertyID,
                                                     void* inClientData)
{
    switch (inPropertyID) {

        case kAudioDevicePropertyBufferFrameSize: {
            //printf("TCoreAudioRenderer::BSNotificationCallback kAudioDevicePropertyBufferFrameSize\n");
            // Check new buffer size
            UInt32 tmp_buffer_size;
            UInt32 outSize = sizeof(UInt32);
            OSStatus err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyBufferFrameSize, &outSize, &tmp_buffer_size);
            if (err != noErr) {
                printf("Cannot get current buffer size");
                printError(err);
            } else {
                //printf("TCoreAudioRenderer::BSNotificationCallback : checked buffer size = %d\n", tmp_buffer_size);
            }
            fState = true;
            break;
        }
    }

    return noErr;
}

int TCoreAudioRenderer::SetupBufferSize(long buffer_size)
{
    // Setting buffer size
    OSStatus err = noErr;
    UInt32 tmp_buffer_size = buffer_size;
    UInt32 outSize = sizeof(UInt32);

    err = AudioDeviceGetProperty(fDeviceID, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyBufferFrameSize, &outSize, &tmp_buffer_size);
    if (err != noErr) {
        printf("Cannot get buffer size %ld\n", buffer_size);
        printError(err);
        return -1;
    } else {
        //printf("TCoreAudioRenderer::SetupBufferSize : current buffer size = %ld\n", tmp_buffer_size);
    }

    // If needed, set new buffer size
    if ((UInt32)buffer_size != tmp_buffer_size) {
        tmp_buffer_size = buffer_size;

        // To get BS change notification
        err = AudioDeviceAddPropertyListener(fDeviceID, 0, true, kAudioDevicePropertyBufferFrameSize, BSNotificationCallback, this);
        if (err != noErr) {
            printf("Error calling AudioDeviceAddPropertyListener with kAudioDevicePropertyBufferFrameSize\n");
            printError(err);
            return -1;
        }

        // Waiting for BS change notification
        int count = 0;
        fState = false;

        err = AudioDeviceSetProperty(fDeviceID, NULL, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyBufferFrameSize, outSize, &tmp_buffer_size);
        if (err != noErr) {
            printf("SetupBufferSize : cannot set buffer size = %ld\n", tmp_buffer_size);
            printError(err);
            goto error;
        }

        while (!fState && count++ < WAIT_NOTIFICATION_COUNTER) {
            usleep(100000);
            printf("TCoreAudioRenderer::SetupBufferSize : wait count = %d\n", count);
        }

        if (count >= WAIT_NOTIFICATION_COUNTER) {
            printf("Did not get buffer size notification...\n");
            goto error;
        }

        // Check new buffer size
        outSize = sizeof(UInt32);
        err = AudioDeviceGetProperty(fDeviceID, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyBufferFrameSize, &outSize, &tmp_buffer_size);
        if (err != noErr) {
            printf("Cannot get current buffer size\n");
            printError(err);
        } else {
            //printf("TCoreAudioRenderer::SetupBufferSize : checked buffer size = %ld\n", tmp_buffer_size);
        }

        // Remove BS change notification
        AudioDeviceRemovePropertyListener(fDeviceID, 0, true, kAudioDevicePropertyBufferFrameSize, BSNotificationCallback);
    }

    return 0;

error:

    // Remove BS change notification
    AudioDeviceRemovePropertyListener(fDeviceID, 0, true, kAudioDevicePropertyBufferFrameSize, BSNotificationCallback);
    return -1;
}

int TCoreAudioRenderer::SetupSampleRateAux(AudioDeviceID inDevice, long sample_rate)
{
    OSStatus err = noErr;
    UInt32 outSize;
    Float64 tmp_sample_rate;

    // Get sample rate
    outSize =  sizeof(Float64);
    err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, &outSize, &tmp_sample_rate);
    if (err != noErr) {
        printf("Cannot get current sample rate\n");
        printError(err);
        return -1;
    } else {
        //printf("TCoreAudioRenderer::SetupSampleRateAux : current sample rate = %f\n", tmp_sample_rate);
    }

    // If needed, set new sample rate
    if (sample_rate != (long)tmp_sample_rate) {
        tmp_sample_rate = (Float64)sample_rate;

        // To get SR change notification
        err = AudioDeviceAddPropertyListener(inDevice, 0, true, kAudioDevicePropertyNominalSampleRate, SRNotificationCallback, 0);
        if (err != noErr) {
            printf("Error calling AudioDeviceAddPropertyListener with kAudioDevicePropertyNominalSampleRate\n");
            printError(err);
            return -1;
        }

        // Waiting for SR change notification
        int count = 0;
        fState = false;

        err = AudioDeviceSetProperty(inDevice, NULL, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, outSize, &tmp_sample_rate);
        if (err != noErr) {
            printf("Cannot set sample rate = %ld\n", sample_rate);
            printError(err);
            goto error;
        }

        while (!fState && count++ < WAIT_NOTIFICATION_COUNTER) {
            usleep(100000);
            printf("TCoreAudioRenderer::SetupSampleRateAux : wait count = %d\n", count);
        }

        if (count >= WAIT_NOTIFICATION_COUNTER) {
            printf("Did not get sample rate notification...\n");
            goto error;
        }

        // Check new sample rate
        outSize = sizeof(Float64);
        err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, &outSize, &tmp_sample_rate);
        if (err != noErr) {
            printf("Cannot get current sample rate\n");
            printError(err);
        } else {
            //printf("TCoreAudioRenderer::SetupSampleRateAux : checked sample rate = %f\n", tmp_sample_rate);
        }

        // Remove SR change notification
        AudioDeviceRemovePropertyListener(inDevice, 0, true, kAudioDevicePropertyNominalSampleRate, SRNotificationCallback);
    }

    return 0;

error:

    // Remove SR change notification
    AudioDeviceRemovePropertyListener(inDevice, 0, true, kAudioDevicePropertyNominalSampleRate, SRNotificationCallback);
    return -1;
}

long TCoreAudioRenderer::Open(long inChan, long outChan, long bufferSize, long samplerate)
{
   ComponentResult err1;
    UInt32 outSize;
	Boolean isWritable;
	AudioStreamBasicDescription srcFormat, dstFormat;
    long in_nChannels = 0;
    long out_nChannels = 0;
    
    SInt32 major;
    SInt32 minor;
    Gestalt(gestaltSystemVersionMajor, &major);
    Gestalt(gestaltSystemVersionMinor, &minor);
    
    // Starting with 10.6 systems, the HAL notification thread is created internally
    if (major == 10 && minor >= 6) {
        CFRunLoopRef theRunLoop = NULL;
        AudioObjectPropertyAddress theAddress = { kAudioHardwarePropertyRunLoop, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster };
        OSStatus osErr = AudioObjectSetPropertyData (kAudioObjectSystemObject, &theAddress, 0, NULL, sizeof(CFRunLoopRef), &theRunLoop);
        if (osErr != noErr) {
            printf("TCoreAudioRenderer::Open kAudioHardwarePropertyRunLoop error\n");
            printError(osErr);
        }
    }
 	
	if (GetDefaultDevice(inChan, outChan, samplerate, &fDeviceID) != noErr) {
		printf("Cannot open default device\n");
		return OPEN_ERR;
	}
	
    if (SetupBufferSize(bufferSize) < 0) {
        return OPEN_ERR;
    }
 
    if (SetupSampleRateAux(fDeviceID, samplerate) < 0) {
        return OPEN_ERR;
    }
  
    // AUHAL
    ComponentDescription cd = {kAudioUnitType_Output, kAudioUnitSubType_HALOutput, kAudioUnitManufacturer_Apple, 0, 0};
    Component HALOutput = FindNextComponent(NULL, &cd);

    err1 = OpenAComponent(HALOutput, &fAUHAL);
    if (err1 != noErr) {
		printf("Error calling OpenAComponent");
        printError(err1);
        goto error;
	}

    err1 = AudioUnitInitialize(fAUHAL);
    if (err1 != noErr) {
		printf("Cannot initialize AUHAL unit");
		printError(err1);
        goto error;
	}
    
    if (inChan > 0) {
        outSize = 1;
        printf("Setup AUHAL input on\n");
    } else {
        outSize = 0;
        printf("Setup AUHAL input off\n");
    }
    
    err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &outSize, sizeof(outSize));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input\n");
        printError(err1);
        goto error;
    }

    if (outChan > 0) {
        outSize = 1;
        printf("Setup AUHAL output on\n");
    } else {
        outSize = 0;
        printf("Setup AUHAL output off\n");
    }
    
    err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output, 0, &outSize, sizeof(outSize));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output\n");
        printError(err1);
        goto error;
    }
    
    err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_CurrentDevice, kAudioUnitScope_Global, 0, &fDeviceID, sizeof(AudioDeviceID));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_CurrentDevice\n");
        printError(err1);
        goto error;
    }
  
    if (inChan > 0) {
        err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 1, (UInt32*)&bufferSize, sizeof(UInt32));
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_MaximumFramesPerSlice\n");
            printError(err1);
            goto error;
        }
    }
    
    if (outChan > 0) {
        err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, (UInt32*)&bufferSize, sizeof(UInt32));
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_MaximumFramesPerSlice\n");
            printError(err1);
            goto error;
        }
    }

    err1 = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Input, 1, &outSize, &isWritable);
    if (err1 != noErr) {
        printf("Error calling AudioUnitGetPropertyInfo - kAudioOutputUnitProperty_ChannelMap-INFO 1\n");
        printError(err1);
    } else {
        in_nChannels = (err1 == noErr) ? outSize / sizeof(SInt32) : 0;
    }
    
    printf("in_nChannels %d\n", in_nChannels);

    err1 = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Output, 0, &outSize, &isWritable);
    if (err1 != noErr) {
        printf("Error calling AudioUnitGetPropertyInfo - kAudioOutputUnitProperty_ChannelMap-INFO 0\n");
        printError(err1);
    } else {
        out_nChannels = (err1 == noErr) ? outSize / sizeof(SInt32) : 0;
    }
    
    printf("out_nChannels %d\n", out_nChannels);

    /*
    // Just ignore this case : seems to work without any further change...
     
    if (outChan > out_nChannels) {
        printf("This device hasn't required output channels\n");
        goto error;
    }
    if (inChan > in_nChannels) {
        printf("This device hasn't required input channels\n");
        goto error;
    }
    */
    
    //if (inChan > 0 && inChan <= in_nChannels) {
    if (inChan < in_nChannels) {  // Taken from Faust coreaudio-dsp.h
        SInt32 chanArr[in_nChannels];
        for (int i = 0; i < in_nChannels; i++) {
            chanArr[i] = -1;
        }
        for (int i = 0; i < inChan; i++) {
            chanArr[i] = i;
        }
        err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Input, 1, chanArr, sizeof(SInt32) * in_nChannels);
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap 1\n");
            printError(err1);
        }
    }
 
    //if (outChan > 0  && outChan <= out_nChannels) {
    if (outChan < out_nChannels) {  // Taken from Faust coreaudio-dsp.h
        SInt32 chanArr[out_nChannels];
        for (int i = 0;	i < out_nChannels; i++) {
            chanArr[i] = -1;
        }
        for (int i = 0; i < outChan; i++) {
            chanArr[i] = i;
        }
        err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Output, 0, chanArr, sizeof(SInt32) * out_nChannels);
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap 0\n");
            printError(err1);
        }
    }
  
    // Setup stream converters
    if (inChan > 0) {
        
        outSize = sizeof(AudioStreamBasicDescription);
        err1 = AudioUnitGetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &srcFormat, &outSize);
        if (err1 != noErr) {
            printf("Error calling AudioUnitGetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Output\n");
            printError(err1);
            goto error;
        }
        PrintStreamDesc(&srcFormat);
        
        printf("Setup AUHAL input stream converter SR = %ld\n", samplerate);
        srcFormat.mSampleRate = samplerate;
        srcFormat.mFormatID = kAudioFormatLinearPCM;
        srcFormat.mBitsPerChannel = 32;
        srcFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked | kLinearPCMFormatFlagIsNonInterleaved;
        srcFormat.mBytesPerPacket = sizeof(float);
        srcFormat.mFramesPerPacket = 1;
        srcFormat.mBytesPerFrame = sizeof(float);
        srcFormat.mChannelsPerFrame = inChan;
        srcFormat.mBitsPerChannel = 32;
        PrintStreamDesc(&srcFormat);
       
        err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &srcFormat, sizeof(AudioStreamBasicDescription));
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Output\n");
            printError(err1);
            goto error;
        }
    }
    
    if (outChan > 0) {
        
        outSize = sizeof(AudioStreamBasicDescription);
        err1 = AudioUnitGetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, &dstFormat, &outSize);
        if (err1 != noErr) {
            printf("Error calling AudioUnitGetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Input\n");
            printError(err1);
            goto error;
        }
        PrintStreamDesc(&dstFormat);
        
        printf("Setup AUHAL output stream converter SR = %ld", samplerate);
        dstFormat.mSampleRate = samplerate;
        dstFormat.mFormatID = kAudioFormatLinearPCM;
        dstFormat.mBitsPerChannel = 32;
        dstFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked | kLinearPCMFormatFlagIsNonInterleaved;
        dstFormat.mBytesPerPacket = sizeof(float);
        dstFormat.mFramesPerPacket = 1;
        dstFormat.mBytesPerFrame = sizeof(float);
        dstFormat.mChannelsPerFrame = outChan;
        dstFormat.mBitsPerChannel = 32;
        PrintStreamDesc(&dstFormat);
        
        err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, &dstFormat, sizeof(AudioStreamBasicDescription));
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Input\n");
            printError(err1);
            goto error;
        }
    }     
 
    if (inChan > 0 && outChan == 0) {
        AURenderCallbackStruct output;
        output.inputProc = Render;
        output.inputProcRefCon = this;
        err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_SetInputCallback, kAudioUnitScope_Global, 0, &output, sizeof(output));
        if (err1 != noErr) {
            printf("Error calling  AudioUnitSetProperty - kAudioUnitProperty_SetRenderCallback 1\n");
            printError(err1);
            goto error;
        }
    } else {
        AURenderCallbackStruct output;
        output.inputProc = Render;
        output.inputProcRefCon = this;
        err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input, 0, &output, sizeof(output));
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_SetRenderCallback 0\n");
            printError(err1);
            goto error;
        }
    }

    fInputData = (AudioBufferList*)malloc(sizeof(UInt32) + inChan * sizeof(AudioBuffer));
    if (fInputData == 0) {
		printf("Cannot allocate memory for input buffers\n");
        goto error;
	}

    // Prepare buffers
    fInputData->mNumberBuffers = inChan;
    for (int i = 0; i < inChan; i++) {
        fInputData->mBuffers[i].mNumberChannels = 1;
        fInputData->mBuffers[i].mData = malloc(bufferSize * sizeof(float));
        fInputData->mBuffers[i].mDataByteSize = bufferSize * sizeof(float);
    }
 	
    return TAudioRenderer::Open(inChan, outChan, bufferSize, samplerate);

error:
    AudioUnitUninitialize(fAUHAL);
    CloseComponent(fAUHAL);
    return OPEN_ERR;
}

long TCoreAudioRenderer::OpenImp(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long samplerate)
{
	return NO_ERR;
}

long TCoreAudioRenderer::Close()
{   
    if (fInputData) {
        for (int i = 0; i < fInput; i++) {
            free(fInputData->mBuffers[i].mData);
        }   
        free(fInputData);
    }
	AudioUnitUninitialize(fAUHAL);
    CloseComponent(fAUHAL);
    DestroyAggregateDevice(fDeviceID);
    return NO_ERR;
}

long TCoreAudioRenderer::Start()
{
    // Init timing here
    memset(&fInfo, 0, sizeof(RendererInfo));
    
    fAnchorFrameTime = 0;
    fAnchorHostTime = 0;
         
    return Cont();
}

long TCoreAudioRenderer::Stop()
{
    OSStatus err = AudioOutputUnitStop(fAUHAL);
   
    if (err != noErr) {
        printf("Error while closing device : device close error \n");
        return OPEN_ERR;
    } else {
        
        // Keep current time if really started...
        if (fAnchorHostTime > 0) {
            UInt64 cur_host_time = AudioGetCurrentHostTime();
            fInfo.fCurFrame = uint64_t(fCallbackTime.mSampleTime + ConvertUsec2Sample(AudioConvertHostTimeToNanos(cur_host_time - fCallbackHostTime)/1000.) - fAnchorFrameTime);
            fInfo.fCurUsec = AudioConvertHostTimeToNanos(cur_host_time - fAnchorHostTime)/1000.;
        }
        
        // Renderer is stopped...
        fAnchorFrameTime = 0;
        fAnchorHostTime = 0;
        
        return NO_ERR;
	}
}

long TCoreAudioRenderer::Pause()
{
    return Stop();
}

long TCoreAudioRenderer::Cont()
{
    if (AudioOutputUnitStart(fAUHAL) != noErr) {
        printf("Error while opening device : device open error \n");
        return OPEN_ERR;
    } else {
        
        /*
        // Waiting for Render callback to be called (= driver has started)
        fState = false;
        int count = 0;
         
        while (!fState && count++ < WAIT_COUNTER) {
            usleep(50000);
            printf("TCoreAudioRenderer::Start : wait count = %d\n", count);
        }
         
        if (count < WAIT_COUNTER) {
            printf("CoreAudio driver is running...\n");
            return NO_ERR;
        } else {
            return OPEN_ERR;
        }
        */
        return NO_ERR;
	}
}

void TCoreAudioRenderer::GetInfo(RendererInfoPtr info)
{
    // TODO : fCallbackTime access in not atomic...
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    if (fAnchorHostTime == 0) { // Renderer is stopped, get values when stopped...
        info->fCurFrame = fInfo.fCurFrame;
        info->fCurUsec = fInfo.fCurUsec;
    } else {
        UInt64 cur_host_time = AudioGetCurrentHostTime();
        info->fCurFrame = uint64_t(fCallbackTime.mSampleTime + ConvertUsec2Sample(AudioConvertHostTimeToNanos(cur_host_time - fCallbackHostTime)/1000.) - fAnchorFrameTime);
        info->fCurUsec = AudioConvertHostTimeToNanos(cur_host_time - fAnchorHostTime)/1000.;
    }
}

long TCoreAudioRenderer::GetDeviceCount()
{
	return 1;
}

void TCoreAudioRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{
    OSStatus err;
    UInt32 outSize;
    Boolean isWritable;
    
    AudioDeviceID fDeviceID;
    AudioUnit fAUHAL;
    
    // AUHAL
    ComponentDescription cd = {kAudioUnitType_Output, kAudioUnitSubType_HALOutput, kAudioUnitManufacturer_Apple, 0, 0};
    Component HALOutput = FindNextComponent(NULL, &cd);
    
    if (GetDefaultDevice(2, 2, 44100, &fDeviceID) == noErr) {
         printf("Default stereo device opened...\n");
    } else {
        printf("Cannot open default stereo device\n");
        if (GetDefaultDevice(0, 2, 44100, &fDeviceID) == noErr) {
            printf("Default output device opened...\n");
        } else {
            printf("Cannot open default output device\n"); 
            if (GetDefaultDevice(2, 0, 44100, &fDeviceID) == noErr) {
                printf("Default input device opened...\n");
            } else {
                printf("Cannot open default input device\n");
                return;
            }
        }
    }
    
    err = OpenAComponent(HALOutput, &fAUHAL);
    if (err != noErr) {
		printf("Error calling OpenAComponent");
        printError(err);
        goto end;
	}
    
    err = AudioUnitInitialize(fAUHAL);
    if (err != noErr) {
		printf("Cannot initialize AUHAL unit");
		printError(err);
        goto end;
	}
    
    outSize = 1;
    
    err = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &outSize, sizeof(outSize));
    if (err != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input\n");
        printError(err);
        goto end;
    }
    
    err = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output, 0, &outSize, sizeof(outSize));
    if (err != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output\n");
        printError(err);
        goto end;
    }
    
    err = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_CurrentDevice, kAudioUnitScope_Global, 0, &fDeviceID, sizeof(AudioDeviceID));
    if (err != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_CurrentDevice\n");
        printError(err);
        goto end;
    }
     
    err = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Input, 1, &outSize, &isWritable);
    if (err != noErr) {
        printf("Error calling AudioUnitGetPropertyInfo - kAudioOutputUnitProperty_ChannelMap-INFO 1\n");
        printError(err);
    }
    
    info->fMaxInputChannels = (err == noErr) ? outSize / sizeof(SInt32) : 0;
    
    printf("info->fMaxInputChannels %d\n", info->fMaxInputChannels);
    
    err = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Output, 0, &outSize, &isWritable);
    if (err != noErr) {
        printf("Error calling AudioUnitGetPropertyInfo - kAudioOutputUnitProperty_ChannelMap-INFO 0\n");
        printError(err);
    }
    
    info->fMaxOutputChannels = (err == noErr) ? outSize / sizeof(SInt32) : 0;
    
    printf("info->fMaxOutputChannels %d\n", info->fMaxOutputChannels);
    
    strcpy(info->fName, "CoreAudio backend");
	info->fDefaultSampleRate = 512;	
	info->fDefaultBufferSize = 44100.0;
  
end:
    AudioUnitUninitialize(fAUHAL);
    CloseComponent(fAUHAL);
}

long TCoreAudioRenderer::GetDefaultInputDevice()
{
	return 0;
}

long TCoreAudioRenderer::GetDefaultOutputDevice()
{
	return 0;
}
