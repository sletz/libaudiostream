/*

Copyright © Grame 2006-2007

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

#define WAIT_COUNTER 60

typedef	UInt8	CAAudioHardwareDeviceSectionID;

#define	kAudioDeviceSectionInput	((CAAudioHardwareDeviceSectionID)0x01)
#define	kAudioDeviceSectionOutput	((CAAudioHardwareDeviceSectionID)0x00)
#define	kAudioDeviceSectionGlobal	((CAAudioHardwareDeviceSectionID)0x00)
#define	kAudioDeviceSectionWildcard	((CAAudioHardwareDeviceSectionID)0xFF)


#define DEBUG 1

// TODO : use jackdmp code...

static void PrintStreamDesc(AudioStreamBasicDescription *inDesc)
{
    printf("- - - - - - - - - - - - - - - - - - - -\n");
    printf("  Sample Rate:%f\n", inDesc->mSampleRate);
    printf("  Format ID:%.*s\n", (int) sizeof(inDesc->mFormatID), (char*)&inDesc->mFormatID);
    printf("  Format Flags:%lX\n", inDesc->mFormatFlags);
    printf("  Bytes per Packet:%ld\n", inDesc->mBytesPerPacket);
    printf("  Frames per Packet:%ld\n", inDesc->mFramesPerPacket);
    printf("  Bytes per Frame:%ld\n", inDesc->mBytesPerFrame);
    printf("  Channels per Frame:%ld\n", inDesc->mChannelsPerFrame);
    printf("  Bits per Channel:%ld\n", inDesc->mBitsPerChannel);
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
    AudioUnitRender(renderer->fAUHAL, ioActionFlags, inTimeStamp, 1, inNumberFrames, renderer->fInputData);
	memset((float*)ioData->mBuffers[0].mData, 0, ioData->mBuffers[0].mDataByteSize); // Necessary since renderer does a  *mix*
	renderer->Run((float*)renderer->fInputData->mBuffers[0].mData, (float*)ioData->mBuffers[0].mData, inNumberFrames);
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
			//printf("GetDefaultDevice : error input = %ld and output = %ld are not the same\n", inDefault, outDefault);
            if (CreateAggregateDevice(inDefault, outDefault, samplerate, id) != noErr) {
                return kAudioHardwareBadDeviceError;
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
    vector<AudioDeviceID> captureDeviceIDArray;

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

    err = AudioDeviceGetProperty(playbackDeviceID, 0, kAudioDeviceSectionGlobal, kAudioAggregateDevicePropertyActiveSubDeviceList, &outSize, sub_device);
    vector<AudioDeviceID> playbackDeviceIDArray;

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

OSStatus TCoreAudioRenderer::CreateAggregateDeviceAux(vector<AudioDeviceID> captureDeviceID, vector<AudioDeviceID> playbackDeviceID, int samplerate, AudioDeviceID* outAggregateDevice)
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
        //printf("Separated input = '%s' \n", device_name);
    }

    for (UInt32 i = 0; i < playbackDeviceID.size(); i++) {
        GetDeviceNameFromID(playbackDeviceID[i], device_name);
        //printf("Separated output = '%s' \n", device_name);
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
    pluginAVT.mOutputData = &fPluginID;
    pluginAVT.mOutputDataSize = sizeof(fPluginID);

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

    CFStringRef AggregateDeviceNameRef = CFSTR("JackDuplex");
    CFStringRef AggregateDeviceUIDRef = CFSTR("com.grame.JackDuplex");

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

    vector<CFStringRef> captureDeviceUID;
    for (UInt32 i = 0; i < captureDeviceID.size(); i++) {
        CFStringRef ref = GetDeviceName(captureDeviceID[i]);
        if (ref == NULL)
            return -1;
        captureDeviceUID.push_back(ref);
        // input sub-devices in this example, so append the sub-device's UID to the CFArray
        CFArrayAppendValue(subDevicesArray, ref);
   }

    vector<CFStringRef> playbackDeviceUID;
    for (UInt32 i = 0; i < playbackDeviceID.size(); i++) {
        CFStringRef ref = GetDeviceName(playbackDeviceID[i]);
        if (ref == NULL)
            return -1;
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

    osErr = AudioObjectGetPropertyDataSize(fPluginID, &pluginAOPA, 0, NULL, &outDataSize);
    if (osErr != noErr) {
        printf("TCoreAudioRenderer::CreateAggregateDevice : AudioObjectGetPropertyDataSize error\n");
        printError(osErr);
        goto error;
    }

    osErr = AudioObjectGetPropertyData(fPluginID, &pluginAOPA, sizeof(aggDeviceDict), &aggDeviceDict, &outDataSize, outAggregateDevice);
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
    osErr = AudioObjectSetPropertyData(*outAggregateDevice, &pluginAOPA, 0, NULL, outDataSize, &captureDeviceUID[0]);  // First apture is master...
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

    if (subDevicesArrayClock)
        CFRelease(subDevicesArrayClock);

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
    DestroyAggregateDevice();
    return -1;
}

OSStatus TCoreAudioRenderer::DestroyAggregateDevice()
{
    OSStatus osErr = noErr;
    AudioObjectPropertyAddress pluginAOPA;
    pluginAOPA.mSelector = kAudioPlugInDestroyAggregateDevice;
    pluginAOPA.mScope = kAudioObjectPropertyScopeGlobal;
    pluginAOPA.mElement = kAudioObjectPropertyElementMaster;
    UInt32 outDataSize;

    if (fPluginID > 0)   {

        osErr = AudioObjectGetPropertyDataSize(fPluginID, &pluginAOPA, 0, NULL, &outDataSize);
        if (osErr != noErr) {
            printf("TCoreAudioRenderer::DestroyAggregateDevice : AudioObjectGetPropertyDataSize error\n");
            printError(osErr);
            return osErr;
        }

        osErr = AudioObjectGetPropertyData(fPluginID, &pluginAOPA, 0, NULL, &outDataSize, &fDeviceID);
        if (osErr != noErr) {
            printf("TCoreAudioRenderer::DestroyAggregateDevice : AudioObjectGetPropertyData error\n");
            printError(osErr);
            return osErr;
        }

    }

    return noErr;
}

OSStatus TCoreAudioRenderer::SRNotificationCallback(AudioDeviceID inDevice,
                                                     UInt32 inChannel,
                                                     Boolean	isInput,
                                                     AudioDevicePropertyID inPropertyID,
                                                     void* inClientData)
{
    TCoreAudioRenderer* driver = (TCoreAudioRenderer*)inClientData;
    
    switch (inPropertyID) {
            
        case kAudioDevicePropertyNominalSampleRate: {
            printf("JackCoreAudioDriver::SRNotificationCallback kAudioDevicePropertyNominalSampleRate\n");
            driver->fState = true;
            // Check new sample rate
            Float64 sampleRate;
            UInt32 outSize =  sizeof(Float64);
            OSStatus err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, &outSize, &sampleRate);
            if (err != noErr) {
                printf("Cannot get current sample rate\n");
                printError(err);
            } else {
                printf("SRNotificationCallback : checked sample rate = %f\n", sampleRate);
            }
            break;
        }
    }
    
    return noErr;
}

int TCoreAudioRenderer::SetupSampleRateAux(AudioDeviceID inDevice, long samplerate)
{
    OSStatus err = noErr;
    UInt32 outSize;
    Float64 sampleRate;
    
    // Get sample rate
    outSize =  sizeof(Float64);
    err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, &outSize, &sampleRate);
    if (err != noErr) {
        printf("Cannot get current sample rate\n");
        printError(err);
        return -1;
    } else {
        printf("Current sample rate = %f\n", sampleRate);
    }
    
    // If needed, set new sample rate
    if (samplerate != (long)sampleRate) {
        sampleRate = (Float64)samplerate;
        
        // To get SR change notification
        err = AudioDeviceAddPropertyListener(inDevice, 0, true, kAudioDevicePropertyNominalSampleRate, SRNotificationCallback, this);
        if (err != noErr) {
            printf("Error calling AudioDeviceAddPropertyListener with kAudioDevicePropertyNominalSampleRate\n");
            printError(err);
            return -1;
        }
        err = AudioDeviceSetProperty(inDevice, NULL, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, outSize, &sampleRate);
        if (err != noErr) {
            printf("Cannot set sample rate = %ld\n", samplerate);
            printError(err);
            return -1;
        }
        
        // Waiting for SR change notification
        int count = 0;
        while (!fState && count++ < WAIT_COUNTER) {
            usleep(100000);
            printf("Wait count = %d\n", count);
        }
        
        // Check new sample rate
        outSize =  sizeof(Float64);
        err = AudioDeviceGetProperty(inDevice, 0, kAudioDeviceSectionGlobal, kAudioDevicePropertyNominalSampleRate, &outSize, &sampleRate);
        if (err != noErr) {
            printf("Cannot get current sample rate\n");
            printError(err);
        } else {
            printf("Checked sample rate = %f\n", sampleRate);
        }
        
        // Remove SR change notification
        AudioDeviceRemovePropertyListener(inDevice, 0, true, kAudioDevicePropertyNominalSampleRate, SRNotificationCallback);
    }
    
    return 0;
}


long TCoreAudioRenderer::OpenDefault(long inChan, long outChan, long bufferSize, long samplerate)
{
	OSStatus err = noErr;
    ComponentResult err1;
    UInt32 outSize;
	Boolean isWritable;
	AudioStreamBasicDescription srcFormat, dstFormat;
    long in_nChannels, out_nChannels;
    
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
            printf("JackCoreAudioDriver::Open kAudioHardwarePropertyRunLoop error\n");
            printError(osErr);
        }
    }
 	
	if (GetDefaultDevice(inChan, outChan, samplerate, &fDeviceID) != noErr){
		printf("Cannot open default device\n");
		return OPEN_ERR;
	}
	
	// Setting buffer size
    outSize = sizeof(UInt32);
    err = AudioDeviceSetProperty(fDeviceID, NULL, 0, false, kAudioDevicePropertyBufferFrameSize, outSize, &bufferSize);
    if (err != noErr) {
        printf("Cannot set buffer size %ld\n", bufferSize);
        printError(err);
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
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap-INFO 1\n");
        printError(err1);
    }

    in_nChannels = (err1 == noErr) ? outSize / sizeof(SInt32) : 0;

    err1 = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Output, 0, &outSize, &isWritable);
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap-INFO 0\n");
        printError(err1);
    }

    out_nChannels = (err1 == noErr) ? outSize / sizeof(SInt32) : 0;

    if (outChan > out_nChannels) {
        printf("This device hasn't required output channels\n");
        goto error;
    }
    if (inChan > in_nChannels) {
        printf("This device hasn't required input channels\n");
        goto error;
    }

    if (outChan < out_nChannels) {
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

    if (inChan < in_nChannels) {
        SInt32 chanArr[in_nChannels];
        for (int i = 0; i < in_nChannels; i++) {
            chanArr[i] = -1;
        }
        for (int i = 0; i < inChan; i++) {
            chanArr[i] = i;
        }
        AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_ChannelMap , kAudioUnitScope_Input, 1, chanArr, sizeof(SInt32) * in_nChannels);
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap 1\n");
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
        srcFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
        srcFormat.mChannelsPerFrame = inChan;
        srcFormat.mFramesPerPacket = 1;
        srcFormat.mBytesPerFrame = srcFormat.mBitsPerChannel * srcFormat.mChannelsPerFrame / 8;
        srcFormat.mBytesPerPacket = srcFormat.mBytesPerFrame * srcFormat.mFramesPerPacket;
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
        dstFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
        dstFormat.mChannelsPerFrame = outChan;
        dstFormat.mFramesPerPacket = 1;
        dstFormat.mBytesPerFrame = dstFormat.mBitsPerChannel * dstFormat.mChannelsPerFrame / 8;
        dstFormat.mBytesPerPacket = dstFormat.mBytesPerFrame * dstFormat.mFramesPerPacket;
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
    fInputData->mNumberBuffers = inChan;

    // Prepare buffers
	fInputData->mBuffers[0].mNumberChannels = inChan;
	fInputData->mBuffers[0].mDataByteSize = inChan * (bufferSize) * sizeof(float);
 	
    return TAudioRenderer::OpenDefault(inChan, outChan, bufferSize, samplerate);

error:
    AudioUnitUninitialize(fAUHAL);
    CloseComponent(fAUHAL);
    return OPEN_ERR;
}

long TCoreAudioRenderer::Open(long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long samplerate)
{
	return NO_ERR;
}

long TCoreAudioRenderer::Close()
{
	free(fInputData);
	AudioUnitUninitialize(fAUHAL);
    CloseComponent(fAUHAL);
    return NO_ERR;
}

long TCoreAudioRenderer::Start()
{
	OSStatus err = AudioOutputUnitStart(fAUHAL);
  
    if (err != noErr) {
        printf("Error while opening device : device open error \n");
        return OPEN_ERR;
    } else {
        return NO_ERR;
	}
}

long TCoreAudioRenderer::Stop()
{
   OSStatus err = AudioOutputUnitStop(fAUHAL);

    if (err != noErr) {
        printf("Error while closing device : device close error \n");
        return OPEN_ERR;
    } else {
        return NO_ERR;
	}
}

void TCoreAudioRenderer::GetInfo(RendererInfoPtr info)
{
    info->fInput = fInput;
    info->fOutput = fOutput;
    info->fSampleRate = fSampleRate;
    info->fBufferSize = fBufferSize;
    //info->fCurFrame = long(Pa_StreamTime(fStream)); // To finish
    info->fCurMs = ConvertSample2Ms(info->fCurFrame);
}

long TCoreAudioRenderer::GetDeviceCount()
{
	return 1;
}

void TCoreAudioRenderer::GetDeviceInfo(long deviceNum, DeviceInfoPtr info)
{}

long TCoreAudioRenderer::GetDefaultInputDevice()
{
	return 0;
}

long TCoreAudioRenderer::GetDefaultOutputDevice()
{
	return 0;
}
