/*

Copyright © Grame 2006

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

#define DEBUG 1

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

OSStatus TCoreAudioRenderer::GetDefaultDevice(int inChan, int outChan, AudioDeviceID* id)
{
    UInt32 theSize = sizeof(UInt32);
    AudioDeviceID inDefault;
    AudioDeviceID outDefault;
	OSStatus res;

    if ((res = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultInputDevice,
                                        &theSize, &inDefault)) != noErr)
        return res;

    if ((res = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice,
                                        &theSize, &outDefault)) != noErr)
        return res;
	
	// Duplex mode
	if (inChan > 0 && outChan > 0) {
		// Get the device only if default input and output are the same
		if (inDefault == outDefault) {
			*id = inDefault;
			return noErr;
		} else {
			printf("GetDefaultDevice : error input = %ld and output = %ld are not the same\n", inDefault, outDefault);
			return kAudioHardwareBadDeviceError;
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

long TCoreAudioRenderer::Open(long* inChan, long* outChan, long* bufferSize, long* samplerate)
{

	OSStatus err = noErr;
    ComponentResult err1;
    UInt32 outSize;
	Boolean isWritable;
	AudioStreamBasicDescription srcFormat, dstFormat, sampleRate;
    long in_nChannels, out_nChannels;
	
	if (GetDefaultDevice(*inChan, *outChan, &fDeviceID) != noErr){
		printf("Cannot open default device\n");
		return OPEN_ERR;
	}
	
	// Setting buffer size
    outSize = sizeof(UInt32);
    err = AudioDeviceSetProperty(fDeviceID, NULL, 0, false, kAudioDevicePropertyBufferFrameSize, outSize, bufferSize);
    if (err != noErr) {
        printf("Cannot set buffer size %ld\n", *bufferSize);
        printError(err);
        return OPEN_ERR;
    }

    // Setting sample rate
    outSize = sizeof(AudioStreamBasicDescription);
    err = AudioDeviceGetProperty(fDeviceID, 0, false, kAudioDevicePropertyStreamFormat, &outSize, &sampleRate);
    if (err != noErr) {
        printf("Cannot get current sample rate\n");
        printError(err);
        return OPEN_ERR;
    }

    if (*samplerate != (unsigned long)sampleRate.mSampleRate) {
        sampleRate.mSampleRate = (Float64)(*samplerate);
        err = AudioDeviceSetProperty(fDeviceID, NULL, 0, false, kAudioDevicePropertyStreamFormat, outSize, &sampleRate);
        if (err != noErr) {
            printf("Cannot set sample rate = %ld\n", *samplerate);
            printError(err);
            return OPEN_ERR;
        }
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

    err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_CurrentDevice, kAudioUnitScope_Global, 0, &fDeviceID, sizeof(AudioDeviceID));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_CurrentDevice\n");
        printError(err1);
        goto error;
    }

    err1 = AudioUnitInitialize(fAUHAL);
    if (err1 != noErr) {
		printf("Cannot initialize AUHAL unit");
		printError(err1);
        goto error;
	}

    outSize = 1;
    err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output, 0, &outSize, sizeof(outSize));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Output\n");
        printError(err1);
        goto error;
    }

    if (inChan > 0) {
        outSize = 1;
        err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input, 1, &outSize, sizeof(outSize));
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_EnableIO, kAudioUnitScope_Input\n");
            printError(err1);
            goto error;
        }
    }

    err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, (UInt32*)bufferSize, sizeof(UInt32));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_MaximumFramesPerSlice\n");
        printError(err1);
        goto error;
    }

    err1 = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Input, 1, &outSize, &isWritable);
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap-INFO 1\n");
        printError(err1);
    }

    in_nChannels = outSize / sizeof(SInt32);

    err1 = AudioUnitGetPropertyInfo(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Output, 0, &outSize, &isWritable);
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap-INFO 0\n");
        printError(err1);
    }

    out_nChannels = outSize / sizeof(SInt32);

    if (*outChan > out_nChannels) {
        printf("This device hasn't required output channels\n");
        goto error;
    }
    if (*inChan > in_nChannels) {
        printf("This device hasn't required input channels\n");
        goto error;
    }

    if (*outChan < out_nChannels) {
        SInt32 chanArr[out_nChannels];
        for (int i = 0;	i < out_nChannels; i++) {
            chanArr[i] = -1;
        }
        for (int i = 0; i < *outChan; i++) {
            chanArr[i] = i;
        }
        err1 = AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_ChannelMap, kAudioUnitScope_Output, 0, chanArr, sizeof(SInt32) * out_nChannels);
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap 0\n");
            printError(err1);
        }
    }

    if (*inChan < in_nChannels) {
        SInt32 chanArr[in_nChannels];
        for (int i = 0; i < in_nChannels; i++) {
            chanArr[i] = -1;
        }
        for (int i = 0; i < *inChan; i++) {
            chanArr[i] = i;
        }
        AudioUnitSetProperty(fAUHAL, kAudioOutputUnitProperty_ChannelMap , kAudioUnitScope_Input, 1, chanArr, sizeof(SInt32) * in_nChannels);
        if (err1 != noErr) {
            printf("Error calling AudioUnitSetProperty - kAudioOutputUnitProperty_ChannelMap 1\n");
            printError(err1);
        }
    }
	
	outSize = sizeof(AudioStreamBasicDescription);
	err1 = AudioUnitGetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, &srcFormat, &outSize);
    if (err1 != noErr) {
        printf("Error calling AudioUnitGetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Input\n");
        printError(err1);
    }
	PrintStreamDesc(&srcFormat);
	
	outSize = sizeof(AudioStreamBasicDescription);
	err1 = AudioUnitGetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &dstFormat, &outSize);
    if (err1 != noErr) {
        printf("Error calling AudioUnitGetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Output\n");
        printError(err1);
    }
	PrintStreamDesc(&dstFormat);
	
	srcFormat.mSampleRate = *samplerate;
    srcFormat.mFormatID = kAudioFormatLinearPCM;
	srcFormat.mBitsPerChannel = 32;
	srcFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
	srcFormat.mChannelsPerFrame = *outChan;
	srcFormat.mFramesPerPacket = 1;
	srcFormat.mBytesPerFrame = srcFormat.mBitsPerChannel * srcFormat.mChannelsPerFrame / 8;
    srcFormat.mBytesPerPacket = srcFormat.mBytesPerFrame * srcFormat.mFramesPerPacket;
		
	PrintStreamDesc(&srcFormat);

    err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, &srcFormat, sizeof(AudioStreamBasicDescription));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Input\n");
        printError(err1);
    }
	
    dstFormat.mSampleRate = *samplerate;
	dstFormat.mFormatID = kAudioFormatLinearPCM;
	dstFormat.mBitsPerChannel = 32;
	dstFormat.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
	dstFormat.mChannelsPerFrame = *inChan;
	dstFormat.mFramesPerPacket = 1;
	dstFormat.mBytesPerFrame = dstFormat.mBitsPerChannel * dstFormat.mChannelsPerFrame / 8;
    dstFormat.mBytesPerPacket = dstFormat.mBytesPerFrame * dstFormat.mFramesPerPacket;
	
	PrintStreamDesc(&dstFormat);

    err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Output, 1, &dstFormat, sizeof(AudioStreamBasicDescription));
    if (err1 != noErr) {
        printf("Error calling AudioUnitSetProperty - kAudioUnitProperty_StreamFormat kAudioUnitScope_Output\n");
        printError(err1);
    }

    if (*inChan > 0 && *outChan == 0) {
        AURenderCallbackStruct output;
        output.inputProc = Render;
        output.inputProcRefCon = this;
        err1 = AudioUnitSetProperty(fAUHAL, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Output, 1, &output, sizeof(output));
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

    fInputData = (AudioBufferList*)malloc(sizeof(UInt32) + *inChan * sizeof(AudioBuffer));
    if (fInputData == 0) {
		printf("Cannot allocate memory for input buffers\n");
        goto error;
	}
    fInputData->mNumberBuffers = *inChan;

    // Prepare buffers
	fInputData->mBuffers[0].mNumberChannels = *inChan;
	fInputData->mBuffers[0].mDataByteSize = *inChan * (*bufferSize) * sizeof(float);
 	
    return NO_ERR;

error:
    AudioUnitUninitialize(fAUHAL);
    CloseComponent(fAUHAL);
    return OPEN_ERR;
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

