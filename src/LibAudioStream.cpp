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

#include "TAudioEngine.h"
#include "TAudioRendererFactory.h"
#include "TAudioStreamFactory.h"
#include "TVolAudioEffect.h"
#include "TPitchShiftAudioEffect.h"
#include "TPanAudioEffect.h"
#include "TFaustAudioEffect.h"
#include "TWrapperAudioEffect.h"
#include "TBufferedInputAudioStream.h"

#ifdef WIN32
	#define	AUDIOAPI __declspec(dllexport)
#else
	#define	AUDIOAPI __attribute__ ((visibility("default")))
#endif

    struct AudioPlayer {
        TAudioRendererPtr fRenderer;
		TAudioMixerPtr	fMixer;  	
    };

    // Opaque pointers
    typedef AudioPlayer* AudioPlayerPtr;
	typedef TAudioStreamPtr AudioStream;			// smart pointer type
	typedef AudioStream* AudioStreamPtr;

	typedef TAudioRenderer* AudioRendererPtr;	
	typedef TAudioClient* AudioClientPtr;

 	typedef TAudioEffectListPtr AudioEffectList;	// smart pointer type
    typedef TAudioEffectInterfacePtr AudioEffect;	// smart pointer type

	typedef AudioEffectList* AudioEffectListPtr;
    typedef AudioEffect* AudioEffectPtr;	
	typedef TAudioEffectInterface* AudioEffectInterfacePtr;
	
	typedef void (*StopCallback)(void* context);

#ifdef __cplusplus
extern "C"
{
#endif

    AUDIOAPI long LibVersion();
    AUDIOAPI const char* GetLastLibError();
		
	// Device scanning
	AUDIOAPI long GetDeviceCount(AudioRendererPtr renderer);
	AUDIOAPI void GetDeviceInfo(AudioRendererPtr renderer, long deviceNum, DeviceInfo* info);
	AUDIOAPI long GetDefaultInputDevice(AudioRendererPtr renderer);
	AUDIOAPI long GetDefaultOutputDevice(AudioRendererPtr renderer);
	
	AUDIOAPI AudioStreamPtr MakeSoundPtr(AudioStream sound) ;
	AUDIOAPI void DeleteSoundPtr(AudioStreamPtr sound);
	
	// Build sound (using pointer on smartptr)
	AUDIOAPI AudioStreamPtr MakeNullSoundPtr(long lengthFrame);
    AUDIOAPI AudioStreamPtr MakeReadSoundPtr(char* name);
    AUDIOAPI AudioStreamPtr MakeRegionSoundPtr(char* name, long beginFrame, long endFrame);
	AUDIOAPI AudioStreamPtr	MakeStereoSoundPtr(AudioStreamPtr sound);
    AUDIOAPI AudioStreamPtr MakeFadeSoundPtr(AudioStreamPtr sound, long fadeIn, long fadeOut);
    AUDIOAPI AudioStreamPtr MakeLoopSoundPtr(AudioStreamPtr sound, long n);
    AUDIOAPI AudioStreamPtr MakeCutSoundPtr(AudioStreamPtr sound, long beginFrame, long endFrame);
    AUDIOAPI AudioStreamPtr MakeSeqSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade);
    AUDIOAPI AudioStreamPtr MakeMixSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2);
    AUDIOAPI AudioStreamPtr MakeTransformSoundPtr(AudioStreamPtr sound, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);
	AUDIOAPI AudioStreamPtr MakeRubberBandSoundPtr(AudioStreamPtr sound, double* pitch_shift, double* time_strech);
    AUDIOAPI AudioStreamPtr MakeWriteSoundPtr(char* name, AudioStreamPtr s, long format);
    AUDIOAPI AudioStreamPtr MakeInputSoundPtr();
    AUDIOAPI AudioStreamPtr MakeSharedBufferedInputSoundPtr(long beginFrame);
    AUDIOAPI AudioStreamPtr MakeRendererSoundPtr(AudioStreamPtr s);

    AUDIOAPI long GetLengthSoundPtr(AudioStreamPtr s);
    AUDIOAPI long GetChannelsSoundPtr(AudioStreamPtr s);
    AUDIOAPI long ReadSoundPtr(AudioStreamPtr stream, float* buffer, long buffer_size, long channels);
	AUDIOAPI void ResetSoundPtr(AudioStreamPtr sound);

	AUDIOAPI AudioEffectListPtr MakeAudioEffectListPtr();
    AUDIOAPI AudioEffectListPtr AddAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect);
    AUDIOAPI AudioEffectListPtr RemoveAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect);
	AUDIOAPI AudioEffectListPtr ClearAudioEffectListPtr(AudioEffectListPtr list_effect);

    AUDIOAPI AudioEffectPtr MakeVolAudioEffectPtr(float vol);
	AUDIOAPI AudioEffectPtr MakeMonoPanAudioEffectPtr(float pan);
	AUDIOAPI AudioEffectPtr MakeStereoPanAudioEffectPtr(float panLeft, float panRight);
	AUDIOAPI AudioEffectPtr MakePitchShiftAudioEffectPtr(float pitch);
    AUDIOAPI AudioEffectPtr MakeFaustAudioEffectPtr(const char* name);
#ifdef __APPLE__
    AUDIOAPI AudioEffectPtr MakeDispatchFaustAudioEffectPtr(const char* name);
#endif
	AUDIOAPI AudioEffectPtr MakeWrapperAudioEffectPtr(AudioEffectInterfacePtr effect);

	AUDIOAPI long GetControlCountEffectPtr(AudioEffectPtr effect);
	AUDIOAPI void GetControlParamEffectPtr(AudioEffectPtr effect, long param, char* label, float* min, float* max, float* init);
	AUDIOAPI void SetControlValueEffectPtr(AudioEffectPtr effect, long param, float f);
	AUDIOAPI float GetControlValueEffectPtr(AudioEffectPtr effect, long param);
	
	AUDIOAPI void SetStateEffectPtr(AudioEffectPtr effect, long state);
	AUDIOAPI long GetStateEffectPtr(AudioEffectPtr effect);
	AUDIOAPI void ResetEffectPtr(AudioEffectPtr effect);

	AUDIOAPI void ProcessEffectPtr(AudioEffectPtr effect, float** input, float** output, long framesNum, long channels);
    AUDIOAPI const char* GetJsonEffectPtr(AudioEffectPtr effect);
	
	AUDIOAPI void DeleteEffectListPtr(AudioEffectListPtr list_effect);
	AUDIOAPI void DeleteEffectPtr(AudioEffectPtr effect);

	  // Open/Close
	AUDIOAPI void SetAudioLatencies(long inputLatency, long outputLatency);
    AUDIOAPI AudioPlayerPtr OpenAudioPlayer(long inChan, 
                                            long outChan, 
                                            long channels, 
                                            long sample_rate, 
                                            long buffer_size, 
                                            long stream_buffer_size, 
                                            long rtstream_buffer_size, 
                                            long renderer,
                                            long thread_num);
	AUDIOAPI AudioPlayerPtr OpenAudioClient(AudioRendererPtr renderer);	
									
    AUDIOAPI void CloseAudioPlayer(AudioPlayerPtr player);
	AUDIOAPI void CloseAudioClient(AudioPlayerPtr player);

    // Load a sound in a channel
    AUDIOAPI long LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight);
	AUDIOAPI long LoadChannelPtr(AudioPlayerPtr player, AudioStreamPtr sound, long chan, float vol, float panLeft, float panRight);
    AUDIOAPI void GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfoPtr info); // Obsolete version
    AUDIOAPI void GetChannelInfo(AudioPlayerPtr player, long chan, ChannelInfoPtr info);
    AUDIOAPI void GetChannelInfo(AudioPlayerPtr player, long chan, ChannelInfoPtr info);
	AUDIOAPI void SetStopCallbackChannel(AudioPlayerPtr player, long chan, StopCallback callback, void* context);

    // Transport
    AUDIOAPI void StartAudioPlayer(AudioPlayerPtr player);		// Start the global player
    AUDIOAPI void StopAudioPlayer(AudioPlayerPtr player);		// Stop the global player

    AUDIOAPI void StartChannel(AudioPlayerPtr player, long chan);	// Start a sound region from the beginning
    AUDIOAPI void ContChannel(AudioPlayerPtr player, long chan);	// Play a sound region from the current location
    AUDIOAPI void StopChannel(AudioPlayerPtr player, long chan);	// Stop playing
	AUDIOAPI void AbortChannel(AudioPlayerPtr player, long chan);	// Stop playing

    // Params
    AUDIOAPI void SetVolChannel(AudioPlayerPtr player, long chan, float vol);
    AUDIOAPI void SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight);
	AUDIOAPI void SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectList effect_list, long fadeIn, long fadeOut);
	AUDIOAPI void SetEffectListChannelPtr(AudioPlayerPtr player, long chan, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);

    // Master
    AUDIOAPI void SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight);
    AUDIOAPI void SetVolAudioPlayer(AudioPlayerPtr player, float vol);
	AUDIOAPI void SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectList effect_list, long fadeIn, long fadeOut);
	AUDIOAPI void SetEffectListAudioPlayerPtr(AudioPlayerPtr player, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);
    AUDIOAPI AudioRendererPtr GetAudioPlayerRenderer(AudioPlayerPtr player);
	
	// Renderer
	AUDIOAPI AudioRendererPtr MakeAudioRenderer(long renderer);
	AUDIOAPI void DeleteAudioRenderer(AudioRendererPtr renderer);
	
	long AUDIOAPI OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
	AUDIOAPI void CloseAudioRenderer(AudioRendererPtr renderer); 
	AUDIOAPI void StartAudioRenderer(AudioRendererPtr renderer); 
    AUDIOAPI void StartAudioRenderer(AudioRendererPtr renderer); 
    AUDIOAPI void GetAudioRendererInfo(AudioRendererPtr renderer, RendererInfoPtr info); 
	
	AUDIOAPI void AddAudioClient(AudioRendererPtr renderer, AudioClientPtr client); 
	AUDIOAPI void RemoveAudioClient(AudioRendererPtr renderer, AudioClientPtr client); 
	
	// Globals
	AUDIOAPI void AudioGlobalsInit(long inChan, 
									long outChan, 
									long channels, 
									long sample_rate,
									long buffer_size, 
									long stream_buffer_size, 
									long rtstream_buffer_size,
									long thread_num);
	AUDIOAPI void AudioGlobalsDestroy();

#ifdef __cplusplus
}
#endif

// Build sound (using smartptr)
AudioStream AUDIOAPI MakeNullSound(long lengthFrame);
AudioStream AUDIOAPI MakeReadSound(char* name);
AudioStream AUDIOAPI MakeRegionSound(char* name, long beginFrame, long endFrame);
AudioStream AUDIOAPI MakeStereoSound(AudioStream sound);
AudioStream AUDIOAPI MakeFadeSound(AudioStream sound, long fadeIn, long fadeOut);
AudioStream AUDIOAPI MakeLoopSound(AudioStream sound, long n);
AudioStream AUDIOAPI MakeCutSound(AudioStream sound, long beginFrame, long endFrame);
AudioStream AUDIOAPI MakeSeqSound(AudioStream s1, AudioStream s2, long crossFade);
AudioStream AUDIOAPI MakeMixSound(AudioStream s1, AudioStream s2);
AudioStream AUDIOAPI MakeTransformSound(AudioStream sound, AudioEffectList effect_list, long fadeIn, long fadeOut);
AudioStream AUDIOAPI MakeRubberBandSound(AudioStreamPtr sound, double* pitch_shift, double* time_strech);
AudioStream AUDIOAPI MakeWriteSound(char* name, AudioStream s, long format);
AudioStream AUDIOAPI MakeInputSound();
AudioStream AUDIOAPI MakeRendererSound(AudioStream s);

AUDIOAPI long GetLengthSound(AudioStream s);
AUDIOAPI long GetChannelsSound(AudioStream s);
AUDIOAPI long ReadSound(AudioStream stream, float* buffer, long buffer_size, long channels);
AUDIOAPI void ResetSound(AudioStream sound);

// Effect management (using smartptr)
AudioEffectList AUDIOAPI MakeAudioEffectList();
AudioEffectList AUDIOAPI AddAudioEffect(AudioEffectList list_effect, AudioEffect effect);
AudioEffectList AUDIOAPI RemoveAudioEffect(AudioEffectList list_effect, AudioEffect effect);
AudioEffectList AUDIOAPI ClearAudioEffectList(AudioEffectList list_effect);

AudioEffect AUDIOAPI MakeVolAudioEffect(float vol);
AudioEffect AUDIOAPI MakeMonoPanAudioEffect(float pan);
AudioEffect AUDIOAPI MakeStereoPanAudioEffect(float panLeft, float panRight);
AudioEffect AUDIOAPI MakePitchShiftAudioEffect(float pitch);
AudioEffect AUDIOAPI MakeFaustAudioEffect(const char* name);

AUDIOAPI long GetControlCountEffect(AudioEffect effect);
AUDIOAPI void GetControlParamEffect(AudioEffect effect, long param, char* label, float* min, float* max, float* init);
AUDIOAPI void SetControlValueEffect(AudioEffect effect, long param, float f);
AUDIOAPI float GetControlValueEffect(AudioEffect effect, long param);

AUDIOAPI void SetStateEffect(AudioEffect effect, long state);
AUDIOAPI long GetStateEffect(AudioEffect effect);
AUDIOAPI void ResetEffect(AudioEffect effect);

AUDIOAPI void ProcessEffect(AudioEffectPtr effect, float** input, float** output, long framesNum, long channels);

char gLastLibError[512] = {0};

AUDIOAPI long LibVersion()
{
	return 1266;
}

AUDIOAPI const char* GetLastLibError()
{
    return gLastLibError;
}

AudioStream AUDIOAPI MakeNullSound(long lengthFrame)
{
	return TAudioStreamFactory::MakeNullSound(lengthFrame);
}

AudioStream AUDIOAPI MakeReadSound(char* name)
{
    return TAudioStreamFactory::MakeReadSound(name);
}

AudioStream AUDIOAPI MakeRegionSound(char* name, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeRegionSound(name, beginFrame, endFrame);
}

AudioStream AUDIOAPI MakeStereoSound(AudioStream sound)
{
	return TAudioStreamFactory::MakeStereoSound(sound);
}

AudioStream AUDIOAPI MakeFadeSound(AudioStream sound, long fadeIn, long fadeOut)
{
    return TAudioStreamFactory::MakeFadeSound(static_cast<TAudioStreamPtr>(sound), fadeIn, fadeOut);
}

AudioStream AUDIOAPI MakeLoopSound(AudioStream sound, long n)
{
    return TAudioStreamFactory::MakeLoopSound(static_cast<TAudioStreamPtr>(sound), n);
}

AudioStream AUDIOAPI MakeCutSound(AudioStream sound, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeCutSound(static_cast<TAudioStreamPtr>(sound), beginFrame, endFrame);
}

AudioStream AUDIOAPI MakeSeqSound(AudioStream s1, AudioStream s2, long crossFade)
{
    return TAudioStreamFactory::MakeSeqSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioStreamPtr>(s2), crossFade);
}

AudioStream AUDIOAPI MakeMixSound(AudioStream s1, AudioStream s2)
{
    return TAudioStreamFactory::MakeMixSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioStreamPtr>(s2));
}

AudioStream AUDIOAPI MakeInputSound()
{
    return TAudioStreamFactory::MakeInputSound();
}

AudioStream AUDIOAPI MakeSharedBufferedInputSound(long beginFrame)
{
    return TAudioStreamFactory::MakeSharedBufferedInputSound(beginFrame);
}

AudioStream AUDIOAPI MakeTransformSound(AudioStream s1, AudioEffectList list_effect, long fadeIn, long fadeOut)
{
	return TAudioStreamFactory::MakeTransformSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioEffectListPtr>(list_effect), fadeIn, fadeOut);
}

AudioStream AUDIOAPI MakePitchSchiftTimeStretchSound(AudioStream s1, double* pitch_shift, double* time_strech)
{
	return TAudioStreamFactory::MakeRubberBandSound(static_cast<TAudioStreamPtr>(s1), pitch_shift, time_strech);
    /*
#ifdef SOUND_TOUCH
	return TAudioStreamFactory::MakeSoundTouchSound(static_cast<TAudioStreamPtr>(s1), pitch_shift, time_strech);
#else
    return 0;
#endif
    */
}

AudioStream AUDIOAPI MakeWriteSound(char* name, AudioStream s, long format)
{
    return TAudioStreamFactory::MakeWriteSound(name, static_cast<TAudioStreamPtr>(s), format);
}

AudioStream AUDIOAPI MakeRendererSound(AudioStream s)
{
    return TAudioStreamFactory::MakeDTRenderer(static_cast<TAudioStreamPtr>(s));
}

AUDIOAPI long GetLengthSound(AudioStream s)
{
    return (s) ? (static_cast<TAudioStreamPtr>(s))->Length() : 0;
}

AUDIOAPI long GetChannelsSound(AudioStream s)
{
    return (s) ? (static_cast<TAudioStreamPtr>(s))->Channels() : 0;
}

AUDIOAPI long ReadSound(AudioStream sound, float* buffer, long buffer_size, long channels)
{
    if (sound && buffer) {
        TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
		UAudioTools::ZeroFloatBlk(buffer, buffer_size, channels);
        return static_cast<TAudioStreamPtr>(sound)->Read(&process, buffer_size, 0, channels);
    } else {
        return 0;
    }
}

AUDIOAPI void ResetSound(AudioStream sound)
{
	static_cast<TAudioStreamPtr>(sound)->Reset();
}

AUDIOAPI AudioStreamPtr MakeSoundPtr(AudioStream sound) 
{
	return new LA_SMARTP<TAudioStream>(sound);
}

AUDIOAPI void DeleteSoundPtr(AudioStreamPtr sound) 
{
	delete sound;
}

AUDIOAPI AudioStreamPtr MakeNullSoundPtr(long lengthFrame)
{
	return MakeSoundPtr(TAudioStreamFactory::MakeNullSound(lengthFrame));
}

AUDIOAPI AudioStreamPtr MakeReadSoundPtr(char* name)
{
	AudioStream sound = TAudioStreamFactory::MakeReadSound(name);
	return (sound) ? MakeSoundPtr(sound) : 0;
}

AUDIOAPI AudioStreamPtr MakeRegionSoundPtr(char* name, long beginFrame, long endFrame)
{
	AudioStream sound = TAudioStreamFactory::MakeRegionSound(name, beginFrame, endFrame);
	return (sound) ? MakeSoundPtr(sound) : 0;
}

AUDIOAPI AudioStreamPtr MakeStereoSoundPtr(AudioStreamPtr sound)
{
	return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeStereoSound(static_cast<TAudioStreamPtr>(*sound))) : 0;
}

AUDIOAPI AudioStreamPtr MakeFadeSoundPtr(AudioStreamPtr sound, long fadeIn, long fadeOut)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeFadeSound(static_cast<TAudioStreamPtr>(*sound), fadeIn, fadeOut)) : 0;
}

AUDIOAPI AudioStreamPtr MakeLoopSoundPtr(AudioStreamPtr sound, long n)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeLoopSound(static_cast<TAudioStreamPtr>(*sound), n)) : 0;
}

AUDIOAPI AudioStreamPtr MakeCutSoundPtr(AudioStreamPtr sound, long beginFrame, long endFrame)
{
	if (sound) {
		TAudioStreamPtr cut = TAudioStreamFactory::MakeCutSound(static_cast<TAudioStreamPtr>(*sound), beginFrame, endFrame);
		return (cut) ?  MakeSoundPtr(cut) : 0;
	} else {
        return 0;
	}  
}

AUDIOAPI AudioStreamPtr MakeSeqSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade)
{
    return (s1 && s2) ? MakeSoundPtr(TAudioStreamFactory::MakeSeqSound(static_cast<TAudioStreamPtr>(*s1), static_cast<TAudioStreamPtr>(*s2), crossFade)) : 0;
}

AUDIOAPI AudioStreamPtr MakeMixSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2)
{
    return (s1 && s2) ? MakeSoundPtr(TAudioStreamFactory::MakeMixSound(static_cast<TAudioStreamPtr>(*s1), static_cast<TAudioStreamPtr>(*s2))) : 0;
}

AUDIOAPI AudioStreamPtr MakeInputSoundPtr()
{
    return MakeSoundPtr(TAudioStreamFactory::MakeInputSound());
}

AUDIOAPI AudioStreamPtr MakeSharedBufferedInputSoundPtr(long beginFrame)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeSharedBufferedInputSound(beginFrame));
}

AUDIOAPI AudioStreamPtr MakeTransformSoundPtr(AudioStreamPtr sound, AudioEffectListPtr list_effect, long fadeIn, long fadeOut)
{
    return (sound && list_effect) 
		? MakeSoundPtr(TAudioStreamFactory::MakeTransformSound(static_cast<TAudioStreamPtr>(*sound), static_cast<TAudioEffectListPtr>(*list_effect), fadeIn, fadeOut))
		: 0;
}

AUDIOAPI AudioStreamPtr MakePitchSchiftTimeStretchSoundPtr(AudioStream sound, double* pitch_shift, double* time_strech)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeRubberBandSound(static_cast<TAudioStreamPtr>(sound), pitch_shift, time_strech)) : 0;
    /*
#ifdef SOUND_TOUCH
	return (s) ? MakeSoundPtr(TAudioStreamFactory::MakeSoundTouchSound(static_cast<TAudioStreamPtr>(s), pitch_shift, time_strech)) : 0;
#else
    return 0;
#endif
    */
}

AUDIOAPI AudioStreamPtr MakeWriteSoundPtr(char* name, AudioStreamPtr sound, long format)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeWriteSound(name, static_cast<TAudioStreamPtr>(*sound), format)) : 0;
}

AUDIOAPI AudioStreamPtr MakeRendererSoundPtr(AudioStreamPtr sound)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeDTRenderer(static_cast<TAudioStreamPtr>(*sound))) : 0;
}

AUDIOAPI long GetLengthSoundPtr(AudioStreamPtr sound)
{
    return (sound) ? (static_cast<TAudioStreamPtr>(*sound))->Length() : 0;
}

AUDIOAPI long GetChannelsSoundPtr(AudioStreamPtr sound)
{
    return (sound) ? (static_cast<TAudioStreamPtr>(*sound))->Channels() : 0;
}

AUDIOAPI void ResetSoundPtr(AudioStreamPtr sound)
{
	static_cast<TAudioStreamPtr>(*sound)->Reset();
}

AUDIOAPI long ReadSoundPtr(AudioStreamPtr sound, float* buffer, long buffer_size, long channels)
{
    if (sound && buffer) {
        TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
		UAudioTools::ZeroFloatBlk(buffer, buffer_size, channels);
        return static_cast<TAudioStreamPtr>(*sound)->Read(&process, buffer_size, 0, channels);
    } else {
        return 0;
    }
}

// Effect management
AudioEffectList AUDIOAPI MakeAudioEffectList()
{
    return new TAudioEffectList();
}

AudioEffectList AUDIOAPI AddAudioEffect(AudioEffectList list_effect, AudioEffect effect)
{
    if (list_effect && effect) {
       static_cast<TAudioEffectListPtr>(list_effect)->push_back(static_cast<TAudioEffectInterfacePtr>(effect));
    }
    return list_effect;
}

AudioEffectList AUDIOAPI RemoveAudioEffect(AudioEffectList list_effect, AudioEffect effect)
{
    if (list_effect && effect) {
        static_cast<TAudioEffectListPtr>(list_effect)->remove(static_cast<TAudioEffectInterfacePtr>(effect));
    }
    return list_effect;
}

AudioEffectList AUDIOAPI ClearAudioEffectList(AudioEffectList list_effect)
{
	if (list_effect) {
        static_cast<TAudioEffectListPtr>(list_effect)->clear();
    }
    return list_effect;
}

AudioEffect AUDIOAPI MakeVolAudioEffect(float vol)
{
    return new TVolAudioEffect(vol);
}

AudioEffect AUDIOAPI MakeMonoPanAudioEffect(float pan)
{
    return new TMonoPanAudioEffect(pan);
}

AudioEffect AUDIOAPI MakeStereoPanAudioEffect(float panLeft, float panRight)
{
    return new TStereoPanAudioEffect(panLeft, panRight);
}

AudioEffect AUDIOAPI MakePitchShiftAudioEffect(float pitch)
{
    return new TPitchShiftAudioEffect(pitch);
}

AudioEffect AUDIOAPI MakeFaustAudioEffect(const char* name)
{
	try {
		return new TModuleFaustAudioEffect(name);
	} catch (const char* error) {
	    strncpy(gLastLibError, error, 512);
        try {
            return new TCodeFaustAudioEffect(name);
        } catch (const char* error) {
            strncpy(gLastLibError, error, 512);
            return 0;
        }
	}
}

AUDIOAPI long GetControlCountEffect(AudioEffect effect) 
{
	return static_cast<TAudioEffectInterfacePtr>(effect)->GetControlCount();
}

AUDIOAPI void GetControlParamEffect(AudioEffect effect, long control, char* label, float* min, float* max, float* init)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->GetControlParam(control, label, min, max, init);
}

AUDIOAPI void SetControlValueEffect(AudioEffect effect, long control, float f)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->SetControlValue(control, f);
}

AUDIOAPI float GetControlValueEffect(AudioEffect effect, long control)
{
	return static_cast<TAudioEffectInterfacePtr>(effect)->GetControlValue(control);
}

AUDIOAPI void SetStateEffect(AudioEffect effect, long state)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->SetState(bool(state));
}

AUDIOAPI long GetStateEffect(AudioEffect effect)
{
	return static_cast<TAudioEffectInterfacePtr>(effect)->GetState();
}

AUDIOAPI void ResetEffect(AudioEffect effect)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->Reset();
}

AUDIOAPI void ProcessEffect(AudioEffect effect, float** input, float** output, long framesNum, long channels)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->Process(input, output, framesNum, channels);
}

AUDIOAPI const char* GetJsonEffect(AudioEffect effect) 
{ 
    TAudioEffectInterface* effect_tmp = static_cast<TAudioEffectInterface*>(effect);
    TFaustAudioEffectBase* faust_effect;
    if ((faust_effect = dynamic_cast<TFaustAudioEffectBasePtr>(effect_tmp))) {
        return faust_effect->GetJson();
    } else {
        return "";
    }
}

// Effect management with pointer
AUDIOAPI void DeleteEffectListPtr(AudioEffectListPtr list_effect) 
{
	delete list_effect;
}

AUDIOAPI void DeleteEffectPtr(AudioEffectPtr effect) 
{
	delete effect;
}

AUDIOAPI AudioEffectListPtr MakeAudioEffectListPtr()
{
    return new LA_SMARTP<TAudioEffectList>(new TAudioEffectList());
}

AUDIOAPI AudioEffectListPtr AddAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
    if (list_effect && effect) {
        static_cast<TAudioEffectListPtr>(*list_effect)->push_back(static_cast<TAudioEffectInterfacePtr>(*effect));
    }
    return list_effect;
}

AUDIOAPI AudioEffectListPtr RemoveAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
    if (list_effect && effect) {
        static_cast<TAudioEffectListPtr>(*list_effect)->remove(static_cast<TAudioEffectInterfacePtr>(*effect));
    }
    return list_effect;
}

AUDIOAPI AudioEffectListPtr ClearAudioEffectListPtr(AudioEffectListPtr list_effect)
{
	if (list_effect) {
        static_cast<TAudioEffectListPtr>(*list_effect)->clear();
    }
    return list_effect;
}

AUDIOAPI AudioEffectPtr MakeVolAudioEffectPtr(float vol)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TVolAudioEffect(vol));
}

AUDIOAPI AudioEffectPtr MakeMonoPanAudioEffectPtr(float pan)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TMonoPanAudioEffect(pan));
}

AUDIOAPI AudioEffectPtr MakeStereoPanAudioEffectPtr(float panLeft, float panRight)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TStereoPanAudioEffect(panLeft, panRight));
}

AUDIOAPI AudioEffectPtr MakePitchShiftAudioEffectPtr(float pitch)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TPitchShiftAudioEffect(pitch));
}

AUDIOAPI AudioEffectPtr MakeFaustAudioEffectPtr(const char* name)
{
    try {
        return new LA_SMARTP<TAudioEffectInterface>(new TModuleFaustAudioEffect(name));
    } catch (const char* error) {
        strncpy(gLastLibError, error, 512);
        try {
            return new LA_SMARTP<TAudioEffectInterface>(new TCodeFaustAudioEffect(name));
        } catch (const char* error) {
            strncpy(gLastLibError, error, 512);
            return 0;
        }
    }
}

#ifdef __APPLE__
#include<dispatch/dispatch.h>
static TCodeFaustAudioEffect* gDSP = NULL;

AudioEffectPtr MakeDispatchFaustAudioEffectPtr(const char* name)
{
    try {
        return new LA_SMARTP<TAudioEffectInterface>(new TModuleFaustAudioEffect(name));
    } catch (const char* error) {
        strncpy(gLastLibError, error, 512);
        dispatch_sync(dispatch_get_main_queue(),
        ^{ 
            try {
                gDSP = new TCodeFaustAudioEffect(name); 
            } catch (const char* error) {
                strncpy(gLastLibError, error, 512);
                gDSP = NULL;
            } 
        });
        return (gDSP) ? new LA_SMARTP<TAudioEffectInterface>(gDSP) : 0; 
    }
}
#endif

AUDIOAPI AudioEffectPtr MakeWrapperAudioEffectPtr(AudioEffectInterfacePtr effect)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TWrapperAudioEffect(static_cast<TAudioEffectInterface*>(effect)));
}

AUDIOAPI long GetControlCountEffectPtr(AudioEffectPtr effect) 
{
	return static_cast<TAudioEffectInterfacePtr>(*effect)->GetControlCount();
}

AUDIOAPI void GetControlParamEffectPtr(AudioEffectPtr effect, long control, char* label, float* min, float* max, float* init)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->GetControlParam(control, label, min, max, init);
}

AUDIOAPI void SetControlValueEffectPtr(AudioEffectPtr effect, long control, float f)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->SetControlValue(control, f);
}

AUDIOAPI float GetControlValueEffectPtr(AudioEffectPtr effect, long control)
{
	return static_cast<TAudioEffectInterfacePtr>(*effect)->GetControlValue(control);
}

AUDIOAPI void SetStateEffectPtr(AudioEffectPtr effect, long state)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->SetState(bool(state));
}

AUDIOAPI long GetStateEffectPtr(AudioEffectPtr effect)
{
	return static_cast<TAudioEffectInterfacePtr>(*effect)->GetState();
}

AUDIOAPI void ResetEffectPtr(AudioEffectPtr effect)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->Reset();
}

AUDIOAPI void ProcessEffectPtr(AudioEffectPtr effect, float** input, float** output, long framesNum, long channels)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->Process(input, output, framesNum, channels);
}

AUDIOAPI const char* GetJsonEffectPtr(AudioEffectPtr effect) 
{ 
    TAudioEffectInterface* effect_tmp = static_cast<TAudioEffectInterface*>(*effect);
    TFaustAudioEffectBase* faust_effect;
    if ((faust_effect = dynamic_cast<TFaustAudioEffectBasePtr>(effect_tmp))) {
        return faust_effect->GetJson();
    } else {
        return "";
    }
}

// Open/Close

AUDIOAPI void SetAudioLatencies(long inputLatency, long outputLatency)
{
	TAudioGlobals::fInputLatency = inputLatency;
	TAudioGlobals::fOutputLatency = outputLatency;
}

AUDIOAPI AudioPlayerPtr OpenAudioPlayer(long inChan, 
                                        long outChan, 
                                        long channels, 
                                        long sample_rate, 
                                        long buffer_size, 
                                        long stream_buffer_size, 
                                        long rtstream_buffer_size, 
                                        long renderer,
                                        long thread_num)
{
    int res;
	
	if (thread_num < 1) {
		printf("OpenAudioPlayer error: thread_num parameter should be at least one !! \n");
        strncpy(gLastLibError, "OpenAudioPlayer error: thread_num parameter should be at least one !!", 256);
    }

    TAudioGlobals::Init(inChan, outChan, channels, sample_rate, buffer_size, stream_buffer_size, rtstream_buffer_size, thread_num);

    AudioPlayerPtr player = static_cast<AudioPlayerPtr>(calloc(1, sizeof(AudioPlayer)));
    if (!player) {
        goto error;
    }

	player->fRenderer = TAudioRendererFactory::MakeAudioRenderer(renderer); 
    if (!player->fRenderer) {
        goto error;
    }

	player->fMixer = new TAudioMixer;
    if (!player->fMixer) {
        goto error;
    }

	player->fRenderer->AddClient(player->fMixer);
	res = player->fRenderer->OpenDefault(inChan, outChan, buffer_size, sample_rate);

    if (res == NO_ERR) {
        return player;
    }

error:
    CloseAudioPlayer(player);
    return 0;
}

AUDIOAPI AudioPlayerPtr OpenAudioClient(AudioRendererPtr renderer)
{
	AudioPlayerPtr player = static_cast<AudioPlayerPtr>(calloc(1, sizeof(AudioPlayer)));
    if (!player) {
        goto error;
    }
		
	player->fRenderer = renderer;
		
	player->fMixer = new TAudioMixer;
    if (!player->fMixer) {
        goto error;
    }

	player->fRenderer->AddClient(player->fMixer);		
	return player;

error:
    CloseAudioClient(player);
    return 0;
}

AUDIOAPI void CloseAudioPlayer(AudioPlayerPtr player)
{
    if (!player) {
        return;
    }

	if (player->fMixer) {    
		player->fRenderer->RemoveClient(player->fMixer);
        delete player->fMixer;
    }

    if (player->fRenderer) {
		player->fRenderer->Close();
        delete player->fRenderer;
    }

    free(player);
	TAudioGlobals::Destroy();
}

AUDIOAPI void CloseAudioClient(AudioPlayerPtr player)
{
    if (!player) {
        return;
    }

	if (player->fMixer) {
		player->fRenderer->RemoveClient(player->fMixer);
        delete player->fMixer;
    }
  
    free(player);
}

// SoundFile management
AUDIOAPI long LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight)
{
    if (player && player->fMixer && sound) {
        return player->fMixer->Load(static_cast<TAudioStreamPtr>(sound), chan, vol, panLeft, panRight);
    } else {
        return LOAD_ERR;
    }
}

AUDIOAPI long LoadChannelPtr(AudioPlayerPtr player, AudioStreamPtr sound, long chan, float vol, float panLeft, float panRight)
{
    if (player && player->fMixer && sound) {
        return player->fMixer->Load(static_cast<TAudioStreamPtr>(*sound), chan, vol, panLeft, panRight);
    } else {
        return LOAD_ERR;
    }
}

AUDIOAPI void GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfo* info)
{
    if (player && player->fMixer && info) {
        player->fMixer->GetInfo(chan, info);
    }
}

AUDIOAPI void GetChannelInfo(AudioPlayerPtr player, long chan, ChannelInfo* info)
{
    if (player && player->fMixer && info) {
        player->fMixer->GetInfo(chan, info);
    }
}

AUDIOAPI void SetStopCallbackChannel(AudioPlayerPtr player,long chan, StopCallback callback, void* context)
{
    if (player && player->fMixer) {
        player->fMixer->SetStopCallback(chan, callback, context);
    }
}

// Transport
AUDIOAPI void StartChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer) {
        player->fMixer->Start(chan);
    }
}

AUDIOAPI void ContChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer) {
        player->fMixer->Play(chan);
    }
}

AUDIOAPI void StopChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer) {
        player->fMixer->Stop(chan);
    }
}

AUDIOAPI void AbortChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer) {
        player->fMixer->Abort(chan);
    }
}

AUDIOAPI void StartAudioPlayer(AudioPlayerPtr player)
{
    if (player && player->fMixer && player->fRenderer) {
        // Reset real-time input
        TAudioGlobals::fSharedInput->Reset();
        // Start player
        player->fRenderer->Start();
    }
}

AUDIOAPI void StopAudioPlayer(AudioPlayerPtr player)
{
    if (player && player->fMixer && player->fRenderer) {
        player->fRenderer->Stop();
    }
}

// Params
AUDIOAPI void SetVolChannel(AudioPlayerPtr player, long chan, float vol)
{
    if (player && player->fMixer) {
        player->fMixer->SetVol(chan, vol);
    }
}

AUDIOAPI void SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight)
{
    if (player && player->fMixer) {
        player->fMixer->SetPan(chan, panLeft, panRight);
    }
}

AUDIOAPI void SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectList effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer) {
        player->fMixer->SetEffectList(chan, effect_list, fadeIn, fadeOut);
    }
}

AUDIOAPI void SetEffectListChannelPtr(AudioPlayerPtr player, long chan, AudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer) {
        player->fMixer->SetEffectList(chan, *effect_list, fadeIn, fadeOut);
    }
}

// Master
AUDIOAPI void SetVolAudioPlayer(AudioPlayerPtr player, float vol)
{
    if (player && player->fMixer) {
        player->fMixer->SetVol(vol);
    }
}

AUDIOAPI void SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight)
{
    if (player && player->fMixer)
        player->fMixer->SetPan(panLeft, panRight);
}

AUDIOAPI void SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectList effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer) {
        player->fMixer->SetEffectList(effect_list, fadeIn, fadeOut);
    }
}

AUDIOAPI void SetEffectListAudioPlayerPtr(AudioPlayerPtr player, AudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer) {
        player->fMixer->SetEffectList(*effect_list, fadeIn, fadeOut);
    }
}

AUDIOAPI AudioRendererPtr GetAudioPlayerRenderer(AudioPlayerPtr player)
{
    return (player) ? player->fRenderer : NULL;
}

// Globals
AUDIOAPI AudioRendererPtr MakeAudioRenderer(long renderer)
{
	return static_cast<AudioRendererPtr>(TAudioRendererFactory::MakeAudioRenderer(renderer));
}

AUDIOAPI void DeleteAudioRenderer(AudioRendererPtr obj)
{
	TAudioRendererPtr renderer = static_cast<TAudioRendererPtr>(obj);
	delete renderer;
}

long AUDIOAPI OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
	return static_cast<TAudioRendererPtr>(renderer)->Open(inputDevice, outputDevice, inChan, outChan, bufferSize, sampleRate);
}

AUDIOAPI void CloseAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Close();
}

AUDIOAPI void StartAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Start();
}

AUDIOAPI void StopAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Stop();
}

AUDIOAPI void GetAudioRendererInfo(AudioRendererPtr renderer, RendererInfoPtr info)
{
    static_cast<TAudioRendererPtr>(renderer)->GetInfo(static_cast<RendererInfoPtr>(info));
}

AUDIOAPI void AddAudioClient(AudioRendererPtr renderer, AudioClientPtr client)
{
	static_cast<TAudioRendererPtr>(renderer)->AddClient(static_cast<TAudioClientPtr>(client));
}

AUDIOAPI void RemoveAudioClient(AudioRendererPtr renderer, AudioClientPtr client)
{
	static_cast<TAudioRendererPtr>(renderer)->RemoveClient(static_cast<TAudioClientPtr>(client));
}

AUDIOAPI void AudioGlobalsInit(long inChan, 
								long outChan, 
								long channels, 
								long sample_rate,
								long buffer_size, 
								long stream_buffer_size, 
								long rtstream_buffer_size,
								long thread_num)
{
	TAudioGlobals::Init(inChan, outChan, channels, sample_rate, buffer_size, stream_buffer_size, rtstream_buffer_size, thread_num);
}

AUDIOAPI void AudioGlobalsDestroy()
{
	TAudioGlobals::Destroy();
}

AUDIOAPI long GetDeviceCount(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->GetDeviceCount();
}

AUDIOAPI void GetDeviceInfo(AudioRendererPtr renderer, long deviceNum, DeviceInfo* info)
{
	static_cast<TAudioRendererPtr>(renderer)->GetDeviceInfo(deviceNum, info);
}

AUDIOAPI long GetDefaultInputDevice(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->GetDefaultInputDevice();
}

AUDIOAPI long GetDefaultOutputDevice(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->GetDefaultOutputDevice();
}


