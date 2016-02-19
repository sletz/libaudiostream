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

#include "TExpAudioMixer.h"
#include "TAudioRendererFactory.h"
#include "TAudioStreamFactory.h"
#include "TFaustAudioEffect.h"
#include "TBufferedInputAudioStream.h"
#include "TSelectAudioStream.h"
#include "TAudioDate.h"

// Renderers
#include "TPortAudioV19Renderer.h"
#ifdef __COREAUDIO__
#include "TCoreAudioRenderer.h"
#endif
#ifdef __JACK__
#include "TJackRenderer.h"
#include "TNetJackRenderer.h"
#endif
#include "TOfflineRenderer.h"

#ifdef WIN32
	#define	AUDIOAPI __declspec(dllexport)
#else
	#define	AUDIOAPI __attribute__ ((visibility("default")))
#endif

    struct AudioPlayer {
        TAudioRendererPtr fRenderer;
		TExpAudioMixerPtr fMixer;  	
    };
    
    
    // Opaque pointers
    typedef void* AudioPlayerPtr;
    typedef void* AudioRendererPtr;
    typedef void* AudioClientPtr;

    typedef AudioPlayer* IntAudioPlayerPtr;
    typedef TAudioRenderer* IntAudioRendererPtr;	
    typedef TAudioClient* IntAudioClientPtr;

    typedef TAudioStreamPtr AudioStream;			// smart pointer type
    typedef AudioStream* AudioStreamPtr;

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
    AUDIOAPI void GetErrors(ErrorInfo* error, bool clear);
		
	// Device scanning
	AUDIOAPI long GetDeviceCount(long renderer);
	AUDIOAPI void GetDeviceInfo(long renderer, long deviceNum, DeviceInfo* info);
	AUDIOAPI long GetDefaultInputDevice(long renderer);
	AUDIOAPI long GetDefaultOutputDevice(long renderer);
	
	AUDIOAPI AudioStreamPtr MakeSoundPtr(AudioStream sound) ;
	AUDIOAPI void DeleteSoundPtr(AudioStreamPtr sound);
	
	// Build sound (using pointer on smartptr)
	AUDIOAPI AudioStreamPtr MakeNullSoundPtr(long length);
    AUDIOAPI AudioStreamPtr MakeMultiNullSoundPtr(long length);
    AUDIOAPI AudioStreamPtr MakeConstantSoundPtr(long channels, long length, float value);
    AUDIOAPI AudioStreamPtr MakeBufferSoundPtr(float** buffer, long length, long channels, bool clear);
    AUDIOAPI AudioStreamPtr MakeReadSoundPtr(char* name);
    AUDIOAPI AudioStreamPtr MakeRegionSoundPtr(char* name, long beginFrame, long endFrame);
    AUDIOAPI AudioStreamPtr MakeFadeSoundPtr(AudioStreamPtr sound, long fadeIn, long fadeOut);
    AUDIOAPI AudioStreamPtr MakeLoopSoundPtr(AudioStreamPtr sound, long n);
    AUDIOAPI AudioStreamPtr MakeCutSoundPtr(AudioStreamPtr sound, long beginFrame, long endFrame);
    AUDIOAPI AudioStreamPtr MakeSeqSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade);
    AUDIOAPI AudioStreamPtr MakeMixSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2);
    AUDIOAPI AudioStreamPtr MakeParSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2);
    AUDIOAPI AudioStreamPtr MakeSelectSoundPtr(AudioStreamPtr s1, long* selection, long channels);
    AUDIOAPI AudioStreamPtr MakeEffectSoundPtr(AudioStreamPtr sound, AudioEffectPtr effect, long fadeIn, long fadeOut);
    AUDIOAPI AudioStreamPtr MakePitchSchiftTimeStretchSoundPtr(AudioStreamPtr sound, double* pitch_shift, double* time_strech);
    AUDIOAPI AudioStreamPtr MakeWriteSoundPtr(char* name, AudioStreamPtr s, long format);
    AUDIOAPI AudioStreamPtr MakeInputSoundPtr();
    AUDIOAPI AudioStreamPtr MakeSharedInputSoundPtr();
    AUDIOAPI AudioStreamPtr MakeRendererSoundPtr(AudioStreamPtr s);

    AUDIOAPI long GetLengthSoundPtr(AudioStreamPtr s);
    AUDIOAPI long GetChannelsSoundPtr(AudioStreamPtr s);
    AUDIOAPI long ReadSoundPtr(AudioStreamPtr stream, float** buffer, long buffer_size);
    AUDIOAPI long ReadSoundPosPtr(AudioStreamPtr stream, float** buffer, long buffer_size, long frames, long pos);
    AUDIOAPI long WriteSoundPtr(AudioStreamPtr stream, float** buffer, long buffer_size);
    AUDIOAPI void ResetSoundPtr(AudioStreamPtr sound);
    AUDIOAPI long SetPosSoundPtr(AudioStreamPtr sound, long frames);
    AUDIOAPI long GetPosSoundPtr(AudioStreamPtr sound);
    AUDIOAPI AudioStreamPtr MakeCopySoundPtr(AudioStreamPtr sound);

    // Build effect (using pointer on smartptr)
    AUDIOAPI AudioEffectPtr MakeFaustAudioEffectPtr(const char* code, const char* library_path, const char* draw_path);
    AUDIOAPI AudioEffectPtr MakeRemoteFaustAudioEffectPtr(const char* code, const char* library_path, const char* draw_path);
#ifdef __APPLE__
    // To be used from LispWorks
    AUDIOAPI AudioEffectPtr MakeDispatchFaustAudioEffectPtr(const char* code, const char* library_path, const char* draw_path);
#endif
	
	AUDIOAPI long GetControlCountEffectPtr(AudioEffectPtr effect);
	AUDIOAPI void GetControlParamEffectPtr(AudioEffectPtr effect, long param, char* label, float* min, float* max, float* init);
	AUDIOAPI void SetControlValueEffectPtr(AudioEffectPtr effect, long param, float f);
	AUDIOAPI float GetControlValueEffectPtr(AudioEffectPtr effect, long param);
	
	AUDIOAPI void SetStateEffectPtr(AudioEffectPtr effect, long state);
	AUDIOAPI long GetStateEffectPtr(AudioEffectPtr effect);
	AUDIOAPI void ResetEffectPtr(AudioEffectPtr effect);

    AUDIOAPI void ProcessEffectPtr(AudioEffectPtr effect, float** input, float** output, long framesNum);
    AUDIOAPI const char* GetJsonEffectPtr(AudioEffectPtr effect);
    AUDIOAPI const char* GetNameEffectPtr(AudioEffectPtr effect);
    AUDIOAPI AudioEffectPtr MakeCopyEffectPtr(AudioEffectPtr effect);
    AUDIOAPI void DeleteEffectPtr(AudioEffectPtr effect);
    
    AUDIOAPI long SetTimedControlValueEffectPtr(AudioPlayerPtr player, const char* effect, const char* path, float value, SymbolicDatePtr date);
 
    // Open/Close
	AUDIOAPI void SetAudioLatencies(long inputLatency, long outputLatency);
    AUDIOAPI AudioPlayerPtr OpenAudioPlayer(long inChan, 
                                            long outChan, 
                                            long sample_rate, 
                                            long buffer_size, 
                                            long stream_buffer_size, 
                                            long rtstream_duration, 
                                            long renderer,
                                            long thread_num);
	AUDIOAPI AudioPlayerPtr OpenAudioClient(AudioRendererPtr renderer);	
    
    AUDIOAPI long SetMasterEffect(AudioPlayerPtr player, AudioEffect effect);
    AUDIOAPI long SetMasterEffectPtr(AudioPlayerPtr player, AudioEffectPtr effect);
									
    AUDIOAPI void CloseAudioPlayer(AudioPlayerPtr player);
	AUDIOAPI void CloseAudioClient(AudioPlayerPtr player);
    
    AUDIOAPI audio_frame_t GetAudioPlayerDateInUsec(AudioPlayerPtr player);
    AUDIOAPI audio_frame_t GetAudioPlayerDateInFrame(AudioPlayerPtr player);
  
    // Add a sound in the player
    AUDIOAPI long StartSound(AudioPlayerPtr player, AudioStream sound, SymbolicDate date);
    AUDIOAPI long StopSound(AudioPlayerPtr player, AudioStream sound, SymbolicDate date);
    
    AUDIOAPI SymbolicDate GenSymbolicDate(AudioPlayerPtr player);
    AUDIOAPI SymbolicDate GenRealDate(AudioPlayerPtr player, audio_frame_t date);
    AUDIOAPI long SetSymbolicDate(AudioPlayerPtr player, SymbolicDate symbolic_date, audio_frame_t real_date);
    AUDIOAPI audio_frame_t GetSymbolicDate(AudioPlayerPtr player, SymbolicDate symbolic_date);
    
    // Using pointer on smartptr
    AUDIOAPI long StartSoundPtr(AudioPlayerPtr player, AudioStreamPtr sound, SymbolicDatePtr date);
    AUDIOAPI long StopSoundPtr(AudioPlayerPtr player, AudioStreamPtr sound, SymbolicDatePtr date);
    
    AUDIOAPI SymbolicDatePtr GenSymbolicDatePtr(AudioPlayerPtr player);
    AUDIOAPI SymbolicDatePtr GenRealDatePtr(AudioPlayerPtr player, audio_frame_t date);
    AUDIOAPI long SetSymbolicDatePtr(AudioPlayerPtr player, SymbolicDatePtr symbolic_date, audio_frame_t real_date);
    AUDIOAPI audio_frame_t GetSymbolicDatePtr(AudioPlayerPtr player, SymbolicDatePtr symbolic_date);
    AUDIOAPI void DeleteSymbolicDatePtr(SymbolicDatePtr symbolic_date);
   
    // Transport
    AUDIOAPI long StartAudioPlayer(AudioPlayerPtr player);		
    AUDIOAPI long StopAudioPlayer(AudioPlayerPtr player);	
    AUDIOAPI long PauseAudioPlayer(AudioPlayerPtr player);	
    AUDIOAPI long ContAudioPlayer(AudioPlayerPtr player);
    AUDIOAPI long SetPosAudioPlayer(AudioPlayerPtr player, audio_frame_t new_date);
    
    AUDIOAPI long ClearAudioPlayer(AudioPlayerPtr player);

    AUDIOAPI AudioRendererPtr GetAudioPlayerRenderer(AudioPlayerPtr player);
   
	// Renderer
	AUDIOAPI AudioRendererPtr MakeAudioRenderer(long renderer);
	AUDIOAPI void DeleteAudioRenderer(AudioRendererPtr renderer);
	
	AUDIOAPI long OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
	AUDIOAPI void CloseAudioRenderer(AudioRendererPtr renderer); 
	AUDIOAPI long StartAudioRenderer(AudioRendererPtr renderer); 
    AUDIOAPI long StopAudioRenderer(AudioRendererPtr renderer); 
    AUDIOAPI void GetAudioRendererInfo(AudioRendererPtr renderer, RendererInfoPtr info); 
	
	AUDIOAPI void AddAudioClient(AudioRendererPtr renderer, AudioClientPtr client); 
	AUDIOAPI void RemoveAudioClient(AudioRendererPtr renderer, AudioClientPtr client); 
	
	// Globals
	AUDIOAPI void AudioGlobalsInit(long inChan, 
									long outChan, 
									long sample_rate,
									long buffer_size, 
									long stream_buffer_size, 
									long rtstream_duration,
									long thread_num);
	AUDIOAPI void AudioGlobalsDestroy();

    // Build sound (using smartptr)
    AUDIOAPI AudioStream MakeNullSound(long length);
    AUDIOAPI AudioStream MakeMultiNullSound(long channels, long length);
    AUDIOAPI AudioStream MakeConstantSound(long channels, long length, float value);
    AUDIOAPI AudioStream MakeBufferSound(float** buffer, long length, long channels, bool clear);
    AUDIOAPI AudioStream MakeReadSound(const char* name);
    AUDIOAPI AudioStream MakeRegionSound(const char* name, long beginFrame, long endFrame);
    AUDIOAPI AudioStream MakeFadeSound(AudioStream sound, long fadeIn, long fadeOut);
    AUDIOAPI AudioStream MakeLoopSound(AudioStream sound, long n);
    AUDIOAPI AudioStream MakeCutSound(AudioStream sound, long beginFrame, long endFrame);
    AUDIOAPI AudioStream MakeSeqSound(AudioStream s1, AudioStream s2, long crossFade);
    AUDIOAPI AudioStream MakeMixSound(AudioStream s1, AudioStream s2);
    AUDIOAPI AudioStream MakeParSound(AudioStream s1, AudioStream s2);
    AUDIOAPI AudioStream MakeSelectSound(AudioStream s1, long* selection, long channels);
    AUDIOAPI AudioStream MakeRubberBandSound(AudioStreamPtr sound, double* pitch_shift, double* time_strech);
    AUDIOAPI AudioStream MakeEffectSound(AudioStream sound, AudioEffect effect, long fadeIn, long fadeOut);
    AUDIOAPI AudioStream MakeWriteSound(const char* name, AudioStream s, long format);
    AUDIOAPI AudioStream MakeInputSound();
    AUDIOAPI AudioStream MakeSharedInputSound();
    AUDIOAPI AudioStream MakeRendererSound(AudioStream s);
    AUDIOAPI AudioStream MakePitchSchiftTimeStretchSound(AudioStream s, double* pitch_shift, double* time_strech);

    AUDIOAPI long GetLengthSound(AudioStream s);
    AUDIOAPI long GetChannelsSound(AudioStream s);
    AUDIOAPI long ReadSound(AudioStream stream, float** buffer, long buffer_size);
    AUDIOAPI long ReadSoundPos(AudioStream stream, float** buffer, long buffer_size, long frames, long pos);
    AUDIOAPI long WriteSound(AudioStream stream, float** buffer, long buffer_size);
    AUDIOAPI void ResetSound(AudioStream sound);
    AUDIOAPI long SetPosSound(AudioStream sound, long frames);
    AUDIOAPI long GetPosSound(AudioStream sound);
    AUDIOAPI AudioStream MakeCopySound(AudioStream sound);

    // Effect management (using smartptr)
    AUDIOAPI AudioEffect MakeFaustAudioEffect(const char* code, const char* library_path, const char* draw_path);
    AUDIOAPI AudioEffect MakeRemoteFaustAudioEffect(const char* code, const char* library_path, const char* draw_path);

    AUDIOAPI long GetControlCountEffect(AudioEffect effect);
    AUDIOAPI void GetControlParamEffect(AudioEffect effect, long param, char* label, float* min, float* max, float* init);
    AUDIOAPI void SetControlValueEffect(AudioEffect effect, long param, float f);
    AUDIOAPI float GetControlValueEffect(AudioEffect effect, long param);

    AUDIOAPI void SetStateEffect(AudioEffect effect, long state);
    AUDIOAPI long GetStateEffect(AudioEffect effect);
    AUDIOAPI void ResetEffect(AudioEffect effect);

    AUDIOAPI void ProcessEffect(AudioEffect effect, float** input, float** output, long framesNum);
    AUDIOAPI const char* GetJsonEffect(AudioEffect effect);
    AUDIOAPI const char* GetNameEffect(AudioEffect effect);
    AUDIOAPI AudioEffect MakeCopyEffect(AudioEffect effect);

    AUDIOAPI long SetTimedControlValueEffect(AudioPlayerPtr player, const char* effect, const char* path, float value, SymbolicDate date);

#ifdef __cplusplus
}
#endif

AUDIOAPI long LibVersion()
{
	return 203;
}

AUDIOAPI const char* GetLastLibError()
{
    return TAudioGlobals::fLastLibError;
}

AUDIOAPI void GetErrors(ErrorInfo* error, bool clear)
{
    strncpy(error->fStreamError, TAudioGlobals::fLastLibError, 512);
    error->fDiskError = TAudioGlobals::fDiskError;
    error->fSchedulingError = TAudioGlobals::fSchedulingError;
    if (clear) {
        strcpy(TAudioGlobals::fLastLibError, "");
        TAudioGlobals::fDiskError = 0;
        TAudioGlobals::fSchedulingError = 0;
    }
}

AUDIOAPI AudioStream MakeNullSound(long length)
{
	return TAudioStreamFactory::MakeNullSound(length);
}

AUDIOAPI AudioStream MakeMultiNullSound(long channels, long length)
{
	return TAudioStreamFactory::MakeMultiNullSound(channels, length);
}

AUDIOAPI AudioStream MakeConstantSound(long channels, long length, float value)
{
	return TAudioStreamFactory::MakeConstantSound(channels, length, value);
}

AUDIOAPI AudioStream MakeBufferSound(float** buffer, long length, long channels, bool clear)
{
	return TAudioStreamFactory::MakeBufferSound(buffer, length, channels, clear);
}

AUDIOAPI AudioStream MakeReadSound(const char* name)
{
    return TAudioStreamFactory::MakeReadSound(name);
}

AUDIOAPI AudioStream MakeRegionSound(const char* name, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeRegionSound(name, beginFrame, endFrame);
}

AUDIOAPI AudioStream MakeFadeSound(AudioStream sound, long fadeIn, long fadeOut)
{
    return TAudioStreamFactory::MakeFadeSound(static_cast<TAudioStreamPtr>(sound), fadeIn, fadeOut);
}

AUDIOAPI AudioStream MakeLoopSound(AudioStream sound, long n)
{
    return TAudioStreamFactory::MakeLoopSound(static_cast<TAudioStreamPtr>(sound), n);
}

AUDIOAPI AudioStream MakeCutSound(AudioStream sound, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeCutSound(static_cast<TAudioStreamPtr>(sound), beginFrame, endFrame);
}

AUDIOAPI AudioStream MakeSeqSound(AudioStream s1, AudioStream s2, long crossFade)
{
    return TAudioStreamFactory::MakeSeqSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioStreamPtr>(s2), crossFade);
}

AUDIOAPI AudioStream MakeMixSound(AudioStream s1, AudioStream s2)
{
    return TAudioStreamFactory::MakeMixSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioStreamPtr>(s2));
}

AUDIOAPI AudioStream MakeParSound(AudioStream s1, AudioStream s2)
{
    return TAudioStreamFactory::MakeParSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioStreamPtr>(s2));
}

AUDIOAPI AudioStream MakeSelectSound(AudioStream s1, long* selection, long channels)
{
    return TAudioStreamFactory::MakeSelectSound(static_cast<TAudioStreamPtr>(s1), selection, channels);
}

AUDIOAPI AudioStream MakeInputSound()
{
    return TAudioStreamFactory::MakeInputSound();
}

AUDIOAPI AudioStream MakeSharedInputSound()
{
    return TAudioStreamFactory::MakeSharedInputSound();
}

AUDIOAPI AudioStream MakeEffectSound(AudioStream s1, AudioEffect effect, long fadeIn, long fadeOut)
{
	return TAudioStreamFactory::MakeEffectSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioEffectInterfacePtr>(effect), fadeIn, fadeOut);
}

AUDIOAPI AudioStream MakePitchSchiftTimeStretchSound(AudioStream s1, double* pitch_shift, double* time_strech)
{
	return TAudioStreamFactory::MakeRubberBandSound(static_cast<TAudioStreamPtr>(s1), pitch_shift, time_strech);
//
//#ifdef SOUND_TOUCH
//	return TAudioStreamFactory::MakeSoundTouchSound(static_cast<TAudioStreamPtr>(s1), pitch_shift, time_strech);
//#else
//  return 0;
//#endif
}

AUDIOAPI AudioStream MakeWriteSound(const char* name, AudioStream s, long format)
{
    return TAudioStreamFactory::MakeWriteSound(name, static_cast<TAudioStreamPtr>(s), format);
}

AUDIOAPI AudioStream MakeRendererSound(AudioStream s)
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

AUDIOAPI long ReadSound(AudioStream s, float** buffer, long buffer_size)
{
    if (s && buffer) {
        TAudioStreamPtr stream = static_cast<TAudioStreamPtr>(s);
        TSharedNonInterleavedAudioBuffer<float> process_buffer(buffer, buffer_size, stream->Channels());
        UAudioTools::ZeroFloatBlk(buffer, buffer_size, stream->Channels());
        return stream->Read(&process_buffer, buffer_size, 0);
    } else {
        return 0;
    }
}

AUDIOAPI long ReadSoundPos(AudioStream s, float** buffer, long buffer_size, long frames, long pos)
{
    if (s && buffer) {
        TAudioStreamPtr stream = static_cast<TAudioStreamPtr>(s);
        TSharedNonInterleavedAudioBuffer<float> process_buffer(buffer, buffer_size, stream->Channels());
        // Init the correct part of the buffer...
        float** temp = (float**)alloca(process_buffer.GetChannels()*sizeof(float*));
        UAudioTools::ZeroFloatBlk(process_buffer.GetFrame(pos, temp), frames, stream->Channels());
        return stream->Read(&process_buffer, frames, pos);
    } else {
        return 0;
    }
}

AUDIOAPI long WriteSound(AudioStream s, float** buffer, long buffer_size)
{
    if (s && buffer) {
        TAudioStreamPtr stream = static_cast<TAudioStreamPtr>(s);
        TSharedNonInterleavedAudioBuffer<float> process_buffer(buffer, buffer_size, stream->Channels());
        return stream->Write(&process_buffer, buffer_size, 0);
    } else {
        return 0;
    }
}

AUDIOAPI void ResetSound(AudioStream s)
{
    if (s) {
        static_cast<TAudioStreamPtr>(s)->Reset();
    }
}

AUDIOAPI long SetPosSound(AudioStream s, long frames)
{
    if (s) {
        if (frames < 0 || frames > static_cast<TAudioStreamPtr>(s)->Length()) {
            printf("SetPosSound error : frames = %ld Length = %ld\n", frames, static_cast<TAudioStreamPtr>(s)->Length());
            return SET_POS_ERR;
        } else {
            return static_cast<TAudioStreamPtr>(s)->SetPos(frames);
        }
    } else {
        return FILE_NOT_FOUND_ERR;
    }
}

AUDIOAPI long GetPosSound(AudioStream s)
{
    if (s) {
        return static_cast<TAudioStreamPtr>(s)->GetPos();
    } else {
        return FILE_NOT_FOUND_ERR;
    }
}

AUDIOAPI AudioStream MakeCopySound(AudioStream s)
{
	return (s) ? static_cast<TAudioStreamPtr>(s)->Copy() : 0;
}

// Sound Ptr API

AUDIOAPI AudioStreamPtr MakeSoundPtr(AudioStream sound) 
{
	return new LA_SMARTP<TAudioStream>(sound);
}

AUDIOAPI void DeleteSoundPtr(AudioStreamPtr sound) 
{
	delete sound;
}

AUDIOAPI AudioStreamPtr MakeNullSoundPtr(long length)
{
	return MakeSoundPtr(TAudioStreamFactory::MakeNullSound(length));
}

AUDIOAPI AudioStreamPtr MakeMultiNullSoundPtr(long channels, long length)
{
	return MakeSoundPtr(TAudioStreamFactory::MakeMultiNullSound(channels, length));
}

AUDIOAPI AudioStreamPtr MakeConstantSoundPtr(long channels, long length, float value)
{
	return MakeSoundPtr(TAudioStreamFactory::MakeConstantSound(channels, length, value));
}

AUDIOAPI AudioStreamPtr MakeBufferSoundPtr(float** buffer, long length, long channels, bool clear)
{
	return MakeSoundPtr(TAudioStreamFactory::MakeBufferSound(buffer, length, channels, clear));
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
		return (cut) ? MakeSoundPtr(cut) : 0;
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

AUDIOAPI AudioStreamPtr MakeParSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2)
{
    return (s1 && s2) ? MakeSoundPtr(TAudioStreamFactory::MakeParSound(static_cast<TAudioStreamPtr>(*s1), static_cast<TAudioStreamPtr>(*s2))) : 0;
}

AUDIOAPI AudioStreamPtr MakeSelectSoundPtr(AudioStreamPtr s1, long* selection, long channels)
{
    return (s1) ? MakeSoundPtr(TAudioStreamFactory::MakeSelectSound(static_cast<TAudioStreamPtr>(*s1), selection, channels)) : 0;
}

AUDIOAPI AudioStreamPtr MakeInputSoundPtr()
{
    return MakeSoundPtr(TAudioStreamFactory::MakeInputSound());
}

AUDIOAPI AudioStreamPtr MakeSharedInputSoundPtr()
{
    return MakeSoundPtr(TAudioStreamFactory::MakeSharedInputSound());
}

AUDIOAPI AudioStreamPtr MakeEffectSoundPtr(AudioStreamPtr sound, AudioEffectPtr effect, long fadeIn, long fadeOut)
{
    return (sound && effect) 
		? MakeSoundPtr(TAudioStreamFactory::MakeEffectSound(static_cast<TAudioStreamPtr>(*sound), static_cast<TAudioEffectInterfacePtr>(*effect), fadeIn, fadeOut))
		: 0;
}

/*
AUDIOAPI AudioStreamPtr MakePitchSchiftTimeStretchSoundPtr(AudioStreamPtr sound, double* pitch_shift, double* time_strech)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeRubberBandSound(static_cast<TAudioStreamPtr>(*sound), pitch_shift, time_strech)) : 0;
    
//#ifdef SOUND_TOUCH
//	return (s) ? MakeSoundPtr(TAudioStreamFactory::MakeSoundTouchSound(static_cast<TAudioStreamPtr>(s), pitch_shift, time_strech)) : 0;
//#else
//  return 0;
//#endif
    
}
*/

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
    if (sound) {
        static_cast<TAudioStreamPtr>(*sound)->Reset();
    }
}

AUDIOAPI long SetPosSoundPtr(AudioStreamPtr s, long frames)
{
    if (s) {
        if (frames < 0 || frames > static_cast<TAudioStreamPtr>(*s)->Length()) {
            printf("SetPosSoundPtr error : frames = %ld Length = %ld\n", frames, static_cast<TAudioStreamPtr>(*s)->Length());
            return SET_POS_ERR;
        } else {
            return static_cast<TAudioStreamPtr>(*s)->SetPos(frames);
        }
    } else {
        return FILE_NOT_FOUND_ERR;
    }
}

AUDIOAPI long GetPosSoundPtr(AudioStreamPtr s)
{
    if (s) {
        return static_cast<TAudioStreamPtr>(*s)->GetPos();
    } else {
        return FILE_NOT_FOUND_ERR;
    }
}


AUDIOAPI AudioStreamPtr MakeCopySoundPtr(AudioStreamPtr sound)
{
	 return (sound) ? MakeSoundPtr(static_cast<TAudioStreamPtr>(*sound)->Copy()) : 0;
}

AUDIOAPI long ReadSoundPtr(AudioStreamPtr sound, float** buffer, long buffer_size)
{
    if (sound && buffer) {
        TAudioStreamPtr stream = static_cast<TAudioStreamPtr>(*sound);
        TSharedNonInterleavedAudioBuffer<float> process(buffer, buffer_size, stream->Channels());
        UAudioTools::ZeroFloatBlk(buffer, buffer_size, stream->Channels());
        return stream->Read(&process, buffer_size, 0);
    } else {
        return 0;
    }
}

AUDIOAPI long ReadSoundPosPtr(AudioStreamPtr sound, float** buffer, long buffer_size, long frames, long pos)
{
    if (sound && buffer) {
        TAudioStreamPtr stream = static_cast<TAudioStreamPtr>(*sound);
        TSharedNonInterleavedAudioBuffer<float> process_buffer(buffer, buffer_size, stream->Channels());
        // Init the correct part of the buffer...

        float** temp = (float**)alloca(stream->Channels()*sizeof(float*));
        UAudioTools::ZeroFloatBlk(process_buffer.GetFrame(pos, temp), frames, stream->Channels());
        return stream->Read(&process_buffer, frames, pos);
    } else {
        return 0;
    }
}

// Effect management

AudioEffect AUDIOAPI MakeFaustAudioEffect(const char* name, const char* library_path, const char* draw_path)
{
    TAudioGlobals::ClearLibError();
    try {
		return new TModuleFaustAudioEffect(name);
	} catch (TLASException& e) {
        try {
            return TLocalCodeFaustAudioEffectFactory::CreateEffect(name, library_path, draw_path);
        } catch (TLASException& e) {
            TAudioGlobals::AddLibError(e.Message());
            return 0;
        }
	}
}

AudioEffect AUDIOAPI MakeRemoteFaustAudioEffect(const char* name, const char* library_path, const char* draw_path)
{
#if REMOTE_DSP
    TAudioGlobals::ClearLibError();
    try {
		return new TModuleFaustAudioEffect(name);
	} catch (TLASException& e) {
        try {
            return TRemoteCodeFaustAudioEffectFactory::CreateEffect(name, library_path, draw_path);
        } catch (TLASException& e) {
            TAudioGlobals::AddLibError(e.Message());
            return 0;
        }
	}
#else
     return 0;
#endif
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

AUDIOAPI void ProcessEffect(AudioEffect effect, float** input, float** output, long framesNum)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->Process(input, output, framesNum);
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

AUDIOAPI const char* GetNameEffect(AudioEffect effect) 
{ 
    TAudioEffectInterface* effect_tmp = static_cast<TAudioEffectInterface*>(effect);
    TFaustAudioEffectBase* faust_effect;
    if ((faust_effect = dynamic_cast<TFaustAudioEffectBasePtr>(effect_tmp))) {
        return faust_effect->GetName().c_str();
    } else {
        return "";
    }
}

AUDIOAPI AudioEffect MakeCopyEffect(AudioEffect effect)
{
    return (effect) ? static_cast<TAudioEffectInterfacePtr>(effect)->Copy(): 0;
}

AUDIOAPI long SetTimedControlValueEffect(AudioPlayerPtr ext_player, const char* effect, const char* path, float value, SymbolicDate date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        if (TAudioGlobals::fEffectTable.find(effect) != TAudioGlobals::fEffectTable.end()) {
            list<TAudioEffectInterfacePtr>::iterator it;
            for (it = TAudioGlobals::fEffectTable[effect].begin(); it != TAudioGlobals::fEffectTable[effect].end(); it++) {
                player->fMixer->AddControlCommand(new TEffectControlCommand((*it), path, value, date));
            }
            return NO_ERR;
        } else {
            return EFFECT_NOT_FOUND_ERR;
        }
    } 
    return PLAYER_ERR;
}

// Effect management with pointer

AUDIOAPI AudioEffectPtr MakeFaustAudioEffectPtr(const char* code, const char* library_path, const char* draw_path)
{
    TAudioGlobals::ClearLibError();
    try {
        return new LA_SMARTP<TAudioEffectInterface>(new TModuleFaustAudioEffect(code));
    } catch (TLASException& e) {
        try {
            return new LA_SMARTP<TAudioEffectInterface>(TLocalCodeFaustAudioEffectFactory::CreateEffect(code, library_path, draw_path));
        } catch (TLASException& e) {
            TAudioGlobals::AddLibError(e.Message());
            return 0;
        }
    }
}

AUDIOAPI AudioEffectPtr MakeRemoteFaustAudioEffectPtr(const char* code, const char* library_path, const char* draw_path)
{
#if REMOTE_DSP
    TAudioGlobals::ClearLibError();
    try {
        return new LA_SMARTP<TAudioEffectInterface>(new TModuleFaustAudioEffect(code));
    } catch (TLASException& e) {
        try {
            return new LA_SMARTP<TAudioEffectInterface>(TRemoteCodeFaustAudioEffectFactory::CreateEffect(code, library_path, draw_path));
        } catch (TLASException& e) {
            TAudioGlobals::AddLibError(e.Message());
            return 0;
        }
    }
#else
    return 0;
#endif
}

#ifdef __APPLE__
#include<dispatch/dispatch.h>
static TCodeFaustAudioEffect* gDSP = 0;

AUDIOAPI AudioEffectPtr MakeDispatchFaustAudioEffectPtr(const char* code, const char* library_path, const char* draw_path)
{
    TAudioGlobals::ClearLibError();
    try {
        return new LA_SMARTP<TAudioEffectInterface>(new TModuleFaustAudioEffect(code));
    } catch (TLASException& e) {
        dispatch_sync(dispatch_get_main_queue(),
        ^{ 
            try {
                gDSP = TLocalCodeFaustAudioEffectFactory::CreateEffect(code, library_path, draw_path);
            } catch (TLASException& e) {
                TAudioGlobals::AddLibError(e.Message());
                gDSP = 0;
            } 
        });
        return (gDSP) ? new LA_SMARTP<TAudioEffectInterface>(gDSP) : 0; 
    }
}
#endif

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

AUDIOAPI void ProcessEffectPtr(AudioEffectPtr effect, float** input, float** output, long framesNum)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->Process(input, output, framesNum);
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

AUDIOAPI const char* GetNameEffectPtr(AudioEffectPtr effect) 
{ 
    TAudioEffectInterface* effect_tmp = static_cast<TAudioEffectInterface*>(*effect);
    TFaustAudioEffectBase* faust_effect;
    if ((faust_effect = dynamic_cast<TFaustAudioEffectBasePtr>(effect_tmp))) {
        return faust_effect->GetName().c_str();
    } else {
        return "";
    }
}

AUDIOAPI AudioEffectPtr MakeCopyEffectPtr(AudioEffectPtr effect)
{
    return (effect) ? new LA_SMARTP<TAudioEffectInterface>(static_cast<TAudioEffectInterface*>(*effect)->Copy()) : 0;
}

AUDIOAPI void DeleteEffectPtr(AudioEffectPtr effect)
{
    delete effect;
}

AUDIOAPI long SetTimedControlValueEffectPtr(AudioPlayerPtr ext_player, const char* effect, const char* path, float value, SymbolicDatePtr date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        if (TAudioGlobals::fEffectTable.find(effect) != TAudioGlobals::fEffectTable.end()) {
            list<TAudioEffectInterfacePtr>::iterator it;
            for (it = TAudioGlobals::fEffectTable[effect].begin(); it != TAudioGlobals::fEffectTable[effect].end(); it++) {
                player->fMixer->AddControlCommand(new TEffectControlCommand((*it), path, value, *date));
            }
            return NO_ERR;
        } else {
            return EFFECT_NOT_FOUND_ERR;
        }
    } 
    return PLAYER_ERR;
}

// Open/Close

AUDIOAPI void SetAudioLatencies(long inputLatency, long outputLatency)
{
	TAudioGlobals::fInputLatency = inputLatency;
	TAudioGlobals::fOutputLatency = outputLatency;
}

AUDIOAPI AudioPlayerPtr OpenAudioPlayer(long inChan, 
                                        long outChan, 
                                        long sample_rate, 
                                        long buffer_size, 
                                        long stream_buffer_size, 
                                        long rtstream_duration, 
                                        long renderer,
                                        long thread_num)
{
    int res;
  
    TAudioGlobals::ClearLibError();
	
	if (thread_num < 1) {
        TAudioGlobals::AddLibError("OpenAudioPlayer error: thread_num parameter should be at least one !!");
    }

    TAudioGlobals::Init(inChan, outChan, sample_rate, buffer_size, stream_buffer_size, rtstream_duration, thread_num);

    IntAudioPlayerPtr player = static_cast<IntAudioPlayerPtr>(calloc(1, sizeof(AudioPlayer)));
    if (!player) {
        goto error;
    }

	player->fRenderer = TAudioRendererFactory::MakeAudioRenderer(renderer); 
    if (!player->fRenderer) {
        goto error;
    }

    player->fMixer = new TExpAudioMixer();
    if (!player->fMixer) {
        goto error;
    }

	player->fRenderer->AddClient(player->fMixer);
	res = player->fRenderer->Open(inChan, outChan, buffer_size, sample_rate);

    if (res == NO_ERR) {
        return player;
    }

error:
    CloseAudioPlayer(player);
    return 0;
}

AUDIOAPI AudioPlayerPtr OpenAudioClient(AudioRendererPtr ext_renderer)
{
    IntAudioRendererPtr renderer = IntAudioRendererPtr(ext_renderer);
    
	IntAudioPlayerPtr player = static_cast<IntAudioPlayerPtr>(calloc(1, sizeof(AudioPlayer)));
    if (!player) {
        goto error;
    }
		
	player->fRenderer = renderer;
		
    player->fMixer = new TExpAudioMixer();
    if (!player->fMixer) {
        goto error;
    }

	player->fRenderer->AddClient(player->fMixer);		
	return player;

error:
    CloseAudioClient(player);
    return 0;
}

AUDIOAPI void CloseAudioPlayer(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
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

AUDIOAPI void CloseAudioClient(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (!player) {
        return;
    }

	if (player->fMixer) {
		player->fRenderer->RemoveClient(player->fMixer);
        delete player->fMixer;
    }
  
    free(player);
}

AUDIOAPI long SetMasterEffect(AudioPlayerPtr ext_player, AudioEffect effect)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer && effect->Outputs() <= TAudioGlobals::fOutput) {
        player->fMixer->SetEffect(effect);
        return NO_ERR;
    }
    return LOAD_ERR;
}

AUDIOAPI long SetMasterEffect(AudioPlayerPtr ext_player, AudioEffectPtr effect)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer && (*effect)->Outputs() <= TAudioGlobals::fOutput) {
        player->fMixer->SetEffect(*effect);
        return NO_ERR;
    }
    return LOAD_ERR;
}

AUDIOAPI audio_usec_t GetAudioPlayerDateInUsec(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fRenderer) {
        AudioRendererPtr renderer = player->fRenderer;
        RendererInfo info;
        GetAudioRendererInfo(renderer, &info);
        return info.fCurUsec;
    } else {
        return 0;
    }
}

AUDIOAPI audio_frame_t GetAudioPlayerDateInFrame(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fRenderer) {
        AudioRendererPtr renderer = player->fRenderer;
        RendererInfo info;
        GetAudioRendererInfo(renderer, &info);
        return info.fCurFrame;
    } else {
        return 0;
    }
}

AUDIOAPI long StartSound(AudioPlayerPtr ext_player, AudioStream sound, SymbolicDate date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        AudioStream rt_sound;
        if (sound->Channels() <= TAudioGlobals::fOutput) { // Limited to TAudioGlobals::fOutput for now
            rt_sound = sound;
        } else {
            std::vector<long> selection_aux;
            for (int i = 0; i < TAudioGlobals::fOutput; i++) {
                selection_aux.push_back(1);
            }
            rt_sound = new TSelectAudioStream(sound, selection_aux);
        }
        player->fMixer->AddStreamCommand(new TStreamCommand(new TRTRendererAudioStream(rt_sound), date, new TSymbolicDate()));
        return NO_ERR;
    } 
    return LOAD_ERR;
}    
    
AUDIOAPI long StopSound(AudioPlayerPtr ext_player, AudioStream sound, SymbolicDate date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
     
    if (player && player->fMixer && player->fRenderer) {
        TStreamCommandPtr command = player->fMixer->GetStreamCommand(sound);
        if (command) {
            command->SetStopDate(date);
            return NO_ERR;
        } 
    }
    return LOAD_ERR;
}

AUDIOAPI SymbolicDate GenSymbolicDate(AudioPlayerPtr /*player*/)
{
    return new TSymbolicDate();
}

AUDIOAPI SymbolicDate GenRealDate(AudioPlayerPtr /*player*/, audio_frame_t date)
{
    return new TSymbolicDate(date);
}

AUDIOAPI long SetSymbolicDate(AudioPlayerPtr ext_player, SymbolicDate symbolic_date, audio_frame_t real_date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer) {
        symbolic_date->setDate(real_date);
        player->fMixer->NeedSort();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI audio_frame_t GetSymbolicDate(AudioPlayerPtr /*player*/, SymbolicDate symbolic_date)
{
    return symbolic_date->getDate();
}

////

AUDIOAPI long StartSoundPtr(AudioPlayerPtr ext_player, AudioStreamPtr sound, SymbolicDatePtr date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        AudioStream rt_sound;
        if ((*sound)->Channels() <= TAudioGlobals::fOutput) { // Limited to TAudioGlobals::fOutput for now
            rt_sound = *sound;
        } else {
            std::vector<long> selection_aux;
            for (int i = 0; i < TAudioGlobals::fOutput; i++) {
                selection_aux.push_back(1);
            }
            rt_sound = new TSelectAudioStream(*sound, selection_aux);
        }
        player->fMixer->AddStreamCommand(new TStreamCommand(new TRTRendererAudioStream(rt_sound), *date, new TSymbolicDate()));
        return NO_ERR;
    } 
    return LOAD_ERR;
}    

AUDIOAPI long StopSoundPtr(AudioPlayerPtr ext_player, AudioStreamPtr sound, SymbolicDatePtr date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        TStreamCommandPtr command = player->fMixer->GetStreamCommand(*sound);
        if (command) {
            command->SetStopDate(*date);
            return NO_ERR;
        } 
    }
    return LOAD_ERR;
}

AUDIOAPI SymbolicDatePtr GenSymbolicDatePtr(AudioPlayerPtr /*player*/)
{
    return new LA_SMARTP<TSymbolicDate>(new TSymbolicDate());
}

AUDIOAPI SymbolicDatePtr GenRealDatePtr(AudioPlayerPtr /*player*/, audio_frame_t date)
{
    return new LA_SMARTP<TSymbolicDate>(new TSymbolicDate(date));
}

AUDIOAPI long SetSymbolicDatePtr(AudioPlayerPtr ext_player, SymbolicDatePtr symbolic_date, audio_frame_t real_date)
{   
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer) {
        (*symbolic_date)->setDate(real_date);
        player->fMixer->NeedSort();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI audio_frame_t GetSymbolicDatePtr(AudioPlayerPtr /*player*/, SymbolicDatePtr symbolic_date)
{
    return (*symbolic_date)->getDate();
}

AUDIOAPI void DeleteSymbolicDatePtr(SymbolicDatePtr symbolic_date)
{
    delete symbolic_date;
}

AUDIOAPI long ClearAudioPlayer(AudioPlayerPtr player)
{
    if (player) {
        // Reset effect table
        TAudioGlobals::fEffectTable.clear();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI long StartAudioPlayer(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        // Reset real-time input
        if (TAudioGlobals::fSharedInput) {
            TAudioGlobals::fSharedInput->Reset();
        }
        // Reset effect table
        //TAudioGlobals::fEffectTable.clear();
        // Start player
        player->fRenderer->Start();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI long StopAudioPlayer(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        player->fRenderer->Stop();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI long PauseAudioPlayer(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        player->fRenderer->Pause();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI long ContAudioPlayer(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        player->fRenderer->Cont();
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI long SetPosAudioPlayer(AudioPlayerPtr ext_player, audio_frame_t new_date)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
    if (player && player->fMixer && player->fRenderer) {
        player->fMixer->SetPos(new_date);
        return NO_ERR;
    } else {
        return PLAYER_ERR;
    }
}

AUDIOAPI AudioRendererPtr GetAudioPlayerRenderer(AudioPlayerPtr ext_player)
{
    IntAudioPlayerPtr player = IntAudioPlayerPtr(ext_player);
    
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

AUDIOAPI long OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
   return static_cast<TAudioRendererPtr>(renderer)->Open(inChan, outChan, bufferSize, sampleRate);
}

AUDIOAPI void CloseAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Close();
}

AUDIOAPI long StartAudioRenderer(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->Start();
}

AUDIOAPI long StopAudioRenderer(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->Stop();
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
								long sample_rate,
								long buffer_size, 
								long stream_buffer_size, 
								long rtstream_duration,
								long thread_num)
{
	TAudioGlobals::Init(inChan, outChan, sample_rate, buffer_size, stream_buffer_size, rtstream_duration, thread_num);
}

AUDIOAPI void AudioGlobalsDestroy()
{
	TAudioGlobals::Destroy();
}

AUDIOAPI long GetDeviceCount(long renderer)
{
    switch (renderer) {
#ifdef __PORTAUDIO__
    #ifdef __PORTAUDIOV19__
        case kPortAudioRenderer:
            return TPortAudioV19Renderer::GetDeviceCount();
    #endif
#else
    #warning PortAudio renderer is not compiled
            return 0;
#endif
#ifdef __JACK__
        case kJackRenderer:
            return TJackRenderer::GetDeviceCount();
            
        case kNetJackRenderer:
            return TNetJackRenderer::GetDeviceCount();
#else
	#ifdef WIN32
		#pragma message ("JACK renderer is not compiled")
	#else
        #warning Jack renderer is not compiled
	#endif
#endif

#ifdef __COREAUDIO__
        case kCoreAudioRenderer:
            return TCoreAudioRenderer::GetDeviceCount();
#else
    #ifdef WIN32
        #pragma message ("CoreAudio renderer is not compiled")
    #else
        #warning CoreAudio renderer is not compiled
    #endif
#endif
        case kOffLineAudioRenderer:
            return TOfflineRenderer::GetDeviceCount();
        default:
            assert(false);
            break;

    }
}

AUDIOAPI void GetDeviceInfo(long renderer, long deviceNum, DeviceInfo* info)
{
    switch (renderer) {
#ifdef __PORTAUDIO__
    #ifdef __PORTAUDIOV19__
        case kPortAudioRenderer:
            TPortAudioV19Renderer::GetDeviceInfo(deviceNum, info);
            break;
    #endif
#else
    #warning PortAudio renderer is not compiled
#endif
#ifdef __JACK__
        case kJackRenderer:
            TJackRenderer::GetDeviceInfo(deviceNum, info);
            break;
            
        case kNetJackRenderer:
            TNetJackRenderer::GetDeviceInfo(deviceNum, info);
            break;
#else
	#ifdef WIN32
		#pragma message ("JACK renderer is not compiled")
	#else
        #warning Jack renderer is not compiled
	#endif
#endif
#ifdef __COREAUDIO__
        case kCoreAudioRenderer:
            TCoreAudioRenderer::GetDeviceInfo(deviceNum, info);
            break;
#else
	#ifdef WIN32
		#pragma message ("Coreaudio renderer is not compiled")
	#else
        #warning CoreAudio renderer is not compiled
	#endif
#endif
        case kOffLineAudioRenderer:
            TOfflineRenderer::GetDeviceInfo(deviceNum, info);
            break;
        default:
            assert(false);
            break;
    }
}

AUDIOAPI long GetDefaultInputDevice(long renderer)
{
    switch (renderer) {
#ifdef __PORTAUDIO__
    #ifdef __PORTAUDIOV19__
        case kPortAudioRenderer:
            return TPortAudioV19Renderer::GetDefaultInputDevice();
    #endif
#else
    #warning PortAudio renderer is not compiled
#endif
#ifdef __JACK__
        case kJackRenderer:
            return TJackRenderer::GetDefaultInputDevice();
            
        case kNetJackRenderer:
            return TNetJackRenderer::GetDefaultInputDevice();
#else
	#ifdef WIN32
		#pragma message ("JACK renderer is not compiled")
	#else
		#warning Jack renderer is not compiled
	#endif
#endif
#ifdef __COREAUDIO__
        case kCoreAudioRenderer:
            return TCoreAudioRenderer::GetDefaultInputDevice();
#else
	#ifdef WIN32
		#pragma message ("Coreaudio renderer is not compiled")
	#else
        #warning CoreAudio renderer is not compiled
	#endif
#endif
        case kOffLineAudioRenderer:
            return TOfflineRenderer::GetDefaultInputDevice();
        default:
            assert(false);
            return 0;
    }
}

AUDIOAPI long GetDefaultOutputDevice(long renderer)
{
    switch (renderer) {
#ifdef __PORTAUDIO__
    #ifdef __PORTAUDIOV19__
        case kPortAudioRenderer:
            return TPortAudioV19Renderer::GetDefaultOutputDevice();
    #endif
#else
    #warning PortAudio renderer is not compiled
#endif
#ifdef __JACK__
        case kJackRenderer:
            return TJackRenderer::GetDefaultOutputDevice();
            
        case kNetJackRenderer:
            return TNetJackRenderer::GetDefaultOutputDevice();
#else
	#ifdef WIN32
		#pragma message ("JACK renderer is not compiled")
	#else
		#warning Jack renderer is not compiled
	#endif
#endif
#ifdef __COREAUDIO__
        case kCoreAudioRenderer:
            return TCoreAudioRenderer::GetDefaultOutputDevice();
#else
	#ifdef WIN32
		#pragma message ("Coreaudio renderer is not compiled")
	#else
        #warning CoreAudio renderer is not compiled
	#endif
#endif
        case kOffLineAudioRenderer:
            return TOfflineRenderer::GetDefaultOutputDevice();
        default:
            assert(false);
            return 0;
    }
}


