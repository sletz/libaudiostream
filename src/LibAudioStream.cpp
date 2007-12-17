/*
Copyright © Grame 2002-2007

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

	long AUDIOAPI LibVersion();
		
	// Device scanning
	long AUDIOAPI GetDeviceCount(AudioRendererPtr renderer);
	void AUDIOAPI GetDeviceInfo(AudioRendererPtr renderer, long deviceNum, DeviceInfo* info);
	long AUDIOAPI GetDefaultInputDevice(AudioRendererPtr renderer);
	long AUDIOAPI GetDefaultOutputDevice(AudioRendererPtr renderer);
	
	AudioStreamPtr AUDIOAPI MakeSoundPtr(AudioStream sound) ;
	void AUDIOAPI DeleteSoundPtr(AudioStreamPtr sound);
	
	// Build sound (using pointer on smartptr)
	AudioStreamPtr AUDIOAPI MakeNullSoundPtr(long lengthFrame);
    AudioStreamPtr AUDIOAPI MakeReadSoundPtr(char* name);
    AudioStreamPtr AUDIOAPI MakeRegionSoundPtr(char* name, long beginFrame, long endFrame);
	AudioStreamPtr AUDIOAPI	MakeStereoSoundPtr(AudioStreamPtr sound);
    AudioStreamPtr AUDIOAPI MakeFadeSoundPtr(AudioStreamPtr sound, long fadeIn, long fadeOut);
    AudioStreamPtr AUDIOAPI MakeLoopSoundPtr(AudioStreamPtr sound, long n);
    AudioStreamPtr AUDIOAPI MakeCutSoundPtr(AudioStreamPtr sound, long beginFrame, long endFrame);
    AudioStreamPtr AUDIOAPI MakeSeqSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade);
    AudioStreamPtr AUDIOAPI MakeMixSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2);
    AudioStreamPtr AUDIOAPI MakeTransformSoundPtr(AudioStreamPtr sound, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);
	AudioStreamPtr AUDIOAPI MakeRubberBandSoundPtr(AudioStreamPtr sound, double* pitch_shift, double* time_strech);
    AudioStreamPtr AUDIOAPI MakeWriteSoundPtr(char* name, AudioStreamPtr s, long format);
    AudioStreamPtr AUDIOAPI MakeInputSoundPtr();
    AudioStreamPtr AUDIOAPI MakeRendererSoundPtr(AudioStreamPtr s);

    long AUDIOAPI GetLengthSoundPtr(AudioStreamPtr s);
    long AUDIOAPI GetChannelsSoundPtr(AudioStreamPtr s);
    long AUDIOAPI ReadSoundPtr(AudioStreamPtr stream, float* buffer, long buffer_size, long channels);
	void AUDIOAPI ResetSoundPtr(AudioStreamPtr sound);

	AudioEffectListPtr AUDIOAPI MakeAudioEffectListPtr();
    AudioEffectListPtr AUDIOAPI AddAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect);
    AudioEffectListPtr AUDIOAPI RemoveAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect);
	AudioEffectListPtr AUDIOAPI ClearAudioEffectListPtr(AudioEffectListPtr list_effect);

    AudioEffectPtr AUDIOAPI MakeVolAudioEffectPtr(float vol);
	AudioEffectPtr AUDIOAPI MakeMonoPanAudioEffectPtr(float pan);
	AudioEffectPtr AUDIOAPI MakeStereoPanAudioEffectPtr(float panLeft, float panRight);
	AudioEffectPtr AUDIOAPI MakePitchShiftAudioEffectPtr(float pitch);
	AudioEffectPtr AUDIOAPI MakeFaustAudioEffectPtr(const char* name);
	AudioEffectPtr AUDIOAPI MakeWrapperAudioEffectPtr(AudioEffectInterfacePtr effect);

	long AUDIOAPI GetControlCountEffectPtr(AudioEffectPtr effect);
	void AUDIOAPI GetControlParamEffectPtr(AudioEffectPtr effect, long param, char* label, float* min, float* max, float* init);
	void AUDIOAPI SetControlValueEffectPtr(AudioEffectPtr effect, long param, float f);
	float AUDIOAPI GetControlValueEffectPtr(AudioEffectPtr effect, long param);
	
	void AUDIOAPI SetStateEffectPtr(AudioEffectPtr effect, long state);
	long AUDIOAPI GetStateEffectPtr(AudioEffectPtr effect);
	void AUDIOAPI ResetEffectPtr(AudioEffectPtr effect);

	void AUDIOAPI ProcessEffectPtr(AudioEffectPtr effect, float** input, float** output, long framesNum, long channels);
	
	void AUDIOAPI DeleteEffectListPtr(AudioEffectListPtr list_effect);
	void AUDIOAPI DeleteEffectPtr(AudioEffectPtr effect);

	  // Open/Close
    AudioPlayerPtr AUDIOAPI OpenAudioPlayer(long inChan, 
                                            long outChan, 
                                            long channels, 
                                            long sample_rate, 
                                            long buffer_size, 
                                            long stream_buffer_size, 
                                            long rtstream_buffer_size, 
                                            long renderer,
                                            long thread_num);
	AudioPlayerPtr AUDIOAPI OpenAudioClient(AudioRendererPtr renderer);	
									
    void AUDIOAPI CloseAudioPlayer(AudioPlayerPtr player);
	void AUDIOAPI CloseAudioClient(AudioPlayerPtr player);

    // Load a sound in a channel
    long AUDIOAPI LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight);
	long AUDIOAPI LoadChannelPtr(AudioPlayerPtr player, AudioStreamPtr sound, long chan, float vol, float panLeft, float panRight);
    void AUDIOAPI GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfoPtr info);
	void AUDIOAPI SetStopCallbackChannel(AudioPlayerPtr player, long chan, StopCallback callback, void* context);

    // Transport
    void AUDIOAPI StartAudioPlayer(AudioPlayerPtr player);		// Start the global player
    void AUDIOAPI StopAudioPlayer(AudioPlayerPtr player);		// Stop the global player

    void AUDIOAPI StartChannel(AudioPlayerPtr player, long chan);	// Start a sound region from the beginning
    void AUDIOAPI ContChannel(AudioPlayerPtr player, long chan);	// Play a sound region from the current location
    void AUDIOAPI StopChannel(AudioPlayerPtr player, long chan);	// Stop playing
	void AUDIOAPI AbortChannel(AudioPlayerPtr player, long chan);	// Stop playing

    // Params
    void AUDIOAPI SetVolChannel(AudioPlayerPtr player, long chan, float vol);
    void AUDIOAPI SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight);
	void AUDIOAPI SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectList effect_list, long fadeIn, long fadeOut);
	void AUDIOAPI SetEffectListChannelPtr(AudioPlayerPtr player, long chan, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);

    // Master
    void AUDIOAPI SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight);
    void AUDIOAPI SetVolAudioPlayer(AudioPlayerPtr player, float vol);
	void AUDIOAPI SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectList effect_list, long fadeIn, long fadeOut);
	void AUDIOAPI SetEffectListAudioPlayerPtr(AudioPlayerPtr player, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);
	
	// Renderer
	AudioRendererPtr AUDIOAPI MakeAudioRenderer(long renderer);
	void AUDIOAPI DeleteAudioRenderer(AudioRendererPtr renderer);
	
	int AUDIOAPI OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);
	void AUDIOAPI CloseAudioRenderer(AudioRendererPtr renderer); 
	void AUDIOAPI StartAudioRenderer(AudioRendererPtr renderer); 
	void AUDIOAPI StopAudioRenderer(AudioRendererPtr renderer); 
	
	void AUDIOAPI AddAudioClient(AudioRendererPtr renderer, AudioClientPtr client); 
	void AUDIOAPI RemoveAudioClient(AudioRendererPtr renderer, AudioClientPtr client); 
	
	// Globals
	void AUDIOAPI AudioGlobalsInit(long inChan, 
									long outChan, 
									long channels, 
									long sample_rate,
									long buffer_size, 
									long stream_buffer_size, 
									long rtstream_buffer_size,
									long thread_num);
	void AUDIOAPI AudioGlobalsDestroy();

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

long AUDIOAPI GetLengthSound(AudioStream s);
long AUDIOAPI GetChannelsSound(AudioStream s);
long AUDIOAPI ReadSound(AudioStream stream, float* buffer, long buffer_size, long channels);
void AUDIOAPI ResetSound(AudioStream sound);

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

long AUDIOAPI GetControlCountEffect(AudioEffect effect);
void AUDIOAPI GetControlParamEffect(AudioEffect effect, long param, char* label, float* min, float* max, float* init);
void AUDIOAPI SetControlValueEffect(AudioEffect effect, long param, float f);
float AUDIOAPI GetControlValueEffect(AudioEffect effect, long param);

void AUDIOAPI SetStateEffect(AudioEffect effect, long state);
long AUDIOAPI GetStateEffect(AudioEffect effect);
void AUDIOAPI ResetEffect(AudioEffect effect);

void AUDIOAPI ProcessEffect(AudioEffectPtr effect, float** input, float** output, long framesNum, long channels);

long LibVersion()
{
	return 120;
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

AudioStream AUDIOAPI MakeTransformSound(AudioStream s1, AudioEffectList list_effect, long fadeIn, long fadeOut)
{
	return TAudioStreamFactory::MakeTransformSound(static_cast<TAudioStreamPtr>(s1), static_cast<TAudioEffectListPtr>(list_effect), fadeIn, fadeOut);
}

AudioStream AUDIOAPI MakePitchSchiftTimeStretchSound(AudioStream s1, double* pitch_shift, double* time_strech)
{
	//return TAudioStreamFactory::MakeRubberBandSound(static_cast<TAudioStreamPtr>(s1), pitch_shift, time_strech);
	return TAudioStreamFactory::MakeSoundTouchSound(static_cast<TAudioStreamPtr>(s1), pitch_shift, time_strech);
}

AudioStream AUDIOAPI MakeWriteSound(char* name, AudioStream s, long format)
{
    return TAudioStreamFactory::MakeWriteSound(name, static_cast<TAudioStreamPtr>(s), format);
}

AudioStream AUDIOAPI MakeRendererSound(AudioStream s)
{
    return TAudioStreamFactory::MakeDTRenderer(static_cast<TAudioStreamPtr>(s));
}

long AUDIOAPI GetLengthSound(AudioStream s)
{
    return (s) ? (static_cast<TAudioStreamPtr>(s))->Length() : 0;
}

long AUDIOAPI GetChannelsSound(AudioStream s)
{
    return (s) ? (static_cast<TAudioStreamPtr>(s))->Channels() : 0;
}

long AUDIOAPI ReadSound(AudioStream s, float* buffer, long buffer_size, long channels)
{
    if (s && buffer) {
        TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
		UAudioTools::ZeroFloatBlk(buffer, buffer_size, channels);
        return static_cast<TAudioStreamPtr>(s)->Read(&process, buffer_size, 0, channels);
    } else {
        return 0;
    }
}

void AUDIOAPI ResetSound(AudioStream s)
{
	static_cast<TAudioStreamPtr>(s)->Reset();
}

AudioStreamPtr AUDIOAPI MakeSoundPtr(AudioStream sound) 
{
	return new LA_SMARTP<TAudioStream>(sound);
}

void AUDIOAPI DeleteSoundPtr(AudioStreamPtr sound) 
{
	delete sound;
}

AudioStreamPtr AUDIOAPI MakeNullSoundPtr(long lengthFrame)
{
	return MakeSoundPtr(TAudioStreamFactory::MakeNullSound(lengthFrame));
}

AudioStreamPtr AUDIOAPI MakeReadSoundPtr(char* name)
{
	AudioStream sound = TAudioStreamFactory::MakeReadSound(name);
	return (sound) ? MakeSoundPtr(sound) : 0;
}

AudioStreamPtr AUDIOAPI MakeRegionSoundPtr(char* name, long beginFrame, long endFrame)
{
	AudioStream sound = TAudioStreamFactory::MakeRegionSound(name, beginFrame, endFrame);
	return (sound) ? MakeSoundPtr(sound) : 0;
}

AudioStreamPtr AUDIOAPI MakeStereoSoundPtr(AudioStreamPtr sound)
{
	return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeStereoSound(static_cast<TAudioStreamPtr>(*sound))) : 0;
}

AudioStreamPtr AUDIOAPI MakeFadeSoundPtr(AudioStreamPtr sound, long fadeIn, long fadeOut)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeFadeSound(static_cast<TAudioStreamPtr>(*sound), fadeIn, fadeOut)) : 0;
}

AudioStreamPtr AUDIOAPI MakeLoopSoundPtr(AudioStreamPtr sound, long n)
{
    return (sound) ? MakeSoundPtr(TAudioStreamFactory::MakeLoopSound(static_cast<TAudioStreamPtr>(*sound), n)) : 0;
}

AudioStreamPtr AUDIOAPI MakeCutSoundPtr(AudioStreamPtr sound, long beginFrame, long endFrame)
{
	if (sound) {
		TAudioStreamPtr cut = TAudioStreamFactory::MakeCutSound(static_cast<TAudioStreamPtr>(*sound), beginFrame, endFrame);
		return (cut) ?  MakeSoundPtr(cut) : 0;
	} else {
		 return 0;
	}  
}

AudioStreamPtr AUDIOAPI MakeSeqSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade)
{
    return (s1 && s2) ? MakeSoundPtr(TAudioStreamFactory::MakeSeqSound(static_cast<TAudioStreamPtr>(*s1), static_cast<TAudioStreamPtr>(*s2), crossFade)) : 0;
}

AudioStreamPtr AUDIOAPI MakeMixSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2)
{
    return (s1 && s2) ? MakeSoundPtr(TAudioStreamFactory::MakeMixSound(static_cast<TAudioStreamPtr>(*s1), static_cast<TAudioStreamPtr>(*s2))) : 0;
}

AudioStreamPtr AUDIOAPI MakeInputSoundPtr()
{
    return MakeSoundPtr(TAudioStreamFactory::MakeInputSound());
}

AudioStreamPtr AUDIOAPI MakeTransformSoundPtr(AudioStreamPtr s, AudioEffectListPtr list_effect, long fadeIn, long fadeOut)
{
    return (s && list_effect) 
		? MakeSoundPtr(TAudioStreamFactory::MakeTransformSound(static_cast<TAudioStreamPtr>(*s), static_cast<TAudioEffectListPtr>(*list_effect), fadeIn, fadeOut))
		: 0;
}

AudioStreamPtr AUDIOAPI MakePitchSchiftTimeStretchSoundPtr(AudioStream s, double* pitch_shift, double* time_strech)
{
	return (s) ? MakeSoundPtr(TAudioStreamFactory::MakeSoundTouchSound(static_cast<TAudioStreamPtr>(s), pitch_shift, time_strech)) : 0;
}

AudioStreamPtr AUDIOAPI MakeWriteSoundPtr(char* name, AudioStreamPtr s, long format)
{
    return (s) ? MakeSoundPtr(TAudioStreamFactory::MakeWriteSound(name, static_cast<TAudioStreamPtr>(*s), format)) : 0;
}

AudioStreamPtr AUDIOAPI MakeRendererSoundPtr(AudioStreamPtr s)
{
    return (s) ? MakeSoundPtr(TAudioStreamFactory::MakeDTRenderer(static_cast<TAudioStreamPtr>(*s))) : 0;
}

long AUDIOAPI GetLengthSoundPtr(AudioStreamPtr s)
{
    return (s) ? (static_cast<TAudioStreamPtr>(*s))->Length() : 0;
}

long AUDIOAPI GetChannelsSoundPtr(AudioStreamPtr s)
{
    return (s) ? (static_cast<TAudioStreamPtr>(*s))->Channels() : 0;
}

void AUDIOAPI ResetSoundPtr(AudioStreamPtr s)
{
	static_cast<TAudioStreamPtr>(*s)->Reset();
}

long AUDIOAPI ReadSoundPtr(AudioStreamPtr s, float* buffer, long buffer_size, long channels)
{
    if (s && buffer) {
        TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
		UAudioTools::ZeroFloatBlk(buffer, buffer_size, channels);
        return static_cast<TAudioStreamPtr>(*s)->Read(&process, buffer_size, 0, channels);
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
   if (list_effect && effect)
       static_cast<TAudioEffectListPtr>(list_effect)->push_back(static_cast<TAudioEffectInterfacePtr>(effect));
    return list_effect;
}

AudioEffectList AUDIOAPI RemoveAudioEffect(AudioEffectList list_effect, AudioEffect effect)
{
    if (list_effect && effect)
        static_cast<TAudioEffectListPtr>(list_effect)->remove(static_cast<TAudioEffectInterfacePtr>(effect));
    return list_effect;
}

AudioEffectList AUDIOAPI ClearAudioEffectList(AudioEffectList list_effect)
{
	if (list_effect)
        static_cast<TAudioEffectListPtr>(list_effect)->clear();
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
	} catch (int n) {
		printf("MakeFaustAudioEffect exception %d \n", n);
		return 0;
	}
}

long AUDIOAPI GetControlCountEffect(AudioEffect effect) 
{
	return static_cast<TAudioEffectInterfacePtr>(effect)->GetControlCount();
}

void AUDIOAPI GetControlParamEffect(AudioEffect effect, long control, char* label, float* min, float* max, float* init)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->GetControlParam(control, label, min, max, init);
}

void AUDIOAPI SetControlValueEffect(AudioEffect effect, long control, float f)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->SetControlValue(control, f);
}

float AUDIOAPI GetControlValueEffect(AudioEffect effect, long control)
{
	return static_cast<TAudioEffectInterfacePtr>(effect)->GetControlValue(control);
}

void AUDIOAPI SetStateEffect(AudioEffect effect, long state)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->SetState(bool(state));
}

long AUDIOAPI GetStateEffect(AudioEffect effect)
{
	return static_cast<TAudioEffectInterfacePtr>(effect)->GetState();
}

void AUDIOAPI ResetEffect(AudioEffect effect)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->Reset();
}

void AUDIOAPI ProcessEffect(AudioEffect effect, float** input, float** output, long framesNum, long channels)
{
	static_cast<TAudioEffectInterfacePtr>(effect)->Process(input, output, framesNum, channels);
}

// Effect management with pointer
void AUDIOAPI DeleteEffectListPtr(AudioEffectListPtr list_effect) 
{
	delete list_effect;
}

void AUDIOAPI DeleteEffectPtr(AudioEffectPtr effect) 
{
	delete effect;
}

AudioEffectListPtr AUDIOAPI MakeAudioEffectListPtr()
{
    return new LA_SMARTP<TAudioEffectList>(new TAudioEffectList());
}

AudioEffectListPtr AUDIOAPI AddAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
    if (list_effect && effect)
        static_cast<TAudioEffectListPtr>(*list_effect)->push_back(static_cast<TAudioEffectInterfacePtr>(*effect));
    return list_effect;
}

AudioEffectListPtr AUDIOAPI RemoveAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
    if (list_effect && effect)
        static_cast<TAudioEffectListPtr>(*list_effect)->remove(static_cast<TAudioEffectInterfacePtr>(*effect));
    return list_effect;
}

AudioEffectListPtr AUDIOAPI ClearAudioEffectListPtr(AudioEffectListPtr list_effect)
{
	if (list_effect)
        static_cast<TAudioEffectListPtr>(*list_effect)->clear();
    return list_effect;
}

AudioEffectPtr AUDIOAPI MakeVolAudioEffectPtr(float vol)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TVolAudioEffect(vol));
}

AudioEffectPtr AUDIOAPI MakeMonoPanAudioEffectPtr(float pan)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TMonoPanAudioEffect(pan));
}

AudioEffectPtr AUDIOAPI MakeStereoPanAudioEffectPtr(float panLeft, float panRight)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TStereoPanAudioEffect(panLeft, panRight));
}

AudioEffectPtr AUDIOAPI MakePitchShiftAudioEffectPtr(float pitch)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TPitchShiftAudioEffect(pitch));
}

AudioEffectPtr AUDIOAPI MakeFaustAudioEffectPtr(const char* name)
{
	try {
		return new LA_SMARTP<TAudioEffectInterface>(new TModuleFaustAudioEffect(name));
	} catch (int n) {
		printf("MakeFaustAudioEffect exception %d \n", n);
		return 0;
	}
}

AudioEffectPtr AUDIOAPI MakeWrapperAudioEffectPtr(AudioEffectInterfacePtr effect)
{
    return new LA_SMARTP<TAudioEffectInterface>(new TWrapperAudioEffect(static_cast<TAudioEffectInterface*>(effect)));
}

long AUDIOAPI GetControlCountEffectPtr(AudioEffectPtr effect) 
{
	return static_cast<TAudioEffectInterfacePtr>(*effect)->GetControlCount();
}

void AUDIOAPI GetControlParamEffectPtr(AudioEffectPtr effect, long control, char* label, float* min, float* max, float* init)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->GetControlParam(control, label, min, max, init);
}

void AUDIOAPI SetControlValueEffectPtr(AudioEffectPtr effect, long control, float f)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->SetControlValue(control, f);
}

float AUDIOAPI GetControlValueEffectPtr(AudioEffectPtr effect, long control)
{
	return static_cast<TAudioEffectInterfacePtr>(*effect)->GetControlValue(control);
}

void AUDIOAPI SetStateEffectPtr(AudioEffectPtr effect, long state)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->SetState(bool(state));
}

long AUDIOAPI GetStateEffectPtr(AudioEffectPtr effect)
{
	return static_cast<TAudioEffectInterfacePtr>(*effect)->GetState();
}

void AUDIOAPI ResetEffectPtr(AudioEffectPtr effect)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->Reset();
}

void AUDIOAPI ProcessEffectPtr(AudioEffectPtr effect, float** input, float** output, long framesNum, long channels)
{
	static_cast<TAudioEffectInterfacePtr>(*effect)->Process(input, output, framesNum, channels);
}

// Open/Close
AudioPlayerPtr AUDIOAPI OpenAudioPlayer(long inChan, 
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
	
	if (thread_num < 1) 
		printf("OpenAudioPlayer error: thread_num parameter should be at least one !! \n");

    TAudioGlobals::Init(inChan, outChan, channels, sample_rate, buffer_size, stream_buffer_size, rtstream_buffer_size, thread_num);

    AudioPlayerPtr player = static_cast<AudioPlayerPtr>(calloc(1, sizeof(AudioPlayer)));
    if (!player)
        goto error;

	player->fRenderer = TAudioRendererFactory::MakeAudioRenderer(renderer); 
    if (!player->fRenderer)
        goto error;

	player->fMixer = new TAudioMixer;
    if (!player->fMixer)
        goto error;

	player->fRenderer->AddClient(player->fMixer);
	res = player->fRenderer->OpenDefault(inChan, outChan, buffer_size, sample_rate);

    if (res == NO_ERR)
        return player;

error:
    CloseAudioPlayer(player);
    return 0;
}

AudioPlayerPtr AUDIOAPI OpenAudioClient(AudioRendererPtr renderer)
{
	AudioPlayerPtr player = static_cast<AudioPlayerPtr>(calloc(1, sizeof(AudioPlayer)));
    if (!player)
        goto error;
		
	player->fRenderer = renderer;
		
	player->fMixer = new TAudioMixer;
    if (!player->fMixer)
        goto error;

	player->fRenderer->AddClient(player->fMixer);		
	return player;

error:
    CloseAudioClient(player);
    return 0;
}

void AUDIOAPI CloseAudioPlayer(AudioPlayerPtr player)
{
    if (!player)
        return;

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

void AUDIOAPI CloseAudioClient(AudioPlayerPtr player)
{
    if (!player)
        return;

	if (player->fMixer) {
		player->fRenderer->RemoveClient(player->fMixer);
        delete player->fMixer;
    }
  
    free(player);
}

// SoundFile management
long AUDIOAPI LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight)
{
    if (player && player->fMixer && sound) {
        return player->fMixer->Load(static_cast<TAudioStreamPtr>(sound), chan, vol, panLeft, panRight);
    } else
        return LOAD_ERR;
}

long AUDIOAPI LoadChannelPtr(AudioPlayerPtr player, AudioStreamPtr sound, long chan, float vol, float panLeft, float panRight)
{
    if (player && player->fMixer && sound) {
        return player->fMixer->Load(static_cast<TAudioStreamPtr>(*sound), chan, vol, panLeft, panRight);
    } else
        return LOAD_ERR;
}

void AUDIOAPI GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfo* info)
{
    if (player && player->fMixer && info) {
        player->fMixer->GetInfo(chan, info);
    }
}

void AUDIOAPI SetStopCallbackChannel(AudioPlayerPtr player,long chan, StopCallback callback, void* context)
{
    if (player && player->fMixer) {
        player->fMixer->SetStopCallback(chan, callback, context);
    }
}

// Transport
void AUDIOAPI StartChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer)
        player->fMixer->Start(chan);
}

void AUDIOAPI ContChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer)
        player->fMixer->Play(chan);
}

void AUDIOAPI StopChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer)
        player->fMixer->Stop(chan);
}

void AUDIOAPI AbortChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fMixer)
        player->fMixer->Abort(chan);
}

void AUDIOAPI StartAudioPlayer(AudioPlayerPtr player)
{
    if (player && player->fMixer && player->fRenderer)
        player->fRenderer->Start();
}

void AUDIOAPI StopAudioPlayer(AudioPlayerPtr player)
{
    if (player && player->fMixer && player->fRenderer)
        player->fRenderer->Stop();
}

// Params
void AUDIOAPI SetVolChannel(AudioPlayerPtr player, long chan, float vol)
{
    if (player && player->fMixer)
        player->fMixer->SetVol(chan, vol);
}

void AUDIOAPI SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight)
{
    if (player && player->fMixer)
        player->fMixer->SetPan(chan, panLeft, panRight);
}

void AUDIOAPI SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectList effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer)
        player->fMixer->SetEffectList(chan, effect_list, fadeIn, fadeOut);
}

void AUDIOAPI SetEffectListChannelPtr(AudioPlayerPtr player, long chan, AudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer)
        player->fMixer->SetEffectList(chan, *effect_list, fadeIn, fadeOut);
}

// Master
void AUDIOAPI SetVolAudioPlayer(AudioPlayerPtr player, float vol)
{
    if (player && player->fMixer)
        player->fMixer->SetVol(vol);
}

void AUDIOAPI SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight)
{
    if (player && player->fMixer)
        player->fMixer->SetPan(panLeft, panRight);
}

void AUDIOAPI SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectList effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer)
        player->fMixer->SetEffectList(effect_list, fadeIn, fadeOut);
}

void AUDIOAPI SetEffectListAudioPlayerPtr(AudioPlayerPtr player, AudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fMixer)
        player->fMixer->SetEffectList(*effect_list, fadeIn, fadeOut);
}

// Globals
AudioRendererPtr AUDIOAPI MakeAudioRenderer(long renderer)
{
	return static_cast<AudioRendererPtr>(TAudioRendererFactory::MakeAudioRenderer(renderer));
}

void AUDIOAPI DeleteAudioRenderer(AudioRendererPtr obj)
{
	TAudioRendererPtr renderer = static_cast<TAudioRendererPtr>(obj);
	delete renderer;
}

int AUDIOAPI OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate)
{
	return static_cast<TAudioRendererPtr>(renderer)->Open(inputDevice, outputDevice, inChan, outChan, bufferSize, sampleRate);
}

void AUDIOAPI CloseAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Close();
}

void AUDIOAPI StartAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Start();
}

void AUDIOAPI StopAudioRenderer(AudioRendererPtr renderer)
{
	static_cast<TAudioRendererPtr>(renderer)->Stop();
}

void AUDIOAPI AddAudioClient(AudioRendererPtr renderer, AudioClientPtr client)
{
	static_cast<TAudioRendererPtr>(renderer)->AddClient(static_cast<TAudioClientPtr>(client));
}

void AUDIOAPI RemoveAudioClient(AudioRendererPtr renderer, AudioClientPtr client)
{
	static_cast<TAudioRendererPtr>(renderer)->RemoveClient(static_cast<TAudioClientPtr>(client));
}

void AUDIOAPI AudioGlobalsInit(long inChan, 
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

void AUDIOAPI AudioGlobalsDestroy()
{
	TAudioGlobals::Destroy();
}

long AUDIOAPI GetDeviceCount(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->GetDeviceCount();
}

void AUDIOAPI GetDeviceInfo(AudioRendererPtr renderer, long deviceNum, DeviceInfo* info)
{
	static_cast<TAudioRendererPtr>(renderer)->GetDeviceInfo(deviceNum, info);
}

long AUDIOAPI GetDefaultInputDevice(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->GetDefaultInputDevice();
}

long AUDIOAPI GetDefaultOutputDevice(AudioRendererPtr renderer)
{
	return static_cast<TAudioRendererPtr>(renderer)->GetDefaultOutputDevice();
}


