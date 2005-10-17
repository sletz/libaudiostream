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

#include "TAudioEngine.h"
#include "TAudioRendererFactory.h"
#include "TAudioStreamFactory.h"

#include "TVolAudioEffect.h"
#include "TPanAudioEffect.h"
#include "TFaustAudioEffect.h"


#ifdef WIN32
	#define	AUDIOAPI __declspec(dllexport)
#else
	#define	AUDIOAPI
#endif

    enum {kPortAudioRenderer = 0, kJackRenderer};

    struct AudioPlayer {
        TAudioRendererPtr fRenderer;
        TAudioEngine* fEngine;
    };

    // Opaque pointers
    typedef AudioPlayer* AudioPlayerPtr;
	typedef TAudioStreamPtr AudioStream;
	typedef AudioStream* AudioStreamPtr;

 	typedef TAudioEffectListPtr AudioEffectList;
    typedef TAudioEffectInterfacePtr AudioEffect;	
	typedef AudioEffectList* AudioEffectListPtr;
    typedef AudioEffect* AudioEffectPtr;	
	
	typedef void (*StopCallback)(void* context);
	
	/*
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

    AudioEffect AUDIOAPI MakeVolAudioEffect(float vol);
	AudioEffect AUDIOAPI MakeMonoPanAudioEffect(float pan);
	AudioEffect AUDIOAPI MakeStereoPanAudioEffect(float panLeft, float panRight);
	AudioEffect AUDIOAPI MakeFaustAudioEffect(const char* name);
	long AUDIOAPI GetControlCount(AudioEffect effect);
	void AUDIOAPI GetControlParam(AudioEffect effect, long param, char* label, float* min, float* max, float* init);
	void AUDIOAPI SetControlValue(AudioEffect effect, long param, float f);
	float AUDIOAPI GetControlValue(AudioEffect effect, long param);
	*/

#ifdef __cplusplus
extern "C"
{
#endif
	
	long LibVersion();

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
    AudioStream AUDIOAPI MakeWriteSound(char* name, AudioStream s, long format);
    AudioStream AUDIOAPI MakeInputSound();
    AudioStream AUDIOAPI MakeRendererSound(AudioStream s);

    long AUDIOAPI GetLengthSound(AudioStream s);
    long AUDIOAPI GetChannelsSound(AudioStream s);
    long AUDIOAPI ReadSound(AudioStream stream, float* buffer, long buffer_size, long channels);
	void AUDIOAPI ResetSound(AudioStream sound);
	
	
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
    AudioStreamPtr AUDIOAPI MakeWriteSoundPtr(char* name, AudioStreamPtr s, long format);
    AudioStreamPtr AUDIOAPI MakeInputSoundPtr();
    AudioStreamPtr AUDIOAPI MakeRendererSoundPtr(AudioStreamPtr s);

    long AUDIOAPI GetLengthSoundPtr(AudioStreamPtr s);
    long AUDIOAPI GetChannelsSoundPtr(AudioStreamPtr s);
    long AUDIOAPI ReadSoundPtr(AudioStreamPtr stream, float* buffer, long buffer_size, long channels);
	void AUDIOAPI ResetSoundPtr(AudioStreamPtr sound);
	void AUDIOAPI DeleteSoundPtr(AudioStreamPtr sound);
	
    // Effect management (using smartptr)
	
    AudioEffectList AUDIOAPI MakeAudioEffectList();
    AudioEffectList AUDIOAPI AddAudioEffect(AudioEffectList list_effect, AudioEffect effect);
    AudioEffectList AUDIOAPI RemoveAudioEffect(AudioEffectList list_effect, AudioEffect effect);

    AudioEffect AUDIOAPI MakeVolAudioEffect(float vol);
	AudioEffect AUDIOAPI MakeMonoPanAudioEffect(float pan);
	AudioEffect AUDIOAPI MakeStereoPanAudioEffect(float panLeft, float panRight);
	AudioEffect AUDIOAPI MakeFaustAudioEffect(const char* name);
	long AUDIOAPI GetControlCount(AudioEffect effect);
	void AUDIOAPI GetControlParam(AudioEffect effect, long param, char* label, float* min, float* max, float* init);
	void AUDIOAPI SetControlValue(AudioEffect effect, long param, float f);
	float AUDIOAPI GetControlValue(AudioEffect effect, long param);
	
	
	AudioEffectListPtr AUDIOAPI MakeAudioEffectListPtr();
    AudioEffectListPtr AUDIOAPI AddAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect);
    AudioEffectListPtr AUDIOAPI RemoveAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect);

    AudioEffectPtr AUDIOAPI MakeVolAudioEffectPtr(float vol);
	AudioEffectPtr AUDIOAPI MakeMonoPanAudioEffectPtr(float pan);
	AudioEffectPtr AUDIOAPI MakeStereoPanAudioEffectPtr(float panLeft, float panRight);
	AudioEffectPtr AUDIOAPI MakeFaustAudioEffectPtr(const char* name);
	long AUDIOAPI GetControlCountPtr(AudioEffectPtr effect);
	void AUDIOAPI GetControlParamPtr(AudioEffectPtr effect, long param, char* label, float* min, float* max, float* init);
	void AUDIOAPI SetControlValuePtr(AudioEffectPtr effect, long param, float f);
	float AUDIOAPI GetControlValuePtr(AudioEffectPtr effect, long param);
	
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
    void AUDIOAPI CloseAudioPlayer(AudioPlayerPtr player);


    // Load a sound in a channel
    long AUDIOAPI LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight);
	long AUDIOAPI LoadChannelPtr(AudioPlayerPtr player, AudioStreamPtr sound, long chan, float vol, float panLeft, float panRight);
    void AUDIOAPI GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfoPtr info);
	void AUDIOAPI SetStopCallbackChannel(AudioPlayerPtr player, long chan, StopCallback callback, void* context);

    // Transport
    void AUDIOAPI StartAudioPlayer(AudioPlayerPtr player);		// Start the global player
    void AUDIOAPI StopAudioPlayer(AudioPlayerPtr player);		// Stop the global player

    void AUDIOAPI StartChannel(AudioPlayerPtr player, long chan); // Start a sound region from the beginning
    void AUDIOAPI ContChannel(AudioPlayerPtr player, long chan);	// Play a sound region from the current location
    void AUDIOAPI StopChannel(AudioPlayerPtr player, long chan);	// Stop playing

    // Params
    void AUDIOAPI SetVolChannel(AudioPlayerPtr player, long chan, float vol);
    void AUDIOAPI SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight);
	void AUDIOAPI SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);

    // Master
    void AUDIOAPI SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight);
    void AUDIOAPI SetVolAudioPlayer(AudioPlayerPtr player, float vol);
	void AUDIOAPI SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);

#ifdef __cplusplus
}
#endif

long LibVersion()
{
	return 100;
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
    return TAudioStreamFactory::MakeFadeSound((TAudioStreamPtr)sound, fadeIn, fadeOut);
}

AudioStream AUDIOAPI MakeLoopSound(AudioStream sound, long n)
{
    return TAudioStreamFactory::MakeLoopSound((TAudioStreamPtr)sound, n);
}

AudioStream AUDIOAPI MakeCutSound(AudioStream sound, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeCutSound((TAudioStreamPtr)sound, beginFrame, endFrame);
}

AudioStream AUDIOAPI MakeSeqSound(AudioStream s1, AudioStream s2, long crossFade)
{
    return TAudioStreamFactory::MakeSeqSound((TAudioStreamPtr)s1, (TAudioStreamPtr)s2, crossFade);
}

AudioStream AUDIOAPI MakeMixSound(AudioStream s1, AudioStream s2)
{
    return TAudioStreamFactory::MakeMixSound((TAudioStreamPtr)s1, (TAudioStreamPtr)s2);
}

AudioStream AUDIOAPI MakeInputSound()
{
    return TAudioStreamFactory::MakeInputSound();
}

AudioStream AUDIOAPI MakeTransformSound(AudioStream s1, AudioEffectList list_effect, long fadeIn, long fadeOut)
{
	return TAudioStreamFactory::MakeTransformSound((TAudioStreamPtr)s1, (TAudioEffectListPtr)list_effect, fadeIn, fadeOut);
}

AudioStream AUDIOAPI MakeWriteSound(char* name, AudioStream s, long format)
{
    return TAudioStreamFactory::MakeWriteSound(name, (TAudioStreamPtr)s, format);
}

AudioStream AUDIOAPI MakeRendererSound(AudioStream s)
{
    return TAudioStreamFactory::MakeDTRenderer((TAudioStreamPtr)s);
}

long AUDIOAPI GetLengthSound(AudioStream s)
{
    return (s) ? ((TAudioStreamPtr)s)->Length() : 0;
}

long AUDIOAPI GetChannelsSound(AudioStream s)
{
    return (s) ? ((TAudioStreamPtr)s)->Channels() : 0;
}

void AUDIOAPI ResetSound(AudioStream s)
{
	((TAudioStreamPtr)s)->Reset();
}

long AUDIOAPI ReadSound(AudioStream s, float* buffer, long buffer_size, long channels)
{
    if (s && buffer) {
        TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
		UAudioTools::ZeroFloatBlk(buffer, buffer_size, channels);
        return ((TAudioStreamPtr)s)->Read(&process, buffer_size, 0, channels);
    } else {
        return 0;
    }
}

AudioStreamPtr AUDIOAPI MakeSoundPtr(AudioStream sound) 
{
	return new SMARTP<TAudioStream>(sound);
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
	return MakeSoundPtr(TAudioStreamFactory::MakeStereoSound((TAudioStreamPtr)*sound));
}

AudioStreamPtr AUDIOAPI MakeFadeSoundPtr(AudioStreamPtr sound, long fadeIn, long fadeOut)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeFadeSound((TAudioStreamPtr)*sound, fadeIn, fadeOut));
}

AudioStreamPtr AUDIOAPI MakeLoopSoundPtr(AudioStreamPtr sound, long n)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeLoopSound((TAudioStreamPtr)*sound, n));
}

AudioStreamPtr AUDIOAPI MakeCutSoundPtr(AudioStreamPtr sound, long beginFrame, long endFrame)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeCutSound((TAudioStreamPtr)*sound, beginFrame, endFrame));
}

AudioStreamPtr AUDIOAPI MakeSeqSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeSeqSound((TAudioStreamPtr)*s1, (TAudioStreamPtr)*s2, crossFade));
}

AudioStreamPtr AUDIOAPI MakeMixSoundPtr(AudioStreamPtr s1, AudioStreamPtr s2)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeMixSound((TAudioStreamPtr)*s1, (TAudioStreamPtr)*s2));
}

AudioStreamPtr AUDIOAPI MakeInputSoundPtr()
{
    return MakeSoundPtr(TAudioStreamFactory::MakeInputSound());
}

AudioStreamPtr AUDIOAPI MakeTransformSoundPtr(AudioStreamPtr s1, AudioEffectListPtr list_effect, long fadeIn, long fadeOut)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeTransformSound((TAudioStreamPtr)*s1, (TAudioEffectListPtr)*list_effect, fadeIn, fadeOut));
}

AudioStreamPtr AUDIOAPI MakeWriteSoundPtr(char* name, AudioStreamPtr s, long format)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeWriteSound(name, (TAudioStreamPtr)*s, format));
}

AudioStreamPtr AUDIOAPI MakeRendererSoundPtr(AudioStreamPtr s)
{
    return MakeSoundPtr(TAudioStreamFactory::MakeDTRenderer((TAudioStreamPtr)*s));
}

long AUDIOAPI GetLengthSoundPtr(AudioStreamPtr s)
{
    return (s) ? ((TAudioStreamPtr)*s)->Length() : 0;
}

long AUDIOAPI GetChannelsSoundPtr(AudioStreamPtr s)
{
    return (s) ? ((TAudioStreamPtr)*s)->Channels() : 0;
}

void AUDIOAPI ResetSoundPtr(AudioStreamPtr s)
{
	((TAudioStreamPtr)*s)->Reset();
}

long AUDIOAPI ReadSoundPtr(AudioStreamPtr s, float* buffer, long buffer_size, long channels)
{
    if (s && buffer) {
        TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
		UAudioTools::ZeroFloatBlk(buffer, buffer_size, channels);
        return ((TAudioStreamPtr)*s)->Read(&process, buffer_size, 0, channels);
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
       TAudioEffectListPtr(list_effect)->push_back(TAudioEffectInterfacePtr(effect));
    return list_effect;
}

AudioEffectList AUDIOAPI RemoveAudioEffect(AudioEffectList list_effect, AudioEffect effect)
{
    if (list_effect && effect)
        TAudioEffectListPtr(list_effect)->remove(TAudioEffectInterfacePtr(effect));
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

AudioEffect AUDIOAPI MakeFaustAudioEffect(const char* name)
{
	try {
		return new TFaustAudioEffect(name);
	} catch (int n) {
		printf("MakeFaustAudioEffect exception %d \n", n);
		return 0;
	}
}

long AUDIOAPI GetControlCount(AudioEffect effect) 
{
	return ((TAudioEffectInterfacePtr)effect)->GetControlCount();
}

void AUDIOAPI GetControlParam(AudioEffect effect, long control, char* label, float* min, float* max, float* init)
{
	((TAudioEffectInterfacePtr)effect)->GetControlParam(control, label, min, max, init);
}

void AUDIOAPI SetControlValue(AudioEffect effect, long control, float f)
{
	((TAudioEffectInterfacePtr)effect)->SetControlValue(control, f);
}

float AUDIOAPI GetControlValue(AudioEffect effect, long control)
{
	return ((TAudioEffectInterfacePtr)effect)->GetControlValue(control);
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
    return new SMARTP<TAudioEffectList>(new TAudioEffectList());
}

AudioEffectListPtr AUDIOAPI AddAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
    if (list_effect && effect)
        TAudioEffectListPtr(*list_effect)->push_back(TAudioEffectInterfacePtr(*effect));
    return list_effect;
}

AudioEffectListPtr AUDIOAPI RemoveAudioEffectPtr(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
    if (list_effect && effect)
        TAudioEffectListPtr(*list_effect)->remove(TAudioEffectInterfacePtr(*effect));
    return list_effect;
}

AudioEffectPtr AUDIOAPI MakeVolAudioEffectPtr(float vol)
{
    return new SMARTP<TAudioEffectInterface>(new TVolAudioEffect(vol));
}

AudioEffectPtr AUDIOAPI MakeMonoPanAudioEffectPtr(float pan)
{
    return new SMARTP<TAudioEffectInterface>(new TMonoPanAudioEffect(pan));
}

AudioEffectPtr AUDIOAPI MakeStereoPanAudioEffectPtr(float panLeft, float panRight)
{
    return new SMARTP<TAudioEffectInterface>(new TStereoPanAudioEffect(panLeft, panRight));
}

AudioEffectPtr AUDIOAPI MakeFaustAudioEffectPtr(const char* name)
{
	try {
		return new SMARTP<TAudioEffectInterface>(new TFaustAudioEffect(name));
	} catch (int n) {
		printf("MakeFaustAudioEffect exception %d \n", n);
		return 0;
	}
}

long AUDIOAPI GetControlCountPtr(AudioEffectPtr effect) 
{
	return ((TAudioEffectInterfacePtr)*effect)->GetControlCount();
}

void AUDIOAPI GetControlParamPtr(AudioEffectPtr effect, long control, char* label, float* min, float* max, float* init)
{
	((TAudioEffectInterfacePtr)*effect)->GetControlParam(control, label, min, max, init);
}

void AUDIOAPI SetControlValuePtr(AudioEffectPtr effect, long control, float f)
{
	((TAudioEffectInterfacePtr)*effect)->SetControlValue(control, f);
}

float AUDIOAPI GetControlValuePtr(AudioEffectPtr effect, long control)
{
	return ((TAudioEffectInterfacePtr)*effect)->GetControlValue(control);
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
    long tmpInChan = inChan;
    long tmpOutChan = outChan;
    long tmpBufferSize = buffer_size;
    long tmpSampleRate = sample_rate;
    int res;
	
	if (thread_num < 1) 
		printf("OpenAudioPlayer error: thread_num parameter should be at least one !! \n");

    TAudioGlobals::Init(inChan, outChan, channels, sample_rate, buffer_size, stream_buffer_size, rtstream_buffer_size, thread_num);

    AudioPlayerPtr player = (AudioPlayerPtr)calloc(1, sizeof(AudioPlayer));
    if (!player)
        goto error;

    switch (renderer) {

        case kPortAudioRenderer:
            player->fRenderer = TAudioRendererFactory::MakePortAudioRenderer();
            break;

        case kJackRenderer:
            player->fRenderer = TAudioRendererFactory::MakeJackAudioRenderer();
            break;
    }

    if (!player->fRenderer)
        goto error;

    player->fEngine = new TAudioEngine(player->fRenderer);
    if (!player->fEngine)
        goto error;

    res = player->fEngine->Open(&tmpInChan, &tmpOutChan, &tmpBufferSize, &tmpSampleRate);

    if (res == NO_ERR)
        return player;

error:
    CloseAudioPlayer(player);
    return 0;
}

void AUDIOAPI CloseAudioPlayer(AudioPlayerPtr player)
{
    if (!player)
        return ;

    if (player->fEngine) {
        player->fEngine->Close();
        delete player->fEngine;
    }

    if (player->fRenderer) {
        delete player->fRenderer;
    }

    free(player);
	TAudioGlobals::Destroy();
}

// SoundFile management
long AUDIOAPI LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight)
{
    if (player && player->fEngine && sound) {
        return player->fEngine->LoadChannel((TAudioStreamPtr)sound, chan, vol, panLeft, panRight);
    } else
        return LOAD_ERR;
}

long AUDIOAPI LoadChannelPtr(AudioPlayerPtr player, AudioStreamPtr sound, long chan, float vol, float panLeft, float panRight)
{
    if (player && player->fEngine && sound) {
        return player->fEngine->LoadChannel((TAudioStreamPtr)*sound, chan, vol, panLeft, panRight);
    } else
        return LOAD_ERR;
}

void AUDIOAPI GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfo* info)
{
    if (player && player->fEngine && info) {
        player->fEngine->GetInfoChannel(chan, info);
    }
}

void AUDIOAPI SetStopCallbackChannel(AudioPlayerPtr player,long chan, StopCallback callback, void* context)
{
    if (player && player->fEngine ) {
        player->fEngine->SetStopCallbackChannel(chan, callback, context);
    }
}


// Transport
void AUDIOAPI StartChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fEngine)
        player->fEngine->StartChannel(chan);
}

void AUDIOAPI ContChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fEngine)
        player->fEngine->PlayChannel(chan);
}

void AUDIOAPI StopChannel(AudioPlayerPtr player, long chan)
{
    if (player && player->fEngine)
        player->fEngine->StopChannel(chan);
}

void AUDIOAPI StartAudioPlayer(AudioPlayerPtr player)
{
    if (player && player->fEngine)
        player->fEngine->Start();
}

void AUDIOAPI StopAudioPlayer(AudioPlayerPtr player)
{
    if (player && player->fEngine)
        player->fEngine->Stop();
}

// Params
void AUDIOAPI SetVolChannel(AudioPlayerPtr player, long chan, float vol)
{
    if (player && player->fEngine)
        player->fEngine->SetVolChannel(chan, vol);
}

void AUDIOAPI SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight)
{
    if (player && player->fEngine)
        player->fEngine->SetPanChannel(chan, panLeft, panRight);
}

void AUDIOAPI SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fEngine)
        player->fEngine->SetEffectListChannel(chan, *effect_list, fadeIn, fadeOut);
}

// Master
void AUDIOAPI SetVolAudioPlayer(AudioPlayerPtr player, float vol)
{
    if (player && player->fEngine)
        player->fEngine->SetMasterVol(vol);
}

void AUDIOAPI SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight)
{
    if (player && player->fEngine)
        player->fEngine->SetMasterPan(panLeft, panRight);
}

void AUDIOAPI SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
    if (player && player->fEngine)
        player->fEngine->SetEffectListMaster(*effect_list, fadeIn, fadeOut);
}

