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
 grame@rd.grame.fr

*/

#include "TAudioEngine.h"
#include "TAudioRendererFactory.h"

#include "TAudioStreamFactory.h"
#include "TThreadCmdManager.h"

#include "TAudioGlobals.h"
#include "UAudioTools.h"

#include "TVolAudioEffect.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef WIN32
	#define	AUDIOAPI __declspec(dllexport)
#endif

#ifdef __Macintosh__
	#define	AUDIOAPI
#endif

	enum {kPortAudioRenderer = 0, kJackRenderer};

    struct AudioPlayer {
        TAudioRendererPtr	fRenderer;
        TAudioEngine*	fEngine;
    };

    // Opaque pointers
    typedef AudioPlayer* AudioPlayerPtr;
    typedef void* AudioStreamPtr;
    typedef void* AudioEffectListPtr;
	typedef void* AudioEffectPtr;

    // Build sound
    AudioStreamPtr AUDIOAPI MakeNullSound(long lengthFrame);
    AudioStreamPtr AUDIOAPI MakeReadSound(char* name);
    AudioStreamPtr AUDIOAPI MakeRegionSound(char* name, long beginFrame, long endFrame);
    AudioStreamPtr AUDIOAPI MakeFadeSound(AudioStreamPtr sound, long fadeIn, long fadeOut);
    AudioStreamPtr AUDIOAPI MakeLoopSound(AudioStreamPtr sound, long n);
    AudioStreamPtr AUDIOAPI MakeCutSound(AudioStreamPtr sound, long beginFrame, long endFrame);
    AudioStreamPtr AUDIOAPI MakeSeqSound(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade);
    AudioStreamPtr AUDIOAPI MakeMixSound(AudioStreamPtr s1, AudioStreamPtr s2);
    AudioStreamPtr AUDIOAPI MakeTransformSound(AudioStreamPtr sound, AudioEffectListPtr effect_list, long fadeIn, long fadeOut);
    AudioStreamPtr AUDIOAPI MakeWriteSound(char* name, AudioStreamPtr s, long format);
    AudioStreamPtr AUDIOAPI MakeInputSound();
    AudioStreamPtr AUDIOAPI MakeRendererSound(AudioStreamPtr s);

    long AUDIOAPI GetLengthSound(AudioStreamPtr s);
    long AUDIOAPI GetChannelsSound(AudioStreamPtr s);
    long AUDIOAPI ReadSound(AudioStreamPtr stream, float* buffer, long buffer_size, long channels);

    void AUDIOAPI DeleteSound(AudioStreamPtr sound);
	
	// Effect management
	AudioEffectListPtr AUDIOAPI MakeAudioEffectList();
	AudioEffectListPtr AUDIOAPI AddAudioEffect(AudioEffectListPtr list_effect, AudioEffectPtr effect);
	AudioEffectListPtr AUDIOAPI RemoveAudioEffect(AudioEffectListPtr list_effect, AudioEffectPtr effect);
	
	AudioEffectPtr AUDIOAPI MakeVolAudioEffect(float gain);

    // Open/Close
    AudioPlayerPtr AUDIOAPI OpenAudioPlayer(long inChan,
                                            long outChan,
                                            long channels,
                                            long sample_rate,
                                            long buffer_size,
                                            long stream_buffer_size,
                                            long rtstream_buffer_size,
											long renderer);
    void AUDIOAPI CloseAudioPlayer(AudioPlayerPtr player);


    // Load a sound in a channel
    long AUDIOAPI LoadChannel(AudioPlayerPtr player, AudioStreamPtr sound, long chan, long vol, long pan);
    void AUDIOAPI GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfoPtr info);

    // Transport
    void AUDIOAPI StartAudioPlayer(AudioPlayerPtr player);		// Start the global player
    void AUDIOAPI StopAudioPlayer(AudioPlayerPtr player);		// Stop the global player

    void AUDIOAPI StartSound(AudioPlayerPtr player, long chan); // Start a sound region from the beginning
    void AUDIOAPI ContSound(AudioPlayerPtr player, long chan);	// Play a sound region from the current location
    void AUDIOAPI StopSound(AudioPlayerPtr player, long chan);	// Stop playing

    // Params
    void AUDIOAPI SetVolSound(AudioPlayerPtr player, long chan, long vol);
    void AUDIOAPI SetPanSound(AudioPlayerPtr player, long chan, long pan);

    // Master
    void AUDIOAPI SetPanAudioPlayer(AudioPlayerPtr player, long pan);
    void AUDIOAPI SetVolAudioPlayer(AudioPlayerPtr player, long vol);

#ifdef __cplusplus
}
#endif

// Build sound

AudioStreamPtr AUDIOAPI MakeNullSound(long lengthFrame)
{
    return TAudioStreamFactory::MakeNullSound(lengthFrame);
}

AudioStreamPtr AUDIOAPI MakeReadSound(char* name)
{
    return TAudioStreamFactory::MakeReadSound(name);
}

AudioStreamPtr AUDIOAPI MakeRegionSound(char* name, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeRegionSound(name, beginFrame, endFrame);
}

AudioStreamPtr AUDIOAPI MakeFadeSound(AudioStreamPtr sound, long fadeIn, long fadeOut)
{
    return TAudioStreamFactory::MakeFadeSound((TAudioStreamPtr)sound, fadeIn, fadeOut);
}

AudioStreamPtr AUDIOAPI MakeLoopSound(AudioStreamPtr sound, long n)
{
    return TAudioStreamFactory::MakeLoopSound((TAudioStreamPtr)sound, n);
}

AudioStreamPtr AUDIOAPI MakeCutSound(AudioStreamPtr sound, long beginFrame, long endFrame)
{
    return TAudioStreamFactory::MakeCutSound((TAudioStreamPtr)sound, beginFrame, endFrame);
}

AudioStreamPtr AUDIOAPI MakeSeqSound(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade)
{
    return TAudioStreamFactory::MakeSeqSound((TAudioStreamPtr)s1, (TAudioStreamPtr)s2, crossFade);
}

AudioStreamPtr AUDIOAPI MakeMixSound(AudioStreamPtr s1, AudioStreamPtr s2)
{
    return TAudioStreamFactory::MakeMixSound((TAudioStreamPtr)s1, (TAudioStreamPtr)s2);
}

AudioStreamPtr AUDIOAPI MakeInputSound()
{
    return TAudioStreamFactory::MakeInputSound();
}

AudioStreamPtr AUDIOAPI MakeTransformSound(AudioStreamPtr s1, AudioEffectListPtr list_effect, long fadeIn, long fadeOut)
{
    return TAudioStreamFactory::MakeTransformSound((TAudioStreamPtr)s1, (TAudioEffectPtr)list_effect, fadeIn, fadeOut);
}

AudioStreamPtr AUDIOAPI MakeWriteSound(char* name, AudioStreamPtr s, long format)
{
    return TAudioStreamFactory::MakeWriteSound(name, (TAudioStreamPtr)s, format);
}

AudioStreamPtr AUDIOAPI MakeRendererSound(AudioStreamPtr s)
{
    return TAudioStreamFactory::MakeDTRenderer((TAudioStreamPtr)s);
}

long AUDIOAPI GetLengthSound(AudioStreamPtr s)
{
    return ((TAudioStreamPtr)s)->Length();
}

long AUDIOAPI GetChannelsSound(AudioStreamPtr s)
{
    return ((TAudioStreamPtr)s)->Channels();
}

long AUDIOAPI ReadSound(AudioStreamPtr s, float* buffer, long buffer_size, long channels)
{
    TSharedAudioBuffer<float> process(buffer, buffer_size, channels);
    return ((TAudioStreamPtr)s)->Read(&process, buffer_size, 0, channels);
}

void AUDIOAPI DeleteSound(AudioStreamPtr s)
{
    delete(TAudioStreamPtr)s;
}

// Effect management
AudioEffectListPtr MakeAudioEffectList()
{
	return new TAudioEffect();
}

AudioEffectPtr AddAudioEffect(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
	TAudioEffectPtr(list_effect)->push_back(TAudioEffectInterfacePtr(effect));
	return list_effect;
}

AudioEffectPtr RemoveAudioEffect(AudioEffectListPtr list_effect, AudioEffectPtr effect)
{
	TAudioEffectPtr(list_effect)->remove(TAudioEffectInterfacePtr(effect));
	return list_effect;
}

AudioEffectPtr MakeVolAudioEffect(float gain)
{
	return new TVolAudioEffect(gain);
}

// Open/Close

AUDIOAPI AudioPlayerPtr OpenAudioPlayer
(long inChan, long outChan, long channels, long sample_rate, long buffer_size, long stream_buffer_size, long rtstream_buffer_size, long renderer)
{
    long tmpInChan = inChan;
    long tmpOutChan = outChan;
    long tmpBufferSize = buffer_size;
    long tmpSampleRate = sample_rate;
	int res;

    TAudioGlobals::Init(inChan, outChan, channels, sample_rate, buffer_size, stream_buffer_size, rtstream_buffer_size);

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
		return;
		
    if (player->fEngine) {
        player->fEngine->Close();
        delete player->fEngine;
    }

    if (player->fRenderer) {
        delete player->fRenderer;
    }

    TAudioGlobals::Destroy();
    free(player);
}

// SoundFile management
long AUDIOAPI LoadChannel(AudioPlayerPtr player, AudioStreamPtr sound, long chan, long vol, long pan)
{
    if (player->fEngine) {
        return player->fEngine->LoadChannel((TAudioStreamPtr)sound, chan, vol, pan);
    } else
        return LOAD_ERR;
}

void AUDIOAPI GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfo* info)
{
    if (player->fEngine) {
        player->fEngine->GetInfoChannel(chan, info);
    }
}

// Transport
void AUDIOAPI StartSound(AudioPlayerPtr player, long chan)
{
    if (player->fEngine)
        player->fEngine->StartSound(chan);
}

void AUDIOAPI ContSound(AudioPlayerPtr player, long chan)
{
    if (player->fEngine)
        player->fEngine->PlaySound(chan);
}

void AUDIOAPI StopSound(AudioPlayerPtr player, long chan)
{
    if (player->fEngine)
        player->fEngine->StopSound(chan);
}

void AUDIOAPI StartAudioPlayer(AudioPlayerPtr player)
{
    if (player->fEngine)
        player->fEngine->Start();
}

void AUDIOAPI StopAudioPlayer(AudioPlayerPtr player)
{
    if (player->fEngine)
        player->fEngine->Stop();
}

// Params
void AUDIOAPI SetVolSound(AudioPlayerPtr player, long chan, long vol)
{
    if (player->fEngine)
        player->fEngine->SetVolSound(chan, vol);
}

void AUDIOAPI SetPanSound(AudioPlayerPtr player, long chan, long pan)
{
    if (player->fEngine)
        player->fEngine->SetPanSound(chan, pan);
}

// Master
void AUDIOAPI SetVolAudioPlayer(AudioPlayerPtr player, long vol)
{
    if (player->fEngine)
        player->fEngine->SetMasterVol(vol);
}

void AUDIOAPI SetPanAudioPlayer(AudioPlayerPtr player, long pan)
{
    if (player->fEngine)
        player->fEngine->SetMasterPan(pan);
}

