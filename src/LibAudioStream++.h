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

#ifndef __LibAudioStreamPlusPlus__
#define __LibAudioStreamPlusPlus__

#include "la_smartpointer.h"
#include <list>

#define NO_ERR 0
#define OPEN_ERR -1
#define CLOSE_ERR -1
#define LOAD_ERR -3
#define FILE_NOT_FOUND_ERR -4

enum {kPlayingChannel = 0, kIdleChannel};
enum {kPortAudioRenderer = 0, kJackRenderer, kCoreAudioRenderer};

/*!
\brief Sound channel info
*/
typedef struct ChannelInfo* ChannelInfoPtr;
typedef struct ChannelInfo {
	long fStatus;  // 1 = playing , 0 = idle
	long fCurFrame;
	float fVol;
	float fPanLeft;
	float fPanRight;
	long fLeftOut;
	long fRightOut;
}
ChannelInfo;

class TAudioStream : public la_smartable {
	virtual ~TAudioStream() {}
};
typedef SMARTP<TAudioStream> TAudioStreamPtr;

class TAudioEffectInterface : public la_smartable {
	virtual ~TAudioEffectInterface() {}
};
typedef SMARTP<TAudioEffectInterface> TAudioEffectInterfacePtr;

class TAudioEffectList : public std::list<TAudioEffectInterfacePtr>, public la_smartable {
	virtual ~TAudioEffectList() {}
};
typedef SMARTP<TAudioEffectList> TAudioEffectListPtr;

// Opaque pointers
typedef void* AudioPlayerPtr;
typedef void* AudioManagerPtr;

typedef TAudioStreamPtr AudioStream;
typedef TAudioEffectListPtr AudioEffectList;
typedef TAudioEffectInterfacePtr AudioEffect;	

typedef void (*StopCallback)(void* context);

#ifdef __cplusplus
extern "C"
{
#endif


long LibVersion();

/*!
\brief Create a stream that will produce "silence".
\param lengthFrame The number of null frame to be produced.
\return A pointer to new stream object.
*/
AudioStream MakeNullSound(long lengthFrame);
/*!
\brief Create a file reader stream.
\param name The sound file pathname.
\return A pointer to new stream object or NULL if the file cannot be opened.
*/
AudioStream MakeReadSound(char* name);
/*!
\brief Create a file region reader stream. 
\param name The sound file pathname.
\param beginFrame The start frame of the region.
\param endFrame The end frame of the region.
\return A pointer to new stream object or NULL if the wanted region is not part of the file.
*/
AudioStream MakeRegionSound(char* name, long beginFrame, long endFrame);
/*!
\brief Transform a stream in a stereo stream.
\param sound The stream to be transformed.
\return A pointer to new stream object.
*/
AudioStream MakeStereoSound(AudioStream sound);
/*!
\brief Create a fade on a stream.
\param sound The stream to be "faded".
\param fadeIn The fadein length in frames.
\param fadeOut The fadeout length in frames.
\return A pointer to new stream object.
*/
AudioStream MakeFadeSound(AudioStream sound, long fadeIn, long fadeOut);
/*!
\brief Loop a stream.
\param sound The stream to be looped.
\param num The number of loops.
\return A pointer to new stream object.
*/
AudioStream MakeLoopSound(AudioStream sound, long num);
/*!
\brief Cut in a stream : the portion between 0 and beginFrame will be removed, the portion between endFrame and the stream end will be removed.
\param sound The stream to be cutted.
\param beginFrame The start frame number of the stream part to remove.
\param endFrame The end frame number of the stream part to remove
\return A pointer to new stream object.
*/
AudioStream MakeCutSound(AudioStream sound, long beginFrame, long endFrame);
/*!
\brief Put two streams in sequence.
\param s1 The first stream in the sequence.
\param s2 The second stream in the sequence.
\param crossFade A crossface section expressed in frames.
\return A pointer to new stream object.
*/
AudioStream MakeSeqSound(AudioStream s1, AudioStream s2, long crossFade);
/*!
\brief Mix two streams.
\param s1 The first stream in the mix.
\param s2 The second stream in the mix.
\return A pointer to new stream object.
*/
AudioStream MakeMixSound(AudioStream s1, AudioStream s2);
/*!
\brief Apply a list of effects on a stream.
\param sound The stream to be transformed.
\param effect_list The effect list to be used.
\param fadeIn A fadein section frames before the effect is fully applied.
\param fadeOut A fadeout section frames before the effect is fully removed.
\return A pointer to new stream object.
*/
AudioStream MakeTransformSound(AudioStream sound, AudioEffectList effect_list, long fadeIn, long fadeOut);
/*!
\brief Create a stream writer.
\param name The sound file pathname.
\param sound The stream to be saved.
\param format A libsndfile format.
\return A pointer to new stream object or NULL if the file cannot be opened.
*/
AudioStream MakeWriteSound(char* name, AudioStream sound, long format);
/*!
\brief Create an inputstream.
\return A pointer to new stream object.
*/
AudioStream MakeInputSound();
/*!
\brief Create an renderer "wrapper" on a stream, to be used for direct access to the stream content.
\return A pointer to new stream object.
*/
AudioStream MakeRendererSound(AudioStream s);
/*!
\brief Get the stream length in frames.
\param sound The stream.
\return The stream length in frames.
*/
long GetLengthSound(AudioStream sound);
/*!
\brief Get the stream number of channels.
\param sound The stream.
\return The number of channels.
*/
long GetChannelsSound(AudioStream sound);
/*!
\brief Read a buffer of the stream.
\param sound The stream.
\param buffer A buffer to be filled with frames.
\param buffer_size The buffer length.
\param channels The number of channels in the buffer.
\return The number of read frames.
*/
long ReadSound(AudioStream sound, float* buffer, long buffer_size, long channels);
/*!
\brief Reset a stream.
\param sound The stream to be reseted.
*/
void ResetSound(AudioStream sound);

/* Effect management */

/*!
\brief Create an effect list.
\return A pointer to new effect list.
*/
AudioEffectList MakeAudioEffectList();
/*!
\brief Add an effect in an effect list.
\param list The effect list where the effect is added.
\param effect The effect to be added.
\return A pointer to the modified effect list.
*/
AudioEffectList AddAudioEffect(AudioEffectList list_effect, AudioEffect effect);
/*!
\brief Remove an effect from an effect list.
\param list The effect list where the effect is removed.
\param effect The effect to be removed.
\return A pointer to the modified effect list.
*/
AudioEffectList RemoveAudioEffect(AudioEffectList list_effect, AudioEffect effect);
/*!
\brief Clear all effects from the list.
\param list The effect list to be cleared.
\return A pointer to the modified effect list.
*/
AudioEffectList ClearAudioEffectList(AudioEffectList list_effect);
/*!
\brief Create a volume effect.
\param vol The volume between 0 and 1.
\return A pointer to new volume object.
*/
AudioEffect MakeVolAudioEffect(float vol);
/*!
\brief Create a mono pan effect.
\param pan The pan between 0 and 1.
\return A pointer to new mono pan object.
*/
AudioEffect MakeMonoPanAudioEffect(float pan);
/*!
\brief Create a stereo pan effect.
\param panLeft The left pan between 0 and 1.
\param rightPan The right pan between 0 and 1.
\return A pointer to new stereo pan object.
*/
AudioEffect MakeStereoPanAudioEffect(float panLeft, float rightPan);
/*!
\brief Create an effect described in the Faust DSP language.
\param name The name of the Faust effect shared library.
\return A pointer to new effect object or NULL if the effect cannot be located or created.
*/
AudioEffect MakeFaustAudioEffect(const char* name);
/*!
\brief Return the number of effect controls.
\param effect The effect pointer.
\return The number of effect controls.
*/
long GetControlCount(AudioEffect effect);
/*!
\brief Return a description on the effect control: control name, min, max and default values
\param effect The effect pointer.
*/
void GetControlParam(AudioEffect effect, long control, char* label, float* min, float* max, float* init);
/*!
\brief Set the effect control value.
\param effect The effect pointer.
\param control The control number between 0 and GetControlCountPtr.
\param value The new value as a float.
*/
void SetControlValue(AudioEffect effect, long control, float value);
/*!
\brief Get the effect control current value.
\param effect The effect pointer.
\param control The control number between 0 and GetControlCountPtr.
\return The effect control current value.
*/
float GetControlValue(AudioEffect effect, long control);

// Open/Close
/*!
\brief Open the audio player.
\param inChan The number of input channels. <B>Only stereo players are currently supported </b>
\param outChan The number of output channels.
\param channels The number of stream channels.
\param sample_rate The sampling rate.
\param buffer_size The audio player internal buffer size.
\param stream_buffer_size The file reader/writer buffer size (used for double buffering).
\param rtstream_buffer_size The input stream buffer size.
\param renderer The audio renderer used to access audio I/O : can be kPortAudioRenderer or kJackRenderer.
\param thread_num The number of additionnal low-priority threads used to precompute data : must be a least one.
\return A pointer to new audio player object.
*/
AudioPlayerPtr OpenAudioPlayer(long inChan,
							   long outChan,
							   long channels,
							   long sample_rate,
							   long buffer_size,
							   long stream_buffer_size,
							   long rtstream_buffer_size,
							   long renderer,
							   long thread_num);
/*!
\brief Open the audio client, to be added to an externally allocated audio manager.
*/					
AudioPlayerPtr OpenAudioClient(AudioManagerPtr manager);	

/*!
\brief Close the audio player.
\param player The audio player to be closed.
*/
void CloseAudioPlayer(AudioPlayerPtr player);

/*!
\brief Close an audio client that was previously added to an external allocated audio renderer using OpenAudioClient.
\param player The audio client to be closed.
*/					
void CloseAudioClient(AudioPlayerPtr player);

/*!
\brief Load a sound in a channel.
\param player The audio player.
\param sound The stream to be inserted in the channel.
\param chan The audio channel number to be used.
\param vol The volume between 0 and 1.
\param pan The panning between 0 and 1.
\return An error code.
*/
long LoadChannel(AudioPlayerPtr player, AudioStream sound, long chan, float vol, float panLeft, float panRight);
/*!
\brief Retrieve information about a sound channel.
\param player The audio player.
\param chan The audio channel number to be used.
\param info The channel info structure to be filled.
*/
void GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfoPtr info);
/*!
\brief Set a callback to be called when the channel stops.
\param player The audio player.
\param chan The audio channel number to be used.
\param callback The callback to be used.
\param context A pointer to data to be given to the callback.
*/
void SetStopCallbackChannel(AudioPlayerPtr player, long chan, StopCallback callback, void* context);

// Transport
/*!
\brief Start the audio player.
\param player The audio player.
*/
void StartAudioPlayer(AudioPlayerPtr player);
/*!
\brief Stop the audio player.
\param player The audio player.
*/
void StopAudioPlayer(AudioPlayerPtr player);
/*!
\brief Start a sound channel from the beginning.
\param player The audio player.
\param chan The audio channel number to be used.
*/
void StartChannel(AudioPlayerPtr player, long chan);
/*!
\brief Play a sound channel from the current location.
\param player The audio player.
\param chan The audio channel number to be used.
*/
void ContChannel(AudioPlayerPtr player, long chan);
/*!
\brief Stop playing a channel, waiting for the stop fadeout to finish.
\param player The audio player.
\param chan The audio channel number to be used.
*/
void StopChannel(AudioPlayerPtr player, long chan);
/*!
\brief Stop playing a channel, immediately returning without waiting for the stop fadeout to finish.
\param player The audio player.
\param chan The audio channel number to be used.
*/
void AbortChannel(AudioPlayerPtr player, long chan);

// Params
/*!
\brief Set the channel volume [0...1]
\param player The audio player.
\param chan The audio channel number to be used.
\param vol The new volume value.
*/
void SetVolChannel(AudioPlayerPtr player, long chan, float vol);
/*!
\brief Set the channel panning [0...1]
\param player The audio player.
\param chan The audio channel number to be used.
\param pan The new panning value.
*/
void SetPanChannel(AudioPlayerPtr player, long chan, float panLeft, float panRight);
/*!
\brief Set the channel audio effect list.
\param player The audio player.
\param chan The audio channel number to be used.
\param effect_list A list of audio effects.
\param fadeIn The fadein length in frames to be used when starting the effect chain.
\param fadeOut The fadeout length in frames to be used when stopping the effect chain.
*/
void SetEffectListChannel(AudioPlayerPtr player, long chan, AudioEffectList effect_list, long fadeIn, long fadeOut);

// Master
/*!
\brief Set the audio player volume [0...1]
\param player The audio player.
\param vol The new volume value.
*/
void SetVolAudioPlayer(AudioPlayerPtr player, float vol);
/*!
\brief Set the audio player panning [0...1]
\param player The audio player.
\param pan The new panning value.
*/
void SetPanAudioPlayer(AudioPlayerPtr player, float panLeft, float panRight);
/*!
\brief Set the master audio effect list.
\param player The audio player.
\param effect_list A list of audio effects.
\param fadeIn The fadein length in frames to be used when stopping the effect chain.
\param fadeOut The fadeout length in frames to be used when stopping the effect chain.
*/
void SetEffectListAudioPlayer(AudioPlayerPtr player, AudioEffectList effect_list, long fadeIn, long fadeOut);

#ifdef __cplusplus
}
#endif


#endif


