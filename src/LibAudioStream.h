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

#ifndef __LibAudioStream__
#define __LibAudioStream__

#ifdef __cplusplus
extern "C"
{
#endif

#define NO_ERR 0
#define OPEN_ERR -1
#define CLOSE_ERR -1
#define LOAD_ERR -3
#define FILE_NOT_FOUND_ERR -4

    enum {kPlayingChannel = 0, kIdleChannel};
	enum {kPortAudioRenderer = 0, kJackRenderer};

    /*!
    \brief Sound channel info
    */
    typedef struct ChannelInfo* ChannelInfoPtr;
    typedef struct ChannelInfo {
        long fStatus;  // 1 = playing , 0 = idle
        long fCurFrame;
        long fVol;
        long fPan;
        long fLeftOut;
        long fRightOut;
    }
    ChannelInfo;

    // Opaque pointers
    typedef void* AudioPlayerPtr;
    typedef void* AudioStreamPtr;
    typedef void* AudioEffectPtr;

    /*!
    \brief Creates a stream that will produce "silence".
    \param lengthFrame The number of null frame to be produced.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeNullSound(long lengthFrame);
    /*!
    \brief Creates a file reader stream.
    \param name The sound file pathname.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeReadSound(char* name);
    /*!
    \brief Creates a file region reader stream.
	\param name The sound file pathname.
    \param beginFrame The start frame of the region.
    \param endFrame The end frame of the region.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeRegionSound(char* name, long beginFrame, long endFrame);
    /*!
    \brief Creates a fade on a stream.
	\param sound The stream to be "faded".
    \param fadeIn The fadein length in frames.
    \param fadeOut The fadeout length in frames.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeFadeSound(AudioStreamPtr sound, long fadeIn, long fadeOut);
    /*!
    \brief Loops a stream.
    \param sound The stream to be looped.
    \param num The number of loops.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeLoopSound(AudioStreamPtr sound, long num);
    /*!
    \brief Cut in a stream : the portion between 0 and beginFrame will be removed, the portion between endFrame and the stream end will be removed.
	\param sound The stream to be cutted.
	\param beginFrame The start frame number of the stream part to remove.
    \param endFrame The end frame number of the stream part to remove
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeCutSound(AudioStreamPtr sound, long beginFrame, long endFrame);
    /*!
    \brief Put two streams in sequence.
    \param s1 The first stream in the sequence.
    \param s2 The second stream in the sequence.
    \param crossFade A crossface section expressed in frames.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeSeqSound(AudioStreamPtr s1, AudioStreamPtr s2, long crossFade);
    /*!
    \brief Mix two streams.
    \param s1 The first stream in the mix.
    \param s2 The second stream in the mix.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeMixSound(AudioStreamPtr s1, AudioStreamPtr s2);
    /*!
    \brief Apply an effect on a stream.
	\param sound The stream to be transformed.
    \param effect The effect to be used.
    \param fadeIn A fadein section frames before the effect is fully applied.
    \param fadeOut A fadeout section frames before the effect is fully removed.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeTransformSound(AudioStreamPtr sound, AudioEffectPtr effect, long fadeIn, long fadeOut);
    /*!
    \brief Create a stream writer.
	\param name The sound file pathname.
	\param sound The stream to be saved.
    \param format A libsndfile format.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeWriteSound(char* name, AudioStreamPtr sound, long format);
    /*!
    \brief Create an inputstream.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeInputSound();
    /*!
    \brief Create an renderer "wrapper" on a stream, to be used for direct acces to the stream content.
    \return A pointer to new stream object.
    */
    AudioStreamPtr MakeRendererSound(AudioStreamPtr s);
    /*!
    \brief Get the stream length in frames.
    \param sound The stream.
    \return The stream length in frames.
    */
    long GetLengthSound(AudioStreamPtr sound);
    /*!
    \brief Get the stream number of channels.
    \param sound The stream.
    \return The number of channels.
    */
    long GetChannelsSound(AudioStreamPtr sound);
    /*!
    \brief Read a buffer of the stream.
    \param sound The stream.
    \param buffer A buffer to be filled with frames.
    \param buffer_size The buffer length.
    \param channels The number of channels in the buffer.
    \return The number of read frames.
    */
    long ReadSound(AudioStreamPtr sound, float* buffer, long buffer_size, long channels);
    /*!
    \brief Delete a stream.
    \param sound The stream.
    */
    void DeleteSound(AudioStreamPtr sound);

    // Open/Close
    /*!
    \brief Opens the audio player.
    \param inChan The number of input channels. <B>Only stereo players are currently supported </b>
    \param outChan The number of output channels.
    \param channels The number of stream channels.
    \param sample_rate The sampling rate.
    \param buffer_size The audio playerr internal buffer size.
    \param stream_buffer_size The file reader/writer buffer size (used for double buffering).
    \param rtstream_buffer_size The input stream buffer size.
	\param renderer The audio renderer used to access audio I/O : can be kPortAudioRenderer or kJackRenderer.
    \return The new audio player.
    */
    AudioPlayerPtr OpenAudioPlayer(long inChan,
                                   long outChan,
                                   long channels,
                                   long sample_rate,
                                   long buffer_size,
                                   long stream_buffer_size,
                                   long rtstream_buffer_size,
								   long renderer);
    /*!
    \brief Close the audio player.
    \param player The audio player.
    */
    void CloseAudioPlayer(AudioPlayerPtr player);

    /*!
    \brief Load a sound in a channel.
    \param player The audio player.
    \param sound The stream to be inserted in the channel.
    \param chan The audio channel number to be used.
    \param vol The volume between 0 and 127.
    \param pan The panning between 0 and 127.
    \return An error code.
    */
    long LoadChannel(AudioPlayerPtr player, AudioStreamPtr sound, long chan, long vol, long pan);
    /*!
    \brief Retrieve information about a sound channel.
    \param player The audio player.
    \param chan The audio channel number to be used.
    \param info The channel info structure to be filled.
    */
    void GetInfoChannel(AudioPlayerPtr player, long chan, ChannelInfoPtr info);

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
    \brief Start a sound region from the beginning.
    \param player The audio player.
    \param chan The audio channel number to be used.
    */
    void StartSound(AudioPlayerPtr player, long chan);
    /*!
    \brief Play a sound region from the current location.
    \param player The audio player.
    \param chan The audio channel number to be used.
    */
    void ContSound(AudioPlayerPtr player, long chan);
    /*!
    \brief Stop playing a channel.
    \param player The audio player.
    \param chan The audio channel number to be used.
    */
    void StopSound(AudioPlayerPtr player, long chan);

    // Params
    /*!
    \brief Set the channel volume [0...127]
    \param player The audio player.
    \param chan The audio channel number to be used.
    \param vol The new volume value.
    */
    void SetVolSound(AudioPlayerPtr player, long chan, long vol);
    /*!
    \brief Set the channel panning [0...127]
    \param player The audio player.
    \param chan The audio channel number to be used.
    \param pan The new panning value.
    */
    void SetPanSound(AudioPlayerPtr player, long chan, long pan);

    // Master
    /*!
    \brief Set the audio player volume [0...127]
    \param player The audio player.
    \param vol The new volume value.
    */
    void SetVolAudioPlayer(AudioPlayerPtr player, long vol);
    /*!
    \brief Set the audio player panning [0...127]
    \param player The audio player.
    \param pan The new panning value.
    */
    void SetPanAudioPlayer(AudioPlayerPtr player, long pan);

#ifdef __cplusplus
}
#endif

#endif


