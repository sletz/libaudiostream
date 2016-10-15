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

#ifndef __LibAudioStreamMCPlusPlus__
#define __LibAudioStreamMCPlusPlus__

#include <stdint.h>
#include "la_smartpointer.h"
#include <list>
#include <vector>

/*!
 \addtogroup interface LibAudioStreamMC programming interface

 @{
 */

#define NO_ERR                   0
#define OPEN_ERR                -1
#define CLOSE_ERR               -2
#define LOAD_ERR                -3
#define FILE_NOT_FOUND_ERR      -4
#define EFFECT_NOT_FOUND_ERR    -5
#define PLAYER_ERR              -6
#define SET_POS_ERR             -7

#if defined(__cplusplus) && !defined(_MSC_VER)
extern "C"
{
#endif

    typedef int64_t audio_frame_t;
    typedef int64_t audio_usec_t;

    enum {kPortAudioRenderer = 0, kJackRenderer, kCoreAudioRenderer, kOffLineAudioRenderer, kNetJackRenderer};

    /*!
     \brief Audio device info.
     */
    typedef struct DeviceInfo* DeviceInfoPtr;

    typedef struct DeviceInfo {
        char fName[64];
        long fMaxInputChannels;
        long fMaxOutputChannels;
        long fDefaultBufferSize;
        double fDefaultSampleRate;
    } DeviceInfo;

    /*!
     \brief Renderer state.
     */
    typedef struct RendererInfo* RendererInfoPtr;

    typedef struct RendererInfo {
        long fInput;                // Number of input channels
        long fOutput;               // Number of output channels
        long fSampleRate;           // Sampling Rate
        long fBufferSize;           // I/O Buffer size
        uint64_t fCurFrame;         // Currrent date in frames
        uint64_t fCurUsec;          // Current date in microsecond
        long fOutputLatencyFrame;   // Output latency in frames
        long fOutputLatencyUsec;    // Output latency in microsecond
        long fInputLatencyFrame;    // Input latency in frames
        long fInputLatencyUsec;     // Input latency in microsecond
    } RendererInfo;


    typedef struct ErrorInfo {
        char fStreamError[512];
        long fDiskError;            // Counter of disk streaming errors
        long fSchedulingError;      // Counter of too late scheduled stream commmands
    } ErrorInfo;

    class TAudioStream : public la_smartable {

    public:

        virtual ~TAudioStream() {}

    };

    typedef LA_SMARTP<TAudioStream> TAudioStreamPtr;

    class TAudioEffectInterface : public la_smartable {

    private:

        bool fState;	// Running state

    public:

        TAudioEffectInterface(): fState(true)
        {}
        virtual ~TAudioEffectInterface()
        {}

        // Internal methods

        void SetState(bool state)
        {
            fState = state;
        }
        bool GetState()
        {
            return fState;
        }

        void ProcessAux(float** input, float** output, long framesNum, long channels)
        {
            if (fState) {
                Process(input, output, framesNum, channels);
            }
        }

        // Pure virtual : to be implemented by sub-classes

        virtual void Process(float** input, float** output, long framesNum, long channels) = 0;
        virtual TAudioEffectInterface* Copy() = 0;
        virtual void Reset() = 0;
        virtual long Channels() = 0;

        virtual long GetControlCount() = 0;
        virtual void GetControlParam(long param, char* label, float* min, float* max, float* init) = 0;
        virtual void SetControlValue(long param, float f) = 0;
        virtual float GetControlValue(long param) = 0;

    };

    typedef LA_SMARTP<TAudioEffectInterface> TAudioEffectInterfacePtr;

    class TAudioClient {

    public:

        TAudioClient()
        {}
        virtual ~TAudioClient()
        {}

        /*!
         \brief Audio callback called by the AudioManager.
         \param inputBuffer The input buffer as a array of interleaved float samples (stereo here).
         \param outputBuffer The output buffer as a array of interleaved float samples (stereo here).
         \param frames The input/output buffer number of frames.
         \return true if success, false otherwise.
         */
        virtual bool AudioCallback(float** inputs, float** outputs, long frames) = 0;
    };

    typedef TAudioClient* TAudioClientPtr;

    class TSymbolicDate : public la_smartable {};

    typedef LA_SMARTP<TSymbolicDate> TSymbolicDatePtr;

    // Opaque pointers
    typedef void* AudioPlayerPtr;
    typedef void* AudioRendererPtr;
    typedef void* AudioClientPtr;

    typedef TAudioStreamPtr AudioStream;
    typedef TAudioEffectInterfacePtr AudioEffect;
    typedef TSymbolicDatePtr SymbolicDate;

    typedef void (*StopCallback)(void* context);

    /*!
     \brief Gives the library version number.
     \return the library version number as a 3 digits long value.
     */
    long LibVersion();

    /*!
     \brief Return a string describing the last error.
     \return the error.
     */
    const char* GetLastLibError();

     /*!
     \brief Fill the error data structure.
     \param clear True if the error internal state should be cleared.
     */
    void GetErrors(ErrorInfo* error, bool clear);

    /**
     * @defgroup SoundFunctions Sound creation and manipulation functions
     * @{
     */

    /*!
     \brief Create a stream that will produce "silence".
     \param length The number of null frames to be produced.
     \return A pointer to new stream object (which has one channel).
     */
    AudioStream MakeNullSound(long length);

    /*!
     \brief Create a stream that will produce "silence".
     \param channels The number of channels
     \param length The number of null frames to be produced.
     \return A pointer to new stream object.
     */
    AudioStream MakeMultiNullSound(long channels, long length);

    /*!
     \brief Create a stream that will produce a contants value.
     \param channels The number of channels
     \param length The number of frames to be produced.
     \return A pointer to new stream object.
     */
    AudioStream MakeConstantSound(long channels, long length, float value);

    /*!
     \brief Create a stream from a given external buffer. Memory has to be managed by the external code (allocation/deallocation).
     \param buffer The sound buffer a array of non interleaved audio buffers.
     \param length The sound buffer length.
     \param length The sound buffer channels number.
     \param clear A boolean to indicate if internal buffer should be cleared just after Read
     \return A pointer to new stream object.
     */
    AudioStream MakeBufferSound(float** buffer, long length, long channels, bool clear);

    /*!
     \brief Create a file reader stream.
     \param name The sound file pathname.
     \return A pointer to new stream object or NULL if the file cannot be opened.
     */
    AudioStream MakeReadSound(const char* name);

    /*!
     \brief Create a file region reader stream.
     \param name The sound file pathname.
     \param beginFrame The start frame of the region.
     \param endFrame The end frame of the region.
     \return A pointer to new stream object or NULL if the wanted region is not part of the file.
     */
    AudioStream MakeRegionSound(const char* name, long beginFrame, long endFrame);

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
     \brief Put two streams in parallel
     \param s1 The first stream.
     \param s2 The second stream.
     \return A pointer to new stream object.
     */
    AudioStream MakeParSound(AudioStream s1, AudioStream s2);

    /*!
     \brief Creates a selection of channel of a streams
     \param s1 The stream.
     \param selection An array of channels to keep.
     \param channels The number of channels in the selection array.
     \return A pointer to new stream object.
     */
    AudioStream MakeSelectSound(AudioStream s1, long* selection, long channels);

    /*!
     \brief Apply an effect on a stream.
     \param sound The stream to be transformed.
     \param effect The effect to be used.
     \param fadeIn A fadein section frames before the effect is fully applied.
     \param fadeOut A fadeout section frames before the effect is fully removed.
     \return A pointer to new stream object.
     */
    AudioStream MakeEffectSound(AudioStream sound, AudioEffect effect, long fadeIn, long fadeOut);

    /*!
     \brief To pitchshift or timestretch a stream.
     \param sound The stream to be transformed.
     \param pitch_shift The address of a double value to be used as pitch_shift.
     \param time_strech The address of a double value to be used as time_strech.
     \return A pointer to new stream object.
     */
    AudioStream MakePitchSchiftTimeStretchSound(AudioStream sound, double* pitch_shift, double* time_strech);

    /*!
     \brief Create a stream writer.
     \param name The sound file pathname.
     \param sound The stream to be saved.
     \param format A libsndfile format.
     \return A pointer to new stream object or NULL if the file cannot be opened.
     */
    AudioStream MakeWriteSound(const char* name, AudioStream sound, long format);

    /*!
     \brief Create an input stream that starts 'recording' at the time position is is placed in the score.
     \return A pointer to new stream object.
     */
    AudioStream MakeInputSound();

    /*!
     \brief Create a shared stream on the input stream, that starts 'recording' as soon as the Player is started.
     \return A pointer to new stream object.
     */
    AudioStream MakeSharedInputSound();

    /*!
     \brief Create a renderer "wrapper" on a stream, to be used for direct access to the stream content.
     \return A pointer to new stream object.
     */
    AudioStream MakeRendererSound(AudioStream sound);

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
     \return The number of read frames.
     */
    long ReadSound(AudioStream sound, float** buffer, long buffer_size);

    /*!
     \brief Read a buffer of the stream.
     \param sound The stream.
     \param buffer A buffer to be filled with frames.
     \param buffer_size The buffer length.
     \param frames The number of frames to render.
     \param pos The position in buffer.
     \return The number of read frames.
     */
    long ReadSoundPos(AudioStream sound, float** buffer, long buffer_size, long frames, long pos);

    /*!
     \brief Write a buffer to the stream. This fonction can only be used with streams built with MakeBufferSound.
     \param sound The stream.
     \param buffer A buffer with frames to be written.
     \param buffer_size The buffer length.
     \return The number of written frames.
     */
    long WriteSound(AudioStream sound, float** buffer, long buffer_size);

    /*!
     \brief Reset a stream.
     \param sound The stream to be reseted.
     */
    void ResetSound(AudioStream sound);

    /*!
     \brief Change the current position in the stream.
     \param sound The stream to be used.
     \param frames The new position.
     \return An error code (SET_POS_ERR if outside of the stream).
     */
    long SetPosSound(AudioStream sound, long frames);

    /*!
     \brief Create a copy of a stream.
     \param sound The stream to be copied.
     \return The copied stream.
     */
    AudioStream MakeCopySound(AudioStream sound);

    /*@}*/

    /**
     * @defgroup EffectsFunctions Effects creation and manipulation functions
     * @{
     */

    /* Effect management */
    /*!
     \brief Create an effect described in the Faust DSP language.
     \param code The code of the Faust effect, as a DSP filename or DSP string.
     \param library_path The pathname where to locate additional DSP libraries.
     \param draw_path The pathname where to save additional resources produced during compilation (like SVG files).
     \return A pointer to new effect object or NULL if the effect cannot be located or created.
     */
    AudioEffect MakeFaustAudioEffect(const char* code, const char* library_path, const char* draw_path);

    /* Effect management */
    /*!
     \brief Create an effect described in the Faust DSP language to be computed on a remote machine.
     \param code The code of the Faust effect, as a DSP filename or DSP string.
     \param library_path The pathname where to locate additional DSP libraries.
     \param draw_path The pathname where to save additional resources produced during compilation (like SVG files).
     \return A pointer to new effect object or NULL if the effect cannot be located or created.
     */
    AudioEffect MakeRemoteFaustAudioEffect(const char* code, const char* library_path, const char* draw_path);

    /*!
     \brief Return the number of effect controls.
     \param effect The effect pointer.
     \return The number of effect controls.
     */
    long GetControlCountEffect(AudioEffect effect);

    /*!
     \brief Return a description on the effect control: control name, min, max and default values
     \param effect The effect pointer.
     */
    void GetControlParamEffect(AudioEffect effect, long control, char* label, float* min, float* max, float* init);

    /*!
     \brief Set the effect control value.
     \param effect The effect pointer.
     \param control The control number between 0 and GetControlCountPtr.
     \param value The new value as a float.
     */
    void SetControlValueEffect(AudioEffect effect, long control, float value);

    /*!
     \brief Get the effect control current value.
     \param effect The effect pointer.
     \param control The control number between 0 and GetControlCountPtr.
     \return The effect control current value.
     */
    float GetControlValueEffect(AudioEffect effect, long control);

    /*!
     \brief Set the effect running state.
     \param effect The effect to be used.
     \param state The running state.
     */
    void SetStateEffect(AudioEffect effect, long state);

    /*!
     \brief Get the effect running state.
     \param effect The effect to be used.
     \return state The running state.
     */
    long GetStateEffect(AudioEffect effect);

    /*!
     \brief Reset the effect to intial state.
     \param effect The effect to be resetted.
     */
    void ResetEffect(AudioEffect effect);

    /*!
     \brief Process an audio buffer with the effect.
     \param effect The effect to be used.
     \param input The input audio buffer.
     \param output The output audio buffer.
     \param framesNum The number of frame of input/output buffers.
     */
    void ProcessEffect(AudioEffect effect, float** input, float** output, long framesNum);

    /*!
     \brief Get the JSON description of the effect.
     \param effect The effect to be used.
     \return The JSON decription as a string.
     */
    const char* GetJsonEffect(AudioEffect effect);

    /*!
     \brief Get the effect name.
     \param effect The effect to be used.
     \return The effect name as a string.
     */
    const char* GetNameEffect(AudioEffect effect);

    /*!
     \brief Copy the effect.
     \param effect The effect to be copied.
     \return The copied effect.
     */
    AudioEffect MakeCopyEffect(AudioEffect effect);

    /*@}*/

    /**
     * @defgroup PlayerFunctions Player creation and manipulation functions
     * @{
     */

    /*!
     \brief Set the effect control value at a specific date in frames.
     \param player The audio player.
     \param effect The effect to be used.
     \param path The effect label path as a string.
     \param value The effect control value.
     \param date The symbolic date.
     \return An error code.
     */
    long SetTimedControlValueEffect(AudioPlayerPtr player, const char* effect, const char* path, float value, SymbolicDate date);

    // Open/Close
    /*!
     \brief Set global input/output audio latencies. This calls has to be done <B>before </B> OpenAudioPlayer
     and will take effect only when PortAudio is used.
     \param inputLatency The wanted input latency in millisecond.
     \param outputLatency The wanted output latency in millisecond.
     */
    void SetAudioLatencies(long inputLatency, long outputLatency);

    /*!
     * \brief CheckRendererAvailability
     * \param renderer a renderer (kPortAudioRenderer, kJackRenderer, ...)
     * \return True if the given renderer can be used.
     */
    bool CheckRendererAvailability(long renderer);

    /*!
     \brief Open the audio player.
     \param inChan The number of input channels.
     \param outChan The number of output channels.
     \param sample_rate The sampling rate.
     \param buffer_size The audio player internal buffer size.
     \param stream_buffer_size The file reader/writer buffer size (used for double buffering).
     \param rtstream_duration The input stream duration in frames. If 0, then no input stream will be allocated. Note that when an input stream is defined, only *one* player can be used.
     \param renderer The audio renderer used to access audio I/O : can be kPortAudioRenderer, kJackRenderer or kOffLineAudioRenderer.
     \param thread_num The number of additionnal low-priority threads used to precompute data : must be a least one.
     \return A pointer to new audio player object.
     */
    AudioPlayerPtr OpenAudioPlayer(long inChan,
                                   long outChan,
                                   long sample_rate,
                                   long buffer_size,
                                   long stream_buffer_size,
                                   long rtstream_duration,
                                   long renderer,
                                   long thread_num);
    /*!
     \brief Open the audio client, to be added to an externally allocated audio manager.
     */
    AudioPlayerPtr OpenAudioClient(AudioRendererPtr manager);

    long SetMasterEffect(AudioPlayerPtr player, AudioEffect effect);

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

    audio_usec_t GetAudioPlayerDateInUsec(AudioPlayerPtr player);

    audio_frame_t GetAudioPlayerDateInFrame(AudioPlayerPtr player);

    /*!
     \brief Start a sound at a specific date.
     \param player The audio player.
     \param sound The stream to be started.
     \param date The date which can be a symbolic one (see <B> GenSymbolicDate </B> to be set later on) or a real one (see <B> GenRealDate </B>).
     \return An error code.
     */
    long StartSound(AudioPlayerPtr player, AudioStream sound, SymbolicDate date);

    /*!
     \brief Stop a sound at a specific date.
     \param player The audio player.
     \param sound The stream to be stopped.
     \param date The date which can be a symbolic one (see <B> GenSymbolicDate </B> to be set later on) or a real one (see <B> GenRealDate </B>).
     \return An error code.
     */
    long StopSound(AudioPlayerPtr player, AudioStream sound, SymbolicDate date);

    /*!
     \brief Generate a new symbolic date, to be set later on with <B> SetSymbolicDate </B>.
     \param player The audio player.
     \return The new symbolic date.
     */
    SymbolicDate GenSymbolicDate(AudioPlayerPtr player);

    /*!
     \brief Generate a new real date.
     \param player The audio player.
     \param sound The real date in frames.
     \return The new real date.
     */
    SymbolicDate GenRealDate(AudioPlayerPtr player, audio_frame_t date);

    /*!
     \brief Set the symbolic date with a real date.
     \param player The audio player.
     \param date The symbolic date.
     \param date The real date in frames.
     \return An error code.
     */
    long SetSymbolicDate(AudioPlayerPtr player, SymbolicDate symb_date, audio_frame_t real_date);

    /*!
     \brief Get the symbolic date internal date.
     \param player The audio player.
     \return The symbolic date internal date in frames.
     */
    audio_frame_t GetSymbolicDate(AudioPlayerPtr player, SymbolicDate symb_date);

    // Transport
    /*!
     \brief Start the audio player.
     \param player The audio player.
     \return An error code.
     */
    long StartAudioPlayer(AudioPlayerPtr player);

    /*!
     \brief Stop the audio player.
     \param player The audio player.
     \return An error code.
     */
    long StopAudioPlayer(AudioPlayerPtr player);

     /*!
     \brief Pause the audio player.
     \param player The audio player.
     \return An error code.
     */
    long PauseAudioPlayer(AudioPlayerPtr player);

     /*!
     \brief Continue the audio player from the current position.
     \param player The audio player.
     \return An error code.
     */
    long ContAudioPlayer(AudioPlayerPtr player);

    /*!
     \brief Set the current position the audio player.
     \param player The audio player.
     \param new_date The new date in frames.
     \return An error code.
     */
    long SetPosAudioPlayer(AudioPlayerPtr player, audio_frame_t new_date);

    /*!
     \brief Clear the audio player internal effect table.
     \param player The audio player.
     \return An error code.
     */
    long ClearAudioPlayer(AudioPlayerPtr player);

    /*!
     \brief Get the audio player internal renderer.
     \param player The audio player.
     \return The internal audio renderer.
     */
    AudioRendererPtr GetAudioPlayerRenderer(AudioPlayerPtr player);

    // Devices scanning
    /*!
     \brief Scan and return the number of available devices on the machine.
     \param renderer The audio renderer type used to access audio I/O, built using MakeAudioRenderer.
     \return The number of available devices.
     */
    long GetDeviceCount(long renderer);

    /*!
     \brief Fill DeviceInfo structure for a given device.
     \param renderer The audio renderer type used to access audio I/O, built using MakeAudioRenderer.
     \param deviceNum The device index between 0 and GetDeviceCount.
     \param info The device info structure to be filled.
     */
    void GetDeviceInfo(long renderer, long deviceNum, DeviceInfo* info);

    /*!
     \brief Get the default input device index.
     \param renderer The audio renderer type used to access audio I/O, built using MakeAudioRenderer.
     \return The default input device index.
     */
    long GetDefaultInputDevice(long renderer);

    /*!
     \brief Get the default output device index.
     \param renderer The audio renderer type used to access audio I/O, built using MakeAudioRenderer.
     \return The default output device index.
     */
    long GetDefaultOutputDevice(long renderer);

    // Renderer
    /*!
     \brief Create a new audio renderer.
     \param renderer The audio renderer used to access audio I/O : can be kPortAudioRenderer, kJackRenderer or kOffLineAudioRenderer.
     \return A pointer to new audio renderer object.
     */
    AudioRendererPtr MakeAudioRenderer(long renderer);

    /*!
     \brief Delete an audio renderer.
     \param renderer The renderer to be deleted.
     */
    void DeleteAudioRenderer(AudioRendererPtr renderer);

    /*!
     \brief Open the audio renderer.
     \param renderer The audio renderer used.
     \param inputDevice The audio input device index.
     \param outputDevice The audio output device index.
     \param inChan The number of input channels.
     \param outChan The number of output channels.
     \param bufferSize The audio player internal buffer size.
     \param sampleRate The sampling rate.
     \return An error code.
     */
    long OpenAudioRenderer(AudioRendererPtr renderer, long inputDevice, long outputDevice, long inChan, long outChan, long bufferSize, long sampleRate);

    /*!
     \brief Close an audio renderer.
     \param renderer The audio renderer to be closed.
     */
    void CloseAudioRenderer(AudioRendererPtr renderer);

    /*!
     \brief Start an audio renderer.
     \param renderer The audio renderer to be started.
     */
    void StartAudioRenderer(AudioRendererPtr renderer);

    /*!
     \brief Stop an audio renderer.
     \param renderer The audio renderer to be stopped.
     */
    void StopAudioRenderer(AudioRendererPtr renderer);

    /*!
     \brief Get audio renderer infos.
     \param renderer The audio renderer.
     \param info The audio renderer info to be filled.
     */
    void GetAudioRendererInfo(AudioRendererPtr renderer, RendererInfoPtr info);

    /*!
     \brief Add an audio client to the renderer internal client list.
     \param renderer The audio renderer to be used.
     \param client The audio client to be added.
     */
    void AddAudioClient(AudioRendererPtr renderer, AudioClientPtr client);

    /*!
     \brief Remove an audio client from the renderer internal client list.
     \param renderer The audio renderer to be used.
     \param client The audio client to be removed.
     */
    void RemoveAudioClient(AudioRendererPtr renderer, AudioClientPtr client);

    /*!
     \brief Init the global audio context. There is <B> unique </B> to be accessed by all components that need it.
     \param inChan The number of input channels. <B>Only stereo players are currently supported </b>
     \param outChan The number of output channels.
     \param sample_rate The sampling rate.
     \param buffer_size The audio player internal buffer size.
     \param stream_buffer_size The file reader/writer buffer size (used for double buffering).
     \param rtstream_duration The input stream duration in frames.
     \param thread_num The number of additionnal low-priority threads used to precompute data : must be a least one.
     */
    void AudioGlobalsInit(long inChan,
                          long outChan,
                          long sample_rate,
                          long buffer_size,
                          long stream_buffer_size,
                          long rtstream_duration,
                          long thread_num);
    /*!
     \brief Destroy the global audio context.
     */
    void AudioGlobalsDestroy();

    /*@}*/

    /*! @} */

#if defined(__cplusplus)  && !defined(_MSC_VER)
}
#endif

#endif


