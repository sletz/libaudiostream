
#ifndef __AudioExports__
#define __AudioExports__

#ifdef WIN32

#pragma warning (disable : 4267)
#pragma warning (disable : 4275)
#pragma warning (disable : 4251)
#pragma warning (disable : 4786)

#ifdef AUDIOENGINE_EXPORTS
#define AUDIO_EXPORTS __declspec(dllexport)

#elif LIBAUDIOSTREAM_EXPORTS
#define AUDIO_EXPORTS __declspec(dllexport)

#elif defined(STATIC)
#define AUDIO_EXPORTS

#else
#define AUDIO_EXPORTS __declspec(dllimport)
#endif

#else

#define AUDIO_EXPORTS

#endif

#endif

