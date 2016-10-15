/*

Copyright (C) Grame 2006-2007

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

#ifndef __AudioExports__
#define __AudioExports__

#ifdef WIN32

#pragma warning (disable : 4267)
#pragma warning (disable : 4275)
#pragma warning (disable : 4251)
#pragma warning (disable : 4786)

#if defined(AUDIOENGINE_EXPORTS)
#define AUDIO_EXPORTS __declspec(dllexport)

#elif defined(LIBAUDIOSTREAM_EXPORTS)
#define AUDIO_EXPORTS __declspec(dllexport)

#elif defined(STATIC)
#define AUDIO_EXPORTS

#else
#define AUDIO_EXPORTS __declspec(dllimport)
#endif

#elif __APPLE__
# if defined(LIBAUDIOSTREAM_EXPORTS)
#   define AUDIO_EXPORTS __attribute__ ((visibility ("default")))
# else
#  define AUDIO_EXPORTS
# endif
#else
#define AUDIO_EXPORTS
#endif

#endif
