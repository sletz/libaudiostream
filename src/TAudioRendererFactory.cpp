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

#include <cstddef>
#include <cstring>
#include "TAudioRendererFactory.h"
#include "TAudioGlobals.h"

#ifdef __JACK__
#include "TJackRenderer.h"
#include "TNetJackRenderer.h"
#endif

#ifdef __PORTAUDIO__
	#ifdef __PORTAUDIOV19__
		#include "TPortAudioV19Renderer.h"
	#else
		#include "TPortAudioRenderer.h"
	#endif
#endif

#ifdef __COREAUDIO__
#include "TCoreAudioRenderer.h"
#endif

#include "TOfflineRenderer.h"

TAudioRendererPtr TAudioRendererFactory::MakeAudioRenderer(int renderer)
{
    TAudioGlobals::ClearLibError();
	try {
		switch (renderer) {
                
            case kOffLineAudioRenderer:
                return new TOfflineRenderer();

			case kPortAudioRenderer:
			#ifdef __PORTAUDIO__
				#ifdef __PORTAUDIOV19__
					return new TPortAudioV19Renderer(); 
				#else
					return new TPortAudioRenderer();
				#endif
			#else
			#warning PortAudio renderer is not compiled
				return NULL;
			#endif

			case kJackRenderer:
			#ifdef __JACK__
				return new TJackRenderer();
			#else
			#ifdef WIN32
				#pragma message ("Jack renderer is not compiled")
			#else
				#warning Jack renderer is not compiled
			#endif
				return NULL;
			#endif
            
            case kNetJackRenderer:
            #ifdef __JACK__
				return new TNetJackRenderer(-1, DEFAULT_MULTICAST_IP, DEFAULT_PORT, DEFAULT_MTU, 5);
			#endif
		
			 case kCoreAudioRenderer:
			#ifdef __COREAUDIO__
				return new TCoreAudioRenderer();
			#else
			#ifdef WIN32
				#pragma message ("CoreAudio renderer is not compiled")
			#else
				#warning CoreAudio renderer is not compiled
			#endif
				return NULL;
			#endif
				
			default:
				return NULL;
		}
	} catch (...) {
        TAudioGlobals::AddLibError("Renderer allocation error");
        return NULL;
	}
}
