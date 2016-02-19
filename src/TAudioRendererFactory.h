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

#ifndef __TAudioRendererFactory__
#define __TAudioRendererFactory__

#include "AudioExports.h"
#include "TAudioRenderer.h"

enum {kPortAudioRenderer = 0, kJackRenderer, kCoreAudioRenderer, kOffLineAudioRenderer, kNetJackRenderer};

/*!
\brief A factory for audio renderers.
*/

class AUDIO_EXPORTS TAudioRendererFactory
{

	public:

        static TAudioRendererPtr MakeAudioRenderer(int renderer);
};

#endif



