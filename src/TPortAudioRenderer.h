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

#ifndef __TPortAudioRenderer__
#define __TPortAudioRenderer__

#include "TAudioRenderer.h"
#include "portaudiov18.h"

//--------------------------
// Class TPortAudioRenderer
//--------------------------
/*!
\brief Use the <A HREF=http://www.portaudio.com> PortAudio API </A>  to access sound drivers.
*/

class TPortAudioRenderer : public TAudioRenderer
{

    private:

        PortAudioStream * fStream;

        static int Process(void *inputBuffer,
                           void *outputBuffer,
                           unsigned long framesPerBuffer,
                           PaTimestamp outTime,
                           void *userData);

        void DisplayDevices();
        int GetFirstValidInputDevice();
        int GetFirstValidOutputDevice();

    public:

        TPortAudioRenderer(): TAudioRenderer()
        {}
        virtual ~TPortAudioRenderer()
        {}

        long Open(long* inChan, long* outChan, long* bufferSize, long* sampleRate);
        long Close();

        long Start();
        long Stop();

        void GetInfo(RendererInfoPtr info);
};

typedef TPortAudioRenderer * TPortAudioRendererPtr;

#endif



