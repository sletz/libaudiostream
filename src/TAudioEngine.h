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

#ifndef __TAudioEngine__
#define __TAudioEngine__

#include "TAudioRenderer.h"
#include "TAudioMixer.h"

//--------------------
// Class TAudioEngine
//--------------------
/*!
\brief An Audio Engine with a mixer that handles several separated channels.
*/

class TAudioEngine
{

    protected:

        TAudioMixerPtr	fMixer;  	// Writer of Audio output
        TAudioRendererPtr fRenderer; 	// Renderer

    public:

        TAudioEngine(TAudioRendererPtr renderer)
        {
            fMixer = new TAudioMixer();
            fRenderer = renderer;
            fRenderer->AddClient(fMixer);
        }

        virtual ~TAudioEngine()
        {
            delete fMixer;
        }

        virtual long Open(long* inChan, long* outChan, long* bufferSize, long* sampleRate);
        virtual long Close();

        virtual long Start()
        {
            return fRenderer->Start();
        }
        virtual long Stop()
        {
            return fRenderer->Stop();
        }

        long LoadChannel(TAudioStreamPtr stream, int chan, int vol, int pan)
        {
            return fMixer->Load(stream, chan, vol, pan);
        }
        void GetInfoChannel(long chan, ChannelInfo* info)
        {
            fMixer->GetInfo(chan, info);
        }

        void StartSound(long chan)
        {
            fMixer->Start(chan);
        }
        void PlaySound(long chan)
        {
            fMixer->Play(chan);
        }
        void StopSound(long chan)
        {
            fMixer->Stop(chan);
        }

        void SetVolSound(long chan, long vol)
        {
            fMixer->SetVol(chan, vol);
        }
        void SetPanSound(long chan, long pan)
        {
            fMixer->SetPan(chan, pan);
        }

        void SetMasterVol(long vol)
        {
            fMixer->SetMasterVol(vol);
        }
        void SetMasterPan(long pan)
        {
            fMixer->SetMasterPan(pan);
        }
};

typedef TAudioEngine * TAudioEnginePtr;

#endif



