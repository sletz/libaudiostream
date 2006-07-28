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
			fRenderer->RemoveClient(fMixer);
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

        long LoadChannel(TAudioStreamPtr stream, long chan, float vol, float panLeft, float panRight)
        {
            return fMixer->Load(stream, chan, vol, panLeft, panRight);
        }
        void GetInfoChannel(long chan, ChannelInfo* info)
        {
            fMixer->GetInfo(chan, info);
        }

        void StartChannel(long chan)
        {
            fMixer->Start(chan);
        }
        void PlayChannel(long chan)
        {
            fMixer->Play(chan);
        }
        void StopChannel(long chan)
        {
            fMixer->Stop(chan);
        }

		 void AbortChannel(long chan)
        {
            fMixer->Abort(chan);
        }

        void SetVolChannel(long chan, float vol)
        {
            fMixer->SetVol(chan, vol);
        }
        void SetPanChannel(long chan, float panLeft, float panRight)
        {
            fMixer->SetPan(chan, panLeft, panRight);
        }

        void SetMasterVol(float vol)
        {
            fMixer->SetVol(vol);
        }
        void SetMasterPan(float panLeft, float panRight)
        {
            fMixer->SetPan(panLeft, panRight);
        }
		
		void SetStopCallbackChannel(long chan, StopCallback callback, void* context)
		{
            fMixer->SetStopCallback(chan, callback, context);
        }
		StopCallback GetStopCallbackChannel(long chan)
		{
			return fMixer->GetStopCallback(chan);
		}
		
		void SetEffectListChannel(long chan, TAudioEffectListPtr effect_list, long fadeIn, long fadeOut)
		{
            fMixer->SetEffectList(chan, effect_list, fadeIn, fadeOut);
        }
		TAudioEffectListPtr GetEffectListChannel(long chan)
		{
			return fMixer->GetEffectList(chan);
		}
		
		void SetEffectListMaster(TAudioEffectListPtr effect_list, long fadeIn, long fadeOut)
		{
            fMixer->SetEffectList(effect_list, fadeIn, fadeOut);
        }
		TAudioEffectListPtr GetEffectListMaster()
		{
			return fMixer->GetEffectList();
		}
};

typedef TAudioEngine * TAudioEnginePtr;

#endif



