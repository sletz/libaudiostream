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

#ifndef __TAudioMixer__
#define __TAudioMixer__

#include "TAudioClient.h"
#include "TAudioChannel.h"
#include "TAudioGlobals.h"

#include <list>

//-------------------
// Class TAudioMixer
//-------------------
/*!
\brief A mixer contains several TAudiochannels objects.
*/

class TAudioMixer : public TAudioClient
{

    private:

		TAudioEffectListManager	fEffectList;		// Master effect list
		list<TAudioChannelPtr>	fSoundChannelSeq;	// List of running sound channels
        TAudioChannelPtr*		fSoundChannelTable;	// Table of sound channels
        TAudioBuffer<float>*	fMixBuffer;			// Buffer for mixing
        float fVol, fPan;							// Master pan and volume
		float fLeftVol, fRightVol;

        bool IsAvailable(long chan)
        {
            return ((chan >= 0) && (chan < TAudioGlobals::fChannels) && !fSoundChannelTable[chan]->GetState());
        }
        bool IsValid(long chan)
        {
            return ((chan >= 0) && (chan < TAudioGlobals::fChannels));
        }

        bool AudioCallback(float* inputBuffer, float* outputBuffer, long frames);

    public:

        TAudioMixer ();
        virtual ~TAudioMixer();

        long Load(TAudioStreamPtr stream, long channel, float vol, float pan);
        void GetInfo(long chan, ChannelInfo* info);

        void Start(long chan);
        void Play(long chan);
        void Stop(long chan);
        void Reset();

        void SetVol(long chan, float vol);
        void SetPan(long chan, float pan);

        void SetVol(float vol)
        {
            fVol = vol;
			TPanTable::GetLR(fVol, fPan, &fLeftVol, &fRightVol);
        }
        void SetPan(float pan)
        {
            fPan = pan;
			TPanTable::GetLR(fVol, fPan, &fLeftVol, &fRightVol);
        }
		
		void SetStopCallback(long chan, StopCallback callback, void* context);
		StopCallback GetStopCallback(long chan);
		
		void SetEffectList(TAudioEffectListPtr effect_list, long fadeIn, long fadeOut)
		{
			fEffectList.SetEffectList(effect_list, fadeIn, fadeOut);
        }

        TAudioEffectListPtr GetEffectList()
		{
			return  fEffectList.GetEffectList();
        }
		
		void SetEffectList(long chan, TAudioEffectListPtr effect_list, long fadeIn, long fadeOut);
		TAudioEffectListPtr GetEffectList(long chan);
};

typedef TAudioMixer * TAudioMixerPtr;

#endif




