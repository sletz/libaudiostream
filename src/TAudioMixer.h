/*

Copyright (C) Grame 2002-2012

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
        FLOAT_BUFFER	fMixBuffer;			// Buffer for mixing
		float fVol;
		float fPanLeft;		// Pan for left signal
		float fPanRight;	// Pan for right signal
		float fLLVol;		// Volume for left output for left channel
		float fLRVol;		// Volume for right output for left channel
		float fRLVol;		// Volume for left output for right channel
		float fRRVol;		// Volume for right output for right channel
  
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

        long Load(TAudioStreamPtr stream, long channel, float vol, float panLeft, float panRight);
        void GetInfo(long chan, ChannelInfo* info);

        void Start(long chan);
        void Play(long chan);
        void Stop(long chan);
		void Abort(long chan);
        void Reset();

        void SetVol(long chan, float vol);
        void SetPan(long chan, float panLeft, float panRight);

        void SetVol(float vol)
        {
            fVol = vol;
			TPanTable::GetLR(fVol, fPanLeft, &fLLVol, &fLRVol);
			TPanTable::GetLR(fVol, fPanRight, &fRLVol, &fRRVol);
        }
        void SetPan(float panLeft, float panRight)
        {
            fPanLeft = panLeft;
			fPanRight = panRight;
			TPanTable::GetLR(fVol, fPanLeft, &fLLVol, &fLRVol);
			TPanTable::GetLR(fVol, fPanRight, &fRLVol, &fRRVol);
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




