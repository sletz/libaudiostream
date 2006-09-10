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

#ifdef WIN32 
#pragma warning (disable : 4786)
#endif

#include "TAudioMixer.h"
#include "UAudioTools.h"
#include "TPanTable.h"
#include "TSharedBuffers.h"

/*--------------------------------------------------------------------------*/
// Internal API
/*--------------------------------------------------------------------------*/

TAudioMixer::TAudioMixer ()
{
    // Initialisation
    SetVol(DEFAULT_VOL);
    SetPan(DEFAULT_PAN_LEFT, DEFAULT_PAN_RIGHT);

    fMixBuffer = new TLocalAudioBuffer<float>(TAudioGlobals::fBuffer_Size, TAudioGlobals::fOutput);
    fSoundChannelTable = new TAudioChannelPtr[TAudioGlobals::fChannels];

    for (int j = 0; j < TAudioGlobals::fChannels; j++) {
        fSoundChannelTable[j] = new TAudioChannel();
        assert(fSoundChannelTable[j]);
    }
}

TAudioMixer::~TAudioMixer()
{
    for (int j = 0; j < TAudioGlobals::fChannels; j++)
        delete fSoundChannelTable[j];
    delete[] fSoundChannelTable;
    delete fMixBuffer;
}

bool TAudioMixer::AudioCallback(float* inputBuffer, float* outputBuffer, long frames)
{
    // Init buffer
    UAudioTools::ZeroFloatBlk(fMixBuffer->GetFrame(0), TAudioGlobals::fBuffer_Size, TAudioGlobals::fOutput);
	
    // Mix all SoundChannels
	list<TAudioChannelPtr>::iterator iter = fSoundChannelSeq.begin();
	while (iter != fSoundChannelSeq.end()) {
		TAudioChannelPtr channel = *iter;
		if (!channel->Mix(fMixBuffer, TAudioGlobals::fBuffer_Size, TAudioGlobals::fOutput)) { // End of channel
            channel->SetState(false); // Important : used to mark the insertion state
            iter = fSoundChannelSeq.erase(iter);
		} else {
			iter++;
		}
	}
	
	// Master Effects
	fEffectList.Process(fMixBuffer->GetFrame(0), TAudioGlobals::fBuffer_Size, TAudioGlobals::fOutput);
	
    // Master Pan and Vol
	UAudioTools::MixFrameToFrameBlk(outputBuffer,
									fMixBuffer->GetFrame(0),
									TAudioGlobals::fBuffer_Size,
									TAudioGlobals::fOutput, fLLVol, fLRVol, fRLVol, fRRVol);
    return true;
}

/*--------------------------------------------------------------------------*/
// Internal API
/*--------------------------------------------------------------------------*/

long TAudioMixer::Load(TAudioStreamPtr stream, long chan, float vol, float panLeft, float panRight)
{
    if (IsAvailable(chan)) {
        TAudioChannelPtr channel = fSoundChannelTable[chan];
        assert(channel);
        assert(stream);
        channel->SetStream(stream); // Owner of the old stream pointer has to do the desallocation
        channel->SetVol(vol);
        channel->SetPan(panLeft, panRight);
        return NO_ERR;
    } else {
        printf("Allocate : Channel already inserted  %ld\n", chan);
        return LOAD_ERR;
    }
}

void TAudioMixer::Start(long chan)
{
    TAudioChannelPtr channel;

    if (IsAvailable(chan) && (channel = fSoundChannelTable[chan])) {
        channel->Reset();
        channel->SoundOn();
        channel->SetState(true);
		// This is supposed to be unsafe since fSoundChannelSeq list is also read in AudioCallback thread... but no crash even occured
        fSoundChannelSeq.push_front(channel);
    } else {
        printf("Start : Channel already playing : %ld\n", chan);
    }
}

void TAudioMixer::Play(long chan)
{
    TAudioChannelPtr channel;

    if (IsAvailable(chan) && (channel = fSoundChannelTable[chan])) {
        channel->SoundOn();
        channel->SetState(true);
		// This is supposed to be unsafe since fSoundChannelSeq list is also read in AudioCallback thread... but no crash even occured
        fSoundChannelSeq.push_front(channel);
    } else {
        printf("Start : Channel already playing : %ld\n", chan);
    }
}

// The channel will be removed from the active channel list when the stream is finished (after FadeOut)

void TAudioMixer::Stop(long chan)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SoundOff(true);
}

void TAudioMixer::Abort(long chan)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SoundOff(false);
}

void TAudioMixer::SetVol(long chan, float vol)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SetVol(vol);
}

void TAudioMixer::SetPan(long chan, float panLeft, float panRight)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SetPan(panLeft, panRight);
}

void TAudioMixer::SetStopCallback(long chan, StopCallback callback, void* context)
{
	TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SetStopCallback(callback, context);
}

StopCallback TAudioMixer::GetStopCallback(long chan)
{
	TAudioChannelPtr channel;
    return (IsValid(chan) && (channel = fSoundChannelTable[chan])) ? channel->GetStopCallback() : 0;
}

void TAudioMixer::GetInfo(long chan, ChannelInfo* info)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->GetInfo(info);
}

void TAudioMixer::Reset()
{
    for (list<TAudioChannelPtr>::iterator iter = fSoundChannelSeq.begin(); iter != fSoundChannelSeq.end(); iter++) {
        TAudioChannelPtr channel = *iter;
        channel->SetStream(0);
        channel->SetState(false); // Important : used to mark the insertion state
    }
    fSoundChannelSeq.clear();
}

void TAudioMixer::SetEffectList(long chan, TAudioEffectListPtr effect_list, long fadeIn, long fadeOut)
{
	TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SetEffectList(effect_list, fadeIn, fadeOut);
}

TAudioEffectListPtr TAudioMixer::GetEffectList(long chan)
{
	TAudioChannelPtr channel;
    return (IsValid(chan) && (channel = fSoundChannelTable[chan])) ? channel->GetEffectList() : 0;
}




