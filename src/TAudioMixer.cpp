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
    fVol = DEFAULT_VOL;
    fPan = DEFAULT_PAN;

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
	
    // Master Pan and Vol
    MY_FLOAT leftvol = TPanTable::GetVolLeft(fVol, fPan);
    MY_FLOAT rightvol = TPanTable::GetVolRight(fVol, fPan);

    UAudioTools::ReplaceFrameToFrameBlk(outputBuffer,
                                        fMixBuffer->GetFrame(0),
                                        TAudioGlobals::fBuffer_Size,
                                        TAudioGlobals::fOutput, leftvol, rightvol);
    return true;
}

/*--------------------------------------------------------------------------*/
// Internal API
/*--------------------------------------------------------------------------*/

long TAudioMixer::Load(TAudioStreamPtr stream, long chan, long vol, long pan)
{
    if (IsAvailable(chan)) {
        TAudioChannelPtr channel = fSoundChannelTable[chan];
        assert(channel);
        assert(stream);
        channel->SetStream(stream); // Owner of the old stream pointer has to do the desallocation
        channel->SetVol(vol);
        channel->SetPan(pan);
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
        channel->SoundOff();
}

void TAudioMixer::SetVol(long chan, long vol)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SetVol(vol);
}

void TAudioMixer::SetPan(long chan, long pan)
{
    TAudioChannelPtr channel;
    if (IsValid(chan) && (channel = fSoundChannelTable[chan]))
        channel->SetPan(pan);
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






