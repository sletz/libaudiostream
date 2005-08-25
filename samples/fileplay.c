/*
  Copyright (C) 2005  Grame
  Grame Research Dpt, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the AudioStream Library use.
*/


#include <stdio.h>
#include <stdlib.h>
#include "LibAudioStream.h"

#define IN_CHANNELS  2 
#define OUT_CHANNELS 2
#define CHANNELS     8
#define SAMPLE_RATE  44100

#define AUDIO_BUFFER	512
#define FILE_BUFFER		65536 * 4
#define STREAM_BUFFER	131072 * 4

#define VOL	  1.0f
#define PAN   0.5f

static int playfile (char * filename, long beginFrame, long endFrame)
{
    ChannelInfo info;
    AudioPlayerPtr player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, 
                            SAMPLE_RATE, AUDIO_BUFFER, FILE_BUFFER, STREAM_BUFFER, 
                            kPortAudioRenderer, 1);
    AudioStreamPtr sound  = MakeRegionSoundPtr (filename, beginFrame, endFrame);

    StartAudioPlayer(player);
	LoadChannelPtr (player, sound, 1, VOL, PAN);
	StartChannel(player, 1);

	do {
		GetInfoChannel (player, 1, &info);
	} while (info.fStatus);
			
    StopAudioPlayer(player);
    CloseAudioPlayer(player);
    DeleteSoundPtr (sound);
}

int main (int argc, char *argv[]) {
	if (argc != 4) {
		fprintf (stderr, "usage: fileplay 'file' 'start' 'end'\n");
		return 1;
	}
	else {
		int start = atoi(argv[2]);
		int end = atoi(argv[3]);
		fprintf (stdout, "playing file %s from %d to %d\n", argv[1], start, end);
		playfile (argv[1], start * SAMPLE_RATE, end * SAMPLE_RATE);
	}
	return 0;
}
