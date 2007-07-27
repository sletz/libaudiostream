/*

Copyright © Grame 2006-2007

This library is free software; you can redistribute it and modify it under
the terms of the GNU Library General Public License as published by the
Free Software Foundation version 2 of the License, or any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
research@grame.fr

*/

#include <stdio.h>
#include <stdlib.h>
#include "LibAudioStream.h"

#ifdef WIN32
#include <windows.h>
#endif

#define IN_CHANNELS  2 
#define OUT_CHANNELS 2
#define CHANNELS     8
#define SAMPLE_RATE  44100

#define AUDIO_BUFFER	1024
#define FILE_BUFFER		65536 * 4
#define STREAM_BUFFER	131072 * 4

#define VOL	  1.0f
#define PAN_LEFT   1.0f
#define PAN_RIGHT  0.0f

static void DisplayDevice(AudioRendererPtr manager)
{
	int i;
	
	for (i = 0; i < GetDeviceCount(manager); i++) {
		DeviceInfo info;
		GetDeviceInfo(manager, i, &info);
		printf("\n--------------------------------\n");
		printf("Device name: %s\n", info.fName);
		printf("Device max input: %ld\n", info.fMaxInputChannels);
		printf("Device max output: %ld\n", info.fMaxOutputChannels);
		printf("Device default buffer size: %ld\n", info.fDefaultBufferSize);
		printf("Device default sample rate: %f\n", info.fDefaultSampleRate);	
	}

	printf("Device default input device: %ld\n", GetDefaultInputDevice(manager));
	printf("Device default output device: %ld\n", GetDefaultOutputDevice(manager));
}

static void DisplayAllDevices()
{
	AudioRendererPtr manager1 = MakeAudioRenderer(kJackRenderer);
	if (manager1) {
		printf("--------------------\n");
		printf("Jack Device\n");
		printf("--------------------\n");
		DisplayDevice(manager1);
		DeleteAudioRenderer(manager1);
	}
	
	AudioRendererPtr manager2 = MakeAudioRenderer(kPortAudioRenderer);
	if (manager2) {
		printf("--------------------\n");
		printf("PortAudio Device\n");
		printf("--------------------\n");
		DisplayDevice(manager2);
		DeleteAudioRenderer(manager2);
	}
}

static int PlayFile(char * filename, long beginFrame, long endFrame, int renderer)
{
    ChannelInfo info;
	AudioStreamPtr sound;
	AudioPlayerPtr player;
	
	player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, 
                     		SAMPLE_RATE, AUDIO_BUFFER, FILE_BUFFER, STREAM_BUFFER, 
                     		renderer, 1);
	if (!player) {
		fprintf(stdout, "cannot start player, now quit...\n");
		return -1;
	}
							
	sound = MakeStereoSoundPtr(MakeRegionSoundPtr(filename, beginFrame, endFrame));
	
	if (!sound) {
		fprintf(stdout, "sound %s cannot be opened: check name and parameters\n", filename);
		return -1;
	}
	
	StartAudioPlayer(player);
	LoadChannelPtr(player, sound, 1, VOL, PAN_LEFT, PAN_RIGHT);
	StartChannel(player, 1);
	
	do {
		GetInfoChannel(player, 1, &info);
		#ifdef WIN32
			Sleep(1000);
		#else
			sleep(1);
		#endif
	} while (info.fStatus);
				
    StopAudioPlayer(player);
    CloseAudioPlayer(player);
    DeleteSoundPtr(sound);
	return 0;
}

int main(int argc, char *argv[]) 
{
	int renderer = kPortAudioRenderer;
	
	if (argc != 5) {
		fprintf(stderr, "usage: fileplay -p (or -j) 'file' 'start' 'end'\n");
		return 1;
	} else {
		renderer = (strcmp(argv[1], "-j") == 0) ? kJackRenderer : kPortAudioRenderer;
		int start = atoi(argv[3]);
		int end = atoi(argv[4]);
		fprintf(stdout, "playing file %s from %d to %d\n", argv[2], start, end, renderer);
		DisplayAllDevices();
		PlayFile(argv[2], start * SAMPLE_RATE, end * SAMPLE_RATE, renderer);
	}
	return 0;
}
