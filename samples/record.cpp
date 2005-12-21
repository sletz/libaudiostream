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

#include "LibAudioStream++.h"
#include <sndfile.h>
#include <stdio.h>
#define IN_CHANNELS 2 // stereo player
#define OUT_CHANNELS 0 // stereo player
#define CHANNELS 8


int main(int argc, char* argv[])
{
	char c;
    printf("------------------------------\n");
    printf("LibAudioStream based Recorder \n");
    printf("------------------------------\n");
	
	if (argc == 1 || argv[1][0] == '-') {
		 printf("Usage: recorder filename (WAV file)\n");
		 exit(0);
	}
	
	printf("Type 'b' to start recording\n");
	printf("Type 'c' to continue recording\n");
    printf("Type 's' to stop recording\n");
	printf("Type 'q' to quit\n");
	
	// Try to open Jack version
    AudioPlayerPtr player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, 44100, 512, 65536 * 4, 131072 * 4, kJackRenderer, 1);
    // Is failure opens PortAudio version
    if (!player)
        player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, 44100, 512, 65536 * 4, 131072 * 4, kPortAudioRenderer, 1);
    StartAudioPlayer(player);
	AudioStream record_stream = MakeWriteSound(argv[1], MakeInputSound(), SF_FORMAT_WAV | SF_FORMAT_PCM_16);
	
	int res = LoadChannel(player, record_stream, 1, 1.0f, 1.0f, 0.0f);
	if (res == NO_ERR) {
        while ((c = getchar()) && (c != 'q')) {

			switch (c) {

				case 'b':
					StartChannel(player, 1);
					break;

				case 'c':
					ContChannel(player, 1);
					break;

				case 's':
					StopChannel(player, 1);
					break;
	
			}
		}
    } else {
        printf("LoadChannel error %d \n", res);
    }
	
	StopChannel(player, 1);
    StopAudioPlayer(player);
    CloseAudioPlayer(player);
    return 0;
}


