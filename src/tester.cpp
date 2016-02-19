/*

Copyright (C) Grame 2002-2014

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
#include <stdio.h>

#define SAMPLE_RATE 44100
#define CHANNELS 8
#define BUFFER_SIZE 1024

int main(int argc, char* argv[])
{
    printf("----------------------------\n");
    printf("LibAudioStream Player test \n");
    printf("----------------------------\n\n");

    printf("LibVersion = %d\n", LibVersion());
    
    AudioPlayerPtr player;
    
	printf("\n==================================\n");
    printf("OpenAudioPlayer : in = %d out = %d sample_rate = %d\n", 0, 2, SAMPLE_RATE);
    player = OpenAudioPlayer(0, 2, CHANNELS, SAMPLE_RATE, BUFFER_SIZE, 65536 * 8, SAMPLE_RATE * 60 * 10, kCoreAudioRenderer, 1);
    if (!player) {
        printf("Cannot OpenAudioPlayer\n");
    } else {
        CloseAudioPlayer(player);
    }
    
    printf("\n==================================\n");
    printf("OpenAudioPlayer : in = %d out = %d sample_rate = %d\n", 1, 2, SAMPLE_RATE);
    player = OpenAudioPlayer(1, 2, CHANNELS, SAMPLE_RATE, BUFFER_SIZE, 65536 * 8, SAMPLE_RATE * 60 * 10, kCoreAudioRenderer, 1);
    if (!player) {
        printf("Cannot OpenAudioPlayer\n");
    } else {
        CloseAudioPlayer(player);
    }
    
    printf("\n==================================\n");
    printf("OpenAudioPlayer : in = %d out = %d sample_rate = %d\n", 2, 2, SAMPLE_RATE);
    player = OpenAudioPlayer(2, 2, CHANNELS, SAMPLE_RATE, BUFFER_SIZE, 65536 * 8, SAMPLE_RATE * 60 * 10, kCoreAudioRenderer, 1);
    if (!player) {
        printf("Cannot OpenAudioPlayer\n");
    } else {
        CloseAudioPlayer(player);
    }  
    
    printf("\n==================================\n");
    printf("OpenAudioPlayer : in =  %d out = %d sample_rate = %d\n", 2, 1, SAMPLE_RATE);
    player = OpenAudioPlayer(2, 1, CHANNELS, SAMPLE_RATE, BUFFER_SIZE, 65536 * 8, SAMPLE_RATE * 60 * 10, kCoreAudioRenderer, 1);
    if (!player) {
        printf("Cannot OpenAudioPlayer\n");
    } else {
        CloseAudioPlayer(player);
    }
    
    printf("\n==================================\n");
    printf("OpenAudioPlayer : in =  %d out = %d sample_rate = %\n", 2, 0, SAMPLE_RATE);
    player = OpenAudioPlayer(2, 0, CHANNELS, SAMPLE_RATE, BUFFER_SIZE, 65536 * 8, SAMPLE_RATE * 60 * 10, kCoreAudioRenderer, 1);
    if (!player) {
        printf("Cannot OpenAudioPlayer\n");
    } else {
        CloseAudioPlayer(player);
    }
        
    return 0;
}
