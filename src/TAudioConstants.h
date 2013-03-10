/*
Copyright (C) Grame 2002-2013

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

#ifndef __TAudioConstants__
#define __TAudioConstants__

#ifndef Boolean
typedef unsigned char Boolean;
#endif

#define ADVANCE 131072

#define SAMPLE_SIZE  	2 // sample size in Byte

#define kBufferNum  	10
#define MAX_CHANNELS  	64 


#define MAX_PLUG_CHANNELS 4

// For MidiDiMan Delta 1010
#define DELTA_OUTVOICES 	8
#define DELTA_INVOICES 		8
#define DELTA_SAMPLE_SIZE 	4    // In bytes
#define MULTI_BUFFER_SIZE FRAME_SIZE*DELTA_OUTVOICES

// For Stereo cards
#define STEREO_VOICES 	2
#define STEREO_SAMPLE_SIZE 	2   // In bytes
#define STEREO_BUFFER_SIZE BUFFER_SIZE*2

#define SAMPLE_RATE 44100.0f
#define FADE_TIME1 64  	// in frames

#define DEFAULT_VOL 1.0f		// (0 .. 1)
#define DEFAULT_PAN 0.5f		// (0 .. 1)

#define DEFAULT_PAN_LEFT 1.0f		// (0 .. 1)
#define DEFAULT_PAN_RIGHT 0.0f		// (0 .. 1)

// Number of chhanels
#define RT_CHANNELS 2
#define MIXER_BUFFER_SIZE BUFFER_SIZE*CHANNELS

#define NO_ERR 0
#define OPEN_ERR -1
#define CLOSE_ERR -2
#define LOAD_ERR -3
#define FILE_NOT_FOUND_ERR -4

#endif


