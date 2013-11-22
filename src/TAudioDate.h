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

// ===========================================================================
//	 TAudioDate.h
// ===========================================================================

#ifndef __TAudioDate__
#define __TAudioDate__

#include <stdint.h>

typedef uint64_t audio_frames_t;

//------------------
// Class TAudioDate
//------------------

struct TAudioDate
{
    long fBlockNum;
    long fSampleInblock;
        
    static void Convert(audio_frames_t date_sample, int buffer_size, TAudioDate& date)
    {
        date.fBlockNum = date_sample / buffer_size;
        date.fSampleInblock = date_sample % buffer_size;
    }
};

struct TSymbolicDate 
{
    audio_frames_t fRealDate;
    int fIndex;
    
    TSymbolicDate():fRealDate(0),fIndex(0)
    {}
    
    static int fSymbolIndex;
};

#endif




