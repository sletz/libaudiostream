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
#include "la_smartpointer.h"

typedef uint64_t audio_frames_t;

//---------------------
// Class TSymbolicDate
//---------------------

class TSymbolicDate : public la_smartable1
{
    private:
    
        audio_frames_t fRealDate;
   
    public : 
        TSymbolicDate():fRealDate(UINT64_MAX)
        {}
        TSymbolicDate(audio_frames_t date):fRealDate(date)
        {}
        
        audio_frames_t getDate() { return fRealDate; }
        void setDate(audio_frames_t date) { fRealDate = date; }
        
        bool operator< (TSymbolicDate date) 
        { 
            return fRealDate < date.fRealDate; 
        }
    
};

typedef LA_SMARTP<TSymbolicDate> TSymbolicDatePtr;

typedef TSymbolicDatePtr SymbolicDate;

#endif




