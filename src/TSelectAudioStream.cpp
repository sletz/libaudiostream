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

#include "TSelectAudioStream.h"

long TSelectAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    float* temp1[Channels()];
    float* temp2[fStream->Channels()];
    
    /* Cleanup temporary fBuffer */
	UAudioTools::ZeroFloatBlk(fBuffer->GetFrame(0, temp1), TAudioGlobals::fBufferSize, fStream->Channels());
    
    /* Read stream */
    long res = fStream->Read(fBuffer, framesNum, framePos);
    
    /* Copy selection in output */
    UAudioTools::SelectChannelsTo(buffer->GetFrame(0, temp1), fBuffer->GetFrame(0, temp2), framesNum, fSelection);
    return res;
}

TAudioStreamPtr TSelectAudioStream::CutBegin(long frames)
{
     return new TSelectAudioStream(fStream->CutBegin(frames), fSelection);
}

TAudioStreamPtr TSelectAudioStream::Copy()
{
    return new TSelectAudioStream(fStream, fSelection);
}
