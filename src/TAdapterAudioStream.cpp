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

#include "TAdapterAudioStream.h"
#include "TAudioGlobals.h"

long TAdapterAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    if (fStream->Channels() == fChannels) {
        return fStream->Read(buffer, framesNum, framePos);
    } else {
        int res = fStream->Read(fAdaptBuffer, framesNum, framePos);
        float* temp1[buffer->GetChannels()];
        float* temp2[fAdaptBuffer->GetChannels()];
        UAudioTools::Adapt(buffer->GetFrame(framePos, temp1), fAdaptBuffer->GetFrame(framePos, temp2), framesNum, fStream->Channels(), fChannels);
        return res;
    }
}