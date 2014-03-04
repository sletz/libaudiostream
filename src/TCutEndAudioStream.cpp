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

#include "TCutEndAudioStream.h"
#include "UTools.h"
#include <assert.h>

TCutEndAudioStream::TCutEndAudioStream(TAudioStreamPtr stream, long end) : TDecoratedAudioStream(stream)
{
	assert(end >= 0);
    fFramesNum = end;
    fCurFrame = 0;
}

long TCutEndAudioStream::Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
{
    //printf("TCutEndAudioStream::Read framesNum = %ld fFramesNum = %ld fCurFrame = %ld\n", framesNum, fFramesNum, fCurFrame);
    assert_stream(framesNum, framePos);
    
    int res = fStream->Read(buffer, UTools::Min(framesNum, fFramesNum - fCurFrame), framePos);
    fCurFrame += res;
    return res;
}

TAudioStreamPtr TCutEndAudioStream::CutBegin(long frames)
{
    assert(fFramesNum - frames >= 0);
    return new TCutEndAudioStream(fStream->CutBegin(frames), fFramesNum - frames);
}

void TCutEndAudioStream::Reset()
{
    assert(fStream);
    fCurFrame = 0;
    fStream->Reset();
}



