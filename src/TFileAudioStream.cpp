/*

Copyright (C) Grame 2002-2012

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

#include "TFileAudioStream.h"
#include "TAudioGlobals.h"
#include "UAudioTools.h"
#include "UTools.h"
#include <stdio.h>
#include <string.h>

// Callback called by command manager
void TFileAudioStream::ReadBufferAux(TFileAudioStreamPtr obj, TAudioBuffer<short>* buffer, long framesNum, long framePos)
{
    obj->TBufferedAudioStream::ReadBuffer(buffer, framesNum, framePos);
}

// Handle the disk read function with the command manager: either direct or low-priority thread based
void TFileAudioStream::ReadBuffer(TAudioBuffer<short>* buffer, long framesNum, long framePos)
{
    fReady = false;
    if (fManager == 0)
        printf("Error : stream rendered without command manager\n");
    assert(fManager);
    fManager->ExecCmd((CmdPtr)ReadBufferAux, (long)this, (long)buffer, framesNum, framePos, 0);
}

// Callback called by command manager
void TFileAudioStream::WriteBufferAux(TFileAudioStreamPtr obj, TAudioBuffer<short>* buffer, long framesNum, long framePos)
{
    obj->TBufferedAudioStream::WriteBuffer(buffer, framesNum, framePos);
}

// Handle the disk write function with the command manager: either direct or low-priority thread based
void TFileAudioStream::WriteBuffer(TAudioBuffer<short>* buffer, long framesNum, long framePos)
{
    fReady = false;
    if (fManager == 0)
        printf("Error : stream rendered without command manager\n");
    assert(fManager);
    fManager->ExecCmd((CmdPtr)WriteBufferAux, (long)this, (long)buffer, framesNum, framePos, 0);
}

