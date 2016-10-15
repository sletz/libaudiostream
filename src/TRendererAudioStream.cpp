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

#include "TRendererAudioStream.h"
#include "TThreadCmdManager.h"

void TRendererAudioStream::SetManager(TAudioStreamPtr stream, TCmdManagerPtr manager)
{
    TCmdHandlerList lst(stream);

    for (auto handler : lst) 
    {
        handler->SetManager(manager);
    }
}

void TRendererAudioStream::SetStream(TAudioStreamPtr stream)
{
    fStream = stream;
    SetManager(stream, GetManager());
}

void TDTRendererAudioStream::Init()
{
    fManager = new TDirectCmdManager();
}
void TDTRendererAudioStream::Destroy()
{
    delete fManager;
    fManager = nullptr;
}
void TDTRendererAudioStream::Flush()
{
    fManager->FlushCmds();
}

void TRTRendererAudioStream::Init(long thread_num)
{
    //fManager = new TThreadCmdManager(thread_num);
    fManager = new TWaitThreadCmdManager(thread_num);
}
void TRTRendererAudioStream::Destroy()
{
    delete fManager;
    fManager = nullptr;
}
void TRTRendererAudioStream::Flush()
{
    fManager->FlushCmds();
}


