/*
  Copyright (C) 2003  Grame

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

*/

#include "smartpointer.h"
#include "TThreadCmdManager.h"

unsigned long smartable::removeReference() 
{ 
	if (--refCount == 0) {
		delete this; 
	}
	return refCount;
}

void smartable1::removeReferenceAux(smartable1* obj, long u1, long u2, long u3)
{
	delete obj;
}

unsigned long smartable1::removeReference() 
{ 
	if (--refCount == 0 && fManager) {
		fManager->ExecCmd((CmdPtr)removeReferenceAux, (long)this, 0, 0, 0, 0);
	}
	return refCount;
}

void smartable1::Init()
{
    fManager = new TThreadCmdManager(1);
}

void smartable1::Destroy()
{
	fManager->FlushCmds(); // Hum...
    delete fManager;
	fManager = NULL;
}
