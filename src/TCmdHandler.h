/*
Copyright © Grame 2002-2007

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

#ifndef __TCmdHandler__
#define __TCmdHandler__

#include "TCmdManager.h"
#include "TBinaryAudioStream.h"
#include <list>

using namespace std;

//-------------------
// Class TCmdHandler
//-------------------
/*!
\brief  A TCmdHandler uses a TCmdManager to handle callback.
*/

class TCmdHandler
{

    protected:

        TCmdManagerPtr fManager;

    public:

        TCmdHandler(): fManager(0)
        {}
        virtual ~TCmdHandler()
        {}

        void SetManager(TCmdManagerPtr manager)
        {
            fManager = manager;
        }
};

typedef TCmdHandler * TCmdHandlerPtr;

//-----------------------
// Class TCmdHandlerList
//-----------------------
/*!
\brief A STL list which contains the TCmdHandler nodes of a stream tree.
*/

class TCmdHandlerList : public list<TCmdHandlerPtr>
{

    private:

        // Build a list of TCmdHandler leaves
        /*
        void MakeList(TAudioStreamPtr stream) 
        {
        	// Goes inside the unary stream
        	if (TUnaryAudioStreamPtr  unary = dynamic_cast<TUnaryAudioStreamPtr>(stream)) { 
        		MakeList(unary->GetBranch1()); 
        	// Goes inside the two branches
        	}else if (TBinaryAudioStreamPtr binary = dynamic_cast<TBinaryAudioStreamPtr>(stream)){
        		MakeList(binary->GetBranch1());
        		MakeList(binary->GetBranch2()); 
        	}
        	
        	if (TCmdHandlerPtr handler = dynamic_cast<TCmdHandlerPtr>(stream)) {
        		push_front(handler);
        	}
        }
        */

        // Build a list of TCmdHandler leaves
		
        void MakeList(TAudioStreamPtr stream)
        {
            // Goes inside the two branches
	       if (TBinaryAudioStreamPtr binary = dynamic_cast<TBinaryAudioStreamPtr>((TAudioStream*)stream)) {
                MakeList(binary->GetBranch1());
                MakeList(binary->GetBranch2());
                // Goes inside the unary stream
            } else if (TUnaryAudioStreamPtr unary = dynamic_cast<TUnaryAudioStreamPtr>((TAudioStream*)stream)) {
                MakeList(unary->GetBranch1());
            }
	
            if (TCmdHandlerPtr handler = dynamic_cast<TCmdHandlerPtr>((TAudioStream*)stream))
                push_front(handler);
	    }

    public:

        TCmdHandlerList(TAudioStreamPtr stream)
        {
            MakeList(stream);
        }
        virtual ~TCmdHandlerList()
        {}
};

typedef TCmdHandlerList* TCmdHandlerListPtr;

#endif
