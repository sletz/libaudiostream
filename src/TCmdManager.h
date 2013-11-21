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

#ifndef __TCmdManager__
#define __TCmdManager__

#include <stdio.h>

typedef void (* CmdPtr)(long a1, long a2, long a3, long a4, long a5);

//-------------------
// Class TCmdManager
//-------------------
/*!
\brief Allows to call callback functions
*/

class TCmdManager
{

    protected:

        static TCmdManager* fInstance;	// Unique instance

    public:

        TCmdManager()
        {
            fInstance = 0;
        }
        virtual ~TCmdManager()
        {}

        static void Open(TCmdManager* manager)
        {
            fInstance = manager;
        }
        // a revoir
        static void Close()
        {
            fInstance = 0;
            //delete fInstance;
        }
    
        void ExecCmd(CmdPtr fun, long a1, long a2, long a3, long a4, long a5)
        {
            ExecCmdAux(fun, a1, a2, a3, a4, a5);
        }
        
        static void Run()
        {
            if (fInstance)
                fInstance->RunAux();
        }

        virtual void ExecCmdAux(CmdPtr fun, long a1, long a2, long a3, long a4, long a5)
		{}
        virtual void RunAux()
        {}
        virtual void FlushCmds()
        {}
};

typedef TCmdManager * TCmdManagerPtr;

// Utilisation
/*
TCmdManager::Open();
TCmdManager::ExecCmd(&TFileAudioStream::Read, this, buffer, length, 0);
TCmdManager::Close();
*/

//-------------------------
// Class TDirectCmdManager
//-------------------------
/*!
\brief A direct caller command manager.
*/

class TDirectCmdManager : public TCmdManager
{

    public:

        TDirectCmdManager()
        {}
        virtual ~TDirectCmdManager()
        {}

		void ExecCmdAux(CmdPtr fun, long a1, long a2, long a3, long a4, long a5)
        {
            fun(a1, a2, a3, a4, a5);
        }
};

typedef TDirectCmdManager * TDirectCmdManagerPtr;

#endif





