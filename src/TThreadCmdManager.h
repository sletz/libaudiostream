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

#ifndef __TThreadCmdManager__
#define __TThreadCmdManager__

#if defined(__APPLE__)
# include <pthread.h>
# include <mach/thread_act.h>
#elif defined(linux)
# include <pthread.h>
#elif WIN32
# include <windows.h>
#endif

#include "TCmdManager.h"
#include "lffifo.h"
#include "lflifo.h"
#include <vector>

//-------------
// Struct TCmd
//-------------

/*
Warning : the fifocell data structure size (see lffifo.h) MUST match the TCmd data structure size 
*/ 
/*!
\brief A command structure
*/

typedef struct TCmd
{
    TCmd* link;
    CmdPtr fun;
    long arg1;
    long arg2;
    long arg3;
    long arg4;
    long arg5;
} TCmd;

#define MAXCOMMAND 256

//-------------------------
// Class TThreadCmdManager
//-------------------------
/*!
\brief A low-priority thread based command manager.
*/

/*
On Mar 23, 2004, at 8:58 PM, Steve Sisak wrote:
At 4:20 PM -0800 3/23/04, Matt Watson wrote:
"Don't ever use the C/C++ language volatile in threaded code. It'll
kill your performance, and the language definition has nothing to do
with what you want when writing threaded code that shares data."
 
Followed by a bunch of religious comments about how the real world 
isn't how he'd like it to be....
 
This conversation has been interesting so far, but before it goes too 
far into the weeds,
 
and then he drags it into the weeds himself...
 
(NOTE: When the guy who wrote the pthreads implementation on the 
platform tells you how it works - and quotes standards and the 
undisputed "Knuth of pthreads" - maybe you should be just a little less 
enthusiastic about contradicting him).
 
I need to point out that your previous example:
 
	pthread_mutex_lock(&m);
	while (condition == false) {
		pthread_cond_wait(&c, &m);
	}
	// Do something that depends on condition being true 
	// Perhaps use/change some global data while we have the lock 
 
	pthread_mutex_unlock(&m);
 
While the "signaller" code should look something like:
 
	// Do something that changes condition 
 
	pthread_mutex_lock(&m);
	// Perhaps use/change some global data while we have the lock 
	condition = true;
	pthread_mutex_unlock(&m);
	pthread_cond_signal(&c);
 
is incorrect if "condition" isn't declared "volatile".
 
This is because, since the compiler knows there are no references to 
"condition" in the loop, it's free to put in in a register and never 
look at it's memory address again.
 
 
No, it is not.  A compiler is not allowed to use a cached value for a 
global (and this example assumes "condition" is a global) after calling 
a function which may have side-effects on that global.  There is 
(provably) no way the compiler could ever tell that pthread_cond_wait() 
didn't have a side-effect on that particular global, so it must fetch 
it after each such call.
 
The processor is free to re-order the completion of these operations 
(and PowerPC normally would).  But on such processors, it is the 
responsibility of the pthread_cond_wait() implementation (via it's 
internal unlock and lock semantics on the mutex) to assure that code 
scheduled before the unlock completes before the unlock is effective.
 
Net result:  this is perfectly valid code on all conforming pthread 
implementations (and while we are lacking in some aspects of pthreads 
on Darwin, this isn't one of them).
 
Further, volatile will not "kill your performance" if you use it 
correctly -- there's nothing stopping you from reading the value into 
a non-volatile temporary and using that value in your code as long as 
you re-read the temporary the volatile variable whenever you care if 
the value changes.
 
If fact this is what "volatile" does -- force the compiler to examine 
the variable exactly when the programmer tells it to -- wether the 
programmer is smarter than the compiler or not is another question. 
:-)
 
No, technically, it doesn't.  Nothing stops the compiler from moving 
all the other operations on non-volatiles out and around that one 
volatile thing.  The only thing stopping it is in this case is that the 
compiler can't assume what happens in that function call 
(pthread_cond_wait()).  So, you are back to exactly the same 
justification as I described above.  The keyword "volatile" didn't buy 
you anything (or cost you anything either) in this case.
 
If you had multiple volatiles accessed in the same block, you could 
make some claims about the order those instructions were issued by the 
compiler.  But you couldn't make any assumptions about the order they 
were completed by the processor without explicit barriers.  And what 
constitutes an effective barrier is often processor family dependent 
(and sometimes even specific to a particular processor in the family).  
It would be very hard to write portable code that depended upon the 
effects of "volatile".
 
That's why it is highly discouraged.
--Jim
*/

/*
Voir aussi: technique utilisée dans jack samples capture_client.c 
*/

class TThreadCmdManager : public TCmdManager
{

    private:
    
        lifo fFreeCmd __attribute__ ((aligned (16)));      // Commands free list
        fifo fRunningCmd __attribute__ ((aligned (16)));   // Running commands
        bool fRunning;

	#if defined(__APPLE__) || defined(linux)
		std::vector<pthread_t> fThreadList; // Execution thread
		pthread_mutex_t fLock;  // Mutex
        pthread_cond_t fCond;   // Condition variable
		static void* CmdHandler(void* arg);
	#elif WIN32
		std::vector<HANDLE> fThreadList; // Execution thread
        HANDLE fCond;   // Condition variable
		static DWORD WINAPI CmdHandler(void* arg);
	#endif
       
    public:

        TThreadCmdManager(long thread);
        ~TThreadCmdManager();

        void ExecCmdAux(CmdPtr fun, long a1, long a2, long a3, long a4, long a5);
        void RunAux();
        void FlushCmds();
};

typedef TThreadCmdManager * TThreadCmdManagerPtr;

class TWaitThreadCmdManager : public TCmdManager
{

    private:
    
        lifo fFreeCmd __attribute__ ((aligned (16)));      // Commands free list
        fifo fRunningCmd __attribute__ ((aligned (16)));   // Running commands
        bool fRunning;

	#if defined(__APPLE__) || defined(linux)
		std::vector<pthread_t> fThreadList; // Execution thread
		static void* CmdHandler(void* arg);
	#elif WIN32
		std::vector<HANDLE> fThreadList; // Execution thread
  		static DWORD WINAPI CmdHandler(void* arg);
	#endif
       
    public:

        TWaitThreadCmdManager(long thread);
        ~TWaitThreadCmdManager();

        void ExecCmdAux(CmdPtr fun, long a1, long a2, long a3, long a4, long a5);
        void RunAux();
        void FlushCmds();
};

typedef TThreadCmdManager * TThreadCmdManagerPtr;

typedef TWaitThreadCmdManager * TWaitThreadCmdManagerPtr;

//Utilisation
/*
TCmdManager::Open();
TCmdManager::ExecCmd(&TFileAudioStream::Read, this, buffer, length, 0);
TCmdManager::Close();
*/

#endif


