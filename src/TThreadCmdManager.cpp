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

#include "TThreadCmdManager.h"
#include "TThreadUtils.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#if defined(__APPLE__) || defined(linux)

void* TThreadCmdManager::CmdHandler(void* arg)
{
    TThreadCmdManager* manager = (TThreadCmdManager*) arg;

    pthread_mutex_lock(&manager->fLock);
    while (manager->fRunning) {
        manager->RunAux();
        pthread_cond_wait(&manager->fCond, &manager->fLock);
    }
    pthread_mutex_unlock(&manager->fLock);
    
    pthread_exit(0);
    return 0;
}

#elif WIN32

DWORD WINAPI TThreadCmdManager::CmdHandler(void* arg)
{
    TThreadCmdManager* manager = (TThreadCmdManager*) arg;

    while (manager->fRunning) {
        manager->RunAux();
		WaitForSingleObject(manager->fCond, INFINITE);
    }

    return 0;
}

#endif

void TThreadCmdManager::RunAux()
{
    TCmd* cmd;

    while ((cmd = (TCmd*) fifoget(&fRunningCmd))) {
        (*((CmdPtr)cmd->fun))(cmd->arg1, cmd->arg2, cmd->arg3, cmd->arg4, cmd->arg5);
        lfpush(&fFreeCmd, (lifocell*)cmd);
    }
}

TThreadCmdManager::TThreadCmdManager(long thread_num)
{   
    TCmd* cmd;
	int i;
    
    fRunning = true;

    // Init variables
    lfinit(&fFreeCmd);

#if defined(__APPLE__) || defined(linux)   
	pthread_mutex_init(&fLock, NULL);
    pthread_cond_init(&fCond, NULL);
#elif WIN32
	fCond = CreateEvent(NULL, FALSE, FALSE, NULL);
#endif

    // Preallocate commands
    for (i = 0; i < MAXCOMMAND; i++) {
        cmd = (TCmd*)malloc(sizeof(TCmd));
        if (cmd)
            lfpush(&fFreeCmd, (lifocell*)cmd);
    }
	fifoinit(&fRunningCmd);

#if defined(__APPLE__) || defined(linux)
	struct sched_param param;
    param.sched_priority = 99;
    for (i = 0; i < thread_num; i++) {
        pthread_t thread;
        pthread_create(&thread, NULL, CmdHandler, (void*)this); // assume it works..
        fThreadList.push_back(thread);
    }
#elif WIN32
	for (i = 0; i < thread_num; i++) {
		DWORD id;
		HANDLE thread = CreateThread(NULL, 0, CmdHandler, (void*)this, 0, &id);
        fThreadList.push_back(thread);
    }
#endif
}

TThreadCmdManager::~TThreadCmdManager()
{
    TCmd* cmd;
    TCmd* next;
    
    // Stop the threads...
    fRunning = false;
    
#if defined(__APPLE__) || defined(linux)      
    pthread_mutex_lock(&fLock);
    pthread_cond_broadcast(&fCond);
    pthread_mutex_unlock(&fLock);
#elif WIN32
    fifoput(&fRunningCmd, (fifocell*)cmd);
    SetEvent(fCond);
#endif
    
    // Wait for thread exit
    for (unsigned int i = 0; i < fThreadList.size(); i++) {
	#if defined(__APPLE__) || defined(linux)
        pthread_cancel(fThreadList[i]);
        pthread_join(fThreadList[i], NULL); 
    #elif WIN32
		TerminateThread(fThreadList[i],0);
		//WaitForSingleObject(fThreadList[i], INFINITE);
    #endif
    }  

	#if defined(__APPLE__) || defined(linux)
		pthread_mutex_destroy(&fLock);
		pthread_cond_destroy(&fCond);
	#elif WIN32
		CloseHandle(fCond);
	#endif

    // Free structures
    while ((cmd = (TCmd*)lfpop(&fFreeCmd))) {
        free(cmd);
	}
	cmd = (TCmd*)fifoflush(&fRunningCmd);
    while (cmd) {
        next = cmd->link;
        free(cmd);
        cmd = next;
    }
}

void TThreadCmdManager::FlushCmds()
{
    TCmd* cmd;
    // Remove cmds from running fifo, put them on free lifo
    while ((cmd = (TCmd*) fifoget(&fRunningCmd))) {
        lfpush(&fFreeCmd, (lifocell*)cmd);
    }
}

void TThreadCmdManager::ExecCmdAux(CmdPtr fun, long arg1, long arg2, long arg3, long arg4, long arg5)
{
    // Get a command structure from the free command list
    // fills it and push it on the running list
    TCmd* cmd = (TCmd*)lfpop(&fFreeCmd);

    if (cmd) {
        cmd->fun = fun;
        cmd->arg1 = arg1;
        cmd->arg2 = arg2;
        cmd->arg3 = arg3;
        cmd->arg4 = arg4;
        cmd->arg5 = arg5;
	// Signal the condition to wake the thread
	#if defined(__APPLE__) || defined(linux)      
        pthread_mutex_lock(&fLock);
        fifoput(&fRunningCmd, (fifocell*)cmd);
        pthread_cond_signal(&fCond);
        pthread_mutex_unlock(&fLock);
	#elif WIN32
		fifoput(&fRunningCmd, (fifocell*)cmd);
		SetEvent(fCond);
	#endif
    } else {
        printf("Error : empty cmd lifo\n");
    }
}



