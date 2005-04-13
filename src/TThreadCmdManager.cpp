/*
Copyright © Grame 2002

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
grame@rd.grame.fr

*/

#include "TThreadCmdManager.h"
#include "TThreadUtils.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

void* TThreadCmdManager::CmdHandler(void* arg)
{
    TThreadCmdManager* manager = (TThreadCmdManager*) arg;

#ifdef WIN32
    // FAIT PLANTER SUR MacOSX
    // pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL);
    // sur WIN32 : pthread_cancel ne marche pas si PTHREAD_CANCEL_ASYNCHRONOUS
#endif

    while (true) {
#ifdef WIN32	// using the "while line" make the application not quit properly (hanging thread)
        manager->RunAux();
        pthread_mutex_lock(&manager->fLock);
        pthread_cond_wait(&manager->fCond, &manager->fLock);
        pthread_mutex_unlock(&manager->fLock);
#else
		manager->RunAux();
        pthread_mutex_lock(&manager->fLock);
        while (fifosize(&manager->fRunningCmd) == 0)
            pthread_cond_wait(&manager->fCond, &manager->fLock);
        pthread_mutex_unlock(&manager->fLock);
#endif

    }

    pthread_exit(0);
    return 0;
}

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
    struct sched_param param;
    TCmd* cmd;

    // Init variables
    lfinit(&fFreeCmd);

    pthread_mutex_init(&fLock, NULL);
    pthread_cond_init(&fCond, NULL);

    // Preallocate commands
    for (int i = 0; i < MAXCOMMAND; i++) {
        cmd = (TCmd*)malloc(sizeof(TCmd));
        if (cmd)
            lfpush(&fFreeCmd, (lifocell*)cmd);
    }
    cmd = (TCmd*)malloc(sizeof(TCmd));
    fifoinit(&fRunningCmd, (fifocell*)cmd);

    param.sched_priority = 99;
    for (int i = 0; i < thread_num; i++) {
        pthread_t thread;
        pthread_create(&thread, NULL, CmdHandler, (void*)this);
        fThreadList.push_back(thread);
    }
    
    //err = pthread_create(&fThread, NULL, CmdHandler, (void*)this);
}

TThreadCmdManager::~TThreadCmdManager()
{
    TCmd* cmd;
    TCmd* next;
    
    printf("fThreadList.size() %ld\n", fThreadList.size());

    // Wait for thread exit
    for (unsigned int i = 0; i < fThreadList.size(); i++) {
    #ifdef __APPLE__
        mach_port_t machThread = pthread_mach_thread_np(fThreadList[i]);
        thread_terminate(machThread); 
    #else 
        pthread_cancel(fThreadList[i]);
        pthread_join(fThreadList[i], NULL); 
    #endif
    }
 
    pthread_mutex_destroy(&fLock);
    pthread_cond_destroy(&fCond);

    // Free structures
    while ((cmd = (TCmd*)lfpop(&fFreeCmd))) {
        free(cmd);
	}
    cmd = (TCmd*)fifoclear(&fRunningCmd);
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
        pthread_mutex_lock(&fLock);
        fifoput(&fRunningCmd, (fifocell*)cmd);
        pthread_mutex_unlock(&fLock);
        pthread_cond_signal(&fCond);
    } else {
        printf("Error : empty cmd lifo\n");
    }
}



