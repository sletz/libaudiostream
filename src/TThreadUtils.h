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


#ifndef __TThreadUtils__
#define __TThreadUtils__

#if(defined(__ppc__) && defined(__APPLE__))

#ifdef __cplusplus
extern "C"
{
#endif

#include <mach/mach.h> //used for setting policy of thread
#include <pthread.h>

#ifdef __cplusplus
}
#endif

unsigned long GetThreadBasePriority(pthread_t inThread)
{
    thread_basic_info_data_t	threadInfo;
    policy_info_data_t	thePolicyInfo;
    unsigned int	count;

    // get basic info
    count = THREAD_BASIC_INFO_COUNT;
    thread_info (pthread_mach_thread_np (inThread), THREAD_BASIC_INFO, (integer_t*)&threadInfo, &count);

    switch (threadInfo.policy) {

    case POLICY_TIMESHARE:
        count = POLICY_TIMESHARE_INFO_COUNT;
        thread_info(pthread_mach_thread_np (inThread), THREAD_SCHED_TIMESHARE_INFO, (integer_t*)&(thePolicyInfo.ts), &count);
        return thePolicyInfo.ts.base_priority;
        break;

    case POLICY_FIFO:
        count = POLICY_FIFO_INFO_COUNT;
        thread_info(pthread_mach_thread_np (inThread), THREAD_SCHED_FIFO_INFO, (integer_t*)&(thePolicyInfo.fifo), &count);
        if (thePolicyInfo.fifo.depressed) {
            return thePolicyInfo.fifo.depress_priority;
        } else {
            return thePolicyInfo.fifo.base_priority;
        }
        break;

    case POLICY_RR:
        count = POLICY_RR_INFO_COUNT;
        thread_info(pthread_mach_thread_np (inThread), THREAD_SCHED_RR_INFO, (integer_t*)&(thePolicyInfo.rr), &count);
        if (thePolicyInfo.rr.depressed) {
            return thePolicyInfo.rr.depress_priority;
        } else {
            return thePolicyInfo.rr.base_priority;
        }
        break;
    }
    return 0;
}

void SetRealTime(pthread_t pThread)
{
    pthread_attr_t	theThreadAttrs;

    int result = pthread_attr_init(&theThreadAttrs);
    //THROW_RESULT("pthread_attr_init - Thread attributes could not be created.")

    result = pthread_attr_setdetachstate(&theThreadAttrs, PTHREAD_CREATE_DETACHED);
    //THROW_RESULT("pthread_attr_setdetachstate - Thread attributes could not be detached.")

    pthread_attr_destroy(&theThreadAttrs);

    // we've now created the thread and started it
    // we'll now set the priority of the thread to the nominated priority
    // and we'll also make the thread fixed
    thread_extended_policy_data_t	theFixedPolicy;
    thread_precedence_policy_data_t	thePrecedencePolicy;
    int	relativePriority;

    // make thread fixed
    theFixedPolicy.timeshare = false;	// set to true for a non-fixed thread
    result = thread_policy_set(pthread_mach_thread_np(pThread), THREAD_EXTENDED_POLICY, (thread_policy_t) & theFixedPolicy, THREAD_EXTENDED_POLICY_COUNT);
    //THROW_RESULT("thread_policy - Couldn't set thread as fixed priority.")

    // set priority
    // precedency policy's "importance" value is relative to spawning thread's priority
    relativePriority = 62 - GetThreadBasePriority(pthread_self());

    thePrecedencePolicy.importance = relativePriority;
    result = thread_policy_set(pthread_mach_thread_np(pThread), THREAD_PRECEDENCE_POLICY, (thread_policy_t) & thePrecedencePolicy, THREAD_PRECEDENCE_POLICY_COUNT);

    //THROW_RESULT("thread_policy - Couldn't set thread priority.")
}

#endif

#if (defined(__Windows__))
void SetRealTime(pthread_t pThread)
{}
#endif

#endif

