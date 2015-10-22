/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
 
*/

#ifndef __lffifo__
#define __lffifo__

/*****************************************************************
 *****************************************************************
                       LOCK FREE FIFO STACK 

 Implements a lock-free shared FIFO stack made of a list of cells
 linked together. A cell can be anything provided it starts with
 a pointer available to link together the cells of the stack. 
 
 ****************************************************************
                          OPERATIONS
 ****************************************************************

 void 	       	fifoinit(fifo* ff, fifocell * dummy);
 unsigned long 	fifosize (fifo * ff);
 void 	       	fifoput (fifo * ff, fifocell * cl);
 fifocell * 	fifoget (fifo * ff);
 fifocell * 	fifoavail (fifo * ff); 
 fifocell * 	fifoflush (fifo * ff);
 fifocell * 	fifoclear (fifo * ff);

 Warning : all operations expect non-null lifo and cell pointers.
 It is the caller responsability to check the arguments !
 *****************************************************************
 *****************************************************************/

#include "msAtomic.h"

/*****************************************************************
                           DATA STRUCTURES
 *****************************************************************/
#ifndef __ppc__
# define ffCount(name) atomic_long volatile name
#else
# define ffCount(name) long name[7]
#endif

typedef struct fifocell {
	struct fifocell* volatile link;	/* next cell in the list */
	atomic_long value[3];			/* any data here */
} fifocell;

// Has to be __attribute__ ((aligned (16)));

struct fifo {
	fifocell * volatile head;	/* pointer to the head cell */
	ffCount(oc);
    fifocell * volatile tail;	/* pointer to the tail cell */
	ffCount(ic);
	TAtomic	count;
	atomic_long padding[3];		/* for aligment */
	fifocell dummy;
};
typedef struct fifo fifo;

#ifdef __cplusplus
extern "C" {
#endif

 void 	       	fifoinit(fifo* ff);
atomic_long 	fifosize (fifo * ff);
 void 	       	fifoput (fifo * ff, fifocell * cl);
 fifocell * 	fifoget (fifo * ff);
 fifocell * 	fifoavail (fifo * ff); 
 fifocell * 	fifoflush (fifo * ff);
// fifocell * 	fifoclear (fifo * ff);  => obsolete

#ifdef __cplusplus
}
#endif

#endif
