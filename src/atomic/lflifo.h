/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#ifndef __lflifo__
#define __lflifo__

//
/*****************************************************************
 *****************************************************************
                       LOCK FREE LIFO STACK 

 Implements a lock-free shared LIFO stack made of a list of cells
 linked together. A cell can be anything provided it starts with
 a pointer available to link together the cells of the stack. 
 
 A LIFO stack <ic, [a b ...], oc> is made of 3 parts : an
 input counter 'ic' incremented every time a cell is added (lfpush)
 to the lifo stack, a linked list of cells [a b ...], and
 an output counter 'oc' incremented every time a cell is removed
 (lfpop) from the lifo stack. The 'lfpush' and 'lfpop' operations
 use 'CMPXCHG8B' to update the LIFO stack. The 'ic' and 'oc' 
 counters are used to avoid the ABA problem.
  
                           OPERATIONS
 ------------------------------------------------------------------
 void          lfinit  (lifo* lf);
 lifocell*     lfavail (lifo* lf);
 unsigned long lfsize  (lifo * lf);
 void          lfpush  (lifo * lf, lifocell * cl);
 lifocell*     lfpop   (lifo * lf);
 ------------------------------------------------------------------

 Warning : all operations expect non-null lifo and cell pointers.
 It is the caller responsability to check the arguments !
 
 *****************************************************************
 *****************************************************************/

#include "msAtomic.h"

/*****************************************************************
                           DATA STRUCTURES
 *****************************************************************/
#ifndef __ppc__
# define lfCount(name) atomic_long volatile name
#else
# define lfCount(name) long name[7]
#endif

typedef struct lifocell {
	struct lifocell* volatile link;	/* next cell in the list */
	atomic_long value[3];			/* any data here		 */
} lifocell;

// Has to be __attribute__ ((aligned (16)));

struct lifo {
	lifocell * volatile top;	/* top of the stack          */
	lfCount(oc);				/* used to avoid ABA problem */
	char	unused[2*sizeof(atomic_long)];		/* alignment */
	TAtomic	count;
	char	padding[3*sizeof(atomic_long)];		/* alignment */
};
typedef struct lifo lifo;

#ifdef __cplusplus
extern "C" {
#endif

 void          lfinit  (lifo* lf);
 void          lfpush  (lifo * lf, lifocell * cl);
 lifocell*     lfpop   (lifo * lf);

#ifdef __cplusplus
}
#endif

static inline lifocell* lfavail(lifo* lf) 		{ return (lifocell*)lf->top; }
static inline atomic_long lfsize (const lifo * lf) 	{ return lf->count.value; }

#endif
