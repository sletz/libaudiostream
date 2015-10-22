/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/


#include "stdio.h"
#include "lffifo.h"

//#define __trace__
#define fifo_end(ff)	(fifocell *)ff

//----------------------------------------------------------------
void fifoinit (fifo* ff)
{
	ff->count.value = 0;
	ff->oc = ff->ic = 0;
	ff->dummy.link = fifo_end(ff);
	ff->head = ff->tail = &ff->dummy;
#ifdef __trace__
printf("fifoinit %p %p\n", ff, ff->head);
#endif
}


//----------------------------------------------------------------
atomic_long fifosize (fifo * ff)
{
    return ff->count.value;
}

//----------------------------------------------------------------
void fifoput (fifo * ff, fifocell * cl) 
{
    atomic_long ic;
	fifocell * volatile tail;

	cl->link = fifo_end(ff);	/* set the cell next pointer to the end marker */
	while (1) {
		ic = ff->ic;			/* read the tail modification count */
		tail = ff->tail;		/* read the tail cell */
		/* try to link the cell to the tail cell */
        if (CAS (&tail->link, fifo_end(ff), cl))
			break;
		else
		/* tail was not pointing to the last cell, try to set tail to the next cell */
            CAS2 (&ff->tail, tail, ic, tail->link, ic+1);
	} 
    /* enqeue is done, try to set tail to the enqueued cell */
	CAS2 (&ff->tail, tail, ic, cl, ic+1);
	msAtomicInc (&ff->count);
#ifdef __trace__
fprintf(stdout, "fifoput %p %p %ld\n", ff, cl, ff->count.value); 
fflush(stdout);
#endif
}

//----------------------------------------------------------------
fifocell *  fifoget(fifo * ff) 
{
	fifocell * volatile head;
	fifocell * next; 
	atomic_long ic, oc;
	short done = 0;
	
	do {
		oc = ff->oc;					/* read the head modification count */
		ic = ff->ic;					/* read the tail modification count */
		head = ff->head;				/* read the head cell */
		next = head->link;				/* read the next cell */
		if (oc == ff->oc) {				/* ensures that next is a valid pointer */
                                        /* to avoid failure when reading next value */
			if (head == ff->tail) {			/* is queue empty or tail falling behind? */
				if (next == fifo_end(ff))	/* is queue empty?             */
					return 0;				/* queue is empty; return NULL */
                /* tail is pointing to head in a non empty queue, */
                /* try to set tail to the next cell               */
				CAS2 (&ff->tail, head, ic, next, ic+1);
			}
			else if (next != fifo_end(ff)) { /* if we are not competing on the dummy cell */
                /* and we try to set head to the next cell */
				done = CAS2 (&ff->head, head, oc, next, oc+1);
			}
		}
	} while (!done);
	msAtomicDec (&ff->count);
	if (head == &ff->dummy) {
		fifoput(ff,head);
		head = fifoget(ff);
	}
#ifdef __trace__
fprintf(stdout, "fifoget %p %p %ld\n", ff, head, ff->count.value);
fflush(stdout);
#endif
	return (fifocell *)head;
}

//----------------------------------------------------------------
/*	fifoavail returns a pointer to the "first" cell in the fifo
	if it is not empty. This is meaningful if there is only one
	reader for the fifo. The pointer returned may actually be the 
	second cell of the fifo to skip the dummy cell.
	It ensures that : fifoavail(ff) == fifoget(ff)
*/
fifocell* fifoavail (fifo* ff) 
{
	/* simulated atomic read of the required fields*/
	while (1) {
		atomic_long count = ff->oc;
		fifocell * 	hd  = ff->head;
		fifocell*	n   = hd->link;
		fifocell*	tail= ff->tail;
		
		if ((hd == ff->head) && (count == ff->oc)) {
			/*	no cells were removed during reading, therefore
				we have coherent (but maybe outdated) data*/
			return (hd==tail) ? 0 : (hd==&ff->dummy) ? n : hd;
		}
	}
	return 0;	/* never used !*/
}

//----------------------------------------------------------------
fifocell* fifoflush (fifo* ff) 
{
	fifocell	*next, *cur;
	fifocell* first;
	if (ff->head==0) return 0;	
	
	first = fifoget(ff);
	if (first==0) return 0;	
	cur = first;	
	while ((next = fifoget(ff))) {
		cur->link = next;
		cur = next;
	}
	cur->link = 0;
#ifdef __trace__
printf("fifoflush %p %p %ld\n", ff, first, ff->count.value);
#endif
	return first;
}

//----------------------------------------------------------------
/* fifoclear is now obsolete
fifocell* fifoclear (fifo* ff) 
{
	fifocell* head = ff->head;
	fifocell* tail = ff->tail;
	fifoinit(ff);
	if (tail) tail->link = 0;
	return head;
}
*/
