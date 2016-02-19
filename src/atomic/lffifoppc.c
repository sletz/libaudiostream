/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#include "lffifo.h"

#define fifo_end(ff)	(fifocell *)ff

//----------------------------------------------------------------
void fifoinit (fifo* ff)
{
	ff->count.value = 0;
	ff->dummy.link = fifo_end(ff);
	ff->head = ff->tail = &ff->dummy;
}

//----------------------------------------------------------------
atomic_long fifosize (fifo * ff)
{
    return ff->count.value;
}

//----------------------------------------------------------------
void fifoput (fifo * ff, fifocell * cl) 
{
	fifocell * volatile tail;

    cl->link = fifo_end(ff);	/* set the cell next pointer to the end marker */
	while (1) {
		tail = ff->tail;		/* read the tail cell */
		/* try to link the cell to the tail cell */
		if (CAS (&tail->link, fifo_end(ff), cl)) {
			break;		
        }
        else {
            /* tail was not pointing to the last cell, try to set tail to the next cell */
            CASLNE (&ff->tail, (void *)tail, fifo_end(ff));
        }
	} 
	CASLNE (&ff->tail, (void *)tail, fifo_end(ff));
	msAtomicInc (&ff->count);
}

//----------------------------------------------------------------
fifocell * fifoget (fifo * ff) 
{
	fifocell * volatile head;
	fifocell * next;
	short done = 0;
	
	do {
		LWARX (&ff->head);
        head = ff->head;				/* read the head cell */
		next = head->link;				/* read the next cell */
		/*
		  WARNING: the next pointer still needs to be checked before reading its value 
		*/
		if (head == ff->tail) {			/* is queue empty or tail falling behind? */
			if (head->link == fifo_end(ff) && STWCX (&ff->head, (void *)head, (void *)head)) /* is queue really empty? */
				return 0;				/* queue is empty; return NULL */
			/* tail is pointing to head in a non empty queue, */
			/* try to set tail to the next cell               */
			CASLNE (&ff->tail, (void *)head, fifo_end(ff));
		}
		else if (next != fifo_end(ff)) { /* if we are not competing on the dummy cell */
			/* and we try to set head to the next cell */
			done = STWCX (&ff->head, (void *)head, next);
		}
	} while (!done);
	msAtomicDec (&ff->count);
	if (head == &ff->dummy) {
		fifoput(ff,head);
		head = fifoget(ff);
	}
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
		LWARX (&ff->head);
		fifocell* 	hd  = ff->head;
		fifocell*	n   = hd->link;
		fifocell*	tail= ff->tail;
		
		if (STWCX (&ff->head, (void *)hd, hd)) {
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
	fifocell* next, *cur;
	fifocell* first;
		
	first = fifoget(ff);
	if (first==0) return 0;	
	cur = first;	
	while ((next = fifoget(ff))) {
		cur->link = next;
		cur = next;
	}
	cur->link = 0;
	return first;
}

//----------------------------------------------------------------
/* fifoclear is now obsolete
fifocell* fifoclear (fifo* ff) 
{
	fifocell* head = ff->head;
	fifocell* tail = ff->tail;
	fifoinit(ff, 0);
	if (tail) tail->link = 0;	
	return head;
}
*/
