/*
  MidiShare Project
  Copyright (C) Grame 1999

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
  
  modifications history:
  [01-10-99] DF - YO - sorter interface simplification
             externalization of the synchronisation mechanisms to provide a better
             module independance

*/

#include "msSorter.h"

/*-------------------------------------------------------------------------*/

#if defined(macintosh) || defined(__PPC__) || defined(__ppc__)  					/* Big	Endian */
	enum { kLev0, kLev1, kLev2, kLev3 };
	#define GetFifo(sorter, date, lev) (sorter->level[lev].pri->fifo[date.part[lev]])
	#define GetLevel(index) (unsigned char)index
#elif defined(__i386__) || defined(__INTEL__) || defined (_M_IX86) || defined(__x86_64__)		/* Litte Endian */
	enum { kLev3, kLev2, kLev1, kLev0 };
	#define GetFifo(sorter, date, lev) (sorter->level[3-lev].pri->fifo[date.part[lev]])
	#define GetLevel(index) 3-index;
#else
#	error "msSorter.c : cannot dertermine endianness"
#endif

typedef union {
	unsigned long date;
	unsigned char part[sizeof(long)];
} SorterDate;

#define Next(e) e->link


/*===========================================================================
  Internal macros
=========================================================================== */

#define SWAP(level) { TBufferPtr tmp; tmp=level->alt; level->alt=level->pri; level->pri=tmp; }


/*===========================================================================
  Internal functions prototypes
  =========================================================================== */
  
static void 		FifoInit	( TSFifoPtr fifo);
static void 		BufferInit	( TBufferPtr buffer);
static void 		LevelInit	( TLevelPtr level, unsigned char index);

static void 		FifoPut		( TSFifoPtr fifo, TDatedEvPtr event);
static TDatedEvPtr	BufferPut	( TBufferPtr buff, TDatedEvPtr event, unsigned char index);

static void 		Resort2		( TLevelPtr level, TSFifoPtr fifo);
static void 		ResortAll	( TLevelPtr level, TSFifoPtr fifo);
static void 		LevelClock	( TLevelPtr level, unsigned char date);

static void 		HandleInput (TSorterPtr sorter, TDatedEvPtr ev);
static void 		PutEvent (TSorterPtr sorter, TDatedEvPtr event);


/*===========================================================================
  External initialization functions implementation
  =========================================================================== */

/*-------------------------------------------------------------------------*/
/* - SorterInit - Initialize the sorter */
/*-------------------------------------------------------------------------*/
void  SorterInit (TSorterPtr sorter, long rs)
{
	unsigned char i;
	sorter->sysDate = 0;
	sorter->rSize = rs;
	FifoInit(&sorter->late);
	for (i=0;i<4;i++) {
		LevelInit (&sorter->level[i], i);
	}
}	

/*-------------------------------------------------------------------------*/
/* - SorterClock - insert events, sort and return the ready events         */
/*-------------------------------------------------------------------------*/
TDatedEvPtr SorterClock (TSorterPtr sorter, TDatedEvPtr in)
{
	TLevelPtr	lastLevel;
	TSFifoPtr	fifo, late;
	SorterDate	date;
	TDatedEvPtr out;

	HandleInput (sorter, in);
	
	date.date = sorter->sysDate += 1;
	LevelClock(&sorter->level[0], date.part[kLev0]);
	LevelClock(&sorter->level[1], date.part[kLev1]);
	LevelClock(&sorter->level[2], date.part[kLev2]);

	lastLevel = &sorter->level[3];			/* last level processing  */
	if ( !date.part[kLev3] ) {				/* check if we need to invert the Buffers */
		SWAP(lastLevel);
		lastLevel->fifo = &lastLevel->pri->fifo[0];	/* initialize Fifo to pri[0] */
	}
	fifo = lastLevel->fifo++;				/* current fifo = next fifo */
	late = &sorter->late;
	if (late->first) {
		late->last->link = fifo->first;
		out = late->first;
		FifoInit(late);
	}
	else out = fifo->first;
	FifoInit(fifo);
	return out;
}
		
/*===========================================================================
  Internal functions implementation
  =========================================================================== */
static void HandleInput (TSorterPtr sorter, TDatedEvPtr ev)
{
	TDatedEvPtr next;
	while (ev) {
		next = ev->link;
		PutEvent (sorter, ev);
		ev = next;
	}
}
	
/*-------------------------------------------------------------------------*/
static void PutEvent (TSorterPtr sorter, TDatedEvPtr event)
{
	if ( event->date > sorter->sysDate ) {
		SorterDate evDate,sysDate;
		evDate.date = event->date;
		sysDate.date = sorter->sysDate;
		if ( evDate.part[kLev0] > sysDate.part[kLev0] ) {
			FifoPut(&GetFifo(sorter,evDate,kLev0), event);
		} else if ( evDate.part[kLev1] > sysDate.part[kLev1] ) {
			FifoPut(&GetFifo(sorter,evDate,kLev1), event);
		} else if ( evDate.part[kLev2] > sysDate.part[kLev2] ) {
			FifoPut(&GetFifo(sorter,evDate,kLev2), event);
		} else {
			FifoPut(&GetFifo(sorter,evDate,kLev3), event);
		}
	}
	else {
		FifoPut(&sorter->late, event);
	}
}

/*-------------------------------------------------------------------------*/
static void FifoInit (TSFifoPtr fifo)
{
  fifo->first = 0;
  fifo->last = (TDatedEvPtr)fifo;
}

/*-------------------------------------------------------------------------*/
static void BufferInit (TBufferPtr buffer)
{
  short i = 256;

  while ( i-- ) FifoInit(&buffer->fifo[i]);
}

/*-------------------------------------------------------------------------*/
static void LevelInit (TLevelPtr level, unsigned char index)
{
	level->lev = GetLevel(index); 
	level->pri=&level->buf[0];
	level->alt=&level->buf[1];	

	/* level 3, corresponds to a date LSB - no next level */
	level->next = ( index == 3 ) ? 0 : level + 1;

	level->fifo = &level->pri->fifo[1];
	level->pos = 1;
	BufferInit(level->pri);
	BufferInit(level->alt);
}

/*-------------------------------------------------------------------------*/
static void FifoPut (TSFifoPtr fifo, TDatedEvPtr event)
{
	event->link = 0;
	fifo->last->link = event;
	fifo->last = event;
}

/*-------------------------------------------------------------------------*/
static TDatedEvPtr BufferPut (TBufferPtr Buf, TDatedEvPtr event, unsigned char index)
{
	TDatedEvPtr	nextEv;
	TSFifoPtr	fifo;
	SorterDate	date;

	date.date = event->date;
	fifo = &Buf->fifo[date.part[index]];
	Next(fifo->last) = event;
	fifo->last = event;
	nextEv = Next(event);
	Next(event) = 0;
	return nextEv;
}

/*-------------------------------------------------------------------------*/
static void Resort2 (TLevelPtr level, TSFifoPtr fifo)
{
  TDatedEvPtr event;	

  if ( fifo->first ) {
    event = BufferPut(level->alt,fifo->first,level->lev);
    if ( event ) event = BufferPut(level->alt,event,level->lev);
    fifo->first = event;
    if ( !event ) fifo->last = (TDatedEvPtr)fifo;
  }
}

/*-------------------------------------------------------------------------*/
static void ResortAll (TLevelPtr level, TSFifoPtr fifo)
{
  TDatedEvPtr event;	

  event = fifo->first;
  if ( event ) {
    do {
		event = BufferPut(level->alt,event,level->lev);
    } while ( event );
    fifo->first = 0;
    fifo->last = (TDatedEvPtr)fifo;
  }
}

/*-------------------------------------------------------------------------*/
static void LevelClock (TLevelPtr level, unsigned char date)
{
  if ( date != level->pos ) Resort2(level->next,level->fifo);
  else {
    ResortAll(level->next,level->fifo);
    if ( !level->pos ) SWAP(level);
    if ( !++level->pos ) level->fifo = &level->alt->fifo[0];
    else level->fifo++;
  }
}
