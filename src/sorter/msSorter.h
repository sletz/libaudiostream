/*
  MidiShare Project
  Copyright (C) Grame 1999

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#ifndef __msSorter__
#define __msSorter__


/*______________________________________________________________*/
/*                   data structures  		     	  			*/
/*______________________________________________________________*/


/*--------------------- Time stamped event -------------------- */
typedef struct TDatedEv * TDatedEvPtr;
typedef struct TDatedEv {
  TDatedEvPtr  	link;		/* next event pointer (keep first) */
  unsigned long	date;		/* 32 bits time stamping           */
} TDatedEv;


/*--------------- Fifo of time stamped events ------------------ */

typedef struct TSFifo * TSFifoPtr;
typedef struct TSFifo {			/* events fifo structure    */
 	TDatedEvPtr	first;			/* first event (keep first) */
 	TDatedEvPtr	last;			/* last event               */
} TSFifo;


/*------------------- Table of 256 Fifo ------------------------ */

typedef struct TBuffer * TBufferPtr;
typedef struct TBuffer {			/* events fifo structure    */
 	TSFifo	fifo[256];			/* first event (keep first) */
} TBuffer;


/*----------------- Structure of a sorter level ----------- */

typedef struct TLevel * TLevelPtr;
typedef struct TLevel {		
  TLevelPtr			next;		/* pointer to the next level       	*/
  TSFifoPtr			fifo;		/* pointer to the fifo to resort  	*/
  TBufferPtr		pri;		/* pointer to the primary buffer   	*/
  TBufferPtr		alt;		/* pointer to the alternate buffer 	*/
  TBuffer			buf[2];		/* the actual buffers				*/
  unsigned char 	lev;		/* corresponding level             	*/
  unsigned char 	pos;		/* resort position                 	*/
  unsigned char 	unused[2];	/* padding for alignment          	*/
} TLevel;


/*--------------------- Structure of a sorter ------------------*/

typedef struct TSorter * TSorterPtr;
typedef struct TSorter {	
  unsigned long sysDate;	/* current date of the system    */
  long			rSize;		/* amount of events to resort    */
  TLevel		level[4];	/* 0: date MSB .. 3: date LSB 	 */
  TSFifo		late;
} TSorter;

/*--------------------- the sorter interface -------------------*/
#ifdef __cplusplus
extern "C" {
#endif
		
void 		SorterInit		(TSorterPtr sb, long rs);
TDatedEvPtr	SorterClock		(TSorterPtr sb, TDatedEvPtr es);

#ifdef __cplusplus
}
#endif

#endif
