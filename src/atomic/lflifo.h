/*

  Copyright © Grame 1999-2005

  This library is free software; you can redistribute it and modify it under 
  the terms of the GNU Library General Public License as published by the 
  Free Software Foundation version 2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
  License for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
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
# define lfCount(name) unsigned long volatile name
#else
# define lfCount(name) long name[7]
#endif

typedef struct lifocell {
	struct lifocell* volatile link;	/* next cell in the list */
									/* any data here		 */
} lifocell;

typedef struct lifo {
	lifocell * volatile top;	/* top of the stack          */
	lfCount(oc);					/* used to avoid ABA problem */
	TAtomic	count;
} lifo;


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
static inline unsigned long lfsize (lifo * lf) 	{ return lf->count.value; }

#endif
