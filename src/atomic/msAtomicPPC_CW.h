/*
  MidiShare Project
  Copyright (C) Grame 1999-2002

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#ifndef __msAtomicPPC_CW__
#define __msAtomicPPC_CW__

//----------------------------------------------------------------
// Memory reservation only
//----------------------------------------------------------------
inline void LWARX (register volatile void * addr) 
{
	register long tmp;
	asm {
        lwarx	tmp, 0, addr       /* creates a reservation on addr  */
 	}
}

//----------------------------------------------------------------
// Store conditionnal
//----------------------------------------------------------------
inline int STWCX (register volatile void * addr, register void * value, register void * newvalue) 
{
    register int result;
	register long tmp;
	asm {
		lwz		tmp, 0(addr)        /* load value in pointed by addr  */
		cmpw	tmp, value	        /* test value at addr             */
		bne-	failed      
        sync                     	/* synchronize instructions       */
		stwcx.	newvalue, 0, addr   /* if the reservation is not altered */
									/* stores the new value at addr   */
		bne-	failed      
        li      result, 1  
		bl		atomic_exit      
	failed: 
        li      result, 0
	atomic_exit:
 	}
    return result;
}

//----------------------------------------------------------------
// Compare and swap
//----------------------------------------------------------------
inline int CAS (register volatile void * addr, register void * value, register void * newvalue) 
{
	register int result;
	register long tmp;
	asm {
    loop:
        lwarx	tmp, 0, addr       /* creates a reservation on addr  */
        cmpw	tmp, value         /* test value at addr             */
        bne-	failed
        sync          	           /* synchronize instructions       */
        stwcx.	newvalue, 0, addr  /* if the reservation is not altered */
                                   /* stores the new value at addr   */
        bne-	failed
        li      result, 1
        bl		atomic_exit
	failed:
        li      result, 0
    atomic_exit:
	}
	return result;
}

//----------------------------------------------------------------
// Compare and swap link if not equal to eq
//----------------------------------------------------------------
inline int CASLNE (register volatile void * addr, register void * value, register void * equal) 
{
	register int result;
	register long tmp, link;
	asm {
		lwarx	tmp, 0, addr    	/* creates a reservation on addr  */
		cmpw	tmp, value	    	/* test value at addr             */
		bne-	failed          	/* fails if not equal to value    */
		lwzx	link, 0, value  	/* load the link pointed by value */
		cmpw	link, equal	    	/* test if equal to equal         */
		beq-	failed     
        sync                    	/* synchronize instructions       */
		stwcx.	link, 0, addr 		/* if the reservation is not altered */
									/* stores the new value at addr   */
        bne-	failed
        li      result, 1
        bl		atomic_exit
	failed:
        li      result, 0
    atomic_exit:
 	}
	return result;
}

#endif
