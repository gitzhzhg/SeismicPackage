/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCBUFFAL: $Revision: 1.3 $ ; $Date: 2011/11/21 16:24:37 $	*/


/*********************** self documentation **********************/
/*************************************************************************
WPCBUFFAL - routines to allocate/initialize and free buffers

buffAlloc1 - allocate a 1D  buffer of size n and initialize
buffRealloc1 - reallocate a 1D buffer of size to n and set the 
		position to end of buffer if n is less than buff->pos 
buffFree1 -  free a 1D buffer
buffAlloc2 - allocate an array[n1] of buffers each with size n2 and
		initialize
buffFree2 -  free a 2D buffer

*************************************************************************
Function Prototypes:
void *buffAlloc1(int n)
void buffRealloc1(void *inbuff, int n)
void buffFree1(void *inbuff)
void **buffAlloc2(int n1, int n2)
void buffFree2(void **inbuff, int n1)

*************************************************************************
buffAlloc1:
Input:
n		size of buffer
Return:
pointer to (void *)

buffRealloc1:
Input:
inbuff		buffer to be reallocated
n		new size

buffFree1:
Input:
inbuff		buffer to be freed

buffAlloc2:
n1		number of buffers in the array
n2		size of each buffer

buffFree2:
inbuff		buffer array to be freed
n1		number of buffers to be freed

*************************************************************************
Author: CWP: Tong Chen, 1994
*************************************************************************/
/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"

void *buffAlloc1(int n)
/*************************************************************************
allocate a 1D  buffer of size n and initialize
*************************************************************************/
{
	wpcBUFF *buff;

	buff = (wpcBUFF *) malloc(sizeof(wpcBUFF));

	buff->code = (unsigned char *) malloc(n*sizeof(char));

	/* init the position and bound */
	buff->mbound = n;
	buff->pos = 0;

	return ((void *)buff);
}

void buffRealloc1(void *inbuff, int n)
/*************************************************************************
reallocate a 1D buffer of size to n and set the position to end of buffer
if n is less than buff->pos 
*************************************************************************/
{
	wpcBUFF *buff = (wpcBUFF *) inbuff;
	unsigned char *code;
	
	code = buff->code;

	buff->code = (unsigned char *) realloc(code, n*sizeof(char));

	/* modify the bound */
	buff->mbound = n;
	buff->pos = (buff->pos > n)? n : buff->pos;
}

void buffFree1(void *inbuff)
/*************************************************************************
free a 1D buffer
*************************************************************************/
{
	wpcBUFF *buff = (wpcBUFF *) inbuff;

	free((void *) buff->code);
	free((void *) buff);
}

void **buffAlloc2(int n1, int n2)
/*************************************************************************
allocate an array[n1] of buffers each with size n2 and initialize
*************************************************************************/
{
	wpcBUFF **buff;
	int i;

	buff = (wpcBUFF **) malloc(n1*sizeof(wpcBUFF *));

	/* the buffers do not have to be consective */
	for(i=0; i<n1; i++) 
	    buff[i] = (wpcBUFF *) buffAlloc1(n2);

	return ((void **)buff);
}

void buffFree2(void **inbuff, int n1)
/*************************************************************************
free a 2D buffer
*************************************************************************/
{
	wpcBUFF **buff = (wpcBUFF **) inbuff;
	int i;

	for(i=0; i<n1; i++)
	    buffFree1((void *) buff[i]);	
	free((void **) buff);
}
