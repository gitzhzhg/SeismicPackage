/* WBUFFALLOC: $Revision: 1.2 $ ; $Date: 1997/01/10 18:39:58 $	*/

/*********************** self documentation **********************/
/*************************************************************************
WBUFFALLOC -  routines to allocate/initialize and free buffers in wavelet
		packet compresson codes

buffAlloc1 - allocate a 1D  buffer of size n and initialize
buffRealloc1 - reallocate a 1D buffer of size to n and set the position 
		to end of buffer if n is less than buff->pos 
buffFree1 - free a 1D buffer
buffAlloc2 - allocate an array[n1] of buffers each with size n2 and initialize
buffFree2 - free a 2d buffer

*************************************************************************
Function Prototypes:
void *buffAlloc1(int n);
void buffRealloc1(void *inbuff, int n);
void buffFree1(void *inbuff);
void **buffAlloc2(int n1, int n2);
void buffFree2(void **inbuff, int n1)

*************************************************************************
buffAlloc1:
Input:
n		size of buffer
Returns:
(void *)buff

buffRealloc1:
inbuff		input buffer
n		size of input buffer

buffFree1:
Input:
inbuff		buffer to be freed

buffAlloc2:
Input:
n1		buffer size in fast dimension
n2		buffer size in slow dimension

*************************************************************************
Author:  CWP: Tong Chen, 1995
*************************************************************************/
/**************** end self doc ********************************/

#include "wpc1.h"
#include "wpc1lib.h"

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
