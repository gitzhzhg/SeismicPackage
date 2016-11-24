/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*************************************************************************
BUFFALLOC - routines to ALLOCate/initialize and free BUFFers

buffAlloc1      allocate a 1D buffer
buffRealloc1    reallocate a 1D buffer
buffFree1       free a 1D buffer
buffAlloc2      allocate a 2D buffer
buffFree2       free a 2D buffer

**************************************************************************
Function Prototypes:
memBUFF *buffAlloc1(int n);
void buffRealloc1(memBUFF *buff, int n);
void buffFree1(memBUFF *buff);
memBUFF **buffAlloc2(int n1, int n2);
void buffFree2(memBUFF **buff, int n1);

**************************************************************************
buffAlloc1:
Input:
n	size of buffer
Returns:
pointer to type memBUFF

buffRealloc1:
Input:
buff	pointer to buffer
n	new size of buffer

buffFree1:
Input:
buff	pointer to buffer

buffAlloc2:
Input:
n1	 size of buffer in fast dimension
n2	 size of buffer in slow dimension
Returns:
pointer to type memBUFF

**************************************************************************
Author:      Tong Chen
*************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"

memBUFF *buffAlloc1(int n)
/*************************************************************************
allocate a 1D  buffer of size n and initialize
*************************************************************************/
{
	memBUFF *buff;

	buff = (memBUFF *) malloc(sizeof(memBUFF));

	buff->code = (unsigned char *) malloc(n*sizeof(char));

	/* init the position and bound */
	buff->mbound = n;
	buff->pos = 0;

	return (buff);
}


void buffRealloc1(memBUFF *buff, int n)
/*************************************************************************
reallocate a 1D buffer of size to n and set the position to end of buffer
if n is less than buff->pos 
*************************************************************************/
{
	unsigned char *code;
	
	code = buff->code;

	buff->code = (unsigned char *) realloc(code, n*sizeof(char));

	/* modify the bound */
	buff->mbound = n;
	buff->pos = (buff->pos > n)? n : buff->pos;
}


void buffFree1(memBUFF *buff)
/*************************************************************************
free a 1D buffer
*************************************************************************/
{
	free((void *) buff->code);
	free((void *) buff);
}


memBUFF **buffAlloc2(int n1, int n2)
/*************************************************************************
allocate an array[n1] of buffers each with size n2 and initialize
*************************************************************************/
{
	memBUFF **buff;
	int i;

	buff = (memBUFF **) malloc(n1*sizeof(memBUFF *));

	/* the buffers do not have to be consective */
	for(i=0; i<n1; i++) 
	    buff[i] = (memBUFF *) buffAlloc1(n2);

	return (buff);
}


void buffFree2(memBUFF **buff, int n1)
/*************************************************************************
free a 2D buffer
*************************************************************************/
{
	int i;

	for(i=0; i<n1; i++)
	    buffFree1((void *) buff[i]);	
	free(buff);
}

