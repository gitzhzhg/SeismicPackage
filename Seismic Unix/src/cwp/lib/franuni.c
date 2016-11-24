/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
FRANUNI - Functions to generate a pseudo-random float uniformly distributed
	on [0,1); i.e., between 0.0 (inclusive) and 1.0 (exclusive)

franuni		return a random float
sranuni		seed random number generator

******************************************************************************
Function Prototypes:
float franuni (void);
void sranuni (int seed);

******************************************************************************
franuni:
Input:		(none)
Returned:	pseudo-random float

sranuni:
seed		different seeds yield different sequences of random numbers.

******************************************************************************
Notes:
Adapted from subroutine uni in Kahaner, Moler, and Nash (1988). 
This book references a set of unpublished notes by
Marsaglia.

According to the reference, this random
number generator "passes all known tests and has a period that is ...
approximately 10^19".

******************************************************************************
References:
"Numerical Methods and Software", D. Kahaner, C. Moler, S. Nash,
Prentice Hall, 1988. 

Marsaglia G., "Comments on the perfect uniform random number generator",
Unpublished notes, Wash S. U.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 12/30/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

/* constants used to generate random numbers (16777216=2^24) */
#define CS 362436.0/16777216.0
#define CD 7654321.0/16777216.0
#define CM 16777213.0/16777216.0
#define NBITS 24

/* internal state variables */
static int i=16,j=4;
static float c=CS;
static float u[]={
	0.8668672834288,  0.3697986366357,  0.8008968294805,
	0.4173889774680,  0.8254561579836,  0.9640965269077,
	0.4508667414265,  0.6451309529668,  0.1645456024730,
	0.2787901807898,  0.06761531340295, 0.9663226330820,
	0.01963343943798, 0.02947398211399, 0.1636231515294,
	0.3976343250467,  0.2631008574685
};

float
franuni (void)
/*****************************************************************************
return a pseudo-random float between 0.0 (inclusive) and 1.0 (exclusive)
******************************************************************************
Returned:	pseudo-random float
*****************************************************************************/
{
	float uni;

	/* basic generator is Fibonacci */
	uni = u[i]-u[j];
	if (uni<0.0) uni += 1.0;
	u[i] = uni;
	i--;
	if (i<0) i = 16;
	j--;
	if (j<0) j = 16;
	
	/* second generator is congruential */
	c -= CD;
	if (c<0.0) c += CM;
	
	/* combination generator */
	uni -= c;
	if (uni<0.0) uni += 1.0;
	return uni;
}

void
sranuni (int seed)
/*****************************************************************************
seed random number generator
******************************************************************************
Input:
seed		different seeds yield different sequences of random numbers.
*****************************************************************************/
{
	int ii,jj,i1,j01,k1,l1,m1;
	float s,t;
	
	/* convert seed to four smallish positive integers */
	i1 = (ABS(seed)%177)+1;
	j01 = (ABS(seed)%167)+1;
	k1 = (ABS(seed)%157)+1;
	l1 = (ABS(seed)%147)+1;
	
	/* generate random bit pattern in array based on given seed */
	for (ii=0; ii<17; ii++) {
		s = 0.0;
		t = 0.5;
		
		/* loop over bits in the float mantissa */
		for (jj=0; jj<NBITS; jj++) {
			m1 = (((i1*j01)%179)*k1)%179;
			i1 = j01;
			j01 = k1;
			k1 = m1;
			l1 = (53*l1+1)%169;
			if (((l1*m1)%64)>=32) s += t;
			t *= 0.5;
		}
		u[ii] = s;
	}
	
	/* initialize generators */
	i = 16;
	j = 4;
	c = CS;
}
