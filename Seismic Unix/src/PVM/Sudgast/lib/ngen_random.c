/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*************************************************************/
/*                                                           */
/*  Copyright (c) 1993                                       */
/*  Wenceslau Gouveia                                        */
/*  Center for Wave Phenomena      	                     */
/*  Colorado School of Mines                                 */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:	genesis_random.c
 *
 *  author:     Wenceslau Gouveia	
 *
 *  created:	1993
 *
 *  purpose:    Alternative random generators for GENESIS
 *
 */

#include "cwp.h"

extern float franuni();

double Rand()
{
	double random;

	random = (double) franuni();
	return(random);
}

int Randint(low,high)
int low, high;
{
	float random;
	int intrandom;
	
	random = franuni();
	intrandom = (int) (random * (high - low + 1) + low);
	return (intrandom);
}

double Randdouble(dlow,dhigh)
double dlow, dhigh;
{
	float random;
	double drandom;
	
	random = franuni();
	drandom = (double) (random * (dhigh - dlow) + dlow);
	return (drandom);
}
