/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	mutate.c
 *
 *  purpose:	Perform mutation on the current population.
 *
 *		The global variable Mu_next indicates the position of
 *		next mutation, treating the entire population as a linear
 *		array of positions.  
 *
 *  modified:	7 feb 98
 *
 *		12 nov 86: pass length to Pack() and Unpack()
 */

#include "extern.h"

/*
    Related to random generation
*/
extern double Rand();
extern int Randint();

Mutate()
{
	static int bits;	/* number of bits per pop */
	register int i;		/* index of the Mutated structure */
	register int j;		/* position within the structure */
	register char k;	/* a random allele */
	register int open;	/* currently Unpacked Gene */
	static int firstflag = 1;

	Trace("Mutate entered");
	Dtrace("mutation");

	if (firstflag)
	{
	    bits = Gapsize*Popsize*Length + 0.5;
	    firstflag = 0;
	}

	if (M_rate > 0.0)
	{
		open = -1;
		while (Mu_next<bits)
		{
			i = Mu_next/ Length;	/* Mutated structure */
			j = Mu_next % Length;	/* Mutated position */

			if (open != i)   /* need to Unpack structure i */
			{
				Unpack (New[i].Gene , Bitstring, Length);
				open = i;
			}

			/* choose a random allele */
			if (Randint(0,1))
				k = '1';
			else
				k = '0';

			if (k != Bitstring[j])  /* it's an effective mutation */
			{
				Bitstring[j] = k;
				New[i].Needs_evaluation = 1;
			}

			if (New[i].Needs_evaluation)
				Pack ( Bitstring , New[i].Gene , Length);

			/* update next mutation location */
			if (M_rate < 1.0)
				Mu_next += ceil (log(Rand()) / log(1.0 - M_rate));
			else
				Mu_next += 1;
		}

		/* adjust Mu_next for next Generation */
		Mu_next -= bits;

	}

	Trace("Mutate completed");
}


/** end of file **/
