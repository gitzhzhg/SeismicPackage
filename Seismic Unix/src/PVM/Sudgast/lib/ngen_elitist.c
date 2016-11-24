/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	elitist.c
 *
 *  purpose:	The elitist policy stipulates that the best individual
 *		always survives into the new generation.  The elite
 *		individual is placed in the last position in New pop,
 *		and is not changed through crossover or mutation.
 *
 *  modified:	24 mar 86
 */

#include "extern.h"

Elitist()
{
	register int i;		/* loop control variables */
	register int k;
	register int found;	/* set if elite one is present */


	Trace("Elitist entered");

	/* is any element in the current population		*/
	/* identical to the Best guy in the last Generation?	*/
	for (i=0, found=0; i<Popsize && (!found); i++)
		for (k=0, found=1; (k<Bytes) && (found); k++)
			found = (New[i].Gene[k] 
			    == Old[Best_guy].Gene[k]);
	if (!found)	/* elite one was not present */
	{
		/* replace last guy with the elite one */
		for (k=0; k<Bytes; k++)
			New[Popsize-1].Gene[k] = Old[Best_guy].Gene[k];
		New[Popsize-1].Perf = Old[Best_guy].Perf;
		New[Popsize-1].Needs_evaluation = 0;
		if (Traceflag)
		{
			printf("perf: %e\n", New[Popsize-1].Perf);
		}
	}

	Trace("Elitist completed");
}


/*** end of file ***/
