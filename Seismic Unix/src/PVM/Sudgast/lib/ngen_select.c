/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	select.c
 *
 *  purpose:	choose a new population
 *
 *  modified:	10 sep 90: include ranking option, handle max/min option.
 *		09 oct 90: emulate steady state when gapsize is 2/Popsize.
 */

#include "extern.h"
#include "pvm.h"

/*
    Related to random generation
*/
extern double Rand();
extern int Randint();

Select()
{
	static firstflag = 1;	/* indicates first execution		*/
	static int *sample;	/* pointers to Selected structures	*/
	double expected;	/* expected number of offspring		*/
	double factor;		/* normalizer for expected value        */
	double perf;		/* next best perf (for ranking)		*/
	double ptr;		/* determines fractional selection	*/
	double rank_max;	/* max number of offspring under ranking */
	double sum;             /* control for selection loop           */
	int best;		/* index of next best structure		*/
	register int i;		/* loop control				*/
	register int j;		/* loop control				*/
	register int k;		/* loop control				*/
	register int temp;	/* used for swapping pointers		*/

	Trace("Select entered");
	Dtrace("select");

	if (firstflag)
	{
		sample = (int *) calloc((unsigned) Popsize, sizeof(int));
		firstflag = 0;
	}

	if (Rankflag)
	{
		/* Assign each structure its rank within the population. */
		/* rank = Popsize-1 for best, rank = 0 for worst	*/
		/* Use the Needs_evaluation field to store the rank	*/

		/* clear the rank fields */
		for (i=0; i<Popsize; i++)
			Old[i].Needs_evaluation = 0;

		for (i=0; i < Popsize-1; i++)
		{
			/* find the ith best structure */
			best = -1;
			perf = 0.0;
			for (j=0; j<Popsize; j++)
			{
				if (Old[j].Needs_evaluation == 0 &&
				      (best == -1 || BETTER(Old[j].Perf,perf)))
				{
					perf = Old[j].Perf;
					best = j;
				}
			}
			/* mark best structure with its rank */
			Old[best].Needs_evaluation = Popsize -1 - i;
		}			
		/* normalizer for ranking selection probabilities */
		rank_max = 2.0 - Rank_min;
		factor = (rank_max - Rank_min) / (double) (Popsize -1);
	}
	else
	{
		/* normalizer for proportional selection probabilities */
		factor = Maxflag ? 1.0/(Ave_current_perf - Worst) :
				   1.0/(Worst - Ave_current_perf);
	}

	/* Stochastic universal sampling algorithm by James E. Baker */

	k=0;		/* index of next Selected structure */

	ptr = Rand();   /* spin the wheel one time */
	for (sum=i=0; i < Popsize; i++)
	{
		if (Rankflag)
		{
			expected = Rank_min + Old[i].Needs_evaluation * factor;
		}
		else
		{
			if (Maxflag) {
				if (Old[i].Perf > Worst)
				  expected = (Old[i].Perf - Worst) * factor;
				else expected = 0.0;
			}
			else {
				if (Old[i].Perf < Worst)
				  expected = (Worst - Old[i].Perf) * factor;
				else expected = 0.0;
			}
		}

		for (sum += expected; sum > ptr; ptr++){
/*
    Note that when expected is high, for higher performance members, a big 
    number is added in sum. It is more likely that in order to ptr > sum, 
    for stopping this loop, I need to increase ptr maybe more than on time

    In the case [.75, 1.25] are rank_min and rank_max, sum will be increased
    by 1.25 for the best member and by .75 for the worst member

    Experiments show that if I assign 0 to rank_min, I am going just to select
    the best guy in the population
*/
		  sample[k++] = i;
		}
	}

	if (k != Popsize) {
	  printf("select: Help! %d samples selected instead of %d\n", k, Popsize);
	  abort();
	}

	/* randomly shuffle pointers to new structures */
	for (i=0; i<Popsize; i++)
	{
		j = Randint(i,Popsize-1);
		temp = sample[j];
		sample[j] = sample[i];
		sample[i] = temp;
	}

	if (Gapsize<1.0)		/* Generation Gap */ 
		Gap(sample);

	/* finally, form the new population */
	for (i=0; i<Popsize; i++)
	{
		k = sample[i];
		for (j=0; j<Bytes; j++)
		{
			New[i].Gene[j] = Old[k].Gene[j];
		}
		New[i].Perf = Old[k].Perf;
		New[i].Needs_evaluation = 0;
	}

	Trace("Select completed");
}

/* Choose survivors from old population uniformly, without replacement	*/

Gap(sample)
int sample[];
{
	static firstflag = 1;
	static int *survivors;	/* a random permutation of 0 .. Popsize-1 */
	register int i,j;	/* loop control				*/
	int temp;		/* for swapping				*/

	if (firstflag)
	{
		survivors = (int *) calloc((unsigned) Popsize, sizeof(int));
		firstflag = 0;
	}

	/* construct a uniform shuffle */
	for (j=0; j<Popsize; j++) survivors[j]=j;
	for (j=0; j<Popsize; j++)
	{
		i = Randint(j, Popsize-1);
		temp = survivors[i];
		survivors[i] = survivors[j];
		survivors[j] = temp;
	}

	/* now choose survivors */
	for (i=Gapsize*Popsize; i<Popsize; i++)
		sample[i] = survivors[i];
}


/*** end of file ***/
