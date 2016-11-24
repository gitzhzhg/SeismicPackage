/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	cross.c
 *
 *  purpose:	perform two-point crossover on entire population
 *
 *  modified:	8 apr 86
 *
 *		13 nov 86:  perform crossover on packed structures.
 *
 *              4 august 87: add second crossover point
 *
 *              11 july 89: fix bug when xbyte1 == xbyte2 
 *                   (bug found by Jon Richardson)
 *
 *		09 oct 90: fix bug in diff
 *		     (bug found by Alden Wright and Kevin Lohn)
 */

#include "extern.h"

char premask[CHARSIZE] = { '\000', '\200', '\300', '\340',
				'\360', '\370', '\374', '\376' };

char postmask[CHARSIZE] = { '\377', '\177', '\077', '\037',
				'\017', '\007', '\003', '\001'};
/*
    Related to random generation
*/

extern int Randint();

Crossover()
{
	register int mom, dad;	/* participants in the crossover */
	register int xpoint1;	/* first crossover point w.r.t. structure */
	register int xpoint2;	/* second crossover point w.r.t. structure */
	register int xbyte1;	/* first crossed byte */
	register int xbit1;	/* first crossed bit in xbyte1 */
	register int xbyte2;	/* last crossed byte */
	register int xbit2;	/* last crossed bit in xbyte2 */
	register int i;		/* loop control variable */
	register char temp;	/* used for swapping alleles */
	static int last;	/* last element to undergo Crossover */
	int diff;		/* set if parents differ from offspring */
	char *kid1;		/* pointers to the offspring */
	char *kid2;
	static int firstflag = 1;

	Trace("Crossover entered");
	Dtrace("crossover");

	if (firstflag)
	  {
	    last = (C_rate*Popsize*Gapsize) - 0.5 ;
	    firstflag = 0;
	  }

	for (mom=0; mom < last ; mom += 2)
	{
		dad = mom + 1;

		/* kids start as identical copies of parents */
		kid1 = New[mom].Gene;
		kid2 = New[dad].Gene;

		/* choose two Crossover points */
		xpoint1 = Randint(0,Length);
		xpoint2 = Randint(0,Length-1);

		/* guarantee that xpoint1 < xpoint2 */
		if (xpoint2 >= xpoint1) 
			xpoint2++;
		else
		{
			i = xpoint1;
			xpoint1 = xpoint2;
			xpoint2 = i;
		}

		xbyte1 = xpoint1 / CHARSIZE;
		xbit1 = xpoint1 % CHARSIZE;
		xbyte2 = xpoint2 / CHARSIZE;
		xbit2 = xpoint2 % CHARSIZE;

		/* do parents differ outside cross segment? */
		diff = 0;
		for (i=0; i < xbyte1; i++) diff += (kid1[i] != kid2[i]);
		diff += ( (kid1[xbyte1] & premask[xbit1]) !=
			  (kid2[xbyte1] & premask[xbit1]) );
		diff += ( (kid1[xbyte2] & postmask[xbit2]) !=
			  (kid2[xbyte2] & postmask[xbit2]) );
		for (i=xbyte2+1; i < Bytes; i++) diff += (kid1[i] != kid2[i]);

		if (diff)	/* they do */
		{
			/* perform crossover */
			temp = kid1[xbyte1];
			kid1[xbyte1] = (kid1[xbyte1] & premask[xbit1]) |
					(kid2[xbyte1] & postmask[xbit1]);

			kid2[xbyte1] = (kid2[xbyte1] & premask[xbit1]) |
					(temp & postmask[xbit1]);

			diff  = ((kid1[xbyte1] & postmask[xbit1]) !=
				 (kid2[xbyte1] & postmask[xbit1]) );

			for (i=xbyte1 + 1; i < xbyte2; i++)
			{
				temp = kid1[i];
				kid1[i] = kid2[i];
				kid2[i] = temp;
				diff += (kid1[i] != kid2[i]);
			}

			if (xbyte1 < xbyte2)
			{
			        temp = kid1[xbyte2];
			        kid1[xbyte2] = (kid1[xbyte2] & postmask[xbit2]) |
					(kid2[xbyte2] & premask[xbit2]);

			        kid2[xbyte2] = (kid2[xbyte2] & postmask[xbit2]) |
					(temp & premask[xbit2]);

			        diff  += ((kid1[xbyte2] & premask[xbit2]) !=
				        (kid2[xbyte2] & premask[xbit2]) );
			}
			else
			{
			        temp = kid1[xbyte2];
			        kid1[xbyte2] = (kid1[xbyte2] & premask[xbit2]) |
					(kid2[xbyte2] & postmask[xbit2]);

			        kid2[xbyte2] = (kid2[xbyte2] & premask[xbit2]) |
					(temp & postmask[xbit2]);

			        diff  = ((kid1[xbyte2] & postmask[xbit1] & premask[xbit2]) !=
				        (kid2[xbyte2] & postmask[xbit1] & premask[xbit2]) );

			}

			if (diff)	/* kids differ from parents */
			{
				/* set evaluation flags */
				New[mom].Needs_evaluation = 1;
				New[dad].Needs_evaluation = 1;
			}
		}
	}
	Trace("Crossover completed");
}

/** end of file **/
