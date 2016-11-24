/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	evaluate.c
 *
 *  purpose:	evaluate the current population by
 *		calling the user-defined function "eval"
 *
 *  modified:	13 feb 86
 *		12 nov 86: pass Length to Unpack()
 *		15 sep 90: handle floating pt representation,
 *		           change args to eval
 */

#include "extern.h"
#include "pvm.h"
extern void eval(); 

Evaluate()
{
	register double performance;
	register int i;
	register how_many;	/* # of models to evaluate	*/

	int idebug;		/* DEBUG */

	Trace("Evaluate entered");
	Dtrace("evaluate");

	for (how_many=0, i=0; i<Popsize; i++)
	{
		if ( New[i].Needs_evaluation )
		{
			Unpack(New[i].Gene, Bitstring, Length);
			if (Floatflag)
			{
				FloatRep(Bitstring, to_be_calculated[how_many], Genes);
				how_many++;
			}
		}
	}

	eval(how_many, Genes);

	for (how_many=0, i=0; i < Popsize; i++)
	{
		if ( New[i].Needs_evaluation )
	 	{	
			New[i].Perf = eval_returned[how_many];
			how_many++;
			performance = New[i].Perf;
			New[i].Needs_evaluation = 0;
			Trials++;
			TrialTotal++;
			Spin = 0;   /* we're making progress */
			if (Savesize)  Savebest(i);

			if (Trials == 1)
				Best = performance;
			if (BETTER(performance, Best))
			{
				Best = performance;
			}

			Onsum += performance;
			Offsum += Best;
		}
	}

	Trace("Evaluate completed");
}


/** end of file **/
