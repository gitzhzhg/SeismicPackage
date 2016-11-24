/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	init.c
 *
 *  purpose:	create an initial population of structures,
 *		and initalize some performance variables.
 *		This is called at the start of each experiment.
 *
 *  modified:	7 feb 86
 *		12 nov 66:  pass Length to Pack()
 *		23 sep 90:  handle floating point representation in initfile
 */


#include "extern.h"
#include "communication.h"
#include "pvm.h"

/*
    Related to random generation
*/

extern double Rand();
extern int Randint();

Initialize(master)
int master;
{
	FILE *fp, *fopen();
	register int i, j;	/* loop control				*/
	int idebug;		/* DEBUG 				*/
	int status;		/* indicates end of file in initfile	*/

	Trace("Initialize entered");
	Dtrace("initialize");

	if (Totalexperiments > 1)
		sprintf(Bestfile, "%s.%d", Minfile, Experiment+1);

	/* prepare for new experiment */
	Doneflag = 0;
	Curr_dump = 0;
	Bestsize = 0;
	/*Fit_variance = 0.;
	Fit_var_Begin = 0.;*/
	/* set next mutation location */
	if (M_rate < 1.0)
	  Mu_next = ceil (log(Rand()) / log(1.0 - M_rate));
	else
	  Mu_next = 1;

	Trials = Gen = 0;
	Lost = Conv = 0;
	Plateau = 0;
	Spin = 0;
	Onsum = Offsum = 0.0;
	for (i=0; i<Windowsize; i++) Window[i] = 0.0;

	/* set up initial population */

/*
     Here the master processor will send the randomly generated 
     structures to the cities. Flags Initflag and Floatflag should 
     be set mandatorily
*/
	if (verbose)
	{
        	fprintf(stderr,"Subpopulation %d will receive %d trial solutions from master process\n",instance, Popsize);
	}
/*
    this city will request members from the master
*/
        pvmBeginMessage();
        pvmSend(SEND_MEMBERS,CMASTER,master);

	if (Initflag)		/* get the population from the master */
	{
		if (Floatflag)
		{
			for (i = 0; i < Popsize; i++)
			{
			        pvmReceive(MEMBER);
                		pvmGetNDouble(Genes, Vector);
/*
    Packing the member
*/
				StringRep(Vector, Bitstring, Genes);
				Pack(Bitstring, New[i].Gene, Length);
                		New[i].Perf = 0.;
                		New[i].Needs_evaluation = 1;
    
			}
		}
		else
		{
			fprintf(stderr,"You should set Floatflag !!\n");
			abort();
		}
	}
	else
	{
		fprintf(stderr,"You should set Initflag !!\n");
		abort();
	}
	
	/********************************************************/
	/* The seed for the random number generator is saved	*/
	/* after the initialization of the first population	*/
	/* in each experiment.  The saved value is used as the	*/
	/* Seed in subsequent experiments.  The reason is:	*/
	/* often we will run several experiments with one set	*/
	/* of parameters, and compare the results with several	*/
	/* experiments run with a different set of parameters.	*/
	/* This means that for all runs which use the		*/
	/* same population size, the initial populations for	*/
	/* corresponding experiments will be the same.		*/
	/********************************************************/
	if ( Experiment > 0 ) Seed = Initseed;

	for (; i < Popsize; i++)   /* initialize remainder randomly */ 
	{
		for (j = 0; j < Length; j++)
		{
			if (Randint(0,1))
				Bitstring[j] = '1';
			else
				Bitstring[j] = '0';
		}
		Pack(Bitstring , New[i].Gene , Length);
		New[i].Needs_evaluation = 1;
	}

	Initseed = Seed;

	Trace("Initialize completed");
}

/** end of file **/
