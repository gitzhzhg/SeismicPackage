/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	main.c
 *
 *  purpose:	main program for genesis.
 *
 *  modified:	28 mar 86
 *		15 sep 90 - to use display routines
 */

#include "global.h"
#include "pvm.h"

genesis_main(s, master)
char *s;
int master;
{
	FILE *fp, *fopen();
	int ievolution, ipar, imember, j;     /* counters		*/
	long clock;
	long time();
        static double M_rate_original;        /* original mutation prob.*/
	char *ctime();
	extern void die();		      /* signal handler 	*/

	/* see input.c for the use of command line args */
	genesis_input(s);

	if (Displayflag) {
		initscr();
		signal(SIGINT, die);
		clear();
		refresh();
		
		/* this point is reached only if Interflag is OFF */
		move(1,0);
		printw("run until Trials = %d", Totaltrials);
		move(1, 35);
		printw("executing: ");
		refresh();
	}

do		/* one experiment */
        {	
		if (Traceflag) 
			printf("Experiment %d\n", Experiment); 
/*
    This is the main loop for controlling the whole evolution process
*/
                M_rate_original = M_rate;    /* Original mutation proba.   */
                GenTotal = 0;                /* Generation in ALL evolutions */
		TrialTotal = 0;		     /* Trials in all evolutions  */
		ievolution = 0;

		do
                {
/*
    Reseting necessary flags
*/
                        Gen = 0;
                        Spin = 0;
                        Trials = 0;
			Lost = Conv = 0;
/*
    Note that TRIALS now stores ALL THE EVALUATIONS done in ALL EVOLUTIONS
    performed by the multipopulation scheme
*/
			do	/* see generate.c for main GA loop */
		        {
                                Generate(ievolution,master);
			}  
			while (!Doneflag);

			Check_convergence(master);

			ievolution++;
/*
    An adaptive mutation scheme.... Still experimental
*/
			/* Step mutation */
			if (ievolution < Min_evol_cg)
			 	M_rate = M_rate_original;
			else
				M_rate = M_rate_original / 10.;
		}
		while(IS_CONVERGING_FLAG && ievolution < Numevolutions);
/*
    Writing the answer, that is, the model parameters related to
    the best member
*/
                fp = fopen(Answerfile, "a");
                Unpack(New[Best_guy].Gene, Bitstring, Length);
		if (Floatflag)
			FloatRep(Bitstring, Vector, Genes);
                for (ipar = 0; ipar < Genes; ipar++)
                        fprintf(fp, "%4d  %10.7f\n", ipar, Vector[ipar]);
                fclose(fp);

		if (Traceflag)
		printf("Online %e   Offline %e    Best %e\n",
			Online, Offline, Best);

		/* accumulate performance measurements */
		Totonline += Online;
		Totoffline += Offline;
		Totbest += Best;

		/* get ready for next experiment */
		Experiment++;
		Gen = 0;
	} 
	while (Experiment < Totalexperiments);
	
	/* compute and print final performance measures */

	Totonline /= Totalexperiments;
	Totoffline /= Totalexperiments;
	Totbest /= Totalexperiments;
	if (Onlnflag)
		printf("%e\n", Totonline);
	if (Offlnflag)
		printf("%e\n", Totoffline);
	if (Bestflag)
		printf("%e\n", Totbest);
	if (Logflag)
	{
		fp = fopen(Logfile, "a");
		fprintf(fp, "Online %e    ", Totonline);
		fprintf(fp, "Offline %e   ", Totoffline);
		fprintf(fp, "Best %e\n", Totbest);
		time(&clock);
		fprintf(fp, "%s\n", ctime(&clock));
		fclose(fp);
	}

	if (Displayflag) {
		move(23,0);
		die();
	}
}

/** end of file **/
