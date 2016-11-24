/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	generate.c
 *
 *  purpose:	One generation consists of
 *			(1) forming a new population of structures.
 *			(2) evaluating the population.
 *			(3) gathering performance statistics.
 *
 *  modified:	7 feb 86
 *
 *		2 dec 86: call Measure() before Done() so that
 *		we can quit upon convergence.  Measure() nows
 *		calls Converge().
 */

#include "extern.h"
#include "pvm.h"

extern int  Done();
extern int  Go_uphill();
extern void Smooth();
extern void eval();

Generate(current_evolution, master)
int current_evolution;
int master;
{
	static int rsflag = 1;	/* flag cleared after restart		*/

	STRUCTURE *temp;	/* for swapping population pointers	*/
	register int i;		/* for marking structures		*/
	register int j;		/* counter				*/
	int imember;		/* member to be uphilled		*/
	int n_of_iter;		/* # of uphill iterations		*/
	int how_many;		/* # of members to evaluate		*/
	int idebug;		/* debugging purposes			*/
	double Perf;		/* for the conj. grad. output	        */	

	if (Traceflag)
		printf("                    Gen %d\n",Gen);
	Trace("Generate entered");

	/* create a new population */

	if (Restartflag && rsflag)
	{
		/* this is a restart so read checkpoint file */
		rsflag = 0;	/* disable local restart flag. */
		Converge();
	}
        else if (Gen == 0 && current_evolution == 0)

                /* this is a fresh experiment */

        {
		if (verbose)
		{	
                	fprintf(stderr,"Subpopulation %d will begin %d evolutions\n",instance, Numevolutions);
		}

                Initialize(master);   /* form an initial population */
                Spin++;
        }
        else if(Gen == 0 && current_evolution != 0)
        {

/*
     Exchange of members from different cities will be performed now
     but first, if required by the user, a conjugated gradient
     opt. procedure will be performed on all the members of the  
     subpopulation
*/

                if (Uphillflag && Floatflag && current_evolution >= Min_evol_cg)
                {
			if (verbose)
			{
                        	fprintf(stderr,"\nSubpopulation %d will perform uphill for all members\n",instance);
			}

                        for (imember = 0; imember < Popsize; imember++)
                        {
                        	Unpack(New[imember].Gene, Bitstring, Length);
                        	FloatRep(Bitstring, for_conjg[imember], Genes);
                        	Perf_past[imember] = New[imember].Perf;
                        }

                        n_of_iter = Go_uphill(for_conjg, Perf_past, Genes);

			if (verbose)
			{
				fprintf(stderr,"Subpopulation %d performed %d iterations in the conjugate gradient\n", instance, n_of_iter);
			}
/*
    Now dealing with the dawn effects of quantization
    note that I replace the original imember
*/
                        for (how_many = 0, imember = 0; imember < Popsize; imember++)
                        {
                        	if (Perf_past[imember] < New[imember].Perf)
                        	{
                        		StringRep(for_conjg[imember], Bitstring, Genes);
                        		FloatRep(Bitstring, for_conjg[imember], Genes);

                        		for (j = 0; j < Genes; j++)
                        			to_be_calculated[how_many][j] = for_conjg[imember][j];
                        		how_many++;
                        	}
                        }
/*
    Computing the objective function
*/
			eval(how_many, Genes);

			for (how_many = 0, imember = 0; imember < Popsize; imember++)
			{
				if (Perf_past[imember] < New[imember].Perf)
				{
					if (eval_returned[how_many] < New[imember].Perf)
					{
                                   	   New[imember].Perf = eval_returned[how_many];
                                   	   New[imember].Needs_evaluation = 0;
                                   	   StringRep(to_be_calculated[how_many], Bitstring, Genes);
                                   	   Pack(Bitstring, New[imember].Gene, Length);
					}
					how_many++;
				}
			}
                }

/* Smoothing the models */
		if (smooth)
                	Smooth();

		if (verbose && Ncities > 1)
		{
        		fprintf(stderr,"\nSubpopulation %d will exchange members\n",instance);
		}

                if (Ncities > 1) Exchange_members();
/*
     TRIALS is set to zero. A new population will be processed, due
     the exchange of members and hillclimbing , but new evaluations are not 
     necessary, since these were done at the respective cities
*/
                /*Trials = Popsize;*/
		Trials = 0;  /* ???? 21 - mar */
                Spin++;
        }
	else
		/* form a new population from */
		/* the old one via genetic operators */
	{
		if (verbose)
		{
			fprintf(stderr,"Subpopulation %d performed %d iterations at evolution %d in the GA\n", instance, TrialTotal, current_evolution);
		}

		Select();
		Mutate();
		Crossover();

		if (Eliteflag)
			Elitist();
		if (Allflag)	/* mark structures for evaluation */
			for (i=0; i<Popsize; i++) New[i].Needs_evaluation = 1;
		Spin++;
	}

	/* evaluate the newly formed population */
	Evaluate();
	/* gather performance statistics */
	Measure(current_evolution);
	/* check termination condition for this experiment	*/
	Doneflag = Done();


	/* swap pointers for next generation UNLESS doneflag is set */
	if (!Doneflag)
	{
/*
    * IMPORTANT *
    This swapping should be done just when the EXCHANGE MEMBERS AMONG
    PROCESSORS  *I*S* *N*O*T* the next procedure to happen. If the
    exchange will happen next I need all the current population pointed
    by NEW
*/
		temp = Old;
		Old = New;
		New = temp;
	}

	/* update generation counter */
	Gen++;
        GenTotal++;     /* generation counter for all evolutions */

	Trace("Generate completed");
}

/*  end of file */
