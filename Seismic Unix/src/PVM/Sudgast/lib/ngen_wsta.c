/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "extern.h"
#include "pvm.h"
#include "stdio.h"
#include "su.h"
#include "segy.h"
#include "header.h"
#include "values.h"

/************************************************  file ngen_wsta.c  *******/
/* 111494 */

segy tr;		/* SEGY DATA */

/*    
    Static external variables for routine eval and Go_uphill
*/

static float **CROSS;		/* crosscorrelation	*/
static int ***POINTER;		/* set of pointers	*/
static int *FOLDCMP;		/* fold of each CMP	*/
static int **DERIVATIVE;	/* to compute the derivatives */
static int ***POINT_CROSS;      /* pointers             */
static float *taper;		/* taper for trend detection */
static int **CONSIST;           /* for the penalty function */
static int n_of_consist;        /* """ """ """"""" """""""" */
static char msg[80];            /* used when printing error message     */

eval(how_many, genes)
int how_many;	/* How many members will be evaluated		*/
int genes;      /* number of elements in vect                   */
{
	int *SOURCE, *RECEIVER;		/* POINTERS		*/
	static int firstflag = 1;
	double static_j, static_i; 	/* definig total statics*/
	register int i, j, k;
	int NTRACES;			/* # of traces read	*/
	int shift;
	int ipoint;
	int isample, isource, itrace, ireceiver, icmp, cmp_current, cmp_last;
				/* counters and similar stuff		*/
	int i_to_calc;		/* defines member to be evaluated	*/
        char suff[10];          /* defines data file                    */
	FILE *fp;		/* Data file				*/

	double aux;		/* auxiliar quantity			*/
/*
    Setting the true solution
*/
	if (firstflag) 
	{
                fp = fopen(datafile,"r");

                if (fp == NULL)
                {
                        sprintf(msg, "Can't open %s", datafile);
                        Error(msg);
                }
		if (verbose)
                	fprintf(stderr,"Subpopulation %d read data file %s\n",instance, datafile);

/*
    Memory allocation for vector:
	POINTER[0...NCMP-1][0...FOLD-1][SOURCE_ID, RECEIVER_ID]
*/
		POINTER = alloc3int(2,MAXFOLD,NCMP);
		if (POINTER == NULL) Error("Allocation failed for POINTER");
/*
    Source vector used to index source statics
*/
		SOURCE = alloc1int(NSOURCES);
		if (SOURCE == NULL) Error("Allocation failed for SOURCE");
/*
    Receiver vector used to index receiver statics
*/
		RECEIVER = alloc1int(NRECEIVERS);
		if (RECEIVER == NULL) Error("Allocation failed for RECEIVER");
/*
    Foldcmp vector used for fold of each CMP 
*/
		FOLDCMP = alloc1int(NCMP);
		if (FOLDCMP == NULL) Error("Allocation failed for FOLDCMP");
/*
    Allocating memory for correlation and for consistency vector
*/
                CROSS = alloc2float(TOTAL_LAG+1, MAXFOLD*(MAXFOLD-1)/2);
                if (CROSS == NULL) Error("Allocation failed for CROSS");

                CONSIST = alloc2int(2, NSOURCES);
                if (CONSIST == NULL) Error("Allocation failed for CONSIST");

                to_filter = alloc2int(3, NRECEIVERS);
                if (to_filter == NULL) Error("Allocation failed for to_filter");

		taper = alloc1float(NSOURCES + NRECEIVERS);
                if (taper == NULL) Error("Allocation failed for taper");
/*
    Reading headers. That's just what I need here...
    And copying traces and headers to temporary files 
    NOTE THAT CMPs MUST BE IN ASCENDING ORDER 
*/
		NTRACES = 0;
		isource = 0;
		ireceiver = 0;
		icmp = -1;
		cmp_current = 0;
		cmp_last = 0;
		while (fgettr(fp,&tr)) { 
		 	cmp_current = tr.cdp;
			if (cmp_current != cmp_last)
			{
				if (icmp >= 0) FOLDCMP[icmp] = itrace;
				icmp++;
				itrace = 0;
				cmp_last = cmp_current;
			}	
		 	POINTER[icmp][itrace][0] = tr.sx;	
	                POINTER[icmp][itrace][1] = tr.gx;

			if (icmp == 0 && itrace == 0)
				SOURCE[isource] = POINTER[icmp][itrace][0];

			else if (!(icmp == 0 && itrace == 0) && SOURCE[isource] != POINTER[icmp][itrace][0])
			{
				i = isource - 1;
				while (i >= 0 && SOURCE[i] != POINTER[icmp][itrace][0])
					i--;
				if (i < 0)
				{
					isource++;
					SOURCE[isource] = POINTER[icmp][itrace][0];
				}
			}

                        if (icmp == 0 && itrace == 0)
                                RECEIVER[ireceiver] = POINTER[icmp][itrace][1];

			else if (!(icmp == 0 && itrace == 0) && RECEIVER[ireceiver] != POINTER[icmp][itrace][1])
			{
				i = ireceiver - 1;
				while (i >= 0 && RECEIVER[i] != POINTER[icmp][itrace][1])
					i--;
				if (i < 0)
				{
					ireceiver++;
					RECEIVER[ireceiver] = POINTER[icmp][itrace][1];
				}
			}

			itrace++;
			NTRACES++;
		};
		FOLDCMP[icmp] = itrace;	/* last CMP */
		fclose(fp);
		if (verbose)
		{
			fprintf(stderr,"Subpopulation %d has %d traces read in %d CMPs\n",instance,NTRACES,icmp+1);
			fprintf(stderr,"Subpopulation %d has %d source statics and %d receiver statics\n",instance,isource+1,ireceiver+1);
		}
/*
    Enhancing the pointers
*/
		for (icmp = 0; icmp < NCMP; icmp++)
		{
			for (itrace = 0; itrace < FOLDCMP[icmp]; itrace++) 
			{
				isource = 0;
				while (SOURCE[isource] != POINTER[icmp][itrace][0])
                                        isource++;

				POINTER[icmp][itrace][0] = isource;

				ireceiver = 0;
			 	while (RECEIVER[ireceiver] != POINTER[icmp][itrace][1])
					ireceiver++;	
				POINTER[icmp][itrace][1] = ireceiver;
			}
		}
/*
    Detecting where source and receiver positions are the same.
    In this case I will constrain them to be similar by a 
    Lagrange multiplier
*/
		for (n_of_consist = 0, isource = 0; isource < NSOURCES; isource++) 
		{
			ireceiver = 0;
			while (ireceiver < NRECEIVERS && SOURCE[isource] != 
							 RECEIVER[ireceiver])
				ireceiver++;

			if (ireceiver != NRECEIVERS)
			{
				CONSIST[n_of_consist][0] = isource;
				CONSIST[n_of_consist][1] = ireceiver;
/*
    Source statics pointed by isource and receiver statics pointed
    by ireceivers should be very similar 
*/
				n_of_consist++;
			}
		}
/*
    Now computing the taper for trend detection
*/
		for (i = 0; i < NSOURCES + NRECEIVERS; i++)
			taper[i] = sin(PI * i / ((float) NSOURCES + NRECEIVERS - 1));
/*
    Computing the right indexes for interpolation
*/
		for (ireceiver = 0; ireceiver < NRECEIVERS; ireceiver++)
		{
			i = 0;
			while (i < NRECEIVERS && 
			       (RECEIVER[ireceiver] - dx) != RECEIVER[i])
				i++;

                        j = 0;
                        while (j < NRECEIVERS && 
                               (RECEIVER[ireceiver] + dx) != RECEIVER[j])
                        	j++;

			if (i != NRECEIVERS)
				to_filter[ireceiver][0] = NSOURCES + i;
			else
				to_filter[ireceiver][0] = NSOURCES + ireceiver;

			if (j != NRECEIVERS)
				to_filter[ireceiver][1] = NSOURCES + j;
			else
				to_filter[ireceiver][1] = NSOURCES + ireceiver;
			
			if (i == NRECEIVERS || j == NRECEIVERS)
				to_filter[ireceiver][2] = 2; 
			else
				to_filter[ireceiver][2] = 3;
		}
/*
    Freeing memory
*/
		free1int(SOURCE);
		free1int(RECEIVER);	/* Neither for this pointers	   */

		if (genes != NSOURCES + NRECEIVERS)
			Error("Improper bit string at objective function evaluation");
		firstflag = 0;		/* end of initialization	   */
	}
/*
    Resetting...
*/

	for (i=0; i < how_many; i++)
		eval_returned[i] = 0.;
	
	rewind(Xfp);
	for (icmp = 0; icmp < NCMP; icmp++)
	{
	   fread(CROSS[0],sizeof(float),(TOTAL_LAG+1)*(MAXFOLD*(MAXFOLD-1)/2),Xfp);
	   for (i_to_calc=0; i_to_calc < how_many; i_to_calc++)
           {
		for (ipoint = 0, i = 0; i < FOLDCMP[icmp] - 1; i++)
		{
			for (j = i + 1; j < FOLDCMP[icmp]; j++, ipoint++)
			{
/*  
    Defining static shift for j-th and i-th traces
*/
			   static_j = to_be_calculated[i_to_calc][POINTER[icmp][j][0]] + to_be_calculated[i_to_calc][NSOURCES+POINTER[icmp][j][1]];
			   static_i = to_be_calculated[i_to_calc][POINTER[icmp][i][0]] + to_be_calculated[i_to_calc][NSOURCES+POINTER[icmp][i][1]];

			   shift = TOTAL_LAG/2 + NINT(static_j - static_i);
/*
    That's the lag. note the proper shift.
*/
			   eval_returned[i_to_calc] -= (double) CROSS[ipoint][shift];
			}
		}
           }
	}
	return;
}

/**** this routine will provide a conj. grad. search	       */
/**** for each subpopulation         		   Denver Basin*/

int Go_uphill(best, OBJ_PAST, genes)
/*
     element best will be a initial guess for a conjugate gradient search
*/
double **best; 		/* floating point representation                */
double *OBJ_PAST;	/* initial performances				*/
int genes;		/* # of elements in vect			*/
{
	static int firstflag = 1;
	double **GRADIENT_BEF;			/* the previous iteration */
	double **SEARCH;			/* search direction	*/
	double **GRADIENT; 			/* obvious...		*/
	double **NEW;				/* NEW: current solution*/
	double *OBJ_CUR;			/* current performance  */
	double aux;				/* auxiliar		*/
	double *temp;				/* swapping		*/
	double *descent;			/* to reset SEARCH 	*/
	double *BETA;				/* steps		*/
	double change;				/* relat change in fitness */
	double aux1, aux2;			/* auxiliary quantities */
	register int i, j, jj;			/* counters		*/
	int how_many, i_to_calc;		/* for tape management	*/
	int *DONE;				/* stopping flag per member */
	int *ITERATIONS;			/* iterations / member */
	int DONE_TOTAL=0;			/* overall stopping flag */
	int number_of_iterations=0;		/* # of iter. performed */
	int first_iter=1;			/* first iterat. flag	*/
	int ipoint_base, iparam;
	int itrace, icmp;        		/* pointers             */
/*
    Setting the pointers for the gradient calculation 
*/
	if (firstflag) 
	{
/*
    Allocating memory for DERIVATIVE, and POINT_CROSS
*/
                DERIVATIVE = alloc2int(NSOURCES + NRECEIVERS, NCMP);
                if (DERIVATIVE == NULL) Error("Allocation failed for DERIVATIVE");
		POINT_CROSS = alloc3int(MAXFOLD, NSOURCES + NRECEIVERS, NCMP);
                if (POINT_CROSS == NULL) Error("Allocation failed for POINT_CROSS");
		for (icmp = 0; icmp < NCMP; icmp++)
		{
			for (iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
			{
				itrace = 0;
				if (iparam < NSOURCES)	/* source statics */
				{
					while(itrace < FOLDCMP[icmp] && POINTER[icmp][itrace][0] != iparam)
						itrace++;
					if (itrace == FOLDCMP[icmp])
						DERIVATIVE[icmp][iparam] = -1; 
/*
    In the above situation the parameter IPARAM does not show up in any one
    of the relative shifts in this CMP
*/
					else
						DERIVATIVE[icmp][iparam] = itrace;
				}
				else			/* receiver statics */
				{
					while(itrace < FOLDCMP[icmp] && (POINTER[icmp][itrace][1] + NSOURCES) != iparam)
						itrace++;
					if (itrace == FOLDCMP[icmp])
						DERIVATIVE[icmp][iparam] = -1; 
/*
    In the above situation the parameter IPARAM does not show up in any one
    of the relative shifts in this CMP
*/
					else 
						DERIVATIVE[icmp][iparam] = itrace;
				}

				if (DERIVATIVE[icmp][iparam] != -1)
				{
					for (j = 0, ipoint_base = 0; j < itrace; j++)
	/* this will be used later */		ipoint_base += FOLDCMP[icmp] - 1 - j;

/*
    Computing pointers to use in the derivatives of the Xcorrelation. These
    pointers will able the search of the CORRECT Xcorrelation to be 
    differentiated
*/
					for (i = 0, POINT_CROSS[icmp][iparam][i] = 0; i < FOLDCMP[icmp]; i++, POINT_CROSS[icmp][iparam][i] = 0)
					{
						if (i < itrace)
						{
						/* computing POINT_CROSS */
							for (j = 0; j < i; j++)
								POINT_CROSS[icmp][iparam][i] += FOLDCMP[icmp] - 1 - j;
							POINT_CROSS[icmp][iparam][i] += itrace - i - 1;

					/* POINT_CROSS is the pointer for */
					/* computing the derivative of the */
   					/* cross_correlation 		   */
						}
						else if (i > itrace)
							POINT_CROSS[icmp][iparam][i] = ipoint_base + i - itrace - 1;
					}	/* derivative exists */
				}	/* all traces of CMP icmp */
			}	/* all parameters */
		}	/* for all cmps */
/*
    with the information computed above one should be able to
    calculate the derivatives 
*/
		firstflag = 0;		/* end of initialization	   */
	}
/*
    Necessary memory allocation
*/
        NEW = alloc2double(NSOURCES + NRECEIVERS, Popsize);
        if (NEW == NULL) Error("Allocation failed for NEW");

        GRADIENT = alloc2double(NSOURCES + NRECEIVERS, Popsize);
        if (GRADIENT == NULL) Error("Allocation failed for GRADIENT");

        GRADIENT_BEF = alloc2double(NSOURCES + NRECEIVERS, Popsize);
        if (GRADIENT_BEF == NULL) Error("Allocation failed for GRADIENT_BEF");

        SEARCH = alloc2double(NSOURCES + NRECEIVERS, Popsize);
        if (SEARCH == NULL) Error("Allocation failed for SEARCH");

        OBJ_CUR = alloc1double(Popsize);
        if (OBJ_CUR == NULL) Error("Allocation failed for OBJ_CUR");

        BETA = alloc1double(Popsize);
        if (BETA == NULL) Error("Allocation failed for BETA");

        descent = alloc1double(Popsize);
        if (descent == NULL) Error("Allocation failed for descent");

        DONE = alloc1int(Popsize);
        if (DONE == NULL) Error("Allocation failed for DONE");

        ITERATIONS = alloc1int(Popsize);
        if (ITERATIONS == NULL) Error("Allocation failed for ITERATIONS");
/*
    necessary resetting
*/
	for (i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
	{
		DONE[i_to_calc] = 0;
		BETA[i_to_calc] = 0.;
		ITERATIONS[i_to_calc] = 0;

        	for (iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
        	{       /* that's reseting  */
                	GRADIENT[i_to_calc][iparam] = 0.;
                	GRADIENT_BEF[i_to_calc][iparam] = 0.;
                	SEARCH[i_to_calc][iparam] = 0.;
        	}
	}
/*
    the gradient will be calculated now for the 1st iteration
*/
	Derivatives(DONE, GRADIENT, best);
/*
    BETA, the step to calculate the CONJUGATE GRADIENT directions
*/
	/* DONE and DONE_TOTAL control the iterations */
	while (!DONE_TOTAL)
	{
		/* search directions */
		for (i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
		{
			if (!DONE[i_to_calc])
			{
				for (aux1=0., aux2=0., iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
				{	
					aux1 += GRADIENT[i_to_calc][iparam]*(GRADIENT[i_to_calc][iparam]-GRADIENT_BEF[i_to_calc][iparam]);
					aux2 += GRADIENT_BEF[i_to_calc][iparam]*GRADIENT_BEF[i_to_calc][iparam];
				}
				BETA[i_to_calc] = aux1 / aux2;
/* 
    Computing the new search direction 
*/
				for (descent[i_to_calc] = 0., iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
				{
					SEARCH[i_to_calc][iparam] = -GRADIENT[i_to_calc][iparam] + BETA[i_to_calc]*SEARCH[i_to_calc][iparam];
					descent[i_to_calc] += GRADIENT[i_to_calc][iparam] * SEARCH[i_to_calc][iparam];
				}
				if (descent[i_to_calc] > 0. || first_iter)
				{	/* SEARCH = -GRADIENT */
					/* always true if 1st iter */
					for (descent[i_to_calc] = 0., iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
					{
						SEARCH[i_to_calc][iparam] = -GRADIENT[i_to_calc][iparam];
						descent[i_to_calc] += GRADIENT[i_to_calc][iparam] * SEARCH[i_to_calc][iparam];
					}
				}
                        }       /* closing the if for done */
                }       /* closing the i_to_calc loop */
/* 
	Doing line search
*/
                Line_Search(DONE, ITERATIONS, NEW, OBJ_CUR,
                            best, OBJ_PAST, SEARCH, descent);

		first_iter = 0;		/* no more first_iter */
/* 
     Checking if this procedure was of any good
*/
		/* Is this solution better ? */
                /* Is this solution better ? */
                if (verbose)            /* reporting first */
                {
                        for (number_of_iterations = 0,
                             i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
                                number_of_iterations += ITERATIONS[i_to_calc];
                           	fprintf(stderr,"Subpopulation %d performed %d iterations in the CG\n", instance, number_of_iterations);
                }

		for (i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
		{
			if (!DONE[i_to_calc])
			{
				/* computing relative change */
				change = ABS((OBJ_CUR[i_to_calc] - 
				      	      OBJ_PAST[i_to_calc]) / 
				      	      OBJ_CUR[i_to_calc]);
				if ((OBJ_CUR[i_to_calc] > OBJ_PAST[i_to_calc])
		        	|| (change < EPSLON))
				{
				/* stop the procedure */
					DONE[i_to_calc] = 1;
				}
				else if (ITERATIONS[i_to_calc] > max_iter)
				/* MAX # of iterations reached */
				/* stop, but swap the models */
				{
					DONE[i_to_calc] = 1;
					/* new CURRENT OF */
					OBJ_PAST[i_to_calc] = OBJ_CUR[i_to_calc];	

					/* swapping solutions and gradients*/
					/* M*U*S*T B*E like that */
		 			for (iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
						best[i_to_calc][iparam] = NEW[i_to_calc][iparam];
				}
				else
				{
					/* new CURRENT OF */
					OBJ_PAST[i_to_calc] = OBJ_CUR[i_to_calc];	
					/* swapping solutions and gradients*/
					/* M*U*S*T B*E like that */
		 			for (iparam = 0; iparam < NSOURCES + NRECEIVERS; iparam++)
					{
						aux = NEW[i_to_calc][iparam];
						NEW[i_to_calc][iparam] = best[i_to_calc][iparam];
						best[i_to_calc][iparam] = aux;
					}
	
					temp = GRADIENT[i_to_calc];	
					GRADIENT[i_to_calc] = GRADIENT_BEF[i_to_calc];
					GRADIENT_BEF[i_to_calc] = temp;
				}
			}	/* closing if for DONE */
		}	/* closing LOOP i_to_calc */

/* Computing the derivatives for the models that are not done */
		Derivatives(DONE, GRADIENT, best);
/*
    Now checking the DONE TOTAL flag. Is the whole computation finished ?
*/
		for (DONE_TOTAL = 1, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)	
		{
			DONE_TOTAL *= DONE[i_to_calc];
		}
	}	/* closing while for DONE_TOTAL */
/*
    Returning # of iterations but first freeing memory
*/
	for (number_of_iterations = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
		number_of_iterations += ITERATIONS[i_to_calc];

	free2double(NEW);
	free2double(GRADIENT);
	free2double(GRADIENT_BEF);
	free2double(SEARCH);
	free1double(OBJ_CUR);
	free1double(BETA);
	free1int(DONE);
	free1int(ITERATIONS);

	return(number_of_iterations);
}		

/* 
    this routine will calculate the GRADIENT
    the stacking power at the model provided
*/

Derivatives(DONE, GRADIENT, model)

double **GRADIENT;	/* computed gradient */
double **model;		/* models where the gradient are computed */
int *DONE;		/* defines whether the computation is necessary */
{
        double static_itrace, static_i;         /* statics calculation  */
        double xc0, xc1, xc2;                   /* Xcor derivatives     */
        double xcor_deriv_0; 		        /* Xcor derivatives     */
	double aux;				/* auxiliar quantity	*/
        register int i, ict;  	                /* counters             */
	int i_to_calc;				/* define the model to  */
						/* compute gradient	*/
        int ipoint_cross, iparam;
        int itrace, icmp;   	                /* pointers             */
	int LAG;				/* LAG between traces	*/
/*
    Sweeping over all models and calculate the gradient for the ones
    that are not done yet
*/
/*
    Reading the file
*/
	rewind (Xfp);
/*
    And differentiating the Xcorrelation part of the objective function
    First resetting
*/
	for (i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
		for (iparam=0; iparam < NSOURCES+NRECEIVERS; iparam++)
			GRADIENT[i_to_calc][iparam] = 0.;
			
	for (icmp = 0; icmp < NCMP; icmp++)
	{
		fread(CROSS[0],sizeof(float),(TOTAL_LAG+1)*(MAXFOLD*(MAXFOLD-1)/2),Xfp);

		for (i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
		{
			if (!DONE[i_to_calc])
			{
				for (iparam=0; iparam < NSOURCES+NRECEIVERS; iparam++)
				{
					if (DERIVATIVE[icmp][iparam] != -1) 
						    	    /* i.e. the deriv */
							    /* is not zero    */
					{
						itrace = DERIVATIVE[icmp][iparam];
						static_itrace = model[i_to_calc][POINTER[icmp][itrace][0]] + model[i_to_calc][POINTER[icmp][itrace][1] + NSOURCES];

						for (i = 0; i < FOLDCMP[icmp]; i++)
						{
/*
    statics for trace i
*/
							static_i = model[i_to_calc][POINTER[icmp][i][0]] + model[i_to_calc][POINTER[icmp][i][1] + NSOURCES];
/*
    computing the LAG
*/
							if (i < itrace)
								LAG = TOTAL_LAG / 2 + NINT(static_itrace - static_i);
							if (i > itrace)
								LAG = TOTAL_LAG / 2 + NINT(static_i - static_itrace);
							if (i != itrace)
							{
/*
    ipoint_cross points to the proper Xcorrelation 
*/
								ipoint_cross = POINT_CROSS[icmp][iparam][i];
								if (LAG == 0)
								{
									xc0 = (double) CROSS[ipoint_cross][0];
									xc1 = (double) CROSS[ipoint_cross][1];
									xc2 = (double) CROSS[ipoint_cross][2];
									/* for the gradient */
									xcor_deriv_0 = xc1-xc0;
								}
	
								else if (LAG == TOTAL_LAG+1)
								{
									xc0 = (double) CROSS[ipoint_cross][TOTAL_LAG-1];
									xc1 = (double) CROSS[ipoint_cross][TOTAL_LAG];
									xc2 = (double) CROSS[ipoint_cross][TOTAL_LAG+1];
									xcor_deriv_0 = xc2-xc1;
								}

								else

								{
									xc0  = (double) CROSS[ipoint_cross][LAG-1];
									xc1  = (double) CROSS[ipoint_cross][LAG];
									xc2  = (double) CROSS[ipoint_cross][LAG+1];
									xcor_deriv_0 = .5*(xc2-xc0);
								}
/*
    computing the gradient
*/
								if (i < itrace)
									GRADIENT[i_to_calc][iparam] -= xcor_deriv_0;
								else
									GRADIENT[i_to_calc][iparam] += xcor_deriv_0;
/*
    The signs above are justified if i is less or greater than itrace
    AND the fact that I am minimizing the NEGATIVE of the stacking power
*/
							}	/* i!=itrace */
						}	/* within CMP */
					}	/* derivative exists */
				}	/* for all parameters */
			}	/* close IF for DONE */
		}	/* close for i_to_calc */
	}	/* for all CMPs */
}	/* that's it */

/*
 * Inexact Cubic Line Search 
 *
 * Function directl returns number of function evaluations performed 
 *  doing the line search.
 * Function indirectly returns x(k+1) with f(x(k))>f(x(k+1)).
 *
 * Written by Douglas I. Hart -- July 1993
 * Adapted by Wences Gouveia -- September, 1993
 *
 */

/*------------------------------------------------------------------------*/

Line_Search( int *DONE,            /* DONE flags*/
	     int *ITERATIONS,      /* # of iterations per member */
             double **NEW,         /* minimizer */
	     double *OBJ_CUR,      /* evaluations for minimizer */
 	     double **best,	   /* input point for line search */
	     double *OBJ_PAST,     /* evaluations for input data */
             double **p,       	   /* descent direction */
             double *slope)        /* slope in descent direction */
{
   int *TST; 
   double *OBJ_PREV;
   float *LAMBDA; 		/* start with a full Newton step */
   float lambda2, *LAMBDAPREV, lambdaprev2, lambdatemp;
   float f1, f2;
   float c, cm11, cm12, cm21, cm22;
   float a, b, disc;
   float alpha = .25;
   int *GOLD_TEST, *ARM_TEST;
   int i, j, i_to_calc, how_many;
   int GOLD_TEST_TOTAL, ARM_TEST_TOTAL;

   OBJ_PREV = alloc1double( Popsize );
   LAMBDAPREV = alloc1float( Popsize ); 
   LAMBDA = alloc1float( Popsize ); 
   GOLD_TEST = alloc1int( Popsize ); 
   ARM_TEST = alloc1int( Popsize ); 
   TST = alloc1int( Popsize ); 

   if( OBJ_PREV == NULL || 
       GOLD_TEST == NULL || 
       ARM_TEST == NULL || 
       TST == NULL ||
       LAMBDA == NULL ||
       LAMBDAPREV == NULL ) {
          Error( "Memory allocation error in Line_Search" );
   }

   /* initial LAMBDA */
   for (i = 0; i < Popsize; i++)
   {
      TST[i] = 0;
      LAMBDA[i] = .001;
   }

   /* Let's try the 1st update */
   for (how_many = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
   {
      if (!DONE[i_to_calc])
      {
         for (j = 0; j < NSOURCES + NRECEIVERS; j++)
	 {
	    NEW[i_to_calc][j] = best[i_to_calc][j] + 
				LAMBDA[i_to_calc] * p[i_to_calc][j];
	    /* limiting */
	    if (NEW[i_to_calc][j] > Gene[j].max)
	       NEW[i_to_calc][j] = Gene[j].max;
            if (NEW[i_to_calc][j] < Gene[j].min)
               NEW[i_to_calc][j] = Gene[j].min;
/*
    Gathering the models to be evaluated
*/
            to_be_calculated[how_many][j] = NEW[i_to_calc][j];
	 }
         how_many ++;
      }
   }

   eval(how_many, NSOURCES+NRECEIVERS);

  /* Copying evaluations for OBJ_CUR */
   for (how_many = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
   {    
      if (!DONE[i_to_calc])
      {
         OBJ_CUR[i_to_calc] = eval_returned[how_many];
	 ITERATIONS[i_to_calc]++;
	 how_many++;
      } 
   }
   /* Goldstein's Test for lambda too small of a step size           */
   /* See Fletcher, Practical Methods of Optimization, page26ff.     */
   GOLD_TEST_TOTAL = 1;
   do
   {
      for (how_many = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
      {
         if (!DONE[i_to_calc])
         {
	    if (OBJ_CUR[i_to_calc] < (OBJ_PAST[i_to_calc] 
	    + (1-alpha)*LAMBDA[i_to_calc]*slope[i_to_calc])) 
               GOLD_TEST[i_to_calc] = 1;
	    else 
               GOLD_TEST[i_to_calc] = 0;
	    
            if (GOLD_TEST[i_to_calc])
	    {
               LAMBDA[i_to_calc] *= 3.0; 
			/* increase lambda by 3 if to small of step */
	       
               for( i = 0; i < NSOURCES + NRECEIVERS; i++ ) 
	       {
					/* new trial point */
                  NEW[i_to_calc][i] = best[i_to_calc][i] + 
                                      LAMBDA[i_to_calc] * p[i_to_calc][i];
                  /* limiting */
                  if (NEW[i_to_calc][i] > Gene[i].max)
                     NEW[i_to_calc][i] = Gene[i].max;
                  if (NEW[i_to_calc][i] < Gene[i].min)
                     NEW[i_to_calc][i] = Gene[i].min;

	          to_be_calculated[how_many][i] = NEW[i_to_calc][i];
	       }
	       how_many ++;
	    }
         }
      }
      /* evaluating model */
      if (how_many != 0)
         eval(how_many, NSOURCES + NRECEIVERS);

      GOLD_TEST_TOTAL = 0;
      for (how_many = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
      {
         if (!DONE[i_to_calc])
	 {
	    if (GOLD_TEST[i_to_calc])
	    {
               OBJ_CUR[i_to_calc] = eval_returned[how_many];
               ITERATIONS[i_to_calc]++;
	       how_many ++;
	       GOLD_TEST_TOTAL += GOLD_TEST[i_to_calc];
	    }
         }
      }
   } while (GOLD_TEST_TOTAL);

   /* Armijo's Test for lambda to large (the backtracking algorithm) */
   /* See Dennis and Schnabel, Numerical Methods for Unconstrained   */
   /* Optimization and Nonlinear Equations, page 126ff.              */ 

   do
   {
      for (how_many = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
      {
         if (!DONE[i_to_calc])
         {
	    if (OBJ_CUR[i_to_calc] > (OBJ_PAST[i_to_calc] + 
		alpha*LAMBDA[i_to_calc]*slope[i_to_calc]) && 
		LAMBDA[i_to_calc] > LAMBDA_MIN)

	       ARM_TEST[i_to_calc] = 1;
	    else
	       ARM_TEST[i_to_calc] = 0;

	    if (ARM_TEST[i_to_calc])
	    {
      	       lambda2 = LAMBDA[i_to_calc]*LAMBDA[i_to_calc]; 
				/* intermediate computation */  
               f1 = OBJ_CUR[i_to_calc] - OBJ_PAST[i_to_calc]-
		    slope[i_to_calc] * LAMBDA[i_to_calc]; 
				/* intermediate computation */
               if( TST[i_to_calc] == 0 ) 
	       { /* try a quadratic fit first */
                  lambdatemp = -slope[i_to_calc]*lambda2/(2.0*f1); 
						/* tentative lambda */
                  TST[i_to_calc] = 1; /* don't go here again */    
               } 
	       else 
	       { /* otherwise minimize using a cubic approximation */
                  lambdaprev2 = LAMBDAPREV[i_to_calc]*LAMBDAPREV[i_to_calc]; 
					/* intermediate computation */   
                  f2 = OBJ_PREV[i_to_calc] - OBJ_PAST[i_to_calc] -
		       LAMBDAPREV[i_to_calc] * slope[i_to_calc]; 
					/* intermediate computation */

          	  c    = 1.0/(LAMBDA[i_to_calc]-LAMBDAPREV[i_to_calc]); 
					/* cubic fit computations */
         	  cm11 = 1.0/lambda2;
         	  cm12 = -1.0/lambdaprev2;
         	  cm21 = -LAMBDAPREV[i_to_calc]/lambda2;
         	  cm22 = LAMBDA[i_to_calc]/lambdaprev2; 
 
         	  a    = c*(cm11*f1+cm12*f2);
          	  b    = c*(cm21*f1+cm22*f2);
         	  disc = b*b - 3.0*a*slope[i_to_calc];

         	  if( (fabs(a)>MINFLOAT) && (disc>MINFLOAT) ) { 
            	     /* legitimate cubic */
                     lambdatemp = (-b+sqrt(disc))/(3.0*a);
         	  } 
		  else 
		  { 
            	     /* degenerate quadratic fit */
            	     lambdatemp = slope[i_to_calc] * lambda2/(2.0*f1);
         	  }
 
         	  if( lambdatemp >= 0.5*LAMBDA[i_to_calc] ) 
					/* if lambdatemp is to large */
            	     lambdatemp = 0.5*LAMBDA[i_to_calc];

      	       }

      	       LAMBDAPREV[i_to_calc] = LAMBDA[i_to_calc]; /* save for next cubic fit */
      	       OBJ_PREV[i_to_calc] = OBJ_CUR[i_to_calc]; /* save for next cubic fit */

               if( lambdatemp < (0.1*LAMBDA[i_to_calc]) ) 
					/* prevents large decreases  */
                  LAMBDA[i_to_calc] *= 0.1;
      	       else
         	  LAMBDA[i_to_calc] = lambdatemp;

      	       for( i = 0; i < NSOURCES + NRECEIVERS; i++ ) 
						/* new trial point */
	       {
         	  NEW[i_to_calc][i] = best[i_to_calc][i] + 
				      LAMBDA[i_to_calc]*p[i_to_calc][i];
                  /* limiting */
                  if (NEW[i_to_calc][i] > Gene[i].max)
                     NEW[i_to_calc][i] = Gene[i].max;
                  if (NEW[i_to_calc][i] < Gene[i].min)
                     NEW[i_to_calc][i] = Gene[i].min;

		  to_be_calculated[how_many][i] = NEW[i_to_calc][i];
	       }
	       how_many++;
	    }
	 }
      }

      /* evaluating */
      eval(how_many, NSOURCES + NRECEIVERS);

      ARM_TEST_TOTAL = 0;
      for (how_many = 0, i_to_calc = 0; i_to_calc < Popsize; i_to_calc++)
      {
         if (!DONE[i_to_calc])
         {
            if (ARM_TEST[i_to_calc])
            {
               OBJ_CUR[i_to_calc] = eval_returned[how_many];
               ITERATIONS[i_to_calc]++;
	       how_many++;
               ARM_TEST_TOTAL += ARM_TEST[i_to_calc];
            }
         }
      }
   } while (ARM_TEST_TOTAL);
   /* freeing memory */

   free1int ( TST );
   free1double ( OBJ_PREV );
   free1int ( GOLD_TEST );
   free1int ( ARM_TEST );
   free1float ( LAMBDAPREV );
   free1float ( LAMBDA );

}   
