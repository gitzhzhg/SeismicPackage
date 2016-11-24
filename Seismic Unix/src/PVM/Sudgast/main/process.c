/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Wences Gouveia - CWP - 010194 */
/*
    member program for sudgast
    objective:       Receives a given # of members, named sub_population
		     and applies GA optimization on them. After exchange
		     randomly chosen members among other cities and repeat 
		     the GA with the new sub_population. 
*/

#include "extern.h"
#include "communication.h"
#include "pvm.h"

main()
{
/*
    Declaration of variables
*/
	int master;       	   		/* PVM cotrol parameters    */
        int Ncities, pop_city;                  /* cities and size of popul. */
        int Numevolutions;                      /* cities and size of popul. */
        int iterat;                             /* # of iterations performed */
        int ialoc;                              /* used in memory allocation */
        int i,j,k,ipointer;			/* counters 		     */
        int send, icity;			/* destiny and original city */ 
        int member_exchange, new_members;	/* # members exchanged       */
        int new_pop_city;   			/* new pop size after Xchange*/
        int top_perf;				/* 10% top performers	     */
        int *indx, *top_indx;                   /* used in perform. sorting  */
        int *indxf, *top_indxf;                 /* used in perform. sorting  */
        int seed;       			/* seed for RANDOM generation*/
        float *performance, *top_performance;
       						/* performance measure       */
/*
    Variables related to the execution of the GAs
*/
	char s[40];
/*
    Enroll in PVM
*/
	instance = pvmEnroll(CITIES);
/*
    Receive master instance number
*/
	pvmReceive(MASTER);
	pvmGetNInt(1,&master);
/*
    Receiving the extension of the file for the input data and working 
    directory
*/
        pvmReceive(INPUT_FILE);
        pvmGetString(s);

        pvmReceive(DIRECTORY);
        pvmGetString(workdir);
/*
    Receiving the # of cpus (cities) involved in the evolution
*/
        pvmReceive(NCITIES);
        pvmGetNInt(1,&Ncities);
/*
    Receiving the # of evolutions that will be performed
*/
        pvmReceive(EVOLVE);
        pvmGetNInt(1,&Numevolutions);
/*
    Calling routines for execution of GENESIS. All data input, including
    the initial subpopulation and the exchanging of the further ones 
    will be handled here
*/
  	genesis_main(s,master);

	if (verbose)
        	fprintf(stderr,"Subpopulation %d is done\n",instance);
/*
    Signing end of final evolution
*/
	pvmBeginMessage();
	pvmSend(END_GLOBAL_EVOLUTION,CMASTER,master);

	pvmLeave();     		/* Leaving PVM */
}
