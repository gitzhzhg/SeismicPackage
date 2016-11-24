/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSTAPPLY: $Revision: 1.3 $ ; $Date: 2011/11/21 16:45:13 $       */
/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1994.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (jkc@dix.mines.colorado.edu)
 *----------------------------------------------------------------------
 */

#include "cwp.h"
#include "su.h"
#include "header.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                       ",
" SUSTAPPLY - APPLY the residual STatics computed by SUDGAST		",
"                                                                       ",
" SUSTAPPLY writes in the SEGY header field TSTAT the residual statics  ",
" computed by SUDGAST.							",
"                                                                       ",
" The following field headers are used by SUSTAPPLY: dt.	        ",
"                                                                       ",
" sustapply >stdout required parameters [optional parameters]		",
"									",
" Required parameters:                                                  ",
" sources=                      number of source locations              ",
" receivers=                    number of receiver locations            ",
" cmps=                         number of common mid point locations    ",
" maxfold=                      maximum fold of input data              ",
" datafile=                     name and COMPLETE path of the input file",
" staticsfile=                  name and COMPLETE path of the file     	",
"                               with the statics computed by SUDGAST	",
" Optional parameters:                                                  ",
" verbose=0                     =1 print useful information             ",
NULL};							
/**************** end self doc ***********************************/

/*
 * Credits: CWP Wences Gouveia, 06/08/94,  Colorado School of Mines
 */

segy tr;                /* SEGY DATA */

main(int argc, char**argv)
{
	float *vect;   		/* floating point representation */
	float *data; 		/* seismic data			 */
	float dt;		/* time sampling		 */
	float trace_static;	/* total statics, in SECONDS	 */
	float receiver_statics;
	float source_statics;   /* statics components		 */
	int ***POINTER;		/* pointers		 	 */
        int *SOURCE, *RECEIVER; /* pointers		 	 */
	int *FOLDCMP;		/* fold of each CMP		 */
	int isource, itrace, ireceiver, icmp, cmp_current, cmp_last;
	int i, j;
				/* counters and similar stuff	 */
        int NCMP;               /* number of cmps       	 */
        int NSOURCES;           /* number of sources    	 */
        int NRECEIVERS;         /* number of receivers    	 */
        int MAXFOLD;            /* maximum fold of the data 	 */
        int NSAMPLES;           /* number of samples / trace 	 */
        int NTRACES;            /* used in reading input data    */
        int verbose;            /* dialogue             	 */

	char *datafile;         /* input file                    */
	char *answerfile;       /* input file with statics       */
	char msg[40];		/* Error messages		 */
	FILE *fp;		/* files ....		         */

        initargs(argc,argv);
        requestdoc(0);
/*
    input data
*/
        if (!getparint("sources", &NSOURCES))
        {
                sprintf(msg, "Specify number of sources!");
                Error(msg);
        }

        if (!getparint("receivers", &NRECEIVERS))
        {
                sprintf(msg, "Specify number of receivers!");
                Error(msg);
        }

        if (!getparint("cmps", &NCMP))
        {
                sprintf(msg, "Specify number of cmps!");
                Error(msg);
        }

        if (!getparint("maxfold", &MAXFOLD))
        {
                sprintf(msg, "Specify maximum fold of the data!");
                Error(msg);
        }

        if (!getparstring("datafile", &datafile))
        {
                sprintf(msg, "Specify data file name!");
                Error(msg);
        }

        if (!getparstring("staticsfile", &answerfile))
        {
                sprintf(msg, "Specify data file name!");
                Error(msg);
        }

        if (!getparint("verbose", &verbose))
        {
               verbose = 0;
        }

        checkpars();
	if (verbose)
		fprintf(stderr,"Reading input data set\n");

	fp = fopen(datafile,"r");
	if (fp == NULL)
                Error("Can't open data file");

        /* get info from first trace */
        if (!fgettr(fp,&tr))  Error("can't get first trace");
        NSAMPLES = tr.ns;
        dt = (float) tr.dt / 1000.0;
        rewind(fp);

/*
    Memory allocation for vector:
	POINTER[0...NCMP-1][0...FOLD-1][SOURCE_ID, RECEIVER_ID]
*/
	POINTER = alloc3int(2,MAXFOLD,NCMP);
	if (POINTER == NULL) Error("Allocation failed for auxiliary memory");
/*
    Allocating for vect
*/
	vect = alloc1float(NSOURCES + NRECEIVERS);
	if (vect == NULL) Error("Allocation failed for auxiliary memory");
/*
    Source vector used to index source statics
*/
	SOURCE = alloc1int(NSOURCES);
	if (SOURCE == NULL) Error("Allocation failed for auxiliary memory");
/*
    Receiver vector used to index receiver statics
*/
	RECEIVER = alloc1int(NRECEIVERS);
	if (RECEIVER == NULL) Error("Allocation failed for auxiliary memory");
/*
    Foldcmp vector used for fold of each CMP 
*/
	FOLDCMP = alloc1int(NCMP);
	if (FOLDCMP == NULL) Error("Allocation failed for auxiliary memory");
/*
    Allocating memory for data
*/
        data = alloc1float(NSAMPLES);
        if (data == NULL) Error("Allocation failed for DATA");
/*
    Reading data and headers. 
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
		memcpy(data,tr.data,NSAMPLES*FSIZE);
		itrace++;
		NTRACES++;
	};
	FOLDCMP[icmp] = itrace;	/* last CMP */
	close(fp);

	if (verbose)
	   fprintf(stderr,"%d TRACES were read in %d CMPs\n",NTRACES,icmp+1);
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

	free1int(SOURCE);
	free1int(RECEIVER);
/*	
    Reading the file answer.suffix.id with the residual statics 
*/
        if ((fp = fopen(answerfile, "r")) == NULL)
                Error("Can't open statics file");

	for (i = 0; i < NSOURCES + NRECEIVERS; i++)
        	fscanf(fp,"%d %f\n",&j,&vect[i]);
		
	close(fp);
/*
    And reading and writing the data and headers with the field 
    tstat properly filled
    The input file should be read again
*/

        fp = fopen(datafile,"r");
        if (fp == NULL)
                Error("Can't open data file");
	
        for (icmp = 0; icmp < NCMP; icmp++)
	{
		if (verbose)
			fprintf(stderr,"Processing CMP %d\n", icmp);

                for (itrace = 0; itrace < FOLDCMP[icmp]; itrace++)
		{
			fgettr(fp,&tr);
/*
    trace_static must be in SECONDS
*/
			source_statics = vect[POINTER[icmp][itrace][0]] * dt;
/*
    now receiver statics
*/
			ireceiver = POINTER[icmp][itrace][1];
			receiver_statics = vect[ireceiver + NSOURCES] * dt;

			trace_static = source_statics + receiver_statics; 
/*
    filling in header field
*/
                	tr.tstat = NINT(trace_static); 
			puttr(&tr);
		}
	}
	close(fp);
	if (verbose)
		fprintf(stderr,"SUSTAPPLY finished normally\n");
}

Error(s)
char *s;
{
	fprintf(stderr,"%s\n", s);
	exit(1);
}

/*** end of file ***/
