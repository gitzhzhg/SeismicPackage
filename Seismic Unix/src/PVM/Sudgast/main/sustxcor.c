/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSTXCOR: $Revision: 1.3 $ ; $Date: 2011/11/21 16:45:13 $       */
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

#include "su.h"
#include "cwp.h"
#include "header.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                       ",
" SUSTXCOR - XCORrelation table builder for residual STatics estimation ",
"            by SUDGAST							",
"									",
" SUSTXCOR pre-computes a table of crosscorrelations used by SUDGAST for",
" the estimation of residual statics. This table is used to speed up the",
" stacking process. For more details the user is refered to:		",
"									",
" Paulson, K., 1986, An efficient implementation of Monte Carlo statics:",
"	Proceeding of the 56th SEG meeting, Houston, Texas, 595-597.	",
"									",
" The following field headers are used by SUSTXCOR: dt, ns and cdp.     ",
"									",
" sustxcor required parameters [optional parameters]			",
"									",
" Required parameters:							",
" datafile=                     name and COMPLETE path of the input file",
" xcorrfile=                    name and COMPLETE path of the           ",
"                               crosscorrelation file                   ",
" sources=                      number of source locations              ",
" receivers=                    number of receiver locations            ",
" cmps=                         number of common mid point locations    ",
" maxfold=                      maximum fold of input data              ",
" windbeg=                      initial time of the window where        ",
"                               the stacking power is computed (s)	",
" windend=                      final time of the window where          ",
"                               the stacking power is computed (s)	",
"									",
" Optional parameters:                                                  ",
" verbose=0                     =1 print useful information             ",
" maxlag=160                    maximum lag used in the crosscorrelation",
"                               the absolute maximum statics cannot be  ",
"                               greater than maxlag/2 * dt              ",
NULL};
/**************** end self doc ***********************************/

/*
 * Credits: CWP Wences Gouveia, 06/08/94,  Colorado School of Mines
 */


segy tr;		/* SEGY DATA */

main(int argc, char **argv)
{
	float **data; 			/* seismic data		*/
	float **CROSS;           	/* crosscorrelation     */
        float wallcpu;               	/* wall clock time      */
        float stack=0;                  /* input stacking power */
	float windbeg;			/* beggining of time window */
	float windend;			/* end of time window 	*/
	float dt;			/* time sampling 	*/
	int verbose;			/* dialogue 		*/
	int NCMP;   			/* number of cmps	*/
	int NSOURCES;			/* number of sources    */
	int NRECEIVERS;			/* number of receivers	*/
	int MAXFOLD;			/* maximum fold of the data */
	int NSAMPLES;			/* number of samples / trace */
	int FIRSTSAMPLE;		/* first sample of time window */
	int LENGTH;			/* length of the time window */
	int *FOLDCMP;            	/* fold of each CMP     */
	int NTRACES;			/* # of traces read	*/
	int TOTAL_LAG;			/* Maximum lag of Xcorrelation */
	int alltraces;			/* output of table	*/
	int i, j, k;
	int ipoint;			
        int itrace, icmp, cmp_current, cmp_last; 
					/* counters		*/
	char *datafile;		      	/* input file           */
	char *xcorrfile;      		/* output file          */
	char msg[80];                   /* used for error mesg  */
	FILE *fp;		        /* data file		*/
	FILE *Xfp;		        /* Xcorr file		*/

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

        if (!getparint("maxlag", &TOTAL_LAG))
        {
                TOTAL_LAG = 160;
        }

        if (!getparfloat("windbeg", &windbeg))
        {
                sprintf(msg, "Specify beggining of time window!");
                Error(msg);
        }

        if (!getparfloat("windend", &windend))
        {
                sprintf(msg, "Specify end of time window!");
                Error(msg);
        }

        if (!getparstring("datafile", &datafile))
        {
                sprintf(msg, "Specify data file name!");
                Error(msg);
        }

        if (!getparstring("xcorrfile", &xcorrfile))
        {
                sprintf(msg, "Specify crosscorrelation file name!");
                Error(msg);
        }

        if (!getparint("verbose", &verbose))
        {
               verbose = 0; 
        }
        checkpars();
/*
    opening input file 
*/
        if (verbose)
                fprintf(stderr,"Reading input data set\n");

        fp = fopen(datafile,"r");
	if (fp == NULL)
        {
               	sprintf(msg, "Can't open %s", datafile);
               	Error(msg);
        }

        /* get info from first trace */
        if (!fgettr(fp,&tr))  Error("can't get first trace");
        NSAMPLES = tr.ns;
	dt = (float) tr.dt / 1000000.0;
	FIRSTSAMPLE = NINT(windbeg / dt);
	LENGTH = NINT(windend / dt);
	rewind(fp);
/*
    memory allocation
*/
        data = alloc2float(NSAMPLES, MAXFOLD);
        if (data == NULL) Error("Allocation failed for input data");

        CROSS = alloc2float(TOTAL_LAG+1, MAXFOLD*(MAXFOLD-1)/2);
        if (CROSS == NULL) Error("Allocation failed Xcorrelation table");

        FOLDCMP = alloc1int(NCMP);
        if (FOLDCMP == NULL) Error("Allocation failed for auxiliary vectors");

/*
    opening file for Xcorrelations
*/
        Xfp = fopen(xcorrfile,"w");
        if (Xfp == NULL)
        {
        	sprintf(msg, "Can't open %s", xcorrfile);
                Error(msg);
        }

	NTRACES = 0;
	icmp = -1;
	cmp_current = 0;
	cmp_last = 0;

	while (fgettr(fp,&tr)) { 
	   cmp_current = tr.cdp;
	   if (cmp_current != cmp_last)
	      {
	         if (icmp >= 0) 
		 {
		    FOLDCMP[icmp] = itrace;

		    /* one more CMP read, computing Xcorrelation */

		    /* resetting */
		    for (i = 0; i < MAXFOLD*(MAXFOLD-1)/2; i++)
		       for (j = 0; j < TOTAL_LAG+1; j++)
		          CROSS[i][j] = 0.;

		    if (verbose)
                       fprintf(stderr,"Computing Xcorrelation for CMP %d with fold %d\n",icmp, FOLDCMP[icmp]);
                    for (ipoint = 0, j = 0; j < FOLDCMP[icmp] - 1; j++)
		    {
                       for (i = j + 1; i < FOLDCMP[icmp]; i++, ipoint++)
                          {
                             xcor(LENGTH,FIRSTSAMPLE,data[i],LENGTH,FIRSTSAMPLE,data[j],TOTAL_LAG+1,-TOTAL_LAG/2,CROSS[ipoint]);
                           }
        	    }

		    /* computing input stacking power */
                    alltraces = FOLDCMP[icmp] * (FOLDCMP[icmp]-1) / 2.;
                    for (ipoint = 0; ipoint < alltraces; ipoint++)
                        stack += CROSS[ipoint][TOTAL_LAG/2];

		    /* output xcorrelation in a file */
                    fwrite(CROSS[0],sizeof(float),(TOTAL_LAG+1)*(MAXFOLD*(MAXFOLD-1)/2),Xfp);
		 }
		 icmp++;
		 itrace = 0;
		 cmp_last = cmp_current;
              }	

	      /* reading traces */
              memcpy(data[itrace],tr.data,NSAMPLES*FSIZE);

	      itrace++;
	      NTRACES++;
	};

	FOLDCMP[icmp] = itrace;	/* last CMP */

	/* and again for the last CMP */
        for (i = 0; i < MAXFOLD*(MAXFOLD-1)/2; i++)
           for (j = 0; j < TOTAL_LAG+1; j++)
              CROSS[i][j] = 0.;

	if (verbose)
           fprintf(stderr,"Computing Xcorrelation for CMP %d with fold %d\n",icmp, FOLDCMP[icmp]);

        for (ipoint = 0, j = 0; j < FOLDCMP[icmp] - 1; j++)
        {
           for (i = j + 1; i < FOLDCMP[icmp]; i++, ipoint++)
              {
                  xcor(LENGTH,FIRSTSAMPLE,data[i],LENGTH,FIRSTSAMPLE,data[j],TOTAL_LAG+1,-TOTAL_LAG/2,CROSS[ipoint]);
              }
        }

        /* computing input stacking power */
        alltraces = FOLDCMP[icmp] * (FOLDCMP[icmp]-1) / 2.;
        for (ipoint = 0; ipoint < alltraces; ipoint++)
        	stack += CROSS[ipoint][TOTAL_LAG/2];

        /* output xcorrelation in a file */
 	fwrite(CROSS[0],sizeof(float),(TOTAL_LAG+1)*(MAXFOLD*(MAXFOLD-1)/2),Xfp);

	if (verbose)
	{
	   fprintf(stderr,"\n%d traces were read in %d CMPs\n",NTRACES,icmp+1);
           fprintf(stderr,"Stacking power of the input data: %f\n", stack);
	}

        fclose(fp);
        fclose(Xfp);

        if (verbose)
                fprintf(stderr,"SUSTXCOR finished normally\n");

}	/* that's it */
Error(s)
char *s;
{
        fprintf(stderr,"%s\n", s);
        exit(1);
}
