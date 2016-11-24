/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUDGAST: $Revision: 1.2 $ ; $Date: 2011/11/21 16:43:57 $       */
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
/*----------------------------------------------------------------------
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *----------------------------------------------------------------------
 */

#include "define.h"
#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                       ",
" SUDGAST - Residual STatics estimation by a Distributed Genetic        ",
"           Algorithm							",
"									",
" SUDGAST estimates the residual statics by optimizing the  	       	",
" stacking power objective function using a distributed 		",
" genetic algorithm coupled with a conjugate gradient procedure.	",
"									",
" The genetic algorithm used in this code is a distributed version of   ",
" the sequential genetic algorithm GENESIS, developed by John J. 	",
" Grefenstette at the Naval Research Center. More details on GENESIS	",
" are available by direct E-mail to its author [gref@AIC.NRL.Navy.Mil]. ", 
" The Parallel Virtual Machine library version 3.2 is required to use   ",
" this program. The PVM user's guide, available at the netlib server    ", 
" [netlib@ornl.gov] contains all the necessary information on PVM usage ",
" and installation.					                ",
" 									",
" IMPORTANT !!!								",
" PVM requires that the executable files reside in a user's directory   ",
" called ~/pvm/RIOS.							", 
"									",
" To compute the stacking power efficiently a table of crosscorrelations",
" of the input seismic data must be pre-computed (using the program     ",
" SUSTXCOR) and stored in a file.					",
"									",
" This file is the main reason to use just one processor (workstation)  ",
" for SUDGAST, even if you define more than 1 subpopulation. Due the    ",
" limited bandwidth of current networks (as token ring and ethernet) it ",
" would be too expensive to swap the file across the network among      ",
" several workstations. The alternative would be to use several copies  ",
" of the croscorrelation files, on each one of the workstations used in ",
" the processing. This alternative is not considered here due its high  ",
" cost.									",
"									",
" The output of SUDGAST is a set of residual statics, one per		",
" subpopulation, that the user can directly incorporate in the header   ",
" of the input data using the program SUSTAPPLY. Each set is stored in  ",
" the file answer.suffix.id, where id is the subpopulation index 	",
" (0,1,...) automatically define by the program. SUDGAST also provides  ",
" the history of the best member of each subpopulation as a function of ", 
" iteration number in the file best.suffix.id. 				",
"									",
" The following field headers are used by SUDGAST: dt, sx, gx and cdp.  ",
"									",
" sudgast required parameters [optional parameters] 			",
"									",
" Required parameters:							",
" sources=			number of source locations		",
" receivers=			number of receiver locations		",
" cmps=				number of common mid point locations	",
" dx=				receiver spacing (in the same unit used ",
"			        in the fields sx and gx of the SEGY     ",
"				header)					", 
" maxfold=			maximum fold of input data		",
" minstatic=			minimum residual statics (ms)		",
"			        can be negative				",
" maxstatic=			maximum residual statics (ms) 		",
" workdir=			COMPLETE path of the directory where all",
"			 	the output files will be stored		",
" datafile=			name and COMPLETE path of the input file",
" xcorrfile=			name and COMPLETE path of the 		",
" 				crosscorrelation file 			",
"									",
" Optional parameters:							",
" verbose=1			=1 print useful information		",
" suffix=dga			all output files will have the 		",
"				termination suffix.id where id is the	",
"				subpopulation id assigned by SUDGAST    ", 
" maxlag=160                     maximum lag used in the crosscorrelation",
"                                the absolute maximum statics cannot be ",
"                                greater than maxlag/2*dt               ",
" rand=123456789                 random seed for the GA 		",
" evolutions=20		        number of times that the subpopulation  ",
"				will exchange members among them.       ",
"				Between exchanges the sequential GA and ",
"				the conjugate gradient (when active)    ",
"				will work				",
" iterations=500                 number of iterations done by the	",
"				sequential GA in each evolution		",
" popsize=50			size of the subpopulation		",
" populations=2			number of subpopulations		",
" maxevol=2			number of evolutions where the sub-     ",
"				populations have a variance smaller than",
"				a fixed threshold (stopping criterion)	",
" crossprob=.6			crossover probability			",
" mutprob=.01			mutation probability			",
" uphill=1			=0 for no conjugate gradient. The	",
"				default uses this procedure.		",
" firstcg=2			1st evolution where the conjugate 	",
"				gradient will be used   		",
" maxiteratcg=5			maximum number of iterations done in the",
"				conjugate gradient in each evolution for",
"				each member of the subpopulation	",
" 									",
" 									",
" For a better understanding of this program the user in encouraged 	",
" to browse the following references:					",
"									",
" Gouveia, W., 1994, Residual Statics Estimation by a Hybrid		",
" 	Distributed Genetic Algorithm: CWP Report 137, 165-177.		",
"									",
" Whitley, D., 1993, A Genetic Algorithm Tutorial: Technical Report	",
"	CS-93-103, Colorado State University.				",
"									",
" Stork, C. and Kusuma, T., 1992, Hybrid Genetic Autostatics:		", 
" 	Proceedings of the 62nd SEG meeting, New Orleans, OK, 1127-1131.",
"                                                                       ",
NULL};
/**************** end self doc ***********************************/

/*
 * Credits: CWP Wences Gouveia, 06/08/94,  Colorado School of Mines
 */

segy tr;                /* SEGY DATA */

void setup(argc, argv, suff, workdir, population, ncities, ggenes, mmin, mmax, seed, numevol, offset, max_stat_evol, random_gnr, verbose)
int argc;
char **argv;
char suff[40], workdir[50];
int *population, *ncities;
int *numevol, *ggenes;
int *seed;
int *max_stat_evol;
int *random_gnr;
int *verbose;
double *mmin;
double *mmax;
double *offset;
{
	FILE *fp, *fpp, *fopen();
	int i, j;
	char msg[80];
	char *value;
	char *value_temp;
	char s[40];
	char ga[40];
	char infile[80];
	char templatefile[80];
	char format[20];
	char cmd[80];
	int bitlength;
	int status;
	int genes;
	int repetition;
	int ok;
	unsigned long values;
	unsigned long verify;
	float dt;
	double min, min0;
	double max, max0;

	initargs(argc,argv);
	requestdoc(0);

        if (!getparstring("workdir", &value))
        {
                sprintf(msg, "Setup: Specify working directory!");
                Error(msg);
        }
        sprintf(workdir,"%s",value);

        if (!getparstring("suffix", &value))
        {
                value = "dga";
        }

        sprintf(suff,"%s",value);
        printf("\n\n");
        if (strlen(suff) == 0) {
                strcpy(suff,"dga");
                sprintf(infile, "%s/in.dga", workdir);
                sprintf(templatefile, "%s/template.dga", workdir);
        }
        else {
                sprintf(infile, "%s/in.%s", workdir, suff);
                sprintf(templatefile, "%s/template.%s", workdir, suff);
        }

        if (!getparstring("verbose", &value))
        {
                value = "1";
        }
        if (!atoi(value))
                *verbose = 0;
        else
                *verbose = 1;

	bitlength = 0;
		
	/* get string interpretation */
	if (!getparstring("sources", &value))
	{
		sprintf(msg, "Setup: Specify number of sources!");
		Error(msg);
	}
	genes = atoi(value);

	if (!getparstring("receivers", &value))
	{
		sprintf(msg, "Setup: Specify number of receivers!");
		Error(msg);
	}
	genes += atoi(value);
	*ggenes = genes;

        if (!getparstring("datafile", &value))
        {
                sprintf(msg, "Setup: Specify data file name!");
                Error(msg);
        }
/*
    opening input file
*/
        fpp = fopen(value,"r");
        if (fpp == NULL)
        {
                sprintf(msg, "Can't open %s", value);
                Error(msg);
        }

        /* get info from first trace */
        if (!fgettr(fpp,&tr))  Error("can't get first trace of input data");
        dt = (float) tr.dt / 1000.0;
        close(fpp);

	fp = fopen(templatefile, "w");
	fprintf(fp, "genes: %d\n\n", genes);
	
	for (i=0; i<genes; )
	{
		if (!getpardouble("minstatic", &min))
		{
                	sprintf(msg,"Setup: Specify minimum static in ms!");
                	Error(msg);
		}
		min0 = min;	
	        min = (double) NINT(min / dt);  /* to time samples */

                if (!getpardouble("maxstatic", &max))
                {
                	sprintf(msg, "Setup: Specify maximum statics in ms!");
                        Error(msg);
                }
		max0 = max;
	        max = (double) NINT(max / dt);  /* to time samples */

		ok = 0;
                if (!getparulong("numbits", &values))
	        { 
			values = 64;
                }

		verify = 1L << ilog2(values);
		ok = verify == values;
		if (!ok)
		{
        		sprintf(msg, "Bad choice for values !");
                	Error(msg);
		}

		value="%8.3f";

		repetition=genes;

		for (j=0; j < repetition && i < genes; j++, i++)
		{
			fprintf(fp, "gene %d\n", i);
			fprintf(fp, "min: %g\n", min);
			fprintf(fp, "max: %g\n", max);
			fprintf(fp, "values: %lu\n", values);
			fprintf(fp, "format: %s\n", value);
			fprintf(fp, "\n");
			bitlength += ilog2(values);
		}
		fclose(fp);
	}
        *mmin = min;
        *mmax = max;

	if ((fp = fopen(infile, "w")) == NULL)
	{
		printf("can't open %s\n", infile);
		printf("Setup aborted.\n");
		exit(1);
	}

        if (!getparstring("verbose", &value))
        {
                value = "1";
        }
        if (!atoi(value))
                *verbose = 0;
        else
                *verbose = 1;

        setpar(fp, "Verbose", "1", value, verbose);

	value = "1";
	setpar(fp, "Experiments", "1", value, 0);

	if (*verbose)
	{
		printf("File suffix [dga]: %s\n", suff);
		printf("Minimum statics [?]: %6.2f\n", min0);
		printf("Maximum statics [?]: %6.2f\n", max0);
		printf("Number of bits (must be a power of 2) [64]: %lu\n", values);
	}

        /* get string interpretation */
        if (!getparstring("sources", &value))
        {
                sprintf(msg, "Setup: Specify number of sources!");
                Error(msg);
        }
        genes = atoi(value);
        /* statics parameters */
        setpar(fp, "Sources", "?", value, verbose);

        if (!getparstring("receivers", &value))
        {
                sprintf(msg, "Setup: Specify number of receivers!");
                Error(msg);
        }
        genes += atoi(value);
        /* statics parameters */
        setpar(fp, "Receivers", "?", value, verbose);

        if (!getparstring("cmps", &value))
        {
                sprintf(msg, "Setup: Specify number of cmps!");
                Error(msg);
        }
        genes = atoi(value);
        /* statics parameters */
        setpar(fp, "CMPs", "?", value, verbose);


        if (!getparstring("maxfold", &value))
        {
                sprintf(msg, "Setup: Specify maximum fold of the data!");
                Error(msg);
        }
        /* statics parameters */
        setpar(fp, "Fold", "?", value, verbose);

        if (!getparstring("maxlag", &value))
        {
		value = "160";
        }
        /* statics parameters */
        setpar(fp, "Maxlag", "160", value, verbose);

        if (!getparstring("dx", &value))
        {
                sprintf(msg, "Setup: Specify receiver spacing!");
                Error(msg);
        }
        /* statics parameters */
        setpar(fp, "Dx", "?", value, verbose);

        if (!getparstring("evolutions", &value))
        {
		value = "20";
        } 

	setpar(fp, "Evolutions", "20", value, verbose);
	*numevol = atoi(value);

        if (!getparstring("iterations", &value))
        {
		value = "500";
        }

	setpar(fp, "Trials per Evolution", "500", value, verbose);

        if (!getparstring("popsize", &value))
        {
		value = "50";
        }

	setpar(fp, "Subpopulation Size", "50", value, verbose);
	*population = atoi(value);

        if (!getparstring("populations", &value))
        {
		value = "2";
        }

	setpar(fp, "Number of populations", "2", value, verbose);
	*ncities = atoi(value);
	fprintf(fp, "%18s = %d\n", "Structure Length", bitlength);

        if (!getparstring("crossprob", &value))
        {
		value = ".6";
        }

	setpar(fp, "Crossover probability", "0.6", value, verbose);

        if (!getparstring("mutprob", &value))
        {
		value = ".001";
        }

	setpar(fp, "Mutation probability", "0.001", value, verbose);

	value = "1.";
	setpar(fp, "Generation Gap", "1.0", value, 0);

	value = "5";
	setpar(fp, "Scaling Window", "5", value, 0);

	value = "100";
	setpar(fp, "Report Interval", "100", value, 0);

	value = "10";
	setpar(fp, "Structures Saved", "10", value, 0);

	value = "2";
	setpar(fp, "Max Gens w/o Eval", "2", value, 0);

	value = "0";
	setpar(fp, "Dump Interval", "0", value, 0);

	value = "0";
	setpar(fp, "Dumps Saved", "0", value, 0);

	value_temp = malloc(1);		/* used due a possible bug with char* */

        if (!getparstring("rand", &value))
        {
		*seed = getpid();
		sprintf(value_temp,"%d",*seed);   /* INTEGER -> CHARACTER */
		value = value_temp;
		setpar(fp, "Random Seed", "123456789", value, verbose);
        }
	else
	{
		setpar(fp, "Random Seed", "123456789", value, verbose);
		*seed = atoi(value);
	}

	*random_gnr = 1;

	value = ".5";
	setpar(fp, "Rank Min", "0.5", value, 0);

        if (!getparstring("uphill", &value))
        {
		value = "1";
        }

	setpar(fp, "CG Active", "1", value, verbose);

        if (!getparstring("firstcg", &value))
        {
		value = "10";
        }

	setpar(fp, "Min Evol for CG", "10", value, verbose);

        if (!getparstring("maxiteratcg", &value))
        {
		value = "20";
        }

        setpar(fp, "Max number of iter in CG", "20", value, verbose);

        if (!getparint("maxevol", max_stat_evol))
        {
                *max_stat_evol = 2;
        }

	if (*verbose)
        	printf("Max number of low variance evolutions [2]: %d\n", *max_stat_evol);

        if (*verbose)
                printf("Working directory [?]: %s\n", workdir); 

        if (!getparstring("datafile", &value))
        {
                sprintf(msg, "Setup: Specify data file name!");
                Error(msg);
        }
        setpar(fp, "Datafile", "?", value, verbose);

        if (!getparstring("xcorrfile", &value))
        {
                sprintf(msg, "Setup: Specify crosscorrelation file name!");
                Error(msg);
        }
        setpar(fp, "Xcorrfile", "?", value, verbose);

        fclose(fp);

	*offset = 1.;

	printf("\n");
	/*sprintf(cmd, "cat %s", infile);
	system(cmd);*/
}

int ilog2(n)
	unsigned long n;
{
	int i;

	if (n <= 0)
	{
		printf("Help! values is %d, must be positive!\n", n);
		abort();
	}
	
 	i = 0;
	while ((int) (n & 1) == 0)
	{
		n >>= 1;
		i++;
	}
	return(i);
}


setpar(fp, prompt, defaultstring, s, show)
	FILE *fp;
	char *prompt;
	char *defaultstring;
	char s[80];
	int *show;
{
	if (*show)
		printf("%s [%s]: %s\n", prompt, defaultstring,s);
	if (strlen(s) == 0)
		strcpy(s, defaultstring);
	fprintf(fp, "%18s = %s\n", prompt, s);
}

getstring(s)
char s[];
{
	gets(s);
}

/*** end of file ***/
