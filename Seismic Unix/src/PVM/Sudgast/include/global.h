/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	global.h
 *
 *  purpose:	global variables for genesis.
 */

#include "define.h"


/* File names */
char	Bestfile[100];	/* file of best structures		*/
char   Ckptfile[100];	/* check point file			*/
int    Curr_dump;	/* suffix of most recent dumpfile	*/
char   Dumpfile[100];	/* current dumpfile (if more than one)	*/
char   Initfile[100];	/* file of initial structures		*/
char   Infile[100];	/* input file				*/
char   Logfile[100];	/* logs starts and restarts		*/
char   Minfile[100];	/* file prefix of bestfile		*/
char   Outfile[100];	/* output file				*/
char   Schemafile[100];	/* file for record a schema's history	*/
char   Templatefile[100];/* file describing gene interpretation	*/
char   Reportfile[100];   /* file for record final report         */
char   FlowBestfile[100]; /* file for best member in each generation*/
char   Hammingfile[100];  /* file for hamming distance		*/
char   DownHillfile[100];  /* file for downhill results		*/
char   Modelsfile[100];     /* file for all downhill results        */

char   Answerfile[100];  /* Model related to best member         */
char   Xcorrfile[100];      /* Xcorrelations go to memory */
char   datafile[100];      /* seismic data file */

char   *Bitstring;	/* string representation of chromosome	*/
char   *DebugBuff;      /* Buffer for debugging			*/
double *Vector;		/* floating point representation	*/
int Genes;		/* number of interpreted genes		*/
int instance;		/* Subpopulation id			*/
GENESTRUCT *Gene;	/* pointer to interpretation records	*/
STRUCTURE  *Old;	/* pointer to population		*/
STRUCTURE  *New;	/* pointer to population		*/
STRUCTURE  *Exchange;   /* pointer to exchanged members         */
double *performance;    /* sorting purposes                     */
int *indx_new;          /* sorting purposes                     */
int *indx_exchange;     /* sorting purposes                     */
int **to_filter;         /* correct indexes for interpolation    */
double **to_be_calculated; /* members to be evaluated              */
double *eval_returned;   /* returned evaluations                 */
double **for_conjg;      /* used in the conjugate gradient comp  */
double *Perf_past;       /* used in the conjugate gradient comp  */

/* for the Xcorrelation file */
FILE *Xfp;               /* Xcorrelation file            */

BESTSTRUCT *Bestset;	/* set of best structures		*/

/* The Input file specifies these parameters */

int    Totalexperiments;/* number of experiments		*/
int    Totaltrials;	/* trials per experiment		*/
int    Popsize;		/* population size			*/
int    Ncities;         /* population size                      */
int    Numevolutions;   /* # of evolutions in the optimization  */
int    Min_evol_cg;	/* # after that evol CG will take place */
int    max_iter;	/* Max # of iterations			*/
int    Length;		/* bit length of a structure		*/
int    NSOURCES;	/* # of source statics 			*/
int    NRECEIVERS;	/* # of receiver statics 		*/
int    NCMP;             /* # CMPS                               */
int    TOTAL_LAG;	/* max Xcorr lag			*/
int    MAXFOLD;	        /* maximum fold			*/
int    smooth;          /* option for smoothing statics */
float  dx;	        /* receiver spacing*/
double  C_rate;		/* crossover rate			*/
double  M_rate;		/* mutation rate			*/
double  Gapsize;	/* fraction of pop replaced per gen	*/
/* double Fit_variance;     Fitness variance of the system       */      
/* double Fit_var_Begin;    Initial Fitness varianceof the system*/      
int    Windowsize;	/* used to update worst performance	*/
int    Interval;	/* trials between printing statistics	*/
int    Savesize;	/* number of structures in minfile	*/
int    Maxspin;		/* max gens without evals		*/
int    Dump_freq;	/* gens between checkpointing		*/
int    Num_dumps;	/* number of checkpoint files kept	*/
int    uphill;		/* defines if uphill will be done	*/
int    verbose;		/* specifies dialogue			*/
int    IS_CONVERGING_FLAG;	
			/* flag that indicates convergence	*/
char   Options[40];	/* option flags				*/
char   workdir[50];     /* working directory                    */
unsigned long    Seed;	/* seed for random number generator	*/
unsigned long    OrigSeed; /* original value for random seed	*/

/* data collection and loop control variables */
double  Ave_current_perf;/* ave perf in current generation	*/
double  Best;		/* best performance seen so far		*/
double  Best_current_perf;/* best perf in current generation	*/
int    Best_guy;	/* index of best_current_perf		*/
int    Worst_guy;	/* index of worst_current_perf		*/
int    Bestsize;	/* number of currently saved structures */
double  Bias;		/* ave. domination of alleles		*/
int    Bytes;		/* byte-length of packed structures	*/
int    Conv;		/* number of partially coverged genes	*/
char   Doneflag;	/* set when termination conditions hold	*/
int    Experiment;	/* experiment counter			*/
int    Gen;		/* generation counter			*/
int    GenTotal;        /* generation counter for ALL evolutions*/
int    TrialTotal;       /* trial counter for ALL evolutions*/
unsigned int    Initseed; /* seed used to initialize population	*/
int    Lost;		/* number of totally coverged positions */
int    Mu_next;		/* next mutated position		*/
double  Offline;	/* offline performance			*/
double  Offsum;		/* accumulator for offline performance	*/
double  Online;		/* online performance			*/
double  Onsum;		/* accumulator for online performance	*/
int    Plateau;		/* trial counter for next output	*/
double Rank_min;	/* minimum sampling rate under ranking	*/
int    Spin;		/* number of gens since eval occurred	*/
double  Totbest;	/* total for best			*/
double  Totoffline;	/* total for offline			*/
double  Totonline;	/* total for online			*/
int    Trials;		/* trial counter			*/
double  *Window;	/* circular queue of recent worsts	*/
double  Worst;		/* worst performance seen so far	*/
double  Worst_current_perf;/* worst perf in current generation	*/

/* flags set according to the Options string */
char   Allflag;		/*  evaluate all structures		*/
char   Bestflag;	/*  print final best value		*/
char   FlowBestflag;    /*  print best member of ALL sub_popul. */
char   RandomSent;      /*  Best or Random members exchanged    */
char   Collectflag; 	/*  collect performance data in outfile */
char   Convflag;	/*  collect convergence data in outfile */
char   Displayflag;	/*  display statistics each generation	*/
char   Dumpflag;	/*  dump after each evaluation		*/
char   Eliteflag;	/*  use elitist selection strategy	*/
char   Floatflag;	/*  convert strings to floating point	*/
char   Grayflag;	/*  use gray code 			*/
char   Initflag;	/*  read initial structures		*/
char   Interflag;	/*  interactive mode			*/
char   Lastflag;	/*  dump last generation		*/
char   Logflag;		/*  log starts and restarts		*/
char   Maxflag;		/*  maximize instead of minimize	*/
char   Offlnflag;	/*  print final offline measure		*/
char   Onlnflag;	/*  print final online measure		*/
char   Rankflag;	/*  used rank-based selection		*/
char   Restartflag;	/*  restart a run			*/
char   Schemflag;	/*  trace history of a schema		*/
char   Traceflag;	/*  trace execution			*/
char   Uphillflag;      /*  flag for conjugate gradient search  */
char   StepMutflag;     /*  flag for step or hamming mutation   */
char   DownHillflag;    /*  flag for downhill analysis	   	*/

/** end of file **/
