/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	extern.h
 *
 *  purpose:	external declarations for genesis.
 */

#include "define.h"

/*char *calloc();
char *malloc();*/


/*************************************** from parameters.h **/

/* The Input file specifies these parameters */

extern int    Totalexperiments;	/* number of experiments		*/
extern int    Totaltrials;	/* trials per experiment		*/
extern int    Popsize;		/* population size			*/
extern int    Ncities;          /* # of cities in the optimization      */
extern int    Numevolutions;    /* # of evolutions in the optimization  */
extern int    Min_evol_cg;      /* # after that evol CG will take place */
extern int    max_iter;         /* Max # of iterations                  */
extern int    Length;		/* bit length of a structure		*/
extern int    NSOURCES;        /* # of source statics                  */
extern int    NRECEIVERS;      /* # of receiver statics                */
extern int    NCMP;      	/* # CMPS 				*/
extern int    TOTAL_LAG;       /* max Xcorr lag                        */
extern int    MAXFOLD;	       /* maximum fold			*/
extern int    smooth;	       /* option for smoothing statics */
extern float  dx;	       /* receiver spacing*/
extern double  C_rate;		/* crossover rate			*/
extern double  M_rate;		/* mutation rate			*/
extern double  Gapsize;		/* fraction of pop replaced per gen	*/
/*extern double Fit_variance;     Fitness variance of the system       */      
/*extern double Fit_var_Begin;    Initial Fitness varianceof the system*/      
extern int    Windowsize;	/* used to update worst performance	*/
extern int    Interval;		/* trials between printing statistics	*/
extern int    Savesize;		/* number of structures in minfile	*/
extern int    Maxspin;		/* max gens without evals		*/
extern int    Dump_freq;	/* gens between checkpointing		*/
extern int    Num_dumps;	/* number of checkpoint files kept	*/
extern int    uphill;		/* defines if uphill will be done	*/
extern int    verbose;		/* specifies dialogue			*/
extern char   Options[];	/* option flags				*/
extern char   workdir[];        /* working directory                    */
extern unsigned long Seed;	/* seed for random number generator	*/
extern unsigned long OrigSeed;	/* original value for random seed	*/
extern 		int    IS_CONVERGING_FLAG;      
                        	/* flag that indicates convergence      */

/********************************************  from global.h   ******/

/*	Global variables.	*/


/* File names */
extern char   Bestfile[];	/* file of best structures		*/
extern char   Ckptfile[];	/* check point file			*/
extern int    Curr_dump;	/* suffix of most recent dumpfile	*/
extern char   Dumpfile[];	/* current dumpfile (if more than one)	*/
extern char   Initfile[];	/* file of initial structures		*/
extern char   Infile[];		/* input file				*/
extern char   Logfile[];	/* logs starts and restarts		*/
extern char   Minfile[];	/* file prefix of bestfile		*/
extern char   Outfile[];	/* output file				*/
extern char   Schemafile[];	/* file for record a schema's history	*/
extern char   Templatefile[];	/* file describing gene interpretation	*/
extern char   Reportfile[];   	/* file for record final report         */
extern char   FlowBestfile[]; 	/* file for best member in each generat.*/
extern char   Hammingfile[]; 	/* file for hamming distance		*/
extern char   DownHillfile[]; 	/* file for downhill results		*/
extern char   Modelsfile[]; 	/* file for all downhill results	*/
extern char   Answerfile[];   	/* Model related to best member         */
extern char   Xcorrfile[];      /* Xcorrelations go to memory */
extern char   datafile[];       /* seismic data file                    */

extern char   *Bitstring;	/* string representation of chromosome	*/
extern char   *DebugBuff;       /* buffer for debugging                 */
extern double *Vector;		/* floating point representation	*/
extern int Genes;		/* number of interpreted genes		*/
extern int instance;           /* Subpopulation id                     */

extern GENESTRUCT *Gene;	/* pointer to interpretation records	*/
extern STRUCTURE  *Old;		/* pointer to population		*/
extern STRUCTURE  *New;		/* pointer to population		*/
extern STRUCTURE *Exchange;     /* pointer to exchanged members         */
extern double *performance;     /* sorting purposes                     */
extern int *indx_new;           /* sorting purposes                     */
extern int *indx_exchange;      /* sorting purposes                     */
extern int **to_filter;         /* correct indexes for interpolation    */
extern BESTSTRUCT *Bestset;	/* set of best structures		*/
extern double **to_be_calculated;
				/* members to be evaluated		*/
extern double *eval_returned;	/* returned evaluations			*/
extern double **for_conjg;	/* used in the conjugate gradient comp  */
extern double *Perf_past;	/* used in the conjugate gradient comp  */

/* for the Xcorrelation file */
extern FILE *Xfp;  		/* Xcorrelation file            */

/* data collection and loop control variables */
extern double  Ave_current_perf;/* ave perf in current generation	*/
extern double  Best;		/* best performance seen so far		*/
extern double  Best_current_perf;/* best perf in current generation	*/
extern int    Best_guy;		/* index of best_current_perf		*/
extern int    Worst_guy;       /* index of worst_current_perf          */
extern int    Bestsize;		/* number of currently saved structures */
extern double  Bias;		/* ave. domination of alleles		*/
extern int    Bytes;		/* byte-length of packed structures	*/
extern int    Conv;		/* number of partially coverged genes	*/
extern char   Doneflag;		/* set when termination conditions hold	*/
extern int    Experiment;	/* experiment counter			*/
extern int    Gen;		/* generation counter			*/
extern int    GenTotal;         /* generation counter for ALL evolutions*/
extern int    TrialTotal;       /* trial counter for ALL evolutions*/
extern unsigned int    Initseed; /* seed used to initialize population	*/
extern int    Lost;		/* number of totally coverged positions */
extern int    Mu_next;		/* next mutated position		*/
extern double  Offline;		/* offline performance			*/
extern double  Offsum;		/* accumulator for offline performance	*/
extern double  Online;		/* online performance			*/
extern double  Onsum;		/* accumulator for online performance	*/
extern int    Plateau;		/* trial counter for next output	*/
extern double Rank_min;		/* minimum sampling rate under ranking	*/
extern double  Totbest;		/* total for best			*/
extern double  Totoffline;	/* total for offline			*/
extern double  Totonline;	/* total for online			*/
extern int    Trials;		/* trial counter			*/
extern double  *Window;		/* circular queue of recent worsts	*/
extern double  Worst;		/* worst performance seen so far	*/
extern double  Worst_current_perf;/* worst perf in current generation	*/
extern int    	Spin;		/* number of gens since eval occurred	*/

/* flags set according to the Options string */
extern char   Allflag;		/*  evaluate all structures		*/
extern char   Bestflag;		/*  print final best value		*/
extern char   FlowBestflag;     /*  print best member of ALL sub_popul. */
extern char   RandomSent;       /*  Best or Random members exchanged    */
extern char   Collectflag; 	/*  collect performance data in outfile */
extern char   Convflag;		/*  collect convergence data in outfile */
extern char   Displayflag;	/*  display statistics each generation	*/
extern char   Dumpflag;		/*  dump after each evaluation		*/
extern char   Eliteflag;	/*  use elitist selection strategy	*/
extern char   Floatflag;	/*  convert strings to floating point	*/
extern char   Grayflag;		/*  use gray code 			*/
extern char   Initflag;		/*  read initial structures		*/
extern char   Interflag;	/*  interactive mode			*/
extern char   Lastflag;		/*  dump last generation		*/
extern char   Logflag;		/*  log starts and restarts		*/
extern char   Maxflag;		/*  maximize instead of minimize	*/
extern char   Offlnflag;	/*  print final offline measure		*/
extern char   Onlnflag;		/*  print final online measure		*/
extern char   Rankflag;		/*  used rank-based selection		*/
extern char   Restartflag;	/*  restart a run			*/
extern char   Schemflag;	/*  trace history of a schema		*/
extern char   Traceflag;	/*  trace execution			*/
extern char   Uphillflag;	/*  flag for conjugate gradient search  */
extern char   StepMutflag;	/*  flag for step or hamming mutation  */
extern char   DownHillflag;	/*  flag for downhill analysis	       */
/** end of file **/
