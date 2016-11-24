/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
/*
 *  file:	format.h
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	specify the formats for the input and output files
 */
 
/* the input file is read in according to IN_FORMAT and IN_VARS */

#define IN_FORMAT " \
           Verbose = %d \
       Experiments = %d \
           Sources = %d \
         Receivers = %d \
              CMPs = %d \
              Fold = %d \
            Maxlag = %d \
                Dx = %f \
            Smooth = %f \
        Evolutions = %d \
Trials per Evolution = %d \
Subpopulation Size = %d \
Number of populations = %d \
  Structure Length = %d \
Crossover probability = %lf \
Mutation probability = %lf \
    Generation Gap = %lf \
    Scaling Window = %d \
   Report Interval = %d \
  Structures Saved = %d \
 Max Gens w/o Eval = %d \
     Dump Interval = %d \
       Dumps Saved = %d \
       Random Seed = %d \
          Rank Min = %lf \
         CG Active = %d \
   Min Evol for CG = %d \
Max number of iter in CG = %d \
          Datafile = %s \
         Xcorrfile = %s "

#define IN_VARS &verbose,&Totalexperiments,&NSOURCES,&NRECEIVERS,&NCMP,\
	&MAXFOLD,&TOTAL_LAG,&dx,&smooth,&Numevolutions,&Totaltrials,\
	&Popsize,&Ncities,&Length,&C_rate,&M_rate,&Gapsize,\
	&Windowsize,&Interval,\
	&Savesize,&Maxspin,\
	&Dump_freq,&Num_dumps,\
	&OrigSeed,&Rank_min,&uphill,&Min_evol_cg,&max_iter,datafile,\
	Xcorrfile

/*	LINE_FIN is the input format of each line of the outfile	*/
/*	used by the report program					*/
#define LINE_FIN "%lf %lf %lf %lf %lf %lf %lf %lf %lf"

#define LINE_VIN &line[0],&line[1],&line[2],&line[3],&line[4],\
	&line[5],&line[6],&line[7],&line[8]

/*	Output formats.		*/

/* OUT_FORMAT is the format for printing the input parameters */

#define OUT_FORMAT"\
           Verbose = %d\n\
       Experiments = %d\n\
           Sources = %d\n\
         Receivers = %d\n\
              CMPs = %d\n\
              Fold = %d\n\
            Maxlag = %d\n\
                Dx = %f\n\
            Smooth = %f\n\
        Evolutions = %d\n\
Trials per Evolution = %d\n\
Subpopulation Size = %d\n\
Number of populations = %d\n\
  Structure Length = %d\n\
Crossover probability = %lf\n\
Mutation probability = %lf\n\
    Generation Gap = %lf\n\
    Scaling Window = %d\n\
   Report Interval = %d\n\
  Structures Saved = %d\n\
 Max Gens w/o Eval = %d\n\
     Dump Interval = %d\n\
       Dumps Saved = %d\n\
       Random Seed = %d\n\
          Rank Min = %lf\n\
         CG Active = %d\n\
   Min Evol for CG = %d\n\
Max number of iter in CG = %d\n\
          Datafile = %s\n\
         Xcorrfile = %s " 

/* OUT_VARS are the parameters to be printed according to OUT_FORMAT */
#define OUT_VARS &verbose,&Totalexperiments,&NSOURCES,&NRECEIVERS,&NCMP,\
	&MAXFOLD,&TOTAL_LAG,&dx,&smooth,&Numevolutions,&Totaltrials,\
	&Popsize,&Ncities,&Length,&C_rate,&M_rate,&Gapsize,\
	&Windowsize,&Interval,\
	&Savesize,&Maxspin,\
	&Dump_freq,&Num_dumps,\
	&OrigSeed,&Rank_min,&uphill,&Min_evol_cg,&max_iter,datafile,\
	Xcorrfile

/* OUT_F2 is the format for the data produced by 'Measure'.
 * OUT_V2 describes the variables.
 */
#define OUT_F2 "%5d %5d %2d %2d %5.3f %.6e %.6e %.6e %.6e\n"

#define OUT_V2 Gen,Trials,Lost,Conv,Bias,Online,\
	Offline,Best,Ave_current_perf

/** end of file **/
