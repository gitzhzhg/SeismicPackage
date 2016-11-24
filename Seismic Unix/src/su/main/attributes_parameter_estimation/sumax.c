/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMAX: $Revision: 1.22 $ ; $Date: 2015/08/07 21:54:19 $		*/

#include "su.h"
#include "segy.h"

/*************************** self documentation **************************/
char *sdoc[] = {
"									",
" SUMAX - get trace by trace local/global maxima, minima, or absolute maximum",
"									",
" sumax <stdin >stdout [optional parameters] 			",
"									",
" Required parameters:						",
"	none								",
"									",
" Optional parameters: 						",
"	output=ascii 		write ascii data to outpar		",
"				=binary for binary floats to stdout	",
"				=segy for SEGY traces to stdout		",
"									",
"	mode=maxmin		output both minima and maxima		",
"				=max maxima only			",
"				=min minima only			",
"				=abs absolute maxima only      		",
"				=rms RMS 		      		",
"				=thd search first max above threshold	",
"									",
"	threshamp=0		threshold amplitude value		",
"	threshtime=0		tmin to start search for threshold 	",
"									",
"	verbose=0 		writes global quantities to outpar	",
"				=1 trace number, values, sample location",
"				=2 key1 & key2 instead of trace number  ",
"	key1=fldr		key for verbose=2                       ",
"	key2=ep			key for verbose=2                       ",
"									",
"	outpar=/dev/tty		output parameter file; contains output	",
"					from verbose			",
"									",
" Examples: 								",
" For global max and min values:  sumax < segy_data			",
" For local and global max and min values:  sumax < segy_data verbose=1	",
" To plot values specified by mode:					",
"    sumax < segy_data output=binary mode=modeval | xgraph n=npairs	",
" To plot seismic data with the only values nonzero being those specified",
" by mode=modeval:							",
"    sumax < segy_data output=segy mode=modeval | suxwigb		",
"  									",
" Note:	while traces are counted from 1, sample values are counted from 0.",
"	Also, if multiple min, max, or abs max values exist on a trace,	",
"       only the first one is captured.					",
"									",
" See also: suxmax, supsmax						",
NULL};

/* Credits:
 *	CWP : John Stockwell (total rewrite)
 *	Geocon : Garry Perratt (all ASCII output changed from %e to %e)
 *	                       (added mode=rms).
 *      ESCI: Reginald Beardsley (added header key option)
 *	based on an original program by:
 *	SEP: Shuki Ronen
 *	CWP: Jack K. Cohen
 *      IFM-GEOMAR: Gerald Klein (added threshold option) 
 *
 * Trace header fields accessed: ns dt & user specified keys
 */

/**************** end self doc ***********************************/


segy   tr;

int      main(int argc, char **argv)
{
   char    *outpar;		/* name of file holding output parfile  */
   FILE    *outparfp;		/* ... its file pointer                 */
   char    *output;		/* format (ascii/binary/segy) of output */
   char    *mode;		/* desired output maxmin/max/min/abs    */
   char    *key1;
   char    *key2;
   char    ident[256];          /* trace identification string */

   int      verbose;		/* flag to print extra information      */

   int      nt;			/* number of time points on trace       */
   int      ntr;		/* number of traces                     */
   float    dt;			/* time sampling interval               */

   int      itr = 0;		/* trace number -- bumped at loop top   */
   float    fitr = 0;		/* trace number (as a float)            */
   int      it = 0;		/* time point number                    */

   int      gmaxit = 0;		/* global max sample counter            */
   int      gminit = 0;		/* global min sample counter            */
   int      gabsmaxit = 0;	/* global abs max sample counter        */

   int      gmaxitr = 1;	/* global max trace counter             */
   int      gminitr = 1;	/* global min trace counter             */
   int      gabsmaxitr = 1;	/* global abs max trace counter         */

   int      maxit = 0;		/* sample number of max value           */
   int      minit = 0;		/* sample number of min value           */
   int      absmaxit = 0;	/* sample number of abs max value       */

   float    max;		/* max on a trace                       */
   float    min;		/* min on a trace                       */
   float    absmax;		/* absolute max on a trace              */

   int      trhldit = 0;	/* sample number of max after threshold */
   int      trhldfit;		/* first sample to search for threshold */
   int      trhldgit = 0;	/* sample of global max                 */
   int      trhldgitr = 0;	/* trace of global max                  */
   float    trhldstrt;		/* first time to search for threshold   */
   float    trhldamp;		/* threshold value                      */
   float    trhldmax;		/* first max value after threshold      */
   float    trhldgmax = 0.;	/* global max value after threshold     */

   float    gmax;		/* global max                           */
   float    gmin;		/* global min                           */
   float    gabsmax;		/* global absolute max                  */

   float    grms = 0.0;		/* global rms                           */
   float    grmssumsq = 0.0;	/* global sum of sample value squared   */
   float    rms = 0.0;		/* rms on a trace                       */
   float    rmssumsq = 0.0;	/* sum of sample value squared          */
   float    val;		/* temp for adding to rmssumsq          */
   int ival1=0;
   int ival2=0;
   Value tval;

   /* Initialize */
   initargs(argc, argv);
   requestdoc(1);

   /* Get optional parameters */
   if (!getparint("verbose", &verbose))
      verbose = 0;
   if (!getparstring("output", &output))
      output = "ascii";
   if (!(STREQ(output, "ascii") ||
	 STREQ(output, "binary") || STREQ(output, "segy")
       ))
      err("%s unknown output selection", output);

   if (!getparstring("key1", &key1))
      key1 = "fldr";
   if (!getparstring("key2", &key2))
      key2 = "ep";

   if (!getparstring("mode", &mode))
      mode = "maxmin";
   if (!(STREQ(mode, "maxmin") ||
	 STREQ(mode, "max") ||
	 STREQ(mode, "min") ||
	 STREQ(mode, "thd") || STREQ(mode, "abs") || STREQ(mode, "rms")
       ))
      err("%s unknown mode", mode);

   if (STREQ(mode, "rms") && STREQ(output, "segy")) {
      err("%s output incompatible with %s mode", output, mode);
   }

   if (STREQ(output, "binary") && STREQ(mode, "maxmin")) {
      mode = "max";
      warn("outputting max values");
   }

   if (!getparstring("outpar", &outpar))
      outpar = "/dev/tty";
   outparfp = efopen(outpar, "w");

   /* Get info from first trace */
   if (!gettr(&tr))
      err("can't get first trace");
   nt = tr.ns;
   if (!getparfloat("dt", &dt))
      dt = ((double) tr.dt) / 1000000.0;

   gmin = gmax = tr.data[0];
   gabsmax = ABS(tr.data[0]);

   /* Get/Set variables for threshold mode */
   if (STREQ(mode, "thd")) {
      if (!getparfloat("threshamp", &trhldamp))
	 trhldamp = 0.;
      if (!getparfloat("threshtime", &trhldstrt))
	 trhldstrt = 0.;
      trhldfit = (int) (trhldstrt / dt);

   }
   checkpars();

   /* Loop through data */
   do {
      itr++;

      /* get header values if requested */
      if( verbose == 2 ){
         gethdval(&tr ,key1 ,&tval);
         ival1 = vtoi(hdtype(key1), tval);
         gethdval(&tr ,key2 ,&tval);
         ival2 = vtoi(hdtype(key2), tval);
      }


      /* find local/global max/min values and their locations */
      min = max = tr.data[0];
      absmax = ABS(tr.data[0]);
     /* tr.data[0] = 0.0;	*/ /* Zero first data value */
     /* trhldmax = tr.data[0]; */
     trhldmax = 0.0 ;
      trhldfit = (int) (trhldstrt / dt);
      for (it = 1; it < nt; ++it) {
	 if (tr.data[it] > max) {	/* Find max */
	    max = tr.data[it];
	    maxit = it;
	 }

	 if (tr.data[it] < min) {	/* Find min */
	    min = tr.data[it];
	    minit = it;
	 }

	 if (ABS(tr.data[it]) > absmax) {	/* Find absmax */
	    absmax = ABS(tr.data[it]);
	    absmaxit = it;
	 }

	 if (it >= trhldfit) {	/* Find max after threshold */
	    if (tr.data[it] > trhldamp) {
	       if (tr.data[it] > trhldmax) {
		  trhldmax = tr.data[it];
		  trhldit = it;
	       }
	    }
	    /* else if (trhldmax > trhldamp) trhldfit = nt+1 ; */
	    /* maximum found */
	 }

	/* pick up square of first sample for rms calculation */
	 rmssumsq = tr.data[0]*tr.data[0];
	 grmssumsq = tr.data[0]*tr.data[0];
	 if (STREQ(mode, "rms")) {
	    val = tr.data[it];
	    rmssumsq += val * val;
	    grmssumsq += val * val;
	 }

	 tr.data[it] = 0.0;	/* zero data values */

      }

      if (STREQ(mode, "rms")) {
	 rms = sqrt(rmssumsq / nt);
      }
      rmssumsq = 0.0;	/* reset sum squared */

      /* prepare SEGY output */
      if (STREQ(output, "segy")) {
	 if (STREQ(mode, "maxmin")) {
	    tr.data[maxit] = max;
	    tr.data[minit] = min;
	 }
	 if (STREQ(mode, "max")) {
	    tr.data[maxit] = max;
	 }
	 if (STREQ(mode, "min")) {
	    tr.data[minit] = min;
	 }
	 if (STREQ(mode, "abs")) {
	    tr.data[absmaxit] = absmax;
	 }
	 if (STREQ(mode, "thd")) {
	    tr.data[trhldit] = trhldmax;
	 }
      }
      if (max > gmax) {	/* Find global max */
	 gmax = max;
	 gmaxitr = itr;
	 gmaxit = maxit;
      }

      if (min < gmin) {	/* Find global min */
	 gmin = min;
	 gminitr = itr;
	 gminit = minit;
      }
      if (absmax > gabsmax) {	/* Find global min */
	 gabsmax = absmax;
	 gabsmaxitr = itr;
	 gabsmaxit = absmaxit;
      }
      if (trhldmax > trhldgmax) {	/* Find global first max above
					   threshold */
	 trhldgmax = trhldmax;
	 trhldgitr = itr;
	 trhldgit = trhldit;
      }
      /* Output ASCII */
      if (STREQ(output, "ascii") && verbose) {

         if( verbose == 2){
            sprintf( ident ,"%d %d" ,ival1, ival2 );

         }else{
            sprintf( ident ,"%d" ,itr );

         }
	 /* max min */
	 if (STREQ(mode, "maxmin")) {
	    fprintf(outparfp, "%s %e at point %d time %e\n",
		    ident, max, maxit, dt * (float) maxit);
	    fprintf(outparfp, "   %e at point %d time %e\n",
		    min, minit, dt * (float) minit);
	 }
	 /* max */
	 if (STREQ(mode, "max")) {
	    fprintf(outparfp, "%s %e at point %d time %e\n",
		    ident, max, maxit, dt * (float) maxit);
	 }

	 /* min */
	 if (STREQ(mode, "min")) {
	    fprintf(outparfp, "%s %e at point %d time %e\n",
		    ident, min, minit, dt * (float) minit);
	 }

	 /* abs max */
	 if (STREQ(mode, "abs")) {
	    fprintf(outparfp, "%s %e at point %d time %e\n",
		    ident, absmax, absmaxit, dt * (float) absmaxit);
	 }

	 /* threshold */
	 if (STREQ(mode, "thd")) {
	    fprintf(outparfp, "%s %e at point %d time %e\n",
		    ident, trhldmax, trhldit, dt * (float) trhldit);
	 }

	 /* rms */
	 if (STREQ(mode, "rms")) {
	    fprintf(outparfp, "%s %e\n", ident, rms);
	 }
      }

      /* Output raw BINARY pairs to stdout */
      fitr = itr;	/* convert itr to floats, for binary output */
      if (STREQ(output, "binary")) {
	 if (STREQ(mode, "max")) {
	    efwrite(&fitr, FSIZE, 1, stdout);
	    efwrite(&max, FSIZE, 1, stdout);
	 }
	 if (STREQ(mode, "min")) {
	    efwrite(&fitr, FSIZE, 1, stdout);
	    efwrite(&min, FSIZE, 1, stdout);
	 }
	 if (STREQ(mode, "abs")) {
	    efwrite(&fitr, FSIZE, 1, stdout);
	    efwrite(&absmax, FSIZE, 1, stdout);
	 }
	 if (STREQ(mode, "thd")) {
	    efwrite(&fitr, FSIZE, 1, stdout);
	    efwrite(&trhldmax, FSIZE, 1, stdout);
	 }
	 if (STREQ(mode, "rms")) {
	    efwrite(&fitr, FSIZE, 1, stdout);
	    efwrite(&rms, FSIZE, 1, stdout);
	 }
      }
      /* Output SEGY data */
      if (STREQ(output, "segy"))
	 puttr(&tr);

   } while (gettr(&tr));

   ntr = itr;	/* Set ntr */

   /* Output global max min */
   if (STREQ(mode, "maxmin")) {
      if (verbose) {
	 fprintf(outparfp,
		 "global max = %e, at trace = %d, at sample = %d\n",
		 gmax, gmaxitr, gmaxit);
	 fprintf(outparfp,
		 "global min = %e, at trace = %d, at sample = %d\n",
		 gmin, gminitr, gminit);
      } else {
	 fprintf(outparfp, "%e %e\n", gmax, gmin);
      }
   }

   /* Output global max */
   if (STREQ(mode, "max")) {
      if (verbose) {
	 fprintf(outparfp,
		 "global max = %e, at trace = %d, at sample = %d\n",
		 gmax, gmaxitr, gmaxit);
      } else {
	 fprintf(outparfp, "%e\n", gmax);
      }
   }

   /* Output global min */
   if (STREQ(mode, "min")) {
      if (verbose) {
	 fprintf(outparfp,
		 "global min = %e, at trace = %d, at sample = %d\n",
		 gmin, gminitr, gminit);
      } else {
	 fprintf(outparfp, "%e\n", gmin);
      }
   }

   /* Output global abs max */
   if (STREQ(mode, "abs")) {
      if (verbose) {
	 fprintf(outparfp,
		 "global abs max = %e, at trace = %d, at sample = %d\n",
		 gabsmax, gabsmaxitr, gabsmaxit);
      } else {
	 fprintf(outparfp, "%e\n", gabsmax);
      }
   }

   /* Output global rms */
   if (STREQ(mode, "rms")) {
      grms = sqrt(grmssumsq / (itr * nt));
      if (verbose) {
	 fprintf(outparfp, "global rms = %e\n", grms);
      } else {
	 fprintf(outparfp, "%e\n", grms);
      }
   }

   /* Output global threshold max */
   if (STREQ(mode, "thd")) {
      if (verbose) {
	 fprintf(outparfp,
		 "global max = %e, at trace = %d, at sample = %d\n",
		 trhldgmax, trhldgitr, trhldgit);
      } else {
	 fprintf(outparfp, "%e\n", trhldgmax);
      }
   }

   /* If output=binary, print number of pairs to outpar */
   if (STREQ(output, "binary"))
      fprintf(outparfp, "npairs = %d\n", ntr);

   return (CWP_Exit());
}
