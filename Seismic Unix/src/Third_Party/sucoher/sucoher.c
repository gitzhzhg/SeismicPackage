/* SUCOHER: $Revision: 1.0 $ ; $Date: 93/05/10 13:39:15 $		*/

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (jkc@dix.mines.colorado.edu)
 *
 * Modified from $CWPROOT/su/main/suvelan.c to replace each sample of a
 * given "central" trace of a receiver gather with the "best" summation
 * over all the traces in the gather through each of a set of slowness
 * lines through the given sample.
 *
 * -----------------------------------------------------------------------------
 * Tony Kocurko
 * Seismological Systems Manager                e-mail: tony@sparky.esd.mun.ca
 * Departmant of Earth Sciences                         akocurko@leif.ucs.mun.ca
 * Alexander Murray Building - Room ER-4063     office: (709) 737-4576
 * Memorial University of Newfoundland          fax   : (709) 737-2589
 * St. John's, Newfoundland                     TELEX : 016-4101 MEMORIAL SNF
 * Canada      A1B 3X5
 *----------------------------------------------------------------------
 */

#include "su.h"
#include "segy.h"
#include <sys/time.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUCOHER - replace each sample of centre trace with best slowness sum",
"									",
" sucoher <stdin >stdout [optional parameters]				",
"									",
" Optional Parameters:							",
" ns=12                   number of slowness intervals",
" ni=10                   number of interpolation intervals",
" fv=1500.0               first velocity (inverse of slowness)",
" lv=3000.0               last velocity  (inverse of slowness)",
" g1=0                    first receiver gather to treat",
" gn=-1                   receiver gather on which to end (< 0 => all)",
" m=-1                    index of \"centre\" trace",
"                         set to center trace of each group by default",
" verbose=0               &   1 track gathers",
"                         &   2 track samples",
"                         &   4 track slowness",
"                         &   8 track traces",
"                         &  16 track interpolation",
"                         &  32 track summation",
"                         &  64 track elapsed time",
"                         & 128 track total input traces time",
" every_n=1               if (verbose > 1) report every_n samples",
"									",
"									",
" Notes:",
" Input traces should be sorted by receiver group co-ordinates -",
" sucoher outputs a single trace every time the receiver group",
" co-ordinates change. Therefore, the output will ",
" be useful only if receiver gathers are input.				",
"									",
" Trace header fields accessed:  ns, dt, delrt, offset, gx, gy",
" Trace header fields modified:  none",
NULL};

/**************** end self doc *******************************************/

/* Credits:
 * -----------------------------------------------------------------------------
 * Tony Kocurko
 * Seismological Systems Manager                e-mail: tony@sparky.esd.mun.ca
 * Departmant of Earth Sciences                         akocurko@leif.ucs.mun.ca
 * Alexander Murray Building - Room ER-4063     office: (709) 737-4576
 * Memorial University of Newfoundland          fax   : (709) 737-2589
 * St. John's, Newfoundland                     TELEX : 016-4101 MEMORIAL SNF
 * Canada      A1B 3X5
 *----------------------------------------------------------------------
 * 
 * Modified from Dave Hale's suvelan to replace each sample of a 
 * given "central" trace of a receiver gather with the "best" summation
 * over all the traces in the gather through each of a set of slowness
 * lines through the given sample.
 *
 */

#define MAX_TRACES   1000

#define QUIET           0
#define LOG_GATHER      1
#define LOG_SAMPLE      2
#define LOG_SLOWNESS    4
#define LOG_TRACE       8
#define LOG_INTERP     16
#define LOG_SUM        32
#define LOG_TIME       64
#define LOG_TRACES    128

#define min(x,y)   ( (x) < (y) ?  (x) : (y) )
#define max(x,y)   ( (x) > (y) ?  (x) : (y) )
#define abs(x)     ( (x) <  0  ? -(x) : (x) )
#define sign(x)    ( (x) <  0  ?  -1  :  1  )

main (int argc, char **argv) {

	segy  *trace[MAX_TRACES] ; /* array of pointers to input traces           */
	segy   out_trace         ; /* output trace                                */
	struct timeval tp        ; /* timer structure                             */

	int    i                 ; /* loop counter over slownesses                */
	int    j                 ; /* loop counter over traces                    */
	int    k                 ; /* loop counter over gathers                   */
	int    m                 ; /* nearest index to crossing time of slowness  */
	int    g1                ; /* first receiver gather to treat              */
	int    gn                ; /* last receiver gather to treat               */
	int    nt                ; /* number of traces in current gather          */
	int    ni                ; /* number of interpolation intervals           */
	int    ns                ; /* number of samples in  "centre" trace        */
	int    ns_x              ; /* number of samples in  current trace         */
	int    mid               ; /* index of "centre" trace of receiver gather  */
	int    centre            ; /* sets index of "centre": < 0 => (mid = nt/2) */
	int    sample            ; /* index of "current" sample being computed    */
	int    slow_n            ; /* number of slownesses over which to sum      */
	int    max_traces        ; /* maximum number of traces / gather so far    */
	int    max_sums          ; /* maximum number of sums allocated so far     */
	int    max_out           ; /* maximum number of interpolated samples so.. */
	int    verbose           ; /* verboseness flag                            */
	int    every_n           ; /* if (verbose > 1) report every_n samples     */
	int   *sum_counts        ; /* counts of traces contributing to each sum   */
	int    nxout             ; /* number of interpolation values              */
	long   start_sec         ; /* seconds corresponding to start of program   */
	long   total_traces      ; /* count of total input traces                 */
	float  slowness          ; /* slowness over which to compute sample       */
	float  slow_low          ; /* first slowness to use                       */
	float  slow_high         ; /* last slowness to use                        */
	float  dslow             ; /* slowness increment                          */
	float  delrt             ; /* time of first sample of "centre" trace      */
	float  delrt_x           ; /* time of first sample of current trace       */
	float  dt                ; /* sample rate for "centre" trace              */
	float  dt_x              ; /* sample rate for current trace               */
	float  dt_out            ; /* sample rate of interpolated data            */
	float  delta_x           ; /* distance of jth trace from "centre" trace   */
	float  delta_t           ; /* time between "centre" sample and jth trace's*/
	float  x0                ; /* offset of "centre" trace from its shot point*/
	float  t0                ; /* time of first sample of "centre" trace      */
	float  time              ; /* time corresponding to "centre" trace sample */
	float  time_j            ; /* slowness crossing time on current trace     */
	float  tsample           ; /* floating point index of sample to accumulate*/
	float  d                 ; /* distance of crossing slowness to lower index*/
	float *sums              ; /* slowness sums                               */
	float *xout              ; /* interpolation abscissae                     */
	float *yout              ; /* interpolation ordinates                     */

	int    GetReceiverGather (segy **, int  , int   *, int           );
	void   ints8r            (int    , float, float  , float *, float,
	                          float  , int  , float *, float *       );

	/* get the current time in seconds */

	(void)gettimeofday (&tp, (struct timezone *)NULL);
	start_sec = tp.tv_sec;

	/* hook up getpar */

	initargs(argc,argv);
	requestdoc(0);

	/* get optional parameters */

	if ( !getparint   ("g1"     , &g1       )) g1        =    0  ;
	if ( !getparint   ("gn"     , &gn       )) gn        =   -1  ;
	if ( !getparint   ("ns"     , &slow_n   )) slow_n    =   12  ;
	if ( !getparint   ("ni"     , &ni       )) ni        =   10  ;
	if ( !getparfloat ("fv"     , &slow_high)) slow_high = 1500.0;
	if ( !getparfloat ("lv"     , &slow_low )) slow_low  = 3000.0;
	if ( !getparint   ("m"      , &centre   )) centre    =   -1  ;
	if ( !getparint   ("every_n", &every_n  )) every_n   =  100  ;
	if ( !getparint   ("verbose", &verbose  )) verbose   = QUIET ;

	if ( slow_low             == 0.0 ) err ("first velocity must be non-zero.");
	if ( slow_high            == 0.0 ) err ("final velocity must be non-zero.");
	if ( slow_n               <  1   ) err ("must have at least one velocity.");
	if ( slow_low * slow_high <= 0.0 ) err ("velocities must have same sign." );

	slow_low     =  1.0 / slow_low ;   /* higher velocity to lower  slowness */
	slow_high    =  1.0 / slow_high;   /* lower  velocity to higher slowness */

	dslow        =  (slow_high - slow_low) / (float)slow_n;
	max_traces   = 0;
	max_sums     = 0;
	max_out      = 0;
	total_traces = 0;

/* Read in a receiver gather */


	for (k=0; (nt=GetReceiverGather (trace,centre,&max_traces,MAX_TRACES)); k++) {

		if ( k <  g1            ) continue; /* haven't reached first gather yet  */
		if ( k >= gn && gn >= 0 ) break   ; /* finished final gather             */

		total_traces += nt;

		if ( verbose & LOG_GATHER )
			fprintf (stderr, "\ngather %d has %d traces", k, nt);

/* get index of "centre" trace */

		mid  = (centre < 0) ? nt / 2 : ( (centre >= nt) ? nt - 1 : centre);

/* copy "centre" trace to output trace */

		bcopy ((char *)trace[mid], (char *)&out_trace, sizeof(segy));

/* clear output trace data */

		bzero ((char *)&(out_trace.data), 4 * SU_NFLTS);

		ns   = trace[mid]->ns                        ; /* samples in centre trace */
		dt   = (float)(trace[mid]->dt    )/ 1000000.0; /* microseconds to seconds */
		t0   = (float)(trace[mid]->delrt )/    1000.0; /* milliseconds to seconds */
		x0   = (float)(trace[mid]->offset)           ; /* shotpoint to centre dist*/

/* allocate space for slow_n slowness sums for each sample of centre trace    */
/* and similarly, allocate space for slow_n counts of contributing traces for */
/* each sample of the centre trace                                            */

		if ( ns )                         /* If the centre trace has any data,    */
			if ( ns * slow_n > max_sums) {  /* and if insufficient space has been   */
				if ( max_sums ) {             /* allocated in the past, then free up  */
					free (sums)      ;          /* the space previously allocated and   */
					free (sum_counts); }        /* try to allocate a sufficient amount. */
				max_sums = ns * slow_n;
				if( (sums = (float*)calloc (max_sums, sizeof(float))) == (float *)NULL )
					err("couldn't allocate space for slant sums");
				if( (sum_counts = (int *)calloc(max_sums, sizeof(int)))== (int  *)NULL )
					err("couldn't allocate space for slant counts"); }

/* compute the total number of slowness sums to be held in memory. There is  */
/* one sum for each slowness at each sample. Then clear the sums and the     */
/* counts of traces contributing to each sum.                                */

		bzero ((char *)sums      , ns * slow_n * sizeof(float));
		bzero ((char *)sum_counts, ns * slow_n * sizeof(float));

/* For each trace in the gather, . . . */

		for (j = 0; j < nt; j++) {

			if ( verbose & LOG_TRACE )
				fprintf (stderr, "\nWorking on trace %d", j);

/* Get sample rate, time of first sample, number of samples, and distance from*/
/* the "centre" trace of the current trace. For our purposes, we will take the*/
/* distances to be negative for traces that occur "left" of the "centre" trace*/
/* with respect to trace index. So that if j < mid, then we multiply the mag- */
/* nitude of the difference in offsets by -1, and otherwise by +1.            */

			dt_x    = (float)trace[j]->dt    / 1000000.0;
			delrt_x = (float)trace[j]->delrt /    1000.0;
			ns_x    = trace[j]->ns;
			delta_x = abs ((float)trace[j]->offset - x0) * (float)(sign(j - mid));

/* Allocate enough space for the selected level of interpolation of the trace.*/
/* Suppose that the user wants each input sample interval to have ten interpo-*/
/* lation intervals. That is we want each interval to go from this:           */
/*                                                                            */
/* j-1                  j          j-1                  j                     */
/*  |-------------------| to this:  |-+-+-+-+-+-+-+-+-+-|                     */
/*                                  0 1 2 3 4 5 6 7 8 9                       */
/*                                                                            */
/* Then there will be ten output samples for each input sample except for the */
/* last input sample, which will be represented by itself alone on output.    */
/*                                                                            */
/* In general, then, if the user asks for n_itrp interpolation samples per in-*/
/* put sample, and if there are ns_x input samples, then there will be a total*/
/* of n_itrp * (ns_x - 1) + 1 interpolated output samples.                    */
/*                                                                            */
			if ( ns_x )
				if ( ni * (ns_x - 1) + 1 > max_out ) {    /* Need more space?         */
					if ( max_out ) {                        /* Space previously saved?  */
						free (xout);                          /* Free previously saved    */
						free (yout); }                        /* abscissae and ordinates. */
					max_out = ni * (ns_x - 1) + 1;          /* New maximum saved space. */

					if ((xout = (float *)calloc(max_out, sizeof(float))) == (float *)NULL)
						err("Unable to allocate interpolation abscissae array");
					if ((yout = (float *)calloc(max_out, sizeof(float))) == (float *)NULL)
						err("Unable to allocate interpolation array"); }

/* Compute total number of output interpolated samples, then clear them.      */

			nxout  = ni * (ns_x - 1) + 1;
			dt_out = dt / (float)ni;
			bzero ((char *)yout, nxout * sizeof(float));

/* Initialize the interpolation abscissae, in this case time.                */

			for (i = 0; i < nxout; i++)
				xout[i] = delrt + i * dt_out;

/* CWP canned routine to do sinc interpolation.                              */

			ints8r (ns_x          , dt_x             , delrt_x               ,
			        trace[j]->data, trace[j]->data[0], trace[j]->data[ns_x-1],
			        nxout         , xout             , yout                  );

/* Current trace now contributes to a sum for each sample of the "centre"    */
/* trace and for each slowness value. The interpolated current trace is used */
/* to supply amplitudes for those slowness lines that do not intersect the   */
/* current trace at an input sample.                                         */

/* For each sample of the "centre" trace . . .                               */

			for ( sample = 0; sample < ns; sample++) {

				if ( (verbose & LOG_SAMPLE) && sample % every_n == 0 )
					fprintf (stderr, "\nStarting sample %d of gather %d ... ", sample, k);

			  time = t0 + (float)sample * dt; /* Time corresponding to this sample */

/* For each slowness value, . . . */

				for ( i = 0; i < slow_n; i++) {

/* By definition lines with slopes equal to slownesses pass through each      */
/* sample of the "centre" trace. Thus, we only need to accumulate the "centre"*/
/* trace's sample values.                                                     */

					if ( j == mid ) {
						sums      [i + sample * slow_n] += trace[j]->data[sample];
						sum_counts[i + sample * slow_n]++;
						if ( verbose & LOG_INTERP )
							fprintf (stderr, "\ny[%d](%f) = %f +> sum[%d] = %f",
							                    j, time, trace[j]->data[sample], i, sums[i]);
						continue; }

					if ( verbose & LOG_SLOWNESS )
						fprintf (stderr, "\nWorking on slowness %d", i);

/* Compute the time (and thereby the sample) at which the slowness line     */
/* through the "centre" trace's current sample intercepts the current trace */

					slowness = slow_low + i * dslow;
					delta_t  = slowness * delta_x;
					time_j   = time + delta_t;
					tsample  = (time_j - delrt_x) * (float)(nxout - 1)
					         / (dt_x              * (float)(ns_x  - 1));

/* If the slowness line intercepts the current trace in its time range,   */
/* interpolate a value on that trace and accumulate it into the sum for   */
/* the current slowness through the current sample of the "centre" trace. */

					if ( 0.0 <= tsample && tsample < nxout ) {
						sums      [i + sample * slow_n] += yout[(int)tsample];
						sum_counts[i + sample * slow_n]++;
						if ( verbose & LOG_INTERP )
							fprintf (stderr, "\ny[%d](%f) = %f +> sum[%d] = %f",
							                    j, time_j, yout[(int)tsample], i,
							                    sums[i + sample * slow_n]);
						} /* ---------------------------------> End of tsample range test */
					}   /* ---------------------------------> End of slowness loop      */
				}     /* ---------------------------------> End of sample loop        */
			}       /* ---------------------------------> End of trace loop         */

/* For each sample in the output trace, . . . */
			for (i = 0; i < ns; i++) {

/* Point to the first slowness as having given the "best" sum, whether it did */
				j = 0;

/* Then for each slowness at this sample . . . */
				for (k = 0; k < slow_n; k++) {

/* Calculate the average based on the number of traces that contributed to it.*/

					sums[k + i * slow_n] /= (float)sum_counts[k + i * slow_n];

/* If this average is of greater magnitude than previous best, point to it.*/
					if (abs (sums[k + i * slow_n]) > abs(sums[j + i * slow_n]) ) j = k; }

/* Set the output sample to the "best" of the average sums at the input sample*/
				out_trace.data[i] = sums[j + i * slow_n]; }

/* Write out the trace. */
		puttr (&out_trace); }   /* End of GetReceiverGather loop */

	if ( verbose & LOG_TRACES )
		fprintf (stderr, "\ntotal input traces = %d\n", total_traces);

	(void)gettimeofday (&tp, (struct timezone *)NULL);
	if ( verbose & LOG_TIME )
		fprintf (stderr, "\nelapsed time in seconds = %d\n", tp.tv_sec - start_sec);

	exit(0); }

int
GetReceiverGather (segy **trace, int centre, int *n, int max_traces) {

	                            /* trace      = array of pointers to traces     */
	                            /* centre     = index of "centre" trace         */
	                            /* n          = number traces allocated so far  */
	                            /* max_traces = maximum # traces permitted      */
	int         i, j          ; /* counters of traces in next receiver gather   */
	long        gx            ; /* receiver group x- and y-coordinates defining */
	long        gy            ; /*   the next input receiver group              */
	Bool        done          ; /* Goes true on EOF or new receiver group       */
	static segy previous_trace; /* holds 1st trace of subsequent receiver gather*/

	i = 0;

	if ( feof (stdin) ) return(i);

	if ( *n ) {
		bcopy ((char *)&previous_trace, (char *)trace[0], sizeof(segy));
		i = 1; }
	else if ( (trace[0] = (segy *)calloc (1, sizeof(segy))) == (segy *)NULL ) {
		err   ("can't allocate space for first trace.");
		*n = 1; }
	else if ( !gettr (trace[0]) )
		err   ("can't get first trace of gather.");

	gx = trace[0]->gx;
	gy = trace[0]->gy;

	done = false;
	for (j = 1; j < *n; j++ ) {
		if ( !gettr (trace[j]) ) {
			done = true;
			break; }
		if ( trace[j]->gx != gx || trace[j]->gy != gy ) {
			bcopy ( (char *)trace[j], (char *)&previous_trace, sizeof(segy) );
			done = true;
			break; }
		i++; }

	if ( ! done ) {
		for (*n = j; gettr (&previous_trace); *n = *n + 1) {
			if ( previous_trace.gx != gx || previous_trace.gy != gy ) break;
			if ( *n >= max_traces ) err ("can't allocate any more traces.");
			if ( (trace[*n] = (segy *)calloc (1, sizeof(segy))) == (segy *)NULL )
				err ("can't allocate space for a new trace.");
			bcopy ((char *)&previous_trace, (char *)trace[*n], sizeof(segy));
			i++; } }

	return (i); }
