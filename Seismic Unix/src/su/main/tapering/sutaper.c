/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTAPER: $Revision: 1.18 $ ; $Date: 2011/11/16 23:33:10 $	*/

#include "su.h"
#include "segy.h"

#define TWOPI 2.0*PI

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUTAPER - Taper the edge traces of a data panel to zero.	",
"								",
"								",
" sutaper <stdin >stdout [optional parameters]		  ",
"								",
" Optional Parameters:					  ",
" ntr=tr.ntr	number of traces. If tr.ntr is not set, then	",
" 		ntr is mandatory				",
" tr1=0	 number of traces to be tapered at beginning	",
" tr2=tr1	number of traces to be tapered at end		",
" min=0.		minimum amplitude factor of taper		",
" tbeg=0		length of taper (ms) at trace start		",
" tend=0		length of taper (ms) at trace end		",
" taper=1	taper type					",
"		 =1 linear (default)			   ",
"		 =2 sine					",
"		 =3 cosine					",
"		 =4 gaussian (+/-3.8)			  ",
"		 =5 gaussian (+/-2.0)			  ",
"								",
" Notes:							",
"   To eliminate the taper, choose tbeg=0. and tend=0. and tr1=0",
"								",
NULL};

/* Credits:
 *
 *	CWP: Chris Liner, Jack K. Cohen
 *
 * Trace header fields accessed: ns, ntr
 * 
 * Rewrite: Tagir Galikeev, October 2002
 */
/**************** end self doc ***********************************/

segy tr;

/* Prototypes for functions used internally */
void taper( float t1, float t2, int tap_type, float T, float dt, 
		float *trace );

void weights ( int tr1, int tr2, float max, float min, int type, float *w);

int
main(int argc, char **argv)
{
  float t1, t2;
  float dt, tlen;
  float *taperv=NULL;	/* vector of taper weights  */
  int   ntap;		/* dimension of taper array */
  float min;	
  float max=1.;	
  int   tr1, tr2; 	/* traces to be tapered	*/
  int   nt, ntr, ttaper; 
  int   itr=0;  		/* trace counter */
  short verbose;  /* if 1(yes) echo parameters to stderr  */  
  cwp_Bool have_ntr=cwp_false;/* is ntr known from header or user?*/
  
  /* Initialize */
  initargs(argc, argv);
  requestdoc(0);

  if (!getparshort("verbose", &verbose))  verbose = 0;
  
  /* get minimum amplitude parameter */ 
  if (!getparfloat("min", &min)) 	min = 0.;
	if (min > 1.0) err("min must be less than 1");

  /* get parameters for time domain taper */
  if (!getparfloat("tbeg", &t1)) 	t1 = 0.;
  if (!getparfloat("tend", &t2)) 	t2 = 0.;
  if (!getparint("taper", &ttaper)) 	ttaper = 1;

  /* get trace parameters */
  if (!getparint("tr1", &tr1))   	tr1 = 0;
  if (!getparint("tr2", &tr2))   	tr2 = tr1;

  /* define taper weights for trace tapering */
  ntap = ( (tr1-tr2) ? tr1+tr2+2 : tr1+1 ) ;
  taperv = ealloc1float(ntap);
  weights(tr1,tr2,max,min,ttaper,taperv);
  
  if (verbose) { register int i;	/* output taper weights */
	fprintf(stderr,"print %i taper weights: \n",ntap);
	for (i=0;i<ntap;i++) 
	   fprintf(stderr,"Taper %i = %g \n",i,taperv[i]);
  }	   
   
  /* Get info from first trace */
  if (!gettr(&tr))  err("can't get first trace");
  nt = tr.ns;
  dt = tr.dt / 1000.; /* in ms */	
  /* Get or set ntr */
  ntr = tr.ntr;
  if (ntr) have_ntr = cwp_true;
  if (getparint("n2", &ntr) || 
	getparint("ntr", &ntr)) have_ntr = cwp_true;
  if (!have_ntr) err ("ntr neither set nor getparred");
  checkpars();

  tlen=(nt-1)*dt; /* trace length, ms */
  if (t1 + t2 > tlen)
	err("sum of tapers tbeg=%f, tend=%f exceeds trace length(%f ms)", 
	t1,t2,tlen);
  
  do { float fac=1. ;		/* trace weighting factor */
	
	/* factor for first traces */
	fac*=( itr < tr1 ? *taperv++ : 1. );

	/* add (tr2+1)-1 to the array pointer if assymetric tapering is required 
	 * tr1=tr2  : ntap-tr1-2 = -1
	 * tr1!=tr2 : ntap-tr1-2 = tr2 */
	if ( itr++ == tr1 ) taperv+=ntap-tr1-2; 
	
	/* factor for last traces */
	fac*=( itr > (ntr-tr2) ? *taperv-- : 1. );
	
	if (verbose && 
	 (tr1+tr2) && 
	 (fac < 1.) ) fprintf(stderr,"trace %i factor %g \n",itr,fac);
	
	if ( tr1 || tr2 ) { register int i;	
	   for (i=0;i<nt;i++)
		tr.data[i]*=fac;
	}
	  
	if ( (t1!=0.) || (t2!=0.) )
	  taper( t1, t2, ttaper, tlen, dt, tr.data );
	puttr(&tr); 	 	
	} while (gettr(&tr));

  return(CWP_Exit());
}


#define EPS	3.8090232	/* exp(-EPS*EPS) = 5e-7, "noise" level  */
				/* see sugain.c				*/
void taper ( float t1, float t2, int tap_type, float T, float dt, 
		float *trace )
/*********************************************************************
sweep taper - tapers the sweep
**********************************************************************
Input: 
t1	  start taper in ms
t2	  end taper in ms
tap_type  type of taper to apply: 1 linear, 2 sine, 3 cosine
T	  trace duration in ms
dt	  sample rate in ms
 
Output:
trace	  array of tapered samples
*********************************************************************
This subroutine tapers a sweep mainly to reduce Gibbs phenomena.
Taper coulld be one of the specified above.
*********************************************************************
References:
Any book on Vibroseis.
*********************************************************************
Author: Tagir Galikeev				  Date:7 Oct 1994
Rewrite: Tagir Galikeev				  Date:  Oct 2002
*********************************************************************/
{
	int nt, i, nt1, nt2;
	float env=0.0, f, x;

	nt = (int)(T / dt + 1);
	nt1 = (int)(t1 / dt + 1);
	nt2 = (int)(t2 / dt + 1);
	/* apply start taper */
	if( nt1 > 1 ) {
		for (i=0; i<nt1; i++) {
	  		f = (float)i / (float)nt1;
	  		switch ((char) tap_type)	{
	  			case 1: env=f;
	  				break;
	  			case 2: env=sin(PI*f/2.);
	  				break;
	  			case 3: env=0.5*(1.0-cos(PI*f));
	  				break;
	  			case 4: x=EPS*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			case 5: x=2.0*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			default:err (" taper ?!");
	  		}
	  		trace[i] *= env;
		}
	}
	/* apply end taper */
	if( nt2 > 1 ) {
		for (i=0; i<nt2; i++) {
	  		f = (float)i / (float)nt2;
			switch ((char) tap_type)	{
	  			case 1: env=f;
	  				break;
	  			case 2: env=sin(PI*f/2.);
	  				break;
	  			case 3: env=0.5*(1.0-cos(PI*f));
	  				break;
	  			case 4: x=EPS*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			case 5: x=2.0*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			default:err (" taper ?!");
	  		}
			trace[nt-i]  *= env;
		}
	}
}


void weights ( int tr1, int tr2, float max, float min, int type, float *w)
/*********************************************************************
sweep taper - tapers the sweep
**********************************************************************
Input: 
tr1	  number of traces to apply begin taper
tr2	  number of traces to apply end taper 
max	maximum amplitude factor (=1.)
min	  minimum amplitude factor (=0.)
type	  type of taper to apply: 1 linear, 2 sine, 3 cosine
 
Output:
w	  array of taper weights 
*********************************************************************
This subroutine computes the taper weights 
*********************************************************************
Author: Tagir Galikeev				  Date:7 Oct 1994
Rewriten: Gerald Klein				  Date:31 Mar 2004
*********************************************************************/
{ 
   if ( tr2 && (tr1-tr2) ) { /* end taper differs from begin taper */
		register int i;
		float env=0.0, f, x;
		/* set taper weights for last traces; fill array from end */
		for (i = 0; i <= tr2; ++i) {
			f = (float) (i)/tr2;
			switch ((char) type)	{
	  			case 1: env = min + (max - min) * f;
	  				break;
	  			case 2: env=sin(PI*f/2.);
	  				break;
	  			case 3: env=0.5*(1.0-cos(PI*f));
	  				break;
	  			case 4: x=EPS*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			case 5: x=2.0*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			default:err (" taper ?!");
	  		}
			w[1+tr1+i] = env ;
		} 		
   } 	
   if (tr1) { 	/* set taper weights for first traces */
		register int i;
		float env=0.0, f, x;
		   for (i = 0; i <= tr1; i++) {
			f = (float) (i)/tr1;
			switch ((char) type)	{
	  			case 1: env = min + (max - min) * f;
	  				break;
	  			case 2: env=sin(PI*f/2.);
	  				break;
	  			case 3: env=0.5*(1.0-cos(PI*f));
	  				break;
	  			case 4: x=EPS*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			case 5: x=2.0*(1-f);
	  				env=exp(-(x*x));
	  				break;
	  			default:err (" taper ?!");
	  		}
			  w[i] = env ;
		}
   }
   return;
}
