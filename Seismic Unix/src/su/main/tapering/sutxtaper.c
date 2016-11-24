/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTXTAPER: $Revision: 1.4 $ ; $Date: 2011/11/16 23:33:10 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

#define TWOPI 2.0*PI

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                               ",
" SUTXTAPER - TAPER in (X,T) the edges of a data panel to zero.	",
"                                                               ",
" sutxtaper <stdin >stdout [optional parameters]		",
"                                                               ",
" Optional Parameters:                                          ",
" low=0.    	minimum amplitude factor of taper		",
" tbeg=0    	length of taper (ms) at trace start		",
" tend=0     	length of taper (ms) at trace end		",
" taper=1       taper type                                      ",
"                 =1 linear (default)                           ",
"                 =2 sine                                       ",
"                 =3 cosine                                     ",
"                 =4 gaussian (+/-3.8)                          ",
"                 =5 gaussian (+/-2.0)                          ",
" key=tr	set key to compute x-domain taper weights	",
"               default is using internal tracecount (tr)       ",
" tr1=0         number of traces to be tapered at beg (key=tr)	",
" tr2=tr1       number of traces to be tapered at end (key=tr)	",
" 								",
" min=0.	minimum value of key where taper starts (amp=1.)",
" max=0.	maximum value of key where taper starts (amp=1.)",
" dx=1. 	length of taper (in key units)			",
"		if key=tr (unset) length is tr1 and (ntr-tr2)	",
"                                                               ",
" Notes:                                                        ",
"   Taper type is used for trace (x-domain) tapering as well 	",
"   as for time domain tapering.				",
"   The taper is applied to all traces <tr1 (or key<min) and    ",
"   >tr2 (or key >max) and all time samples <tbeg and >tend. 	",
"   Taper weights are amp*1 for traces n tr1<n<tr2 and computed	",
"   for all other traces corresponding to the taper typ.	",
"   If key is given the taper length is defined by dx, otherwise",
"   the length of taper is tr1 and (ntr-tr2) respectively.	",
"   To eliminate the taper, choose tbeg=0. and tend=0. and tr1=0",
"   If key is set, min,max values take precedence over tr1,tr2.	",
"                                                               ",
NULL};

/* Credits: (based on sutaper)
 *
 *	CWP: Chris Liner, Jack K. Cohen
 *
 * Trace header fields accessed: ns
 * 
 * Rewrite: Tagir Galikeev, October 2002
 * Rewrite: Gerald Klein, IFM-GEOMAR, April 2004
 */
/**************** end self doc ***********************************/

segy tr;
  float *taperv=NULL;		/* vector of taper weights	*/
  float *x2=NULL;		/* vector of taper weights	*/

/* Prototypes for functions used internally */
void taper( float t1, float t2, int tap_type, float T, float dt, 
	    float *trace );

void weights ( int tr1, int tr2, float amp, float low, int type, float *w);

void xweights( const float *y, float ymax, float ymin, float dy, 
               float low, float amp, int ntap, int taper_type, float *w);    

int
main(int argc, char **argv)
{
  float t1, t2;
  float dt, tlen;
  float *trbuf;			/* trace buffer			*/
  int   ntap;			/* dimension of taper array	*/
  float low;	
  float amp=1.;	
  int   tr1, tr2;		/* traces to be tapered		*/
  int   nt, ntr, ttaper; 
  int   itr=0;  		/* trace counter		*/
  short verbose;		/* if 1(yes) echo parameters to stderr  */  
  short debug=0;		/* set to 1 for debugging information */
  cwp_Bool have_ntr=cwp_false;	/* is ntr known from header or user?*/
  cwp_Bool isDepth=cwp_false;	/* Is this key a type of depth? */
  cwp_Bool isCoord=cwp_false;	/* Is this key a type of coordinate?*/

  /* variables for irregular key values of scaling */
  char *key;			/* header key word from segy.h	*/
  char *type=0;			/* ... its type			*/	
  int index=0;			/* ... its index		*/
  Value val;			/* ... its value		*/

  float dkey, min, max;   /* taper definition in x dimension */
  
  Value scale;            /* Value of scaler                      */
  cwp_String type2=NULL;  /* ... its type                         */
  int index2=0;           /* ... its index                        */

  FILE *headerfp;	/* fp for header storage file          */
  FILE *tracefp,*x2fp;  /* fp for data storage file and offkey values */
 /*   float *x2;             * float array of x2 values                  */
  float xmin=FLT_MAX,xmax=-FLT_MAX;
 
  /* Initialize */
  initargs(argc, argv);
  requestdoc(0);

  if (!getparshort("verbose", &verbose))  verbose = 0;
  
  /* get minimum amplitude parameter */ 
  if (!getparfloat("low", &low)) 	low = 0.;
     if (low > 1.0) err("low must be less than 1");

  /* get parameters for time domain taper */
  if (!getparfloat("tbeg", &t1)) 	t1 = 0.;
  if (!getparfloat("tend", &t2)) 	t2 = 0.;
  if (!getparint("taper", &ttaper)) 	ttaper = 1;

  /* get parameters for distance domain taper */
  if (!getparstring("key", &key)) 	key = "tr";
  if (!getparfloat("min", &min)) 	min = 0.;
  if (!getparfloat("max", &max)) 	max = 0.;
  if (!getparfloat("dx", &dkey)) 	dkey = 1.;
  
  /* get trace parameters */
  if (!getparint("tr1", &tr1))   	tr1 = 0;
  if (!getparint("tr2", &tr2))   	tr2 = tr1;

  /* Get info from first trace */
  if (!gettr(&tr))  err("can't get first trace");
  nt = tr.ns;
  dt = tr.dt / 1000.; /* in ms */	
  
  /* Get or set ntr */
  ntr = tr.ntr;
  if (ntr) have_ntr = cwp_true;
  if (getparint("n2", &ntr) || 
      getparint("ntr", &ntr)) have_ntr = cwp_true;
  if (!have_ntr) warn ("ntr neither set nor getparred, \n \
                 traces will be counted ");

  /* Get key type and index */
  if ( strcmp(key, "tr") != 0 ) {
  	type = hdtype(key);
	index = getindex(key);
  }

  tlen=(nt-1)*dt; /* trace length, ms */
  if (t1 + t2 > tlen)
    err("sum of tapers tbeg=%f, tend=%f exceeds trace length(%f ms)", 
	t1,t2,tlen);

  if (debug) fprintf(stderr,"Getparred key=%s index=%i\n",key,index);
  
/* count traces apply time domain taper and get header values **/
  checkpars();

  headerfp = etmpfile();
  tracefp = etmpfile();
  x2fp = etmpfile();

  if (debug) fprintf(stderr,"Tempfiles created \n");
  
  {  float x;
     int   nx=0;
  trbuf = ealloc1float(nt);

  do {  /* count traces */
	++nx;

     if (debug) fprintf(stderr,"Trace nx=%i ... ",nx);

      if (index) {
	
	gethval(&tr,index,&val);

	if (isDepth || isCoord) {
		gethval(&tr,index2,&scale);
		x = (float) (vtod(type,val) *
		pow(10.0,vtod(type2,scale)));
	} else
		x = vtof(type,val);

      } else 
                x=(float) nx ;
		
      if (debug) fprintf(stderr," x=%g ",x );
		
	efwrite(&x, sizeof(float), 1, x2fp);
	
	xmin = MIN(xmin,x);
	xmax = MAX(xmax,x);

      if (debug) fprintf(stderr," to x-file " );

	if ( (t1!=0.) || (t2!=0.) )
          taper( t1, t2, ttaper, tlen, dt, tr.data );	  

      if (debug) fprintf(stderr,", time tapered " );

        efwrite(&tr,HDRBYTES,1,headerfp);
	efwrite(tr.data, FSIZE, nt, tracefp);

      if (debug) fprintf(stderr,", header+data written \n");
      } while (gettr(&tr));
		    
  if (debug) fprintf(stderr,"Trace count nx=%i ntr=%i ",nx,ntr);
  if (!have_ntr) ntr=nx ;
  if (debug) fprintf(stderr,"set ntr=%i\n",ntr);
  
 }

  /* read header values of distance key */
    x2 = ealloc1float(ntr);
  erewind(x2fp);
  efread(x2,sizeof(float),ntr,x2fp);  
  
  if (debug) for (itr=0;itr<ntr;itr++) fprintf(stderr,"x2(%i)=%g\n",itr,x2[itr]);

  /* define taper weights for trace tapering */
  if (debug) fprintf(stderr,"calling weights to define taper weights \n");

  if (index) { /* set taper weights from x2 values */
    ntap=ntr;
    taperv = ealloc1float(ntap);
    if (verbose) fprintf(stderr,"Using x values of key %s \n",key); 
    xweights(x2,max,min,dkey,low,amp,ntap,ttaper,taperv); 
    tr1=ntr+1;
    tr2=-ntr;   
  } else {      /* set taper weights from traces */ 
    ntap = ( (tr1-tr2) ? tr1+tr2+2 : tr1+1 ) ;
    taperv = ealloc1float(ntap);
    if (verbose) fprintf(stderr,"Using trace count \n"); 
    weights(tr1,tr2,amp,low,ttaper,taperv);  
  }

  if (debug) fprintf(stderr,"returned from weights \n");
  
  if (verbose>1) { register int i;
     fprintf(stderr,"print taper weights: \n");
       for (i=0;i<ntap;i++) 
           fprintf(stderr,"Taper %i = %g (x=%g) \n",i,taperv[i],x2[i]);
  }	   

  erewind(headerfp);
  erewind(tracefp);

  for (itr=0;itr<ntr; itr++) { 
  
      float fac=1. ;		/* trace weighting factor */
        
	efread (&tr, 1, HDRBYTES, headerfp);
	efread (trbuf, FSIZE, nt, tracefp);
				
	/* factor for first traces */
	
	
	fac*= ( itr < tr1 ? *taperv++ : 1. );

	/* add (tr2+1)-1 to the array pointer if assymetric tapering is required 
	 * tr1=tr2  : ntap-tr1-2 = -1
	 * tr1!=tr2 : ntap-tr1-2 = tr2 */
	if ( itr == tr1 ) taperv+=ntap-tr1-2; 
	
	/* factor for last traces */
	fac*=( itr > (ntr-tr2-1) ? *taperv-- : 1. );
	
      if ( verbose && 
          (tr1+tr2) && 
	  (fac < 1.) ) fprintf(stderr,"trace %i factor %g \n",itr,fac);
       
       { register int i;       
           for (i=0;i<nt;i++)
	        tr.data[i]=fac*trbuf[i];
       }

        
	tr.unscale= 1./fac ;
	
       puttr(&tr); 	 	
  } 
  
  
  efclose(tracefp);
  efclose(headerfp);
  efclose(x2fp);
  return(CWP_Exit());
}


#define EPS     3.8090232       /* exp(-EPS*EPS) = 5e-7, "noise" level  */
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


void weights ( int tr1, int tr2, float amp, float low, int type, float *w)
/*********************************************************************
sweep taper - tapers the sweep
**********************************************************************
Input: 
tr1	  number of traces to apply begin taper
tr2	  number of traces to apply end taper 
amp       maximum amplitude factor (=1.)
low	  minimum amplitude factor (=0.)
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
	  			case 1: env = low + (amp - low) * f;
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
	  			case 1: env = low + (amp - low) * f;
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

void xweights( const float *y, float ymax, float ymin, float dy, 
               float low, float amp, int ntap, int taper_type, float *w)
/*********************************************************************
trace tapers from x2 header valus
**********************************************************************
Input: 
type	  type of taper to apply: 1 linear, 2 sine, 3 cosine
 
Output:
w	  array of taper weights 

*********************************************************************
This subroutine computes the taper weights 
*********************************************************************
Author: Gerald Klein				  Date:31 Mar 2004
*********************************************************************/
{
  register int i;
  float env=0.0, f=0., x;
  
  for (i=0;i<ntap;i++) {

      /*  Reduce weights in the range of  min-dy  to  min 
       *                        and       max     to  max+dy
       *  set to  0. for y < min-dy and to 1. for y > max+dy
       */

     if  (y[i] < ymin) {
        if (y[i] > (ymin-dy)) {
		             f = 1. - ( ymin - y[i] ) / dy ;
        } else f = 0. ;
     } else 
     if (y[i] > ymax)  {
        if (y[i] < (ymax+dy)) {
	                     f = ( ymax + dy - y[i] ) / dy  ;
        } else  f=0. ;
     } else  f=1. ;

    /* limit weight range from  low  to  amp */
    /*f = low + (amp - low) * f;*/
    
    /* apply shape of the taper corresponding to taper_type */
      switch ((char) taper_type)	{
	  			case 1: env = low + (amp - low) * f;
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
  return ;
}
