/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUNORMALIZE: $Revision: 1.8 $ ; $Date: 2011/11/16 17:23:05 $    */

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                ",
" SUNORMALIZE - Trace NORMALIZation by rms, max, or median       ", 
"               or median balancing                              ",
"                                                                ",
"   sunormalize <stdin >stdout t0=0 t1=TMAX norm=rms             ",
"                                                                ",
" Required parameters:                                           ",
"    dt=tr.dt    if not set in header, dt is mandatory           ",
"    ns=tr.ns    if not set in header, ns is mandatory           ",
"                                                                ",
" Optional parameters:                                           ",
"    norm=rms    type of norm rms, max, med , balmed             ",
"    t0=0.0      startimg time for window                        ",
"    t1=TMAX     ending time for window                          ",
"                                                                ",
" Notes:                                                         ",
" Traces are divided by either the root mean squared amplitude,  ",
" trace maximum, or the median value. The option \"balmed\" is   ",
" median balancing which is a shift of the amplitudes by the	 ",
" median value of the amplitudes.				 ",
"                                                                ",
NULL};

/*
 * Author: Ramone Carbonell, 
 *         Inst. Earth Sciences-CSIC Barcelona, Spain, April 1998.
 * Modifications: Nils Maercklin,
 *         RISSC, University of Naples, Italy, September 2006
 *         (fixed user input of ns, dt, if values are not set in header).
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: none
 */
/**************** end self doc ***********************************/

/* Definitions */
#define NORM_RMS 0
#define NORM_MAX 1
#define NORM_MED 2
#define NORM_BALMED 3

/* Structure used internally  */
typedef struct {
    int quot;               /* quotient */
    int rem;                /* remainder */
} cwp_div_t;

/* Prototypes for subroutines used internally */
int vrmedian(float *x,float *r,float *y,int *n);
void rmvesq(float *r,float *rsq,int *n);
void maxmgv(float *r,float *rmx,int *n);
cwp_div_t cwp_div( int num, int denom);

segy tr;

int
main(int argc, char **argv)
{
    int ns;                 /* number of samples */
    int it0;                /* first sample of time window */
    int it1;                /* last sample of time window */

    int n;                  /* size of temporary arrays */
    int i;                  /* counter */
    int itmp=0;               /* temporary variable */

    float dt;               /* time sampling interval */
    float t0;               /* first time of time window */
    float t1;               /* ending time of time window */

    float rms=0.0;          /* rms */
    float *tmp;             /* temporary array */
    float *z;               /* array */

    cwp_String norm="rms";  /* name of normalization */
    int inorm=NORM_RMS;     /* integer representing norm */

    /* Initialize */
    initargs(argc, argv);
    requestdoc(1);

    /* Get parameters */
    if (!fgettr(stdin, &tr)) err("can't read first trace");
    if (!tr.dt) {
        MUSTGETPARFLOAT("dt", &dt);
    }
    else {
        dt = ((double) tr.dt)/1000000.0;
    }
    if (!tr.ns) {
        MUSTGETPARINT("ns", &ns);
    }
    else {
        ns = (int) tr.ns;
    }

    if (!getparfloat("t0", &t0)) t0=0;
    if (!getparfloat("t1", &t1)) t1=ns*dt;

    /* Define integerized times */
    it0=t0/dt;
    it1=t1/dt;
    n=it1-it0; 

    /* Allocate space for temporary arrays */
    tmp=ealloc1float(n);
    z= ealloc1float(n);

    /* Get norm type */
    getparstring("norm", &norm);
    checkpars();

    if      (STREQ(norm, "max")) inorm = NORM_MAX;
    else if (STREQ(norm, "med")) inorm = NORM_MED;
    else if (STREQ(norm, "median")) inorm = NORM_MED;
    else if (STREQ(norm, "balmed")) inorm = NORM_BALMED;
    else if (!STREQ(norm, "rms"))
        err("unknown operation=\"%s\", see self-doc", norm);

    /* Loop over traces */
    do {
        switch(inorm) { /* beginning of cases */
            case NORM_RMS:
            {
                rmvesq(&tr.data[it0],&rms,&n); 
                if (rms==0.0) rms=1.;
                for (i=0;i<ns;i++) tr.data[i]=tr.data[i]/rms;
            }
            break;    
            case NORM_MED:
            {
                memcpy((void *) z,(const void *) &tr.data[it0], \
                    n*sizeof(float));
                itmp=vrmedian(z,&rms,tmp,&n); 
                if (rms==0.0) rms=1.;
                for (i=0;i<ns;i++) tr.data[i]=tr.data[i]/rms;
            }
            break;    
            case NORM_BALMED:
            {
                memcpy((void *) z,(const void *) &tr.data[it0], \
                    n*sizeof(float));
                itmp=vrmedian(z,&rms,tmp,&n); 
                if (rms==0.0) rms=1.;
                for (i=0;i<ns;i++) tr.data[i]=tr.data[i]-rms;
            }
            break;    
            case NORM_MAX:
            {
                maxmgv(&tr.data[it0],&rms,&n); 
                if (rms==0.0) rms=1.;
                for (i=0;i<ns;i++) tr.data[i]=tr.data[i]/rms;
            }
            break;
            default:  /* defensive programming */
                err("mysterious operation=\"%s\"", norm);
        } /* end of cases */
        
        puttr(&tr);
    } while (gettr(&tr));
            
    return(CWP_Exit());
}

/* Functions used internally */

int vrmedian(float *x,float *r,float *y,int *n)
/* vrmedian -- compute the median */
{
    int  two=2;
    cwp_div_t dv;

    dv=cwp_div(*n,two);

    qksort(*n,x);
    if ( dv.rem) *r=x[dv.quot];
    if (!dv.rem) *r=(x[dv.quot]+x[dv.quot+1])/2.;
    return(CWP_Exit());
}

void maxmgv(float *r,float *rmx,int *n)
/* find the maximum */
{
   int  j;
 
   *rmx=(fabs)(*(r));
   for (j=0;j<(*n);j++) {
       if ( (fabs)(*(r+j)) > (*rmx) ) (*rmx)=(fabs)(*(r+j));
   }
}


void rmvesq(float *r,float *rsq,int *n)
/* compute RMS */
{
   int  j;
 
   for (j=0;j<(*n);j++) {
       (*rsq)+=(*(r+j))*(*(r+j));
   }
   *rsq=sqrt((*rsq)/(*n));
}
 

/* this was originally the source code for div
 * Copyright (c) 1990, 1993
 *    The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    This product includes software developed by the University of
 *    California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


cwp_div_t cwp_div(int num, int denom)
{
    cwp_div_t r;

    r.quot = num / denom;
    r.rem = num % denom;
    /*
     * The ANSI standard says that |r.quot| <= |n/d|, where
     * n/d is to be computed in infinite precision.  In other
     * words, we should always truncate the quotient towards
     * 0, never -infinity.
     *
     * Machine cwp_division and remainer may work either way when
     * one or both of n or d is negative.  If only one is
     * negative and r.quot has been truncated towards -inf,
     * r.rem will have the same sign as denom and the opposite
     * sign of num; if both are negative and r.quot has been
     * truncated towards -inf, r.rem will be positive (will
     * have the opposite sign of num).  These are considered
     * `wrong'.
     *
     * If both are num and denom are positive, r will always
     * be positive.
     *
     * This all boils down to:
     *    if num >= 0, but r.rem < 0, we got the wrong answer.
     * In that case, to get the right answer, add 1 to r.quot and
     * subtract denom from r.rem.
     */
    if (num >= 0 && r.rem < 0) {
        r.quot++;
        r.rem -= denom;
    }
    return (r);
}
