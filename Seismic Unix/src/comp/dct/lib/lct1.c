/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */




/*********************** self documentation **********************/
/***************************************************************************
LCT1 - functions used to perform the 1D Local Cosine Transform (LCT)

rcalloc - allocate the spaces for the rising-cutoff function
rcfmidp - midpoint version of the rising-cutoff function
lct     - the forward LCT
ilct    - the inverse LCT

***************************************************************************
Function Prototypes:
rcFunct *rcalloc(int hlen);
void rcfmidp(rcFunct *rcf);
void lct(float *sig, int n, rcFunct *rcf, float **c);
void ilct(float *sig, int n, rcFunct *rcf, float **c);

****************************************************************************
rcalloc:
Input:
hlen       radius of the rising-cutoff function

rcfmidp:
Input:
rcf        the rising-cutoff function

lct:
Input:
sig        the signal
n          the length of the signal
rcf        the rising-cutoff function used in the transform
c          transform table

ilct:
Input:
sig        the signal
n          the length of the signal
rcf        the rising-cutoff function used in the transform
c          transform table

****************************************************************************
Author:		Tong Chen, 05/22/95
***************************************************************************/

/**************** end self doc ********************************/
#include "comp.h"


/* functions used internally */
static float rcfis(int n, float t);
static void fipc(float *fneg, float *fpos, rcFunct *rcf);
static void fips(float *fneg, float *fpos, rcFunct *rcf);


rcFunct *rcalloc(int hlen)
/**************************************************************************
allocate the spaces for the rising-cutoff function
***************************************************************************
hlen        radius of the rising-cutoff function
**************************************************************************/
{
    rcFunct *rcf;

    rcf = (rcFunct *) malloc(sizeof(rcFunct));
    rcf->rlen = 2*hlen;
    rcf->rlast = hlen;
    rcf->rc = (float *) malloc((rcf->rlen)*sizeof(float));

    return(rcf);
}


void rcfmidp(rcFunct *rcf)
/**************************************************************************
rising cutoff function, sampled at midpoints
***************************************************************************
rcf        the rising-cutoff function
**************************************************************************/
{
    float x, dx;
    float *rc;
    int rlast;
    int j;

    rlast = rcf->rlast;
    rc = rcf->rc + rlast;
    
    x = .5/rlast;
    dx = 1./rlast;

    for(j=0; j<rlast; j++){
	rc[j] = rcfis(1, x);
	rc[-j-1] = rcfis(1, -x);
	x += dx;
    }
}


void lct(float *sig, int n, rcFunct *rcf, float **c)
/***************************************************************************
the forward local cosine transform (LCT), in place
****************************************************************************
sig        the signal
n          the length of the signal
rcf        the rising-cutoff function used in the transform
c          tranform table
***************************************************************************/
{
    fipc(sig, sig, rcf);
    fipc(sig+n, sig+n, rcf);
    dctiv(sig, n, c);
}


void ilct(float *sig, int n, rcFunct *rcf, float **c)
/***************************************************************************
the inverse local cosine transform (LCT), in place
****************************************************************************
sig        the signal
n          the length of the signal
rcf        the rising-cutoff function used in the transform
c          transform table
***************************************************************************/
{
    dctiv(sig, n, c);
    fips(sig, sig, rcf);
    fips(sig+n, sig+n, rcf);
}


static float rcfis(int n, float t)
/**************************************************************************
 define the N-th order differentiable iterated sine rising-cutoff function 
***************************************************************************
n      the differentiable order
t      input point

Output:
       the value of the rising-cutoff function at the input point
**************************************************************************/
{
    float tmp;
    int i;

    if(t>-1.) 
        if(t<1.){
	    tmp = t;
            for(i=0; i<n; i++)
		tmp = sin(.5*PI*tmp);
            tmp = sin(.25*PI*(1.+tmp));
	}
	else tmp = 1.;
    else tmp = 0.;

    return(tmp);
}


static void fipc(float *fneg, float *fpos, rcFunct *rcf)
/**************************************************************************
folding in place, cosine polarity
***************************************************************************
fneg      negative part of the folding 
fpos      positive part of the folding
rcf       rising-cutoff function
**************************************************************************/
{
    int k, rlast;
    float tmp, *rc;

    rlast = rcf->rlast;
    rc = rcf->rc + rlast;

    for(k=0; k<rlast; k++){
	tmp = rc[k]*fpos[k] + rc[-k-1]*fneg[-k-1];
	fneg[-k-1] = rc[k]*fneg[-k-1] - rc[-k-1]*fpos[k];
	fpos[k] = tmp;
    }
}


static void fips(float *fneg, float *fpos, rcFunct *rcf)
/**************************************************************************
folding in place, sine polarity
***************************************************************************
fneg      negative part of the folding 
fpos      positive part of the folding
rcf       rising-cutoff function
**************************************************************************/
{
    int k, rlast;
    float tmp, *rc;

    rlast = rcf->rlast;
    rc = rcf->rc + rlast;

    for(k=0; k<rlast; k++){
	tmp = rc[k]*fpos[k] - rc[-k-1]*fneg[-k-1];
	fneg[-k-1] = rc[k]*fneg[-k-1] + rc[-k-1]*fpos[k];
	fpos[k] = tmp;
    }
}
