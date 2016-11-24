/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUIMPEDANCE: $Revision: 1.4 $ ; $Date: 2011/11/12 00:40:42 $	*/

#include "su.h"
#include "segy.h"


/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUIMPEDANCE - Convert reflection coefficients to impedances.  ",
"								",
" suimpedance <stdin >stdout [optional parameters]		",
"								",
" Optional Parameters:					  	",
" v0=1500.	Velocity at first sample (m/sec)		",
" rho0=1.0e6	Density at first sample  (g/m^3)		",
"								",
" Notes:							",
" Implements recursion [1-R(k)]Z(k) = [1+R(k)]Z(k-1).		",
" The input traces are assumed to be reflectivities, and thus are",
" expected to have amplitude values between -1.0 and 1.0.	",
"								",
NULL};

/* Credits:
 *	SEP: Stew Levin
 *
 * Trace header fields accessed: ns
 * 
 */
/**************** end self doc ***********************************/

static segy tr;

/* Prototype of function used internally */
static void rctoimp(float v0, float rho0, int nt, float *trace);

int
main(int argc, char **argv)
{
  float v0, rho0;
  int nt;
  
  /* Initialize */
  initargs(argc, argv);
  requestdoc(0);
  
  /* set parameters and fill header fields */
  if (!getparfloat("v0", &v0)) v0 = 1500.;
  if (!getparfloat("rho0", &rho0)) rho0 = 1000000.;

  /* Get info from first trace */
  if (!gettr(&tr))  err("can't get first trace");
  nt = tr.ns;

  do {
	/* apply reflectivity to impedence operation */
	rctoimp( v0, rho0, nt, tr.data );
	puttr(&tr); 	 	

	} while (gettr(&tr));

	return(CWP_Exit());
}

static void
rctoimp(float v0, float rho0, int nt, float *trace)
/***************************************************************************
rctoimp - convert reflection coefficient trace to impedence trace
****************************************************************************
Author: SEP: Stew Levin
**************************************************** ***********************/
{
  int it;
  double  zold, rc;

  zold = v0*rho0;

  for(it = 0; it < nt; ++it) {
	rc = trace[it];
	if(rc <= -1.0) rc = -0.9999;
	if(rc >=  1.0) rc =  0.9999;
	trace[it] = (float) zold;
	zold *= (1.0+rc) / (1.0-rc);
  }
}
