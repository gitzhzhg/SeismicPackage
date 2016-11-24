/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"
#include "rke.h"

/*********************** self documentation **********************/

char *sdoc[] = {
"								",
" VERHULST - solve the VERHULST logistic equation		",
"								",
"  verhulst > [stdout]						",
"								",
" Required Parameters: none					",
" Optional Parameters:						",
" a1=1.0		parameter for verhulst equation		",
" a2=2000		parameter for verhulst equation		",
" y0=10			initial value of y[0]			",
" h=.01			increment in time			",
" tol=1.e-08		error tolerance				",
" stepmax=2000		maximum number of steps to compute	",
" mode=x		xy-pairs, =yz yz-pairs, =xz xz-pairs,	",
"			=xyz xyz-triplet, =x only, =y only, =z only",
" Notes:							",
" This program is really just a demo showing how to use the 	",
" differential equation solver rke_solve written by Francois 	",
" Pinard, based on a modified form of the 4th order Runge-Kutta ",
" method, which employs the error checking method of R. England ",
" 1969.								",
"								",
" The output consists of unformated C-style binary floats, of	",
" either pairs or triplets as specified by the \"mode\" paramerter.",
"								",
" Examples:							",
" x is the population						",
" verhulst stepmax=2000 mode=x | suaddhead ns=2000 | suxwigb &	",
" y is dx/dt, the rate of growth of the population		",
" verhulst stepmax=2000 mode=y | suaddhead ns=2000 | suxwigb &	",
"								",
" In the Verhulst equation, a1 is the reproduction rate and	",
" a2 is the carrying capacity					",
" 	x'(t) = a1 * x * ( 1 - x/a2 )			 	",
NULL};

/*
 * The verhulst equation describes a simplified model of a population
 * reproducing in an environment with limited resources,
 * and are given by the autonomous system of ODE's	
 *	y'(t) = a1 * y ( 1 - y/a2 )			
 *
 * Author: CWP: Aug 2009: John Stockwell
 */

/**************** end self doc ********************************/

/* Prototype of function used internally */
static int
verhulst_equation(double t, double y[1] , double yprime[1]);

/* Define values of imode */
#define SXY 0
#define SXZ 1
#define SYZ 2
#define SXYZ 3
#define SX 4
#define SY 5
#define SZ 6

int
main(int argc, char **argv)
{
	
	int i=0;		/* counter */
	int verbose=0;		/* verbose flag =1 chatty, =0 silent */
	int stepmax=0;		/* maximum number of steps */

	double t=0.0;		/* time */
	double h=.001;		/* time increment */
	double tol=0.0;		/* time increment */

	double y[3]={0.0};	/* dependent variable of ODE system */

	rke_variables p;	/* variable used by RKE routines */

	FILE *out_file=stdout;	/* pointer to file that we write out to */

	cwp_String mode="x";	/* output mode of program */
	int imode=SXY;		/* integer flag for mode */


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(0);

	switch(filestat(STDOUT)) { /* Prevent floats from dumping on screen */
	case BADFILETYPE:
		warn("stdout is illegal filetype");
		pagedoc();
	break;
	case TTY:
		warn("stdout can't be tty");
		pagedoc();
	break; 
	default:			   /* rest are OK */
	break;

	}

	/* Get parameters */
	if (!getparint("stepmax", &stepmax))	stepmax = 500;
	if (!getparint("verbose", &verbose))	verbose = 0;

	if (!getpardouble("y0", &y[0]))		y[0]=10;
	if (!getpardouble("h", &h))		h = .01;
	if (!getpardouble("tol", &tol))		tol = RKE_ERR_BIAS_INIT;

        /* Get output mode, recall imode initialized to the default FABS */
        getparstring("mode", &mode);
        if      (STREQ(mode, "yz"))    imode = SYZ;
        else if (STREQ(mode, "xz"))    imode = SXZ;
        else if (STREQ(mode, "x"))    imode = SX;
        else if (STREQ(mode, "y"))    imode = SY;
        else if (STREQ(mode, "z"))    imode = SZ;
        else if (!STREQ(mode, "xy"))
            err("unknown operation=\"%s\", see self-doc", mode);

	/* initialize Runge-Kutta-England routines */
	p = (rke_variables)
		rke_init(1, verhulst_equation);

	/* set tolerance */
	p->error_bias=tol;

	for (i=0; i<stepmax; ++i) {
		register int j;
		register int number=3;
		float yout[3]={0,0,0};
		double aimed_t;
		t=i*h;
		aimed_t=t+h;
  		if (verbose) {
			warn("using %3d accepted and %3d rejected steps",
	 			p->accepted_steps, p->rejected_steps);
			if (verbose) warn("error tolerance = %10.24f",p->error_bias);
		}

		/* convert doubles in y to floats in yout and write out */
		for(j=0; j<number; ++j) yout[j] = (float) y[j];

		/* write out according to the mode */
		{
		 float tmpout[2]={0,0};
		 switch(imode){ 

		 case SXY: /* write out xy pairs */
			tmpout[0]=yout[0];
			tmpout[1]=yout[1];
			efwrite(tmpout,sizeof(float),2,out_file);
		 break;
		 case SYZ: /* write out yz pairs */
			tmpout[0]=yout[1];
			tmpout[1]=yout[2];
			efwrite(tmpout,sizeof(float),2,out_file);
		 break;
		 case SXZ: /* write out xz pairs */
			tmpout[0]=yout[0];
			tmpout[1]=yout[2];
			efwrite(tmpout,sizeof(float),2,out_file);
		 break;
		 case SXYZ: /* write out xyz triplet */
			efwrite(yout,sizeof(float),3,out_file);
                 break;
		 case SX: /* write out x only */
			tmpout[0] = yout[0];
			efwrite(tmpout,sizeof(float),1,out_file);
                 break;
		 case SY: /* write out y only */
			tmpout[0] = yout[1];
			efwrite(tmpout,sizeof(float),1,out_file);
                 break;
		 case SZ: /* write out z only */
			tmpout[0] = yout[2];
			efwrite(tmpout,sizeof(float),1,out_file);
                 break;
                 default:  /* defensive programming */
                        err("mysterious operation=\"%s\"", mode);
                 } /* end scope of imode */
		}


		/* run the Runge-Kutta-England solver */
  		rke_solve (p, &t, y, aimed_t);
	}

	/* end the session with rke */
	rke_term(p);

	return EXIT_SUCCESS;
}


static int
verhulst_equation(double t, double y[3] , double yprime[3])
/*********************************************************************
verhulst_equation - the system of ODEs describing population growth
   by the logistic or verhulst equation.
**********************************************************************
t	independent variable "time"
y 	dependent variable being solved for y(t)
yprime	derivative of dependent variable  y'(t)
**********************************************************************
Notes: This is an example of an autonomous system of ODE's
**********************************************************************/
{
	double a1, a2;
	  
	if (!getpardouble("a1", &a1))		a1 = 1.0;
	if (!getpardouble("a2", &a2))		a2 = 2000;

	yprime[0] = a1*y[0]*( 1 - y[0]/a2 );
	y[1] = yprime[0];
    return 1;
}
