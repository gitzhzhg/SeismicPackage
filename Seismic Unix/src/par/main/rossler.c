/* Copyright (c) Colorado School of Mines, 2013.*/
/* All rights reserved.                       */

#include "par.h"
#include "rke.h"

/*********************** self documentation **********************/

char *sdoc[] = {
"								",
" ROSSLER - compute the ROSSLER attractor				",
"								",
"  rossler > [stdout]						",
"								",
" Required Parameters: none					",
" Optional Parameters:						",
" a=.2			parameter for rossler equations		",
" b=.2			parameter for rossler equations		",
" c=5.7			parameter for rossler equations		",
" y0=1.0		initial value of y[0]			",
" y1=-1.0		initial value of y[1]			",
" y2=1.0		initial value of y[2]			",
" h=.01			increment in time			",
" tol=1.e-08		error tolerance				",
" stepmax=500		maximum number of steps to compute	",
" mode=xy		xy-pairs, =yz yz-pairs, =xz xz-pairs,	",
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
" rossler stepmax=1000 mode=xy | xgraph n=1000	&		",
" rossler stepmax=1000 mode=yz | xgraph n=1000	&		",
" rossler stepmax=1000 mode=xz | xgraph n=1000	&		",
"								",
" rossler stepmax=1000 mode=x | suaddhead ns=1000 | suxwigb &	",
" rossler stepmax=1000 mode=y | suaddhead ns=1000 | suxwigb &	",
" rossler stepmax=1000 mode=z | suaddhead ns=1000 | suxwigb &	",
"								",
" GNUPLOT 3D plot example:                                      ",
" rossler stepmax=2000 mode=xyz > rossler.bin                   ",
" ...when you run gnuplot type the following command ...        ",
" splot \"rossler.bin\" binary record=2000:2000:2000 with points pointsize .1 ",
"                                                               ",
NULL};

/*
 * The rossler equations describe a simple example of a chaotic system
 * and are given by the autonomous system of ODE's	
 *
 *	x'(t) = - y - z
 *	y'(t) = x + a y
 *	z'(t) = b + z(x - c)
 *
 * Author: CWP: Aug 2013: John Stockwell
 */

/**************** end self doc ********************************/

/* Prototype of function used internally */
static int
rossler_equations(double t, double y[3] , double yprime[3]);

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

	double y[3]={0.0,0.0,0.0};	/* dependent variable of ODE system */

	rke_variables p;	/* variable used by RKE routines */

	FILE *out_file=stdout;	/* pointer to file that we write out to */

	cwp_String mode="xy";	/* output mode of program */
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

	if (!getpardouble("y0", &y[0]))		y[0] = 1.0;
	if (!getpardouble("y1", &y[1]))		y[1] = -1.0;
	if (!getpardouble("y2", &y[2]))		y[2] = 1.0;
	if (!getpardouble("h", &h))		h = .01;
	if (!getpardouble("tol", &tol))		tol = RKE_ERR_BIAS_INIT;

        /* Get output mode, recall imode initialized to the default FABS */
        getparstring("mode", &mode);
        if      (STREQ(mode, "yz"))    imode = SYZ;
        else if (STREQ(mode, "xz"))    imode = SXZ;
        else if (STREQ(mode, "xyz"))    imode = SXYZ;
        else if (STREQ(mode, "x"))    imode = SX;
        else if (STREQ(mode, "y"))    imode = SY;
        else if (STREQ(mode, "z"))    imode = SZ;
        else if (!STREQ(mode, "xy"))
            err("unknown operation=\"%s\", see self-doc", mode);

	/* initialize Runge-Kutta-England routines */
	p = (rke_variables)
		rke_init(3, rossler_equations);

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
rossler_equations(double t, double y[3] , double yprime[3])
/*********************************************************************
rossler_equations - the system of ODE's describing the Rossler
**********************************************************************
t	independent variable "time"
y 	dependent variable being solved for y(t)
yprime	derivative of dependent variable  y'(t)
**********************************************************************
Notes: This is an example of an autonomous system of ODE's
**********************************************************************/
{
	double a, b, c;
	  
	if (!getpardouble("a", &a))			a = .2;
	if (!getpardouble("b", &b))			b = .2;
	if (!getpardouble("c", &c))			c = 5.7;

	yprime[0] = - y[1] - y[2];
	yprime[1] = y[0] + a*y[1];
	yprime[2] = b + y[2]*(y[0] - c );
    return 1;
}
