/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FARITH: $Revision: 1.21 $ ; $Date: 2015/02/19 18:25:06 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" FARITH - File ARITHmetic -- perform simple arithmetic with binary files",
" 									",
" farith <infile >outfile [optional parameters]				",
" 									",
" Optional Parameters:							",
" in=stdin	input file						",
" out=stdout	output file						",
" in2=	   second input file (required for binary operations)		",
"		   if it can't be opened as a file, it might be a scalar",
" n=size_of_in,  fastest dimension (used only for op=cartprod is set)	",
" isig=		 index at which signum function acts (used only for 	",
"			op=signum)					",
" scale=	value to scale in by, used only for op=scale)		",
" bias=		value to bias in by, used only for op=bias)		",
"  									",
" op=noop   noop for out = in						",
"	   neg  for out = -in						",
"	   abs  for out = abs(in)					",
"	   scale for out = in *scale					",
"	   bias for out = in + bias 					",
"	   exp  for out = exp(in)					",
"	   sin  for out = sin(in)					",
"	   cos  for out = cos(in)					",
"	   log  for out = log(in)					",
"	   sqrt for out = (signed) sqrt(in)				",
"	   sqr  for out = in*in						",
"	   degrad  for out = in*PI/180					",
"	   raddeg  for out = in*180/PI					",
"	   pinv  for out = (punctuated) 1 / in   			",
"	   pinvsqr  for out = (punctuated) 1 /in*in 			",
"	   pinvsqrt for out = (punctuated signed) 1 /sqrt(in) 		",
"	   add  for out = in + in2					",
"	   sub  for out = in - in2					",
"	   mul  for out = in * in2					",
"	   div  for out = in / in2					",
"		cartprod for out = in x in2					",
"		requires: n=size_of_in, fastest dimension in output	",
"		signum for out[i] = in[i] for i< isig  and			",
"				= -in[i] for i>= isig			",
"		requires: isig=point where signum function acts		",
" Seismic operations:							",
"	   sloth   for  out =  1/in^2		Sloth from velocity in	",
"	   slowp   for  out =  1/in - 1/in2	Slowness perturbation	",
"	   slothp  for  out =  1/in^2 - 1/in2^2   Sloth perturbation	",
"									",
" Notes:								",
" op=sqrt takes sqrt(x) for x>=0 and -sqrt(ABS(x)) for x<0 (signed sqrt)",
"									",
" op=pinv takes y=1/x for x!=0,  if x=0 then y=0. (punctuated inverse)	",
"									",
" The seismic operations assume that in and in2 are wavespeed profiles.	",
" \"Slowness\" is 1/wavespeed and \"sloth\" is  1/wavespeed^2.		",
" Use \"suop\" and \"suop2\" to perform unary and binary operations on	",
" data in the SU (SEGY trace) format.					",
"									",
" The options \"pinvsq\" and \"pinvsqrt\" are also useful for seismic	",
" computations involving converting velocity to sloth and vice versa.	",
"									",
" The option \"cartprod\" (cartesian product) requires also that the	",
" parameter n=size_of_in be set. This will be the fastest dimension	",
" of the rectangular array that is output.				",
"									",
" The option \"signum\" causes a flip in sign for all values with index	",
" greater than \"isig\"	(really -1*signum(index)).			",
"									",
" For file operations on SU format files, please use:  suop, suop2	",
"									",
NULL};

/*
 *   AUTHOR:  Dave Hale, Colorado School of Mines, 07/07/89
 *	Zhaobo Meng added scale and cartprod, 10/01/96
 *	Zhaobo Meng added signum, 9 May 1997
 *	Tony Kocurko added scalar operations, August 1997
 *      John Stockwell added bias option 4 August 2004
 */
/**************** end self doc ********************************/

int
main(int argc, char **argv)
{
	float x;		/* input data values		*/
	float x2;		/* second input data values	*/
	float y;		/* output value 		*/
	float *y1;		/* array of inputs for cartprod */
	float scale;		/* value of scale		*/
	float bias;		/* value of bias		*/
	float scalar=0.0;	/* scalar value interpreted in2 */
	int i1;			/* counter			*/
	int n;
	int isig;
	char *in;		/* input filename		*/
	char *in2;		/* name of second input file	*/
	char *out;		/* output filename		*/
	char *op="noop";	/* operation			*/
 	char *endptr;		/* end of file pointer		*/
	FILE *infp;		/* input file pointer		*/
	FILE *in2fp=NULL;	/* second input file pointer	*/
	FILE *outfp;		/* output file pointer		*/
	cwp_Bool isscalar=cwp_false; /* is scalar ? 		*/

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters */
	if (getparstring("in",&in))
	{
		if ((infp=fopen(in,"r"))==NULL)
			err("Cannot open in=%s\n",in);
	}
	else
		infp = stdin;
	getparstring("op",&op);
	if (STREQ(op,"add") ||
		STREQ(op,"sub") ||
		STREQ(op,"mul") ||
		STREQ(op,"div") ||
		STREQ(op,"cartprod") ||
		STREQ(op,"slowp") ||
		STREQ(op,"slothp")) {
		if (!getparstring("in2",&in2))
			err("Must specify in2 for op=%s\n",op);
		if ((in2fp=fopen(in2,"r"))==NULL) {
				scalar = (float)strtod ((const char *)in2, (char **)(&endptr));
			if ( endptr == in2 )
			err("Can neither open in2=%s nor convert it to a scalar", in2);
			else if ( STREQ(op,"div") && scalar == 0.0 )
			err("Cannot divide by in2=%s", in2);
			else
			isscalar = cwp_true;
		}
	}
	if (getparstring("out",&out)){
		if ((outfp=fopen(out,"w"))==NULL)
			err("Cannot open out=%s",out);
	}
	else
		outfp = stdout;

	/* do the arithmetic operation */
	if (STREQ(op,"noop")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = x;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"neg")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = -x;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"abs")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = fabs(x);
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"scale")) {
		if (!getparfloat("scale",&scale))
			scale=1.0;
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = scale*x;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"bias")) {
		if (!getparfloat("bias",&bias))
			bias=0.0;
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = x + bias;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"signum")) {
		if (!getparint("isig",&isig))
			err("Must specify isig");
		i1=0;
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (i1>=isig) y = -x;
			else y=x;
			i1++;
			efwrite(&y,sizeof(float),1,outfp);
		} 
	} else if (STREQ(op,"exp")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = exp(x);
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"sin")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = sin(x);
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"cos")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = cos(x);
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"log")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = log(x);
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"sqrt")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (x < 0) { 
				y = -sqrt(ABS(x));
			} else {
				y = sqrt(x);
			}
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"sqr")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = x*x;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"sloth")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (x != 0) {
				y = 1.0/x*x;
			} else { 
				warn("zero value detected in input!");
				y = 0.0;
			}
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"degrad")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = x*PI/180.0;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"raddeg")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			y = x*180.0/PI;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"pinv")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (x != 0) {
				y = 1 / x;
			} else {
				y = 0.;
			}
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"pinvsqr")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (x != 0) {
				y = 1 / (x*x);
			} else {
				y = 0.;
			}
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"pinvsqrt")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (x < 0) { 
				y = -1/sqrt(ABS(x));
			} else if (x > 0) {
				y = 1/sqrt(x);
			} else {
				y = 0.0;
			}
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"add")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
		if ( isscalar )
		y = x + scalar;
		else if (efread(&x2,sizeof(float),1,in2fp)==0)
				err("error or end of file reading in2 file");
			else
		y = x+x2;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"sub")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
		if (isscalar)
		y = x - scalar;
			else if (efread(&x2,sizeof(float),1,in2fp)==0)
				err("error or end of file reading in2 file");
		else
			  y = x-x2;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"mul")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
		if (isscalar)
		y = x * scalar;
			else if (efread(&x2,sizeof(float),1,in2fp)==0)
				err("error or end of file reading in2 file");
		else
			  y = x*x2;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"div")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (isscalar)
				y = x / scalar;
			else if (efread(&x2,sizeof(float),1,in2fp)==0)
				err("error or end of file reading in2 file");
			else {
					if ( x2 == 0 ) {
					  warn("zero values detected in denominator!");
					  warn("outputting numerator*FLT_MAX");
					  y = x*FLT_MAX;
				} else {
					y = x/x2;
			}
		}
		efwrite(&y,sizeof(float),1,outfp);
	}

	} else if (STREQ(op,"cartprod")) {
		size_t nread;

		if (!getparint("n",&n))
			err("Must specify n=size_of_in\n");
		y1=alloc1float(n);

		if ((nread=efread(y1,sizeof(float),n,infp))!=n)
			err("Number of samples n_read=%d != n=%d",nread,n);
	
		while(efread(&x,sizeof(float),1,in2fp)!=0) {
			for (i1=0;i1<n;i1++) {
				y = x*y1[i1];
				efwrite(&y,sizeof(float),1,outfp);
			}
		}

	} else if (STREQ(op,"slowp")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (efread(&x2,sizeof(float),1,in2fp)==0)
				err("error or end of file reading in2 file");
			if ( x <= 0 || x2 <= 0 ) 
				err ("zero value detected!");
			y = 1/x - 1/x2;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else if (STREQ(op,"slothp")) {
		while(efread(&x,sizeof(float),1,infp)!=0) {
			if (efread(&x2,sizeof(float),1,in2fp)==0)
				err("error or end of file reading in2 file");
			if ( x <= 0 || x2 <= 0 ) 
				err ("zero value detected!");
			y = 1/x*x - 1/x2*x2;
			efwrite(&y,sizeof(float),1,outfp);
		}
	} else {
		err("op=%s is not a valid operation",op);
	}
        checkpars();
	return(CWP_Exit());
}
