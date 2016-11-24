/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* RECAST: $Revision: 1.25 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" RECAST - RECAST data type (convert from one data type to another)	",
" 									",
" recast <stdin [optional parameters]  >stdout 				",
" 									",
" Required parameters:							",
" 	none								",
" 									",
" Optional parameters:							",
" in=float	input type	(float)					",
" 		=double		(double)				",
" 		=int		(int)					",
" 		=char		(char)					",
"		=uchar		(unsigned char)				",
" 		=short		(short)					",
" 		=long		(long)					",
" 		=ulong		(unsigned long)				",
" 									",
" out=double	output type	(double)				",
" 		=float		(float)					",
" 		=int		(int)					",
" 		=char		(char)					",
" 		=uchar		(unsigned char)				",
" 		=short		(short)					",
" 		=long		(long)					",
" 		=ulong		(unsigned long)				",
" 									",
" outpar=/dev/tty	output parameter file, contains the		",
"				number of values (n1=)			",
" 			other choices for outpar are: /dev/tty,		",
" 			/dev/stderr, or a name of a disk file		",
" 									",
" Notes: Converting bigger types to smaller is hazardous. For float	",
"	 or double conversions to integer types, the results are 	",
"	 rounded to the nearest integer.				",
" 									",
NULL};

/* Credits:
 *
 *	CWP: John Stockwell, Jack K. Cohen
 *
 */
/**************** end self doc ***********************************/

/* macros to do the recasting */
#define RECAST(intype, outtype)						\
{									\
	intype xin;							\
	outtype xout;							\
	while (efread(&xin, sizeof(intype), 1, stdin)) {		\
			++n;						\
			xout = (outtype) (xin);				\
			efwrite(&xout, sizeof(outtype), 1, stdout);	\
		}							\
}									\

#define RECAST_NLONG(intype,outtype)					\
{									\
	intype xin;							\
	outtype xout;							\
	while (efread(&xin, sizeof(intype), 1, stdin)) {		\
			++n;						\
			xout = (outtype) (NLONG(xin));			\
			efwrite(&xout, sizeof(outtype), 1, stdout);	\
		}							\
}									\
/* Don't use NINT which casts to int and could lose precision */
#define NLONG(x) ((long)(((double)(x))>0.0?((double)(x))+0.5:((double)(x))-0.5))

#define RECAST_NULONG(intype,outtype)					\
{									\
	intype xin;							\
	outtype xout;							\
	while (efread(&xin, sizeof(intype), 1, stdin)) {		\
			++n;						\
			xout = (outtype) (NULONG(xin));			\
			efwrite(&xout, sizeof(outtype), 1, stdout);	\
		}							\
}									\
/* Don't use NINT which casts to int and could lose precision */
#define NULONG(x) ((unsigned long)(((double)(x))>0.0?((double)(x))+0.5:((double)(x))-0.5))

int
main(int argc, char **argv)
{
	char *in;		/* input data type			*/
	char *out;		/* output data type			*/
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int n = 0;		/* number of input points	 	*/

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");

	/* Get input and output data types */
	if (!getparstring("in", &in))		in = "float" ;
	if (!getparstring("out", &out))		out = "double" ;
	
        checkpars();

	/* Check in and out to see if types supported */
	if ( !(	STREQ(in,"float") 
		|| STREQ(in,"double") 
		|| STREQ(in,"int") 
		|| STREQ(in,"char")
		|| STREQ(in,"uchar")
		|| STREQ(in,"short") 
		|| STREQ(in,"long") 
		|| STREQ(in,"ulong") 
       		|| STREQ(out,"float") 
		|| STREQ(out,"double") 
		|| STREQ(out,"int")
		|| STREQ(out,"char") 
		|| STREQ(out,"uchar") 
		|| STREQ(out,"short") 
		|| STREQ(out,"long") 
		|| STREQ(out,"ulong") 
		) 
	) err("%s or %s is an unsupported type",in,out);

	/* Read floats, write doubles */
	if (STREQ(in,"float") && STREQ(out,"double")) {
		RECAST(float, double);

	/* Read floats, write nearest integers */
	} else if (STREQ(in,"float") && STREQ(out,"int")) {
		RECAST_NLONG(float, int);
	
	/* Read floats, write nearest signed chars */
	} else if (STREQ(in,"float") && STREQ(out,"char")) {
		RECAST_NLONG(float, signed char);

	/* Read floats, write nearest unsigned chars */
	} else if (STREQ(in,"float") && STREQ(out,"uchar")) {
		RECAST_NLONG(float, unsigned char);
	
	/* Read floats, write nearest shorts */
	} else if (STREQ(in,"float") && STREQ(out,"short")) {
		RECAST_NLONG(float, short);
	
	/* Read floats, write nearest longs */
	} else if (STREQ(in,"float") && STREQ(out,"long")) {
		RECAST_NLONG(float, long);

	/* Read floats, write nearest unsigned longs */
	} else if (STREQ(in,"float") && STREQ(out,"ulong")) {
		RECAST_NULONG(float, unsigned long);
	
	/* Read doubles, write floats */
	} else if (STREQ(in,"double") && STREQ(out,"float")) {
		RECAST(double, float);

	/* Read doubles, write nearest integers */
	} else if (STREQ(in,"double") && STREQ(out,"int")) {
		RECAST_NLONG(double,int);

	/* Read doubles, write nearest signed chars */
	} else if (STREQ(in,"double") && STREQ(out,"char")) {
		RECAST_NLONG(double, signed char);

	/* Read doubles, write nearest unsigned chars */
	} else if (STREQ(in,"double") && STREQ(out,"uchar")) {
		RECAST_NLONG(double, unsigned char);

	/* Read doubles, write nearest shorts */
	} else if (STREQ(in,"double") && STREQ(out,"short")) {
		RECAST_NLONG(double, short);

	/* Read doubles, write nearest longs */
	} else if (STREQ(in,"double") && STREQ(out,"long")) {
		RECAST_NLONG(double, long);

	/* Read doubles, write nearest unsigned longs */
	} else if (STREQ(in,"double") && STREQ(out,"ulong")) {
		RECAST_NULONG(double, unsigned long);

	/* Read integers, write floats */
	} else if (STREQ(in,"int") && STREQ(out,"float")) {
		RECAST(int, float);

	/* Read integers, write doubles */
	} else if (STREQ(in,"int") && STREQ(out,"double")) {
		RECAST(int, double);

	/* Read integers, write chars */
	} else if (STREQ(in,"int") && STREQ(out,"char")) {
		RECAST(int, signed char);

	/* Read integers, write unsigned chars */
	} else if (STREQ(in,"int") && STREQ(out,"uchar")) {
		RECAST(int, unsigned char);

	/* Read integers, write shorts */
	} else if (STREQ(in,"int") && STREQ(out,"short")) {
		RECAST(int, short);

	/* Read integers, write longs */
	} else if (STREQ(in,"int") && STREQ(out,"long")) {
		RECAST_NLONG(long, long);

	/* Read integers, write unsigned longs */
	} else if (STREQ(in,"int") && STREQ(out,"ulong")) {
		RECAST(long, unsigned long);

	/* Read chars, write doubles */
	} else if (STREQ(in,"char") && STREQ(out,"double")) {
		RECAST(signed char, double);

	/* Read chars, write floats */
	} else if (STREQ(in,"char") && STREQ(out,"float")) {
		RECAST(signed char, float);

	/* Read chars, write integers */
	} else if (STREQ(in,"char") && STREQ(out,"int")) {
		RECAST(signed char, int);

	/* Read chars, write shorts */
	} else if (STREQ(in,"char") && STREQ(out,"short")) {
		RECAST(signed char, short);

	/* Read chars, write longs */
	} else if (STREQ(in,"char") && STREQ(out,"long")) {
		RECAST(signed char, long);

	/* Read chars, write unsigned longs */
	} else if (STREQ(in,"char") && STREQ(out,"ulong")) {
		RECAST(signed char, long);

	/* Read unsigned chars, write doubles */
	} else if (STREQ(in,"uchar") && STREQ(out,"double")) {
		RECAST(unsigned char, double);

	/* Read unsigned chars, write floats */
	} else if (STREQ(in,"uchar") && STREQ(out,"float")) {
		RECAST(unsigned char, float);

	/* Read unsigned chars, write integers */
	} else if (STREQ(in,"uchar") && STREQ(out,"int")) {
		RECAST(unsigned char, int);

	/* Read unsigned chars, write shorts */
	} else if (STREQ(in,"uchar") && STREQ(out,"short")) {
		RECAST(unsigned char, short);

	/* Read unsigned chars, write longs */
	} else if (STREQ(in,"uchar") && STREQ(out,"long")) {
		RECAST(unsigned char, long);

	/* Read unsigned chars, write unsigned longs */
	} else if (STREQ(in,"uchar") && STREQ(out,"ulong")) {
		RECAST(unsigned char, long);

	/* Read shorts, write doubles */
	} else if (STREQ(in,"short") && STREQ(out,"double")) {
		RECAST(short, double);

	/* Read shorts, write floats */
	} else if (STREQ(in,"short") && STREQ(out,"float")) {
		RECAST(short, float);

	/* Read shorts, write integers */
	} else if (STREQ(in,"short") && STREQ(out,"int")) {
		RECAST(short, int);

	/* Read shorts, write chars */
	} else if (STREQ(in,"short") && STREQ(out,"char")) {
		RECAST(short, signed char);

	/* Read shorts, write longs */
	} else if (STREQ(in,"short") && STREQ(out,"long")) {
		RECAST(short, long);

	/* Read longs, write doubles */
	} else if (STREQ(in,"long") && STREQ(out,"double")) {
		RECAST(long, double);

	/* Read longs, write floats */
	} else if (STREQ(in,"long") && STREQ(out,"float")) {
		RECAST(long, float);

	/* Read longs, write integers */
	} else if (STREQ(in,"long") && STREQ(out,"int")) {
		RECAST(long, int);

	/* Read longs, write chars */
	} else if (STREQ(in,"long") && STREQ(out,"char")) {
		RECAST(long, signed char);
	
	/* Read longs, write shorts */
	} else if (STREQ(in,"long") && STREQ(out,"short")) {
		RECAST(long, short);

	/* Read unsigned longs, write doubles */
	} else if (STREQ(in,"ulong") && STREQ(out,"double")) {
		RECAST(unsigned long, double);

	/* Read unsigned longs, write floats */
	} else if (STREQ(in,"ulong") && STREQ(out,"float")) {
		RECAST(unsigned long, float);

	/* Read unsigned longs, write integers */
	} else if (STREQ(in,"ulong") && STREQ(out,"int")) {
		RECAST(unsigned long, int);

	/* Read unsigned longs, write chars */
	} else if (STREQ(in,"ulong") && STREQ(out,"char")) {
		RECAST(unsigned long, signed char);
	
	/* Read unsigned longs, write shorts */
	} else if (STREQ(in,"ulong") && STREQ(out,"short")) {
		RECAST(unsigned long, short);

	/* in and out the same */
	} else if (STREQ(in,out)) {
		err("intype and outtype are the same (%s).",in);
	} else  err("Unsupported pair in=%s out=%s",in,out);
	

	/* Make par file */
	fprintf(outparfp, "n=%d\n", n);


	return(CWP_Exit());
}
