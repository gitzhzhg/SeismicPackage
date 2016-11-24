/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PRPLOT: $Revision: 1.11 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" PRPLOT - PRinter PLOT of 1-D arrays f(x1) from a 2-D function f(x1,x2)",
" 									",
" prplot <infile >outfile [optional parameters]				",
" 									",
" Optional Parameters:							",
" n1=all                 number of samples in 1st dimension		",
" d1=1.0                 sampling interval in 1st dimension		",
" f1=d1                  first sample in 1st dimension			",
" n2=all                 number of samples in 2nd dimension		",
" d2=1.0                 sampling interval in 2nd dimension		",
" f2=d2                  first sample in 2nd dimension			",
" label2=Trace           label for 2nd dimension			",
" 									",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 07/07/89
 */

static void prp1d (FILE *fp, char *label, int n, float d, float f, float x[]);

int
main (int argc, char **argv)
{
	int n1,n2,i2;
	float f1,f2,d1,d2,*x;
	char *label2="Trace",label[256];
	FILE *infp=stdin,*outfp=stdout;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get optional parameters */
	if (!getparint("n1",&n1)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
			err("input file size is unknown; specify n1!\n");
		if ((n1=((int) (eftello(infp)/((off_t) sizeof(float)))))<=0)
			err("input file size is unknown; specify n1!\n");
		efseeko(infp,(off_t) 0,SEEK_SET);
	}

	if (!getparfloat("d1",&d1)) d1 = 1.0;
	if (!getparfloat("f1",&f1)) f1 = d1;
	if (!getparint("n2",&n2)) n2 = -1;
	if (!getparfloat("d2",&d2)) d2 = 1.0;
	if (!getparfloat("f2",&f2)) f2 = d2;
	getparstring("label2",&label2);
        checkpars();


	/* allocate space */
	x = ealloc1float(n1);

	/* loop over 2nd dimension */
	for (i2=0; i2<n2 || n2<0; i2++) {

		/* read input array, watching for end of file */
		if (efread(x,sizeof(float),n1,infp)!=n1) break;
			
		/* make plot label */
		sprintf(label,"%s %0.4g",label2,f2+i2*d2);

		/* plot the array */
		prp1d(outfp,label,n1,d1,f1,x);
	}
	
	return(CWP_Exit());
}


#define IFC 0
#define ILC 54

static void prp1d (FILE *fp, char *label, int n, float d, float f, float x[])
{
	int i,ibase,icx,ic;
	float xmin,xmax,xscale,xbase;

	/* print title */
	fprintf(fp,"\n");
	fprintf(fp,"%s\n",label);

	/* minimum and maximum x */
	xmin = x[0];
	xmax = x[0];
	for (i=1; i<n; i++) {
		xmin = MIN(xmin,x[i]);
		xmax = MAX(xmax,x[i]);
	}
	fprintf(fp,"minimum = %g\n",xmin);
	fprintf(fp,"maximum = %g\n",xmax);

	/* determine scale factors and shifts for converting x values to *s */
	if (xmin==xmax)
		xscale = 1.0;
	else
		xscale = (ILC-IFC)/(xmax-xmin);
	if (xmin<0.0 && xmax<0.0) {
		ibase = ILC;
		xbase = xmax;
	} else if (xmin<0.0 && xmax>=0.0) {
		ibase = IFC+(0.0-xmin)*xscale;
		xbase = 0.0;
	} else {
		ibase = IFC;
		xbase = xmin;
	}

	/* loop over x values */
	for (i=0; i<n; i++) {

		/* determine column corresponding to x value */
		icx = ibase+NINT((x[i]-xbase)*xscale);
		icx = MAX(IFC,MIN(ILC,icx));

		/* print the index, x value, and row of *s */
		fprintf(fp,"%10.3e %13.6e ",f+i*d,x[i]);
		for (ic=IFC; ic<MIN(ibase,icx); ic++)
			fprintf(fp," ");
		for (ic=MIN(ibase,icx); ic<=MAX(ibase,icx); ic++)
			fprintf(fp,"*");
		fprintf(fp,"\n");
	}
}
