/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
PRINTERPLOT - Functions to make a printer plot of a 1-dimensional array

pp1d		printer plot of 1d array 
pplot1		printer plot of 1d array 

******************************************************************************
Function Prototypes:
void pp1d (FILE *fp, char *title, int lx, int ifx, float x[]);
void pplot1 (FILE *fp, char *title, int nx, float ax[]);

******************************************************************************
pp1d:
Input:
fp		file pointer for output (e.g., stdout, stderr, etc.)
title		title of plot
lx		length of x
ifx		index of first x
x		array[lx] to be plotted

******************************************************************************
pplot1:
Input:
fp		file pointer for printed output (e.g., stdout, stderr, etc.)
title		title of plot
nx		number of x values to be plotted
ax		array[nx] of x values

******************************************************************************
Notes:
Are two subroutines to do this really necessary?

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

#define IFC 0
#define ILC 60

void pp1d (FILE *fp, char *title, int lx, int ifx, float x[])
/*****************************************************************************
Printer plot of a 1-dimensional array
******************************************************************************
Input:
fp		file pointer for output (e.g., stdout, stderr, etc.)
title		title of plot
lx		length of x
ifx		index of first x
x		array[lx] to be plotted
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/

{
	int ix,ibase,icx,ic;
	float xmin,xmax,xscale,xbase;

	/* print title */
	fprintf(fp,"\n");
	fprintf(fp,"%s\n",title);

	/* minimum and maximum x */
	xmin = x[0];
	xmax = x[0];
	for (ix=1; ix<lx; ix++) {
		xmin = MIN(xmin,x[ix]);
		xmax = MAX(xmax,x[ix]);
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
	for (ix=0; ix<lx; ix++) {

		/* determine column corresponding to x value */
		icx = ibase+NINT((x[ix]-xbase)*xscale);
		icx = MAX(IFC,MIN(ILC,icx));

		/* print the index, x value, and row of *s */
		fprintf(fp,"%4d %13.6e ",ifx+ix,x[ix]);
		for (ic=IFC; ic<MIN(ibase,icx); ic++)
			fprintf(fp," ");
		for (ic=MIN(ibase,icx); ic<=MAX(ibase,icx); ic++)
			fprintf(fp,"*");
		fprintf(fp,"\n");
	}
}

void
pplot1 (FILE *fp, char *title, int nx, float ax[])
/*****************************************************************************
printer plot of a 1-dimensional array
******************************************************************************
Input:
fp		file pointer for printed output (e.g., stdout, stderr, etc.)
title		title of plot
nx		number of x values to be plotted
ax		array[nx] of x values
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 12/26/89
*****************************************************************************/
{
	int ix,ibase,icx,ic;
	int ifx=0;
	float xmin,xmax,xscale,xbase;

	/* print title */
	fprintf(fp,"\n");
	fprintf(fp,"%s\n",title);

	/* minimum and maximum x */
	xmin = ax[0];
	xmax = ax[0];
	for (ix=1; ix<nx; ix++) {
		xmin = MIN(xmin,ax[ix]);
		xmax = MAX(xmax,ax[ix]);
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
	for (ix=0; ix<nx; ix++) {

		/* determine column corresponding to x value */
		icx = ibase+NINT((ax[ix]-xbase)*xscale);
		icx = MAX(IFC,MIN(ILC,icx));

		/* print the index, x value, and row of asterixes */
		fprintf(fp,"%4d %13.6e ",ifx+ix,ax[ix]);
		for (ic=IFC; ic<MIN(ibase,icx); ic++)
			fprintf(fp," ");
		for (ic=MIN(ibase,icx); ic<=MAX(ibase,icx); ic++)
			fprintf(fp,"*");
		fprintf(fp,"\n");
	}
}
