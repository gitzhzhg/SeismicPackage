/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SU3DCHART: $Revision: 1.15 $ ; $Date: 2011/11/16 22:10:29 $    */


#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SU3DCHART - plot x-midpoints vs. y-midpoints for 3-D data	",
" 								",
" su3dchart <stdin >stdout					",
" 								",
" Optional parameters:						",
" outpar=null	name of parameter file				",
" degree=0	=1 convert seconds of arc to degrees		",
" 								",
" The output is the (x, y) pairs of binary floats		",
" 								",
" Example:							",
" su3dchart <segy_data outpar=pfile >plot_data			",
" psgraph <plot_data par=pfile \\				",
"	linewidth=0 marksize=2 mark=8 | ...			",
" rm plot_data 							",
" 								",
" su3dchart <segy_data | psgraph n=1024 d1=.004 \\		",
"	linewidth=0 marksize=2 mark=8 | ...			",
NULL};

/* Note:  sx, etc., are declared double because float has only 7
 * significant numbers, that's not enough, for example,    
 * when tr.scalco=100 and coordinates are in second of arc    
 * and located near 30 degree latitude and 59 degree longitude           
 */                                                            


/* Credits:
 *	CWP: Shuki Ronen
 *	Toralf Foerster
 *
 * Trace header fields accessed: sx, sy, gx, gy, counit, scalco.
 */

/**************** end self doc ***********************************/

segy tr;

int
main(int argc, char **argv)
{
	double sx, sy, gx, gy, factor;
	float mx, my;
        short unit;                                                           
	int degree;
	cwp_String outpar;
	register int npairs;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	if (!getparint("degree", &degree)) degree=0;

	npairs = 0;
	while (gettr(&tr)) {

		sx = tr.sx;
		sy = tr.sy;
		gx = tr.gx;
		gy = tr.gy;

                unit = tr.counit;
		
		/* If tr.scalco not set, use 1 as the value */
		factor = (!tr.scalco) ? 1 : tr.scalco;
		
                /* factor < 0 means divide; factor > 0 means to multiply */
                if (factor < 0) factor = -1/factor;

                /* if necessary, convert from seconds to degrees */
                if (unit == 2 && degree == 1) factor /= 3600;

                mx = (float) (0.5*(sx + gx) * factor);
                my = (float) (0.5*(sy + gy) * factor);

		efwrite(&mx, FSIZE, 1, stdout);
		efwrite(&my, FSIZE, 1, stdout);

		++npairs;
	}


	/* Make parfile if needed */
	if (getparstring("outpar", &outpar))
		fprintf(efopen(outpar, "w"), "n=%d\n", npairs);
        checkpars();

	return(CWP_Exit());
}
