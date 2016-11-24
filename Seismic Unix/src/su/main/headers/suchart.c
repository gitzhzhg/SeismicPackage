/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCHART: $Revision: 1.19 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUCHART - prepare data for x vs. y plot			",
" 								",
" suchart <stdin >stdout key1=sx key2=gx			",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	key1=sx  	abscissa 				",
" 	key2=gx		ordinate				",
"	outpar=null	name of parameter file			",
" 								",
" The output is the (x, y) pairs of binary floats		",
" 								",
" Examples:							",
" suchart < sudata outpar=pfile >plot_data			",
" psgraph <plot_data par=pfile title=\"CMG\" \\			",
"	linewidth=0 marksize=2 mark=8 | ...			",
" rm plot_data 							",
" 								",
" suchart < sudata | psgraph n=1024 d1=.004 \\			",
"	linewidth=0 marksize=2 mark=8 | ...			",
" 								",
" fold chart: 							",
" suchart < stacked_data key1=cdp key2=nhs |			",
"            psgraph n=NUMBER_OF_TRACES d1=.004 \\		",
"	linewidth=0 marksize=2 mark=8 > chart.ps		",
" 								",
" 								",
NULL};

/* Credits:
 *	SEP: Einar Kjartansson
 *	CWP: Jack K. Cohen
 *
 * Notes:
 *	The vtof routine from valpkge converts values to floats.
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	cwp_String key1,  key2;	/* x and y key header words	*/
	Value  val1,  val2;	/* ... their values		*/
	cwp_String type1, type2;/* ... their types		*/
	int index1, index2;	/* ... their indices in hdr.h	*/
	float x, y;		/* temps to hold current x & y 	*/
	cwp_String outpar;	/* name of par file		*/
	register int npairs;	/* number of pairs found	*/


	/* Hook up getpars */
	initargs(argc, argv);
	requestdoc(1);


	/* Prevent byte codes from spilling to screen */
	if (isatty(STDOUT)) err("must redirect or pipe binary output");


	/* Get parameters */
	if (!getparstring("key1", &key1))	key1 = "sx";
	if (!getparstring("key2", &key2))	key2 = "gx";

	type1 = hdtype(key1);
	type2 = hdtype(key2);

	index1 = getindex(key1);
	index2 = getindex(key2);


	/* Loop over traces */
	npairs = 0;
	while(gettr(&tr)) {

		gethval(&tr, index1, &val1);
		gethval(&tr, index2, &val2);

		x = vtof(type1, val1);
		y = vtof(type2, val2);

		efwrite(&x, FSIZE, 1, stdout);
		efwrite(&y, FSIZE, 1, stdout);

		++npairs;
	}


	/* Make parfile if needed */
	if (getparstring("outpar", &outpar))
		fprintf(efopen(outpar, "w"),
			"n=%d label1=%s label2=%s\n",
			npairs, key1, key2);

        checkpars();

	return(CWP_Exit());
}
