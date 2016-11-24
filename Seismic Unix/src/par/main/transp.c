/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRANSP: $Revision: 1.10 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" TRANSP - TRANSPose an n1 by n2 element matrix				",
" 									",
" transp <infile >outfile n1= [optional parameters]			",
" 									",
" Required Parameters:							",
" n1                     number of elements in 1st (fast) dimension of matrix",
" 									",
" Optional Parameters:							",
" n2=all                 number of elements in 2nd (slow) dimension of matrix",
" nbpe=sizeof(float)     number of bytes per matrix element		",
" verbose=0              =1 for diagnostic information			",
" 									",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 07/07/89
 */

int
main (int argc, char **argv)
{
	int n1,n2,nbpe,i1,i2,verbose;
	char *v;
	void *state;
	FILE *infp=stdin,*outfp=stdout;

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("n1",&n1)) err("must specify n1!\n");
	if (!getparint("nbpe",&nbpe)) nbpe = sizeof(float);
	if (!getparint("n2",&n2)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
			err("input file size unknown; specify n2\n");
		n2 = (int) (eftello(infp)/(n1*nbpe));
		efseeko(infp, (off_t) 0,SEEK_SET);
	}
	verbose = 0;  getparint("verbose",&verbose);
	if (verbose) fprintf(stderr,"n1=%d  n2=%d  nbpe=%d\n",n1,n2,nbpe);

        checkpars();

	/* allocate space for a single vector in either dimension */
	v = ealloc1(((n1>n2)?n1:n2),nbpe);

	/* allocate big matrix state */
	state = bmalloc(nbpe,n1,n2);

	/* put vectors along 1st dimension to big matrix */
	if (verbose) fprintf(stderr,"Reading input file\n");
	for (i2=0; i2<n2; i2++) {
		if (fread(v,1,nbpe*n1,infp)!=nbpe*n1)
			err("Error reading input file:  i2=%d\n",i2);
		bmwrite(state,1,0,i2,n1,v);
	}

	/* get vectors along 2nd dimension from big matrix */
	if (verbose) fprintf(stderr,"Writing output file\n");
	for (i1=0; i1<n1; i1++) {
		bmread(state,2,i1,0,n2,v);
		if (fwrite(v,1,nbpe*n2,outfp)!=nbpe*n2)
			err("Error writing output file:  i1=%d\n",i1);
	}

	/* free big matrix state */
	bmfree(state);
	if (verbose) fprintf(stderr,"Transpose done!\n");

	return(CWP_Exit());
}
