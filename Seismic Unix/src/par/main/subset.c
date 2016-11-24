/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUBSET: $Revision: 1.12 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUBSET - select a SUBSET of the samples from a 3-dimensional file	",
" 									",
" subset <infile >outfile [optional parameters]				",
" 									",
" Optional Parameters:							",
" n1=nfloats             number of samples in 1st dimension		",
" n2=nfloats/n1          number of samples in 2nd dimension		",
" n3=nfloats/(n1*n2)     number of samples in 3rd dimension		",
" id1s=1                 increment in samples selected in 1st dimension	",
" if1s=0                 index of first sample selected in 1st dimension",
" n1s=1+(n1-if1s-1)/id1s number of samples selected in 1st dimension	",
" ix1s=if1s,if1s+id1s,...indices of samples selected in 1st dimension	",
" id2s=1                 increment in samples selected in 2nd dimension	",
" if2s=0                 index of first sample selected in 2nd dimension",
" n2s=1+(n2-if2s-1)/id2s number of samples selected in 2nd dimension	",
" ix2s=if2s,if2s+id2s,...indices of samples selected in 2nd dimension	",
" id3s=1                 increment in samples selected in 3rd dimension	",
" if3s=0                 index of first sample selected in 3rd dimension",
" n3s=1+(n3-if3s-1)/id3s number of samples selected in 3rd dimension	",
" ix3s=if3s,if3s+id3s,...indices of samples selected in 3rd dimension	",
" 									",
" For the 1st dimension, output is selected from input as follows:	",
"       output[i1s] = input[ix1s[i1s]], for i1s = 0 to n1s-1		",
" Likewise for the 2nd and 3rd dimensions.				",
" 									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 07/07/89
 */
/**************** end self doc ********************************/

int
main (int argc, char **argv)
{
	int n1,n2,n3,
		n1s,n2s,n3s,
		id1s,id2s,id3s,
		if1s,if2s,if3s,
		*ix1s,*ix2s,*ix3s,
		i1s,i2s,i3s,
		i1,i2,i3;
	off_t offset;
	float *p,*ps;
	FILE *infp=stdin,*outfp=stdout;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get optional parameters */
	if (!getparint("n1",&n1)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
			err("input file size unknown; specify n1\n");
		n1 = (int) (eftello(infp)/sizeof(float));
	}
	if (!getparint("n2",&n2)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
			err("input file size unknown; specify n2\n");
		n2 = (int) (eftello(infp)/(n1*sizeof(float)));
	}
	if (!getparint("n3",&n3)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
			err("input file size unknown; specify n3\n");
		n3 = (int) (eftello(infp)/(n2*n1*sizeof(float)));
	}
	ix1s = alloc1int(countparval("ix1s"));
	if ((n1s=getparint("ix1s",ix1s))==0) {
		free1int(ix1s);
		if (!getparint("id1s",&id1s)) id1s = 1;
		if (!getparint("if1s",&if1s)) if1s = 0;
		if (!getparint("n1s",&n1s)) n1s = 1+(n1-if1s-1)/id1s;
		ix1s = alloc1int(n1s);
		for (i1s=0,i1=if1s; i1s<n1s; i1s++,i1+=id1s)
			ix1s[i1s] = i1;
	}
	ix2s = alloc1int(countparval("ix2s"));
	if ((n2s=getparint("ix2s",ix2s))==0) {
		free1int(ix2s);
		if (!getparint("id2s",&id2s)) id2s = 1;
		if (!getparint("if2s",&if2s)) if2s = 0;
		if (!getparint("n2s",&n2s)) n2s = 1+(n2-if2s-1)/id2s;
		ix2s = alloc1int(n2s);
		for (i2s=0,i2=if2s; i2s<n2s; i2s++,i2+=id2s)
			ix2s[i2s] = i2;
	}
	ix3s = alloc1int(countparval("ix3s"));
	if ((n3s=getparint("ix3s",ix3s))==0) {
		free1int(ix3s);
		if (!getparint("id3s",&id3s)) id3s = 1;
		if (!getparint("if3s",&if3s)) if3s = 0;
		if (!getparint("n3s",&n3s)) n3s = 1+(n3-if3s-1)/id3s;
		ix3s = alloc1int(n3s);
		for (i3s=0,i3=if3s; i3s<n3s; i3s++,i3+=id3s)
			ix3s[i3s] = i3;
	}
        checkpars();


	/* check parameters */
	for (i1s=0; i1s<n1s; i1s++)
		if (ix1s[i1s]<0 || ix1s[i1s]>n1-1)
			err("ix1s[%d]=%d is out of bounds!\n",i1s,ix1s[i1s]);
	for (i2s=0; i2s<n2s; i2s++)
		if (ix2s[i2s]<0 || ix2s[i2s]>n2-1)
			err("ix2s[%d]=%d is out of bounds!\n",i2s,ix2s[i2s]);
	for (i3s=0; i3s<n3s; i3s++)
		if (ix3s[i3s]<0 || ix3s[i3s]>n3-1)
			err("ix3s[%d]=%d is out of bounds!\n",i3s,ix3s[i3s]);

	/* allocate space for input and output arrays */
	p = ealloc1float(n1);
	ps = ealloc1float(n1s);

	/* loop over 3rd dimension */
	for (i3s=0; i3s<n3s; i3s++) {

		/* loop over 2nd dimension */
		for (i2s=0; i2s<n2s; i2s++) {

			/* find beginning of input array */
/*			offset = (off_t) ((ix2s[i2s]+ix3s[i3s]*n2)*n1*sizeof(float));
	*/

		/* fix supplied by Raoul Beauduin */
			offset = (
					((off_t) ix2s[i2s])+
					((off_t) ix3s[i3s])*((off_t) n2))*((off_t) n1)*((off_t) sizeof(float));
		efseeko(infp,offset,SEEK_SET);

			/* read input array, if it exists */
			if (fread(p,sizeof(float),n1,infp)==n1) {

				/* loop over 1st dimension */
				for (i1s=0; i1s<n1s; i1s++) {
					ps[i1s] = p[ix1s[i1s]];
				}

			/* if input does not exist */
			} else {
				err("no input for ix2s[%d]=%d ix3s[%d]=%d!\n",
				i2s,ix2s[i2s],
				i3s,ix3s[i3s]);
			}

			/* write trace to output file */
			efwrite(ps,sizeof(float),n1s,outfp);
		}
	}
	return(CWP_Exit());
}
