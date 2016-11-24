/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPTUNCOMP: $Revision: 1.5 $ ; $Date: 2011/11/17 00:17:48 $	*/

#include "comp.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" WPTUNCOMP - Uncompress  WPT compressed data				",
"									",
"   wptuncomp < stdin > sdtout						",
"									",
" Required Parameters:							",
"  none									",
" Optional Parameters:							",
"  none									",
"									",
NULL};

/*
 * Author: CWP: Tong Chen, Dec 1995
 */
/**************** end self doc ********************************/

int
main(int argc, char **argv)
{
	int i2, n1, n2, npad1, npad2;
	int npow1, npow2, nstage1, nstage2, nfilter1, nfilter2, nsize, nmax;
	float **f, **g;
	waveFilter *filter1;
	waveFilter *filter2;
	float ave, step;
	int *qx;
	memBUFF *ibuff, *obuff;

	initargs(argc, argv);
	requestdoc(1);

	/* get the parameters */
	fread(&nsize, sizeof(int), 1, stdin);
	fread(&n1, sizeof(int), 1, stdin);
	fread(&n2, sizeof(int), 1, stdin);
	fread(&nfilter1, sizeof(int), 1, stdin);
	fread(&nfilter2, sizeof(int), 1, stdin);
	fread(&nstage1, sizeof(int), 1, stdin);
	fread(&nstage2, sizeof(int), 1, stdin);
	fread(&ave, sizeof(float), 1, stdin);
	fread(&step, sizeof(float), 1, stdin);

	/* regular sizes */
	if(n1==1) {

	   npow1 = 0;
	   npad1 = 1;
	} else {
	   npow1 = 0; while(((n1-1)>>npow1)!=0) npow1 ++;
	   npad1 = 1<<npow1;
	}

	if(n2==1) {
	   npow2 = 0;
	   npad2 = 1;
	} else {
	   npow2 = 0; while(((n2-1)>>npow2)!=0) npow2 ++;
	   npad2 = 1<<npow2;
	}


	/* allocate spaces */
	f = alloc2float(npad1,npad2);
	g = alloc2float(npad1,npad2);
	qx = alloc1int(npad1*npad2);
	
	/* filter to use */
	filter1 = waveGetfilter(nfilter1);
	filter2 = waveGetfilter(nfilter2);


	/* allocate the buffers */
	nmax = 2*npad1*npad2;
	ibuff = buffAlloc1(nsize);
	obuff = buffAlloc1(nmax);

	/* read data */
	fread(ibuff->code, sizeof(char), nsize, stdin);

	/* Huffman decoding */
	buffRewind(ibuff);
	if(huffDecompress(ibuff, obuff) == MEM_EOB)
	      err("Inconsistent data \n");
	

	/* run-length decoding */
	buffRealloc1(obuff, obuff->pos);
	buffRealloc1(ibuff, nmax);
        buffRewind(ibuff);
	buffRewind(obuff);
	codeDesilence(obuff, ibuff);
	
	/* prefix decoding */
	buffRealloc1(ibuff, ibuff->pos);
	buffRewind(ibuff);
	if(pDecode(ibuff, qx, npad1*npad2) == MEM_EOB)
	   err("Inconsistent data \n");
	
	/* dequantization */
	uniDequant(g[0], npad1*npad2, ave, step, qx);

	/* peform the inverse transform */
	wavePack_2(f, g, filter1, filter2, npow1, npow2, 
		   nstage1, nstage2, 1);

	/* reconstructed data */
	for(i2=0; i2<n2; i2++)
	   fwrite(f[i2], sizeof(float), n1, stdout);

	return EXIT_SUCCESS;
}


