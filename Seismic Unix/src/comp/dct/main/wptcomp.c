/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPTCOMP: $Revision: 1.6 $ ; $Date: 2011/11/17 00:17:48 $	*/

#include "comp.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" WPTCOMP - Compression by Wavelet Packet Compression 			",
"									",
"   wptcomp < stdin n1= n2=   [optional parameter] > sdtout		",
"									",
" Required Parameters:							",
" n1=			number of samples in the fast (first) dimension	",
" n2=			number of samples in the slow (second) dimension",
"									",
" Optional Parameters:							",
" nfilter1=11		number of filters in direction 1		",
" nfilter2=11		number of filters in direction 2		",
" nstage1=		filter stages (automatically set based on n1)	",
" nstage2=		filter stages (automatically set based on n2)	",
" error=0.01		acceptable error				",
"									",
NULL};

/*
 * Author:  CWP: Tong Chen, Dec 1995
 */
/**************** end self doc ********************************/

int
main(int argc, char **argv)
{
	int i1, i2, n1, n2, npad1, npad2;
	int npow1, npow2, nstage1, nstage2, nfilter1, nfilter2, nsize;
	float **f, **g;
	waveFilter *filter1;
	waveFilter *filter2;
	float error, ave, step;
	int *qx;
	memBUFF *ibuff, *obuff;
	
	initargs(argc, argv);
	requestdoc(1);

	/* get the parameters */
	if(!getparint("n1",&n1)) err("Must specify n1\n");
	if(!getparint("n2",&n2)) err("Must specify n2\n");
	if(!getparint("nfilter1",&nfilter1)) nfilter1=11;
	if(!getparint("nfilter2",&nfilter2)) nfilter2=11;
	if(!getparfloat("error",&error)) error=.01;
	

	/* regular sizes */
	if(n1==1)
	{
	   npow1 = 0;
	   npad1 = 1;
	}
	else 
	{
	   npow1 = 0; while(((n1-1)>>npow1)!=0) npow1 ++;
	   npad1 = 1<<npow1;
	}

	if(n2==1)
	{
	   npow2 = 0;
	   npad2 = 1;
	}
	else 
	{
	   npow2 = 0; while(((n2-1)>>npow2)!=0) npow2 ++;
	   npad2 = 1<<npow2;
	}

	if(!getparint("nstage1",&nstage1)) {
	    if(npow1>4) nstage1=3;
	    else if(npow1>1) nstage1=2;
	    else nstage1=npow1;
	}
	if(!getparint("nstage2",&nstage2)) {
	    if(npow2>4) nstage2=3;
	    else if(npow2>1) nstage2=2;
	    else nstage2=npow2;
	}

        checkpars();

	/* allocate space */
	f = alloc2float(npad1,npad2);
	g = alloc2float(npad1,npad2);
	qx = alloc1int(npad1*npad2);
	
	/* filter to use */
	filter1 = waveGetfilter(nfilter1);
	filter2 = waveGetfilter(nfilter2);

	/* input data */
	for(i2=0; i2<n2; i2++) {
	   fread(f[i2],sizeof(float),n1,stdin);

	   /* pad with zeroes */
	   for(i1=n1; i1<npad1; i1++)
	   f[i2][i1] = 0.;
	}
	
	/* pad with dead traces */
	for(i2=n2; i2<npad2; i2++)
	   for(i1=0; i1<npad1; i1++)
	      f[i2][i1] = 0.;

	/* peform the transform */
	wavePack_2(f, g, filter1, filter2, npow1, npow2, 
		   nstage1, nstage2, 0);
	
	/* quantization */
	step = -1.;
	uniQuant(g[0], npad1*npad2, error, &ave, &step, qx);

	/* prefix encoding */
	ibuff = pEncode(qx, npad1*npad2);

	
	/* allocate out buffer */
	obuff = buffAlloc1(ibuff->mbound);

	/* rewind the in buffer */
	buffRewind(ibuff);
	
	/* run-length coding */
	codeSilence(ibuff, obuff);
	
	/* Huffman coding */
	buffRealloc1(obuff, obuff->pos);
	buffRewind(obuff);
	buffRewind(ibuff);
	nsize = huffCompress(obuff, ibuff);
	
	fprintf(stderr,"size after compression = %d bytes\n", nsize);
	fprintf(stderr,"compression ratio = %f\n", 
		(float)n1*n2*sizeof(int)/nsize);


	/* Write out compressed data */
	fwrite(&nsize, sizeof(int), 1, stdout);
	fwrite(&n1, sizeof(int), 1, stdout);
	fwrite(&n2, sizeof(int), 1, stdout);
	fwrite(&nfilter1, sizeof(int), 1, stdout);
	fwrite(&nfilter2, sizeof(int), 1, stdout);
	fwrite(&nstage1, sizeof(int), 1, stdout);
	fwrite(&nstage2, sizeof(int), 1, stdout);
	fwrite(&ave, sizeof(float), 1, stdout);
	fwrite(&step, sizeof(float), 1, stdout);
	fwrite(ibuff->code, sizeof(char), ibuff->pos, stdout);

/*
	fprintf(stderr,"size after compression = %d bytes\n", obuff->pos);
	fwrite(obuff->code, sizeof(char), obuff->pos, stdout);
*/
	return EXIT_SUCCESS;
	
}

