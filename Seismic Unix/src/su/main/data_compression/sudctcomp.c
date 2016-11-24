/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* DCTCOMP: $Revision: 1.6 $ ; $Date: 2011/11/17 00:17:48 $	*/


#include "comp.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" DCTCOMP - Compression by Discrete Cosine Transform			",
"									",
"   dctcomp < stdin n1= n2=   [optional parameter] > sdtout		",
"									",
" Required Parameters:							",
" n1=			number of samples in the fast (first) dimension	",
" n2=			number of samples in the slow (second) dimension",
" Optional Parameters:							",
" blocksize1=16		blocksize in direction 1			",
" blocksize2=16		blocksize in direction 2			",
" error=0.01		acceptable error				",
"									",
NULL};

/*
 * Author:  CWP: Tong Chen   Dec 1995
 *          fixed by Graham Ganssle, Sandstone Oil & Gas, Sept 2015
 */

/**************** end self doc ********************************/

segy tr;

int
main(int argc, char **argv)
{

	int n1, n2, npad1, npad2, blocksize1, blocksize2, nblock1, nblock2;
	int i1, i2, j1, j2, ibeg1, ibeg2, iblock1, iblock2, nsize;
	float **f, **g, **c1, **c2;
	int *qx;
	memBUFF *ibuff, *obuff;
	float error, ave, step;
	
	initargs(argc, argv);
	requestdoc(1);

	/* get the parameters */
	if(!getparint("n1",&n1)) err("Must specify n1");
	if(!getparint("n2",&n2)) err("Must specify n2");
	if(!getparint("blocksize1",&blocksize1)) blocksize1 = 16;
	if(!getparint("blocksize2",&blocksize2)) blocksize2 = 16;
	if(!getparfloat("error",&error)) error=.01;
	
        checkpars();

	/* regular sizes */
	nblock1 = (n1-1)/blocksize1 + 1;
	nblock2 = (n2-1)/blocksize2 + 1;
	npad1 = nblock1*blocksize1;
	npad2 = nblock2*blocksize2;

	/* allocate spaces */
	f = alloc2float(npad1, npad2);
	g = alloc2float(blocksize1, blocksize2);
	qx = alloc1int(npad1*npad2);

	/* generate the transform tables */
	c1 = dctAlloc(blocksize1);
	c2 = dctAlloc(blocksize2);

	/* data read */
	memset((void *) f[0], 0, npad1*npad2*FSIZE);

	for(i1 = 0; i1 < n2; i1++){
	  gettr(&tr);
	  memcpy((void *) &f[i1][0], (const void *) &tr.data, n1*FSIZE);
	}
	
	/* DCT for each block */
	for(iblock2=0, ibeg2=0; iblock2<nblock2; 
	    iblock2++, ibeg2+=blocksize2)
	   for(iblock1=0, ibeg1=0; iblock1<nblock1; 
	       iblock1++, ibeg1+=blocksize1)
	   {
	      for(j2=0, i2=ibeg2; j2<blocksize2; i2++, j2++)
		 for(j1=0, i1=ibeg1; j1<blocksize1; i1++, j1++)
		    g[j2][j1] = f[i2][i1];
	      
	      dct_2(g, blocksize1, blocksize2, c1, c2, 0);
	      
	      for(j2=0, i2=ibeg2; j2<blocksize2; i2++, j2++)
		 for(j1=0, i1=ibeg1; j1<blocksize1; i1++, j1++)
		    f[i2][i1] = g[j2][j1];
	   }

	/* quantization */
	step = -1.;
	uniQuant(f[0], npad1*npad2, error, &ave, &step, qx);

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

	fwrite(&nsize, sizeof(int), 1, stdout);
	fwrite(&n1, sizeof(int), 1, stdout);
	fwrite(&n2, sizeof(int), 1, stdout);
	fwrite(&blocksize1, sizeof(int), 1, stdout);
	fwrite(&blocksize2, sizeof(int), 1, stdout);
	fwrite(&ave, sizeof(float), 1, stdout);
	fwrite(&step, sizeof(float), 1, stdout);
	fwrite(ibuff->code, sizeof(char), ibuff->pos, stdout);
	
	return EXIT_SUCCESS;
}

