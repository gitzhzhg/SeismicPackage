/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/************************************************************************
WPC - routines for compress a 2D seismic section using wavelet packets 

wpcCompress	compress a 2D section
wpcUncompress	uncompress and generate a 2D section
wpcN1		obtain the # of samples in the fast dimension
wpcN2		obtain the # of samples in the slow dimension
wpcSize		length of the compressed bit stream
wpcFree		free up the space for the compressed data
wpcWrite	write the compressed data to a file
wpcRead		read compressed data from a file 
*************************************************************************
Function Prototypes:

void *wpcCompress(float *f, int n1, int n2, float error, int *nbytes);
int wpcUncompress(void *w, float *f);
int wpcN1(void *wpc);
int wpcN2(void *wpc);
int wpcSize(void *wpc);
void wpcFree(void *wpc);

*************************************************************************

wpcCompress:
Input:
f	pointer to the 2D section
n1	# of samples in the fast dimension
n2	# of samples in the slow dimension
error	relative RMS error tolerable during compression
Output:
nbytes	# of bytes after compression 
Return:	pointer to the compressed data

wpcUncompress:
Input:
w	pointer to the compressed data
Output:
f	pointer to the uncompress 2D section
Return:	consistency flag, 1 if wpcCompressed data, 0 otherwise

wpcN1:
Input:
wpc	pointer to the compressed data
Return:	# of samples in the fast dimension	

wpcN2:
Input:
wpc	pointer to the compressed data
Return:	# of samples in the slow dimension	

wpcSize:
Input:
wpc	pointer to the compressed data
Return:	# of bytes after compression

wpcFree:
Input:
wpc	pointer to the compressed data

wpcWrite:
Input:
wpc	pointer to the compressed data
fp	file pointer to which to write

wpcRead:
Input:
fp	file pointer to which to write

Return:	pointer to the compressed data

************************************************************************
wpcCompress:
Notes:
The compression is done block by block, so the data is tiled first.
The size of the tiling is set in the routine wpcInitConfig. 
Those numbers are obtained empirically. To support upgrading, 
the numbers and the filter parameters are stored as the header.	
Since we cannot determine the size of the output before compression,
memory is allocated within this routine, and adjusted after the
compression.  
************************************************************************
Author:		Tong Chen, 07/27/94
************************************************************************/
/**************** end self doc ********************************/

#include "wpc.h"

/* routine used internally, to initialize some parameters */
static void wpcInitConfig(void *config); 


void *wpcCompress(float *f, int n1, int n2, float error, int *nbytes)
/************************************************************************
Input:
f	pointer to the 2D section
n1	# of samples in the fast dimension
n2	# of samples in the slow dimension
error	relative RMS error tolerable during compression

Output:
nbytes	# of bytes after compression 

Return:	pointer to the compressed data
*************************************************************************
Note:	The compression is done block by block, so the data is tiled first.
	The size of the tiling is set in the routine wpcInitConfig. 
	Those numbers are obtained empirically. To support upgrading, 
	the numbers and the filter parameters are stored as the header.	
	Since we cannot determine the size of the output before compression,
	memory is allocated within this routine, and adjusted after the
	compression.  
************************************************************************/
{
	int tileszt, tileszx;
	int istrip, nblock, nstrip, n1p, n2p, n2l;
	float **strip, **block;
	int i, j, it, istart;
	int size, bsize, mbound, pos;
	void *interblock=NULL;	/* reserve a spot for inter block 
			        communication, not used yet */

	wpcPTR *wpc;
	static wpcCONFIG *config;
	wpcBUFF *buff;
	unsigned char *code;

	mbound = n1*n2;

	/* allocate space for wpc */
	wpc = (wpcPTR *) malloc(sizeof(wpcPTR));
	wpc->config = (wpcCONFIG *) malloc(sizeof(wpcCONFIG));
	wpc->code = (unsigned char *) malloc(sizeof(char)*mbound);

	/* allocate space for buffer */
	buff = (wpcBUFF *)malloc(sizeof(wpcBUFF));

	/* save some parameters */
	wpc->version = 1;	/* version # for portability */
	wpc->size = 0;		/* no data yet */
	wpc->n1 = n1;
	wpc->n2 = n2;

	/* save the configuration info for portability */
	config = wpc->config;
	wpcInitConfig(config);

	code = wpc->code;

	/* prepare for tiling */
	tileszt = config->tileszt;
	tileszx = config->tileszx;

	/* # of strips */
	nblock = (n1-1)/tileszt + 1;
	nstrip = (n2-1)/tileszx + 1;

	/* regular sizes */
	n1p = nblock*tileszt;
	n2p = nstrip*tileszx;
	n2l = n2 - n2p + tileszx;

	strip = alloc2float (n1p, tileszx);
	block = (float **) malloc(tileszx*sizeof(float *));

	/* init */
	istart = 0; 
	size = 0;

	buff->mbound = mbound;
	buff->pos = 0; 
	buff->code = code;

	for(istrip=0;istrip<nstrip-1;istrip++){

	    /* read a strip of traces and pad with zeroes */
	    for(j=0; j<tileszx; j++){

		memcpy(strip[j], f+istart, n1*sizeof(float));

		istart += n1;
	
		/* pad with zeroes */
		for(i=n1;i<n1p;i++) strip[j][i] = 0.;
	    }

	    /* for each block in the strip */
	    for(it=0; it<n1p; it += tileszt){

		/* get one block of data */
		for(i=0;i<tileszx;i++)
		    block[i] = strip[i] + it;	

		/* remember the output location */
		pos = buff->pos;

		/* encoding */
		bsize = wpcEncoder(block, error, config, buff, interblock); 

		/* this almost never happens */
		if(bsize == WPC_EOB){

		    /* reallocate a larger space */
		    mbound *= WPC_BOUND;
		    code = (unsigned char *)realloc(code, mbound*sizeof(char));

		    /* update the buffer */
		    buff->code = code;
		    buff->mbound = mbound;
		    buff->pos = pos;
		    
		    /* recode this block */
		    it -= tileszt;

		    continue;
		}
		size += bsize;

	    }
		
	}

	/* for the last strip */
	for(j=0; j<n2l; j++){

	    /* read a strip of traces */
	    memcpy(strip[j], f+istart, n1*sizeof(float));
	    istart += n1;

	    /* pad with zeroes */
	    for(i=n1; i<n1p; i++) strip[j][i] = 0.;
	}

        /* pad with dead traces */
	for(j=n2l; j<tileszx; j++)
	    for(i=0; i< n1p; i++) 
		strip[j][i] = 0.;

	/* for each block in the strip */
	for(it=0; it<n1p; it += tileszt){

	    /* get one block of data */
	    for(i=0;i<tileszx;i++)
	    	block[i] = strip[i] + it;	

	    /* remember the output location */
	    pos = buff->pos;

	    /* encoding */
	    bsize = wpcEncoder(block, error, config, buff, interblock);

	    /* this almost never happens */
	    if(bsize == WPC_EOB){

		/* reallocate a larger space */
		mbound *= WPC_BOUND;
		code = (unsigned char *)realloc(code, mbound*sizeof(char));

		/* update the buffer */
		buff->code = code;
		buff->mbound = mbound;
		buff->pos = pos;
		    
		/* recode this block */
		it -= tileszt;

		continue;
	     }
	     size += bsize;
	}

	/* safety check */
	if(size == buff->pos){

	    /* adjust the size of the buffer  */
	    code = (unsigned char *)realloc(code, size*sizeof(char));

	    wpc->size = size;

	    *nbytes = size;
	}

	else{
	    free((void *)code);
	    free((void *)config);
	    free((void *)wpc);
	    wpc = NULL;
	}

	free2float(strip);
	free((void *)block);
	free((void *)buff);

	return((void *)wpc);

}


int wpcUncompress(void *w, float *f)
/************************************************************************
Input:
w	pointer to the compressed data

Output:
f	pointer to the uncompress 2D section

Return:	consistency flag, 1 if wpcCompressed data, 0 otherwise
************************************************************************/
{
	int tileszt, tileszx;
	int istrip, nblock, nstrip, n1p, n2p, n2l;
	int n1, n2;
	float **strip, **block;
	int i, j, it, istart;
	int size, retval;
	void *interblock=NULL;

	wpcPTR *wpc = (wpcPTR *)w;
	wpcCONFIG *config;
	wpcBUFF *buff;
	unsigned char *code;

	n1 = wpc->n1;
	n2 = wpc->n2;
	size = wpc->size;

	/* very quick consistency check */
	if(n1>0 && n2>0 && size>0) retval = 1;
	else return (0);

	config = wpc->config;
	code = wpc->code;

	/* prepare for tiling */
	tileszt = config->tileszt;
	tileszx = config->tileszx;

	/* # of strips */
	nblock = (n1-1)/tileszt + 1;
	nstrip = (n2-1)/tileszx + 1;

	/* regular sizes */
	n1p = nblock*tileszt;
	n2p = nstrip*tileszx;
	n2l = n2 - n2p + tileszx;

	/* spaces */
	strip = alloc2float (n1p, tileszx);
	block = (float **) malloc(tileszx*sizeof(float *));
	buff = (wpcBUFF *) malloc(sizeof(wpcBUFF)); 

	/* init */
	istart = 0; 

	buff->code = code;
	buff->mbound = size;
	buff->pos = 0;

	for(istrip=0;istrip<nstrip-1;istrip++){

	    /* for each block in the strip */
	    for(it=0; it<n1p; it += tileszt){

		/* get one block of memory */
		for(i=0;i<tileszx;i++)
		    block[i] = strip[i] + it;	

		/* decode one block */
		retval = wpcDecoder(block, config, buff, interblock);

	
		/* if not consistent */
		if(!retval) break;

	    }

	    /* if not consistent */
	    if(!retval) break;

	    /* write a strip of traces  */
	    for(j=0; j<tileszx; j++){

		memcpy(f+istart, strip[j], n1*sizeof(float));
		istart += n1;
	    }
		
	}


	if(retval){

	    /* for the last strip */
	    for(it=0; it<n1p; it += tileszt){

	    	/* get one block of memory */
	    	for(i=0;i<tileszx;i++)
	    	    block[i] = strip[i] + it;	

	    	/* decoding */
	    	retval = wpcDecoder(block, config, buff, interblock);

		if(!retval) break;
	    }

	    if(retval){
		for(j=0; j<n2l; j++){

	    	    /* write a strip of traces */
	    	    memcpy(f+istart, strip[j], n1*sizeof(float));
	    	    istart += n1;
		}
	    }
	}

	free2float(strip);
	free((void *)block);
	free((void *)buff);

	return (retval);

}


int wpcN1(void *wpc)
/************************************************************************
Input:
wpc	pointer to the compressed data

Return:	# of samples in the fast dimension	
************************************************************************/
{
	wpcPTR *w = (wpcPTR *)wpc;

	return (w->n1);
}


int wpcN2(void *wpc)
/************************************************************************
Input:
wpc	pointer to the compressed data

Return:	# of samples in the slow dimension	
************************************************************************/
{
	wpcPTR *w = (wpcPTR *)wpc;

	return (w->n2);
}


int wpcSize(void *wpc)
/************************************************************************
Input:
wpc	pointer to the compressed data

Return:	# of bytes after compression
************************************************************************/
{
	wpcPTR *w = (wpcPTR *)wpc;

	return (w->size);
}


void wpcFree(void *wpc)
/************************************************************************
Input:
wpc	pointer to the compressed data
************************************************************************/
{
	wpcPTR *w = (wpcPTR *)wpc;

	free((void *)w->config);
	free((void *)w->code);
	free((void *)w);
}


void wpcWrite (void *w, FILE *fp)
/************************************************************************
Input:
wpc	pointer to the compressed data
fp	file pointer to which to write
************************************************************************/
{
	wpcPTR *wpc = (wpcPTR *)w;

	int version = wpc->version;
	int n1 = wpc->n1;
	int n2 = wpc->n2;
	int size = wpc->size;

	wpcCONFIG *config = wpc->config;
	unsigned char *code = wpc->code;

	fwrite(&version, sizeof(int), 1, fp);
	fwrite(&n1, sizeof(int), 1, fp);
	fwrite(&n2, sizeof(int), 1, fp);
	fwrite(&size, sizeof(int), 1, fp);
	fwrite(config, sizeof(wpcCONFIG), 1, fp);
	fwrite(code, sizeof(unsigned char), size, fp);
}

void *wpcRead (FILE *fp)
/************************************************************************
Input:
fp	file pointer to which to write

Return:	pointer to the compressed data
************************************************************************/
{
	wpcPTR *wpc;
	wpcCONFIG *config;
	unsigned char *code;

	int n1, n2;
	int version;
	int size;

	wpc = (wpcPTR *) malloc(sizeof(wpcPTR));
	
	fread(&version, sizeof(int), 1, fp);
	fread(&n1, sizeof(int), 1, fp);
	fread(&n2, sizeof(int), 1, fp);
	fread(&size, sizeof(int), 1, fp);

	wpc->version = version;
	wpc->n1 = n1;
	wpc->n2 = n2;
	wpc->size = size;

	wpc->config = (wpcCONFIG *) malloc(sizeof(wpcCONFIG));
	wpc->code = (unsigned char *) malloc(sizeof(unsigned char)*size);

	config = wpc->config;
	code = wpc->code;

	fread(config, sizeof(wpcCONFIG), 1, fp);
	fread(code, sizeof(unsigned char), size, fp);

	return ((void *) wpc);

}



/* routine used internally to set the user specified parameters */
static void wpcInitConfig(void *conf) 
{

	/* h filter coefficients */
	static float h[WPC_WTLEN] = { 0.016387336464, -0.041464936782, 
                        -0.067372554722, 0.386110066823, 
                         0.812723635450, 0.416998396426, 
                        -0.076488599079, -0.059434418647, 
                         0.023680171946, 0.005611434819, 
                        -0.001823208871, -0.000720549445};

	wpcCONFIG *config = (wpcCONFIG *)conf;
	float *ch, *cg;
	int i, j, len;

	config->powt = WPC_POWT;
	config->powx = WPC_POWX;
	config->tileszt = WPC_TILESZT;
	config->tileszx = WPC_TILESZX;
	config->nblock = WPC_NWTBLK;
	config->lblock = WPC_LWTBLK;
	config->codenblock = WPC_NHUFFBLK;
	config->codelblock = WPC_LHUFFBLK;

	config->staget = WPC_STAGET;
	config->stagex = WPC_STAGEX;

	config->ishift = WPC_WTSHIFT;
	config->len = WPC_WTLEN;

	len = WPC_WTLEN;

	ch = config->filterh;
	cg = config->filterg;

        /* get the filter coefficients */
        for(i=0;i<len;i++) ch[i] = h[i];
        for(i=0,j=len-1;i<len;i++,j--) cg[i] = h[j];
        for(i=1;i<len;i+=2) cg[i] = -cg[i];

}
