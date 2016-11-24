/* WPC1: $Revision: 1.3 $ ; $Date: 1997/07/30 15:25:01 $	*/

/*********************** self documentation **********************/
/************************************************************************
WPC1 - routines for compress a single seismic trace using wavelet packets 

wpc1Compress	compress a single trace
wpc1Uncompress	uncompress and generate a single trace
wpc1Numsmp	obtain the # of samples
wpc1Size	length of the compressed bit stream
wpc1Free	free up the space for the compressed data
wpc1Write	write the compressed data to a file
wpc1Read	read compressed data from a file 

*************************************************************************
Function Prototypes:
void *wpc1Compress(float *f, int n1, float error, int *nbytes);
int wpc1Uncompress(void *w1, float *f);
int wpc1Numsmp(void *wpc1);
int wpc1Size(void *wpc1);
void wpc1Free(void *wpc1);
void wpc1Write (void *wpc1, FILE *fp);
void *wpc1Read (FILE *fp);

*************************************************************************
wpc1Compress:
Input:
f	pointer to the trace
n1	# of samples in the trace
error	relative RMS error tolerable during compression

Output:
nbytes	# of bytes after compression 

Return:	pointer to the compressed data

wpc1Uncompress:
Input:
w1	pointer to the compressed trace

Output:
f	pointer to the uncompressed trace

Return:	consistency flag, 1 if wpc1Compressed data, 0 otherwise

wpc1Numsmp:
Input:
wpc1	pointer to the compressed data

Return:	# of samples per trace

wpc1Size:
Input:
wpc1	pointer to the compressed data

Return:	# of bytes after compression

wpc1Free:
Input:
wpc	pointer to the compressed data

wpc1Write:
Input:
wpc1	pointer to the compressed data
fp	file pointer to which to write

wpc1Read:
Input:
fp	file pointer to which to write

Return:	pointer to the compressed data

*************************************************************************
wpc1Compress:
Notes:
The compression here is done block by block, so the data is tiled 
first. For a single trace, to reduce the overhead, the shape gains
of each block are also quantized, but using 16bit, to maintain the
accuracy. Also, to reduce overhead, the last block (non-regular
block) is NOT transformed at all if it is shorter than half
of the tile size. If it is longer than half of the tile size, zero
padding is performed. An upgrade to the mixed-radix
version is expected soon. The size of the tiling is set in the 
routine wpcInitConfig. Those numbers are obtained empirically. 
Since we cannot determine the size of the output before compression,
memory is allocated within this routine, and adjusted after the
compression.  

*************************************************************************
Author:		Tong Chen, 08/04/94
************************************************************************/
/**************** end self doc ********************************/


#include "wpc1.h"
#include "wpc1lib.h"

/* routine used internally, to initialize some parameters */
static void wpc1InitConfig(void *config); 


void *wpc1Compress(float *f, int n1, float error, int *nbytes)
/************************************************************************
Input:
f	pointer to the trace
n1	# of samples in the trace
error	relative RMS error tolerable during compression

Output:
nbytes	# of bytes after compression 

Return:	pointer to the compressed data
*************************************************************************
Note:	The compression here is done block by block, so the data is tiled 
	first. For a single trace, to reduce the overhead, the shape gains
	of each block are also quantized, but using 16bit, to maintain the
	accuracy. Also, to reduce overhead, the last block (non-regular
	block) is NOT transformed at all if it is shorter than half
	of the tile size. If it is longer than half of the tile size, zero
	padding is performed. An upgrade to the mixed-radix
	version is expected soon. The size of the tiling is set in the 
	routine wpcInitConfig. Those numbers are obtained empirically. 
	Since we cannot determine the size of the output before compression,
	memory is allocated within this routine, and adjusted after the
	compression.  
************************************************************************/
{
	int tileszt, htileszt;
	int ip, nblock, npartition, n1p, nleft;
	float *block, *fpad;
	int it;
	int size;
	size_t mbound;

	wpc1PTR *wpc1;
	static wpc1CONFIG *config;
	wpc1QUANT **quant;
	unsigned char *code;

	mbound = n1*sizeof(float);

	/* allocate space for wpc */
	wpc1 = (wpc1PTR *) malloc(sizeof(wpc1PTR));
	wpc1->code = (unsigned char *) malloc(sizeof(char)*mbound);
	code = wpc1->code;
	wpc1->nsize = (int) mbound;

	/* space for the configuration */
	config = (wpc1CONFIG *) malloc(sizeof(wpc1CONFIG));

	/* save some parameters */
	wpc1->numsmp = n1;

	/* obtain the configuration info */
	wpc1InitConfig(config);

	/* prepare for tiling */
	tileszt = config->tileszt;

	/* # of partitions */
	npartition = n1/tileszt;

	/* regular size */
	n1p = npartition*tileszt;
	nleft = n1 - n1p;

	/* # of blocks */
	htileszt = tileszt >> 1;
	if(nleft == 0) { 
	    nblock = npartition;
	    fpad = f;
	}
	else if(nleft > htileszt){
	    npartition ++;
	    nblock = npartition;
	    n1p = npartition*tileszt;

	    /* allocate space */
	    fpad = (float *) malloc(n1p*sizeof(float));
	    /* copy the data */
	    for(it=0; it < n1; it++) fpad[it] = f[it];
	    /* pad with zeroes */
	    for(it=n1; it < n1p; it++) fpad[it] = 0;
	}
	else{
	     nblock = npartition + 1;
	     fpad = f;
	}

	/* allocate space for quantization status */
	quant = (wpc1QUANT **) malloc(nblock*sizeof(wpc1QUANT *));
	for(ip=0; ip < nblock; ip++) 
	    quant[ip] = (wpc1QUANT *) malloc(sizeof(wpc1QUANT)); 

	/* for each regular block */
	for(it=0, ip=0; ip<npartition; it += tileszt, ip++){

	    /* map the data */
	    block = fpad + it;	    

	    /* 1D wave packet transform */
	    wpc1Trans(block, config, 0);

	    /* space for quantized coefficients */
	    quant[ip]->qx = (int *) malloc(tileszt*sizeof(int));

	    /* quantize */
	    wpc1Quant(block, 0, error, config, quant[ip]);	
	
	}

	/* for the last block, no transform, just quantization */
	if(nblock != npartition){

	    block = fpad + it;

	    /* space for quantized coefficients */
	    quant[nblock-1]->qx = (int *) malloc(nleft*sizeof(int));

	    wpc1Quant(block, nleft, error, config, quant[nblock-1]);	

	}

	/* encoding the entire trace */
	wpc1Encoder(quant, nblock, wpc1);
	
	size = wpc1->nsize;

	/* adjust the size of the buffer  */
	code = (unsigned char *)realloc(code, size*sizeof(char));

	wpc1->nsize = size;

	*nbytes = size;

	if(nleft > htileszt) 
	    free((void *) fpad);

	free((void *) config);
	
	for(ip = 0; ip < nblock; ip++){
	    free((void *) quant[ip]->qx);
	    free((void *) quant[ip]);
	}
	free((void *) quant);

	return((void *)wpc1);

}

int wpc1Uncompress(void *w1, float *f)
/************************************************************************
Input:
w1	pointer to the compressed trace

Output:
f	pointer to the uncompressed trace

Return:	consistency flag, 1 if wpc1Compressed data, 0 otherwise
************************************************************************/
{
	int tileszt, htileszt;
	int n1, ip, nblock, npartition, n1p, nleft;
	float *block, *fpad;
	int it, retval;
	int size;

	wpc1PTR *wpc1 = (wpc1PTR *) w1;
	static wpc1CONFIG *config;
	wpc1QUANT **quant;

	n1 = wpc1->numsmp;
	size = wpc1->nsize;

	/* very quick consistency check */
	if( n1 > 0 && size >= 0) retval = 1;
	else return (0);

	/* space for the configuration */
	config = (wpc1CONFIG *) malloc(sizeof(wpc1CONFIG));

	/* obtain the configuration info */
	wpc1InitConfig(config);

	/* prepare for tiling */
	tileszt = config->tileszt;

	/* # of partitions */
	npartition = n1/tileszt;

	/* regular size */
	n1p = npartition*tileszt;
	nleft = n1 - n1p;

	/* # of blocks */
	htileszt = tileszt >> 1;
	if(nleft == 0) { 
	    nblock = npartition;
	    fpad = f;
	}
	else if(nleft > htileszt){
	    npartition ++;
	    nblock = npartition;
	    n1p = npartition*tileszt;

	    /* allocate space */
	    fpad = (float *) malloc(n1p*sizeof(float));
	}
	else{
	     nblock = npartition + 1;
	     fpad = f;
	}

	/* allocate space for quantization status */
	quant = (wpc1QUANT **) malloc(nblock*sizeof(wpc1QUANT *));
	for(ip=0; ip < nblock; ip++) 
	    quant[ip] = (wpc1QUANT *) malloc(sizeof(wpc1QUANT)); 

	/* for each regular block */
	for(ip=0; ip<npartition; ip++){
	    quant[ip]->qx = (int *) malloc(tileszt*sizeof(int));
	    quant[ip]->numsmp = tileszt;
	}

	/* for the last block */
	if(nblock != npartition) {
	    quant[nblock-1]->qx = (int *) malloc(nleft*sizeof(int));
	    quant[nblock-1]->numsmp = nleft;
	}

	/* decode the entire trace */
	retval = wpc1Decoder(quant, nblock, wpc1);

	/* if consistent data */
	if(retval){

	    /* for each regular block */
	    for(it=0, ip=0; ip<npartition; it += tileszt, ip++){

	    	/* get the memory */
	    	block = fpad + it;	    

	    	/* dequantize */
	    	wpc1Dequant(block, 0, config, quant[ip]);	

	    	/* 1D inverse wave packet transform */
	    	wpc1Trans(block, config, 1);

	    }

	    /* for the last block, no transform, just quantization */
	    if(nblock != npartition){

	    	block = fpad + it;

	     	wpc1Dequant(block, nleft, config, quant[nblock-1]);	
	    }
	
	}

	if(nleft > htileszt){
	    /* copy the data */
	    for(it=0; it < n1; it++) f[it] = fpad[it];
	    /* free up the space */
	    free((void *) fpad);
	}

	free((void *) config);
	
	for(ip = 0; ip < nblock; ip++){
	    free((void *) quant[ip]->qx);
	    free((void *) quant[ip]);
	}
	free((void *) quant);

	return(retval);

}


int wpc1Numsmp(void *wpc1)
/************************************************************************
Input:
wpc1	pointer to the compressed data

Return:	# of samples per trace
************************************************************************/
{
	wpc1PTR *w1 = (wpc1PTR *)wpc1;

	return (w1->numsmp);
}


int wpc1Size(void *wpc1)
/************************************************************************
Input:
wpc1	pointer to the compressed data

Return:	# of bytes after compression
************************************************************************/
{
	wpc1PTR *w1 = (wpc1PTR *)wpc1;

	return (w1->nsize);
}


void wpc1Free(void *wpc1)
/************************************************************************
Input:
wpc	pointer to the compressed data
************************************************************************/
{
	wpc1PTR *w1 = (wpc1PTR *)wpc1;

	free((void *)w1->code);
	free((void *)w1);
}


void wpc1Write (void *wpc1, FILE *fp)
/************************************************************************
Input:
wpc1	pointer to the compressed data
fp	file pointer to which to write
************************************************************************/
{
	wpc1PTR *w1 = (wpc1PTR *)wpc1;

	int numsmp = w1->numsmp;
	int nsize = w1->nsize;
	float step = w1->overallstep;
	float ave = w1->overallave;
	unsigned char *code = w1->code;

	fwrite(&numsmp, sizeof(int), 1, fp);
	fwrite(&nsize, sizeof(int), 1, fp);
	fwrite(&step, sizeof(float), 1, fp);
	fwrite(&ave, sizeof(float), 1, fp);
	fwrite(code, sizeof(unsigned char), nsize, fp);
}

void *wpc1Read (FILE *fp)
/************************************************************************
Input:
fp	file pointer to which to write

Return:	pointer to the compressed data
************************************************************************/
{
	wpc1PTR *w1;

	int numsmp; 
	int nsize;
	float step, ave;
	unsigned char *code;

	if(fread(&numsmp, sizeof(int), 1, fp) != 1)
	    return NULL;
	if(fread(&nsize, sizeof(int), 1, fp) != 1)
	    return NULL;
	if(fread(&step, sizeof(float), 1, fp) != 1)
	    return NULL;
	if(fread(&ave, sizeof(float), 1, fp) != 1)
	    return NULL;

	w1 = (wpc1PTR *) malloc(sizeof(wpc1PTR));
	
	w1->numsmp = numsmp;
	w1->nsize = nsize;
	w1->overallstep = step;
	w1->overallave = ave;

	w1->code = (unsigned char *) malloc(sizeof(unsigned char)*nsize);

	code = w1->code;
	fread(code, sizeof(unsigned char), nsize, fp);

	return ((void *) w1);

}



/* routine used internally to set the user specified parameters */
static void wpc1InitConfig(void *conf) 
{

	/* h filter coefficients */
	static float h[WPC_WTLEN] = { 0.016387336464, -0.041464936782, 
                        -0.067372554722, 0.386110066823, 
                         0.812723635450, 0.416998396426, 
                        -0.076488599079, -0.059434418647, 
                         0.023680171946, 0.005611434819, 
                        -0.001823208871, -0.000720549445};

	wpc1CONFIG *config = (wpc1CONFIG *)conf;
	float *ch, *cg;
	int i, j, len;

	config->powt = WPC_POWT;
	config->tileszt = WPC_TILESZT;

	config->staget = WPC_STAGET;

	config->nblock = WPC_NBLK;
	config->lblock = WPC_LBLK;

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
