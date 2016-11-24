/* WPC1QUANT: $Revision: 1.3 $ ; $Date: 1997/07/30 15:24:20 $	*/

/*********************** self documentation **********************/
/*******************************************************************************

wpc1Quant - quantize
wpc1Dequant - dequantize

********************************************************************************
void wpc1Quant(float *x, int n, float error, void *inconf, void *qstate)
void wpc1Dequant(float *x, int n, void *inconf, void *qstate)

****************************************************************************
wpc1Quant:
Input:
x		array[] for the floats 
n		0 if the signal is transformed, length of signal otherwise
error		percent RMS error tolerable 
inconf		configuration inf

wpc1Dequant:
Output:
qstate		quantization status

Input:
n		0 if transformed data, length of signal otherwise
inconf		configuration info
qstate		quantization status

Output:
x		array[] for the floats 
****************************************************************************
wpc1Quant:
Notes:
That the rms error in quantization is about error*RMS_amplitude. The 
magic numbers are based on some relationships of the error measure 
and the step size. 

****************************************************************************
Author:		Tong Chen, 08/04/94
****************************************************************************/
/**************** end self doc ********************************/

#include "wpc1.h"
#include "wpc1lib.h"

#define QUANTRATIO 0.26
#define QUANTLOADING 5.
#define QUANTLEVEL 8 
#define QUANTMAXNBIT 14

void wpc1Quant(float *x, int n, float error, void *inconf, void *qstate)
/*******************************************************************************
quantization 
********************************************************************************
Input:
x		array[] for the floats 
n		0 if the signal is transformed, length of signal otherwise
error		percent RMS error tolerable 
inconf		configuration inf

Output:
qstate		quantization status
******************************************************************************** Note:	The quantization here is based on the RMS measure, i.e., to garantee 
	that the rms error in quantization is about error*RMS_amplitude. The 
	magic numbers are based on some relationships of the error measure 
	and the step size. 
********************************************************************************
Author:		Tong Chen, 08/04/94
*******************************************************************************/
{
	wpc1CONFIG *config = (wpc1CONFIG *) inconf;
	wpc1QUANT *quant = (wpc1QUANT *) qstate;
	int lblock, tileszt, nlen;
	int i, nbit, level, levelfull;
	int *qx;
	float atmp, average, fmax, dev;
	float clip, loading, bound, step_factor; 

	/* quantization status */
	qx = quant->qx;

	/* if the transformed coefficients */
	if(!n){

	    /* obtain the configuration info */
	    lblock = config->lblock;
	    tileszt = config->tileszt;

	    /* length of the signal */
	    nlen = tileszt;

            /* for each difference block */
	    dev = 0.;
	    fmax = 0.;
            for(i = 0; i < tileszt - lblock; i ++){
            	atmp = ABS(x[i]);
                if(fmax < atmp) fmax = atmp;
                dev += atmp*atmp;
            }

	    /* for the average block */
	    average = 0.;
            for(i=tileszt-lblock; i<tileszt; i++){
		average += x[i]; 
	    }

	    average = average/((float) lblock);

	    /* deviation */
            for(i=tileszt-lblock; i<tileszt; i++){
            	x[i]  = x[i] - average;
            	atmp = ABS(x[i]);
            	if(fmax < atmp) fmax = atmp;
            	dev += atmp*atmp;
            }
	}

	/* else normal data */
	else{

	    /* length of the signal */
	    nlen = n;

	    /* calculate the average */
	    average = 0.;
            for(i=0; i<nlen; i++){
		average += x[i]; 
	    }

	    average = average/((float) n);

	    /* the deviation */
	    fmax = 0.;
	    dev = 0.;
            for(i=0; i<nlen; i++){
            	x[i]  = x[i] - average;
            	atmp = ABS(x[i]);
            	if(fmax < atmp) fmax = atmp;
            	dev += atmp*atmp;
            }
	}

	/* save the # of samples */
	quant->numsmp = nlen;	

	/* standard deviation */
	dev = dev/((float) nlen);
        dev = sqrt(dev);

	/* save this average for reconstruction */
	quant->ave = average;
	
	/* if all zero (DC trace) */
	if(dev <= FLT_MIN){
	    quant->nbit = 0;
	    quant->step = FLT_MIN;
	    for(i=0; i<nlen; i++) quant->qx[i] = 0;
	    return;
	}

	/* loading factor */
	loading = fmax/dev;

	/* calculate the step factor */
	clip = error*dev + 0.0*loading;
	step_factor = QUANTRATIO/clip;

	/* # of bits needed */
	do{
	    bound = fmax*step_factor;

	    level = (int) (bound + .5);
	    
	    /* # of bits needed */
	    if(level == 0) nbit = 0;
	    else{
	    	nbit = 0;
	    	while(level >> nbit ) nbit ++;
	    	nbit --;
	    }

	    if(nbit > QUANTMAXNBIT){
fprintf(stderr,"Warning, cannot handle error so small, will do my best\n");
		step_factor *= .5;
	    } 
	} while(nbit > QUANTMAXNBIT);

	levelfull = 1 << (nbit+1);

	if(level > QUANTLEVEL){
	    /* avoid inefficiency caused by one level */ 
	    if(level==levelfull) 
	    	step_factor *= (level-2.)/level;
	    else if(level==levelfull-1)
		step_factor *= (level-1.)/level;
	}
	
	quant->nbit = nbit;
	quant->step = step_factor;

	/* quantization  */
       	for(i = 0; i < nlen; i ++){
	    atmp = x[i]*step_factor; 		
	    qx[i] = NINT(atmp);
	}
}


void wpc1Dequant(float *x, int n, void *inconf, void *qstate)
/*******************************************************************************
dequantization 
********************************************************************************
Input:
n		0 if transformed data, length of signal otherwise
inconf		configuration info
qstate		quantization status

Output:
x		array[] for the floats 
********************************************************************************
Author:		Tong Chen, 08/04/94
*******************************************************************************/
{
	wpc1CONFIG *config = (wpc1CONFIG *) inconf;
	wpc1QUANT *quant = (wpc1QUANT *) qstate;
	int nblock, lblock, tileszt;
	int *qx;
	int i;
	float step, rstep, ave;

	/* quantization status */
	qx = quant->qx;
	step = quant->step;
	ave = quant->ave;

	/* if transformed coefficients */
	if(!n){

	    /* obtain configuration info */
	    nblock = config->nblock;
	    lblock = config->lblock + 0*nblock;
	    tileszt = config->tileszt;

	    if(step <= FLT_MIN){ 
            	for(i = 0; i < tileszt; i++)
            	    x[i] = 0.;
	    }
	    else{
		rstep = 1./(quant->step);
            	for(i = 0; i < tileszt; i++)
            	    x[i] = ((float) qx[i])*rstep;
	   } 

	   /* put the average back */
           for(i=tileszt-lblock; i<tileszt;i++)
            	x[i]  += ave;
	}

	/* else normal data */
	else{

	    if(step <= FLT_MIN){ 
            	for(i = 0; i < n; i++)
            	    x[i] = 0.;
	    }
	    else{
		rstep = 1./(quant->step);
            	for(i = 0; i < n; i++)
            	    x[i] = ((float) qx[i])*rstep;
	    }

	   /* put the average back */
           for(i=0; i<n; i++){
            	x[i]  += ave;
           }
	}
}
