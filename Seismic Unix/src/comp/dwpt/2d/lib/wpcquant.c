/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCQUANT: $Revision: 1.4 $ ; $Date: 2011/11/21 16:24:37 $	*/



/*********************** self documentation **********************/
/****************************************************************************
WPCQUANT - quantization routines for WPC

quantFixerror - quantize
quantDe	- dequantize

*****************************************************************************
Function Prototypes:
void quantFixerror(float **x, float error, void *inconf, void *qstate);
void quantDe(float **x, void *inconf, void *qstate);

*****************************************************************************
quantFixerror:
Input:
x		array[][] for the floats 
error		percent RMS error tolerable 
inconf		configuration info
qstate		quantization status

quantDe:
x		array[][] for the floats 
inconf		configuration info
qstate		quantization status

*****************************************************************************
Author:		Tong Chen, 07/20/94
Modifier:	Tong Chen, 07/28/94, for API, without iteration
*****************************************************************************/

/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"


#define QUANTBYTEMAXINT 126.
/*
#define QUANTERRATIO 5.4
*/
#define QUANTERRATIO 5
#define QUANTZEROFLAGY 1
#define QUANTZEROFLAGN 0
#define QUANTFLOWFLAGY 1
#define QUANTFLOWFLAGN 0


void quantFixerror(float **x, float error, void *inconf, void *qstate)
/*******************************************************************************
quantization 
********************************************************************************
x		array[][] for the floats 
error		percent RMS error tolerable 
inconf		configuration info
qstate		quantization status
********************************************************************************
Author:		Tong Chen, 07/20/94
Modifier:	Tong Chen, 07/28/94, for API, without iteration
*******************************************************************************/
{
	wpcCONFIG *config = (wpcCONFIG *) inconf;
	wpcQUANT *quant = (wpcQUANT *) qstate;
	int nblock, lblock, tileszt, tileszx, nsz;
	int i;
	int iblock;
	float *fmax, *dev;
	int **qx, *blockind;
	unsigned char *flowflag, *cblockind;
	float atmp, average; 
	float clip, loading, bound, step_factor, rn, ratio; 


	/* obtain the configuration info */
	nblock = config->nblock;
	lblock = config->lblock;
	tileszt = config->tileszt;
	tileszx = config->tileszx;

	/* quantization status */
	qx = quant->qx;
	flowflag = quant->flowflag;
	cblockind = quant->blockind;

	nsz = tileszt*tileszx;

	/* spaces */
	fmax = alloc1float(nblock);
	dev = alloc1float(nblock);
	blockind = alloc1int(nblock);

	rn = 1./((float) lblock);

        /* for each difference block */
	clip = 0.;
        for(iblock = 0; iblock < nblock-1; iblock ++){

            /* compute the maximum amplitude and deviation */
	    fmax[iblock] = 0.;
	    dev[iblock] = 0.;
            for(i=0; i<lblock; i++){
                atmp = ABS(x[iblock][i]);
                if(fmax[iblock] < atmp) fmax[iblock] = atmp;
                dev[iblock] += atmp*atmp;
            }

            clip += dev[iblock];
            dev[iblock] *= rn;
            dev[iblock] = sqrt(dev[iblock]);
        }

	/* for the average block */
	average = 0.;
        for(i=0;i<lblock;i++){
	    average += x[nblock-1][i]; 
	}

	average *= rn;

	/* save this average for reconstruction */
	quant->ave = average;
	
	fmax[nblock-1] = dev[nblock-1] = 0.;
        for(i=0; i<lblock; i++){
            x[nblock-1][i]  = x[nblock-1][i] - average;
            atmp = ABS(x[nblock-1][i]);
            if(fmax[nblock-1] < atmp) fmax[nblock-1] = atmp;
            dev[nblock-1] += atmp*atmp;
        }
        clip += dev[nblock-1];

        dev[nblock-1] *= rn;
        dev[nblock-1] = sqrt(dev[nblock-1]);

        clip = clip/((float) nsz);
        clip = sqrt(clip);

	/* if all zero, no quantization needed */
	if(clip <= FLT_MIN){
	    quant->zeroflag = QUANTZEROFLAGY; 
	    return;
	}


	/* not all zero */
	quant->zeroflag = QUANTZEROFLAGN;

	error = 100.*error;
	clip *= error;

	/* for underflow */
	if(clip < FLT_MIN) clip = FLT_MIN;

	ratio = QUANTERRATIO;
	bound = ratio*clip;

	step_factor = QUANTBYTEMAXINT/bound;

	/* for overflow */
	if(step_factor > FLT_MAX) step_factor = FLT_MAX;

	quant->step = step_factor;

	/* quantization  */
       	for(iblock = 0; iblock < nblock; iblock ++){
            for(i=0; i<lblock; i++){
	    	atmp = x[iblock][i]*step_factor; 		
	      	qx[iblock][i] = NINT(atmp);
	    }

	    loading = fmax[iblock]*step_factor;
	    if(loading > QUANTBYTEMAXINT) 
		flowflag[iblock] = QUANTFLOWFLAGY;
	    else 
		flowflag[iblock] = QUANTFLOWFLAGN;
	}


        /* sort the blocks using deviation to reduce entropy */
        for(iblock=0; iblock<nblock; iblock++) 
            blockind[iblock] = iblock;

	qkisort(nblock, dev, blockind);

        for(iblock=0; iblock<nblock; iblock++) 
            cblockind[iblock] = blockind[iblock];

	/* free spaces */
	free((void *) fmax);
	free((void *) dev);
	free((void *) blockind);
}


void quantDe(float **x, void *inconf, void *qstate)
/*******************************************************************************
dequantization 
********************************************************************************
x		array[][] for the floats 
inconf		configuration info
qstate		quantization status
********************************************************************************
Author:		Tong Chen, 07/21/94
*******************************************************************************/
{
	wpcCONFIG *config = (wpcCONFIG *) inconf;
	wpcQUANT *quant = (wpcQUANT *) qstate;
	int nblock, lblock;
	unsigned char zeroflag;
	int **qx;
	int iblock, i;
	float rstep, ave;

	nblock = config->nblock;
	lblock = config->lblock;

	zeroflag = quant->zeroflag;
	qx = quant->qx;
	rstep = 1./(quant->step);
	ave = quant->ave;

	/* if all zero */
	if(zeroflag == QUANTZEROFLAGY){
       	    for(iblock = 0; iblock < nblock; iblock ++){
            	for(i=0; i<lblock; i++){
		    x[iblock][i] = 0.;
		}
	    }
	    return;
	}

        for(iblock = 0; iblock < nblock; iblock ++){
            for(i=0;i<lblock;i++){
                x[iblock][i] = ((float) qx[iblock][i])*rstep;
            }
	}

        for(i=0;i<lblock;i++){
            x[nblock-1][i]  += ave;
        }
}
