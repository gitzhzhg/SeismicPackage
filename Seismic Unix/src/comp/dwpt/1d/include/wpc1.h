/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */



#ifndef _WPC_H
#define _WPC_H

#include "par.h"
#include "wpc1config.h"

/* structure for customized parameters and info */
typedef struct wpc1ConfigStruct{
	/* for tiling */
	int powt;	/* 2^powt is the tile size along t */	
	int tileszt;	/* tile size along t */	

	/* stages of transform */
	int staget;	/* # of stages for transform along t */	

	/* for quantization */
	int nblock;
	int lblock;

	/* filter shift, length and coefficients */
	int ishift;	/* ishift of the wavelet */
	int len;	/* length of the filter */	
	float filterh[12];	/* filter coeff for h */
	float filterg[12];	/* filter coeff for g */
} wpc1CONFIG;


/* structure for quantization */
typedef struct wpcQuantStruct{
	float step;
	float ave;
	int numsmp;
	int nbit;
	int *qx;
} wpc1QUANT;

/* definitions */
#define WPC_EOB	-1

/* structure for the encoded output */
typedef struct wpc1PtrStruct{
	int numsmp;
	int nsize;
	float overallstep;
	float overallave;
	unsigned char *code;
} wpc1PTR;


/* function prototypes */
void *wpc1Compress(float *f, int n1, float error, int *nsize);
int wpc1Uncompress(void *wpc1, float *f);
int wpc1Numsmp(void *wpc1);
int wpc1Size(void *wpc1);
void wpc1Free(void *wpc1);
void wpc1Write(void *wpc1, FILE *fp);
void *wpc1Read(FILE *fp);
int wpc1Decoder(void *qstate, int nblock, void *wpc1);


#endif 
