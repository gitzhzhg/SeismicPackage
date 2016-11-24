/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#ifndef COMP_H
#define COMP_H

/* include files */
#include "par.h"
#include "membuff.h"
#include "membitbuff.h"


/* structure definitions */
typedef struct waveFilterStruct{
        int id;
        int len;
        int ishift;
        float *filterh;
        float *filterg;
     } waveFilter;      /* wavelet filter */


typedef struct rcfStruct{
    int rlen;
    int rlast;
    float *rc;
 } rcFunct;            /* rising-cutoff function */


typedef struct lpFilterStruct{
   int hlfilter;
   int lfilter;
   int nslope;
   int nblock;
   float ***filter;
 } lpFilter;


/* function declaritions */
float **dctAlloc(int n);
float **dctivAlloc(int n);
void dctiv(float *x, int n, float **c);   /* type-IV DCT */
void dctiv_2(float **x, int n1, int n2, float **c1, 
	     float **c2);   /* type-IV 2D-DCT */
void dct(float *x, int n, float **c, int type);   /* DCT */

void dct_2(float **x, int n1, int n2, float **c1, float **c2, int type);

rcFunct *rcalloc(int hlen);    /* space for rising-cutoff function */
void rcfmidp(rcFunct *rcf);    /* midpoint version of rcf */
void lct(float *sig, int n, rcFunct *rcf, float **c);   /* LCT */
void ilct(float *sig, int n, rcFunct *rcf, float **c);  /* inverse LCT */

waveFilter *waveGetfilter(int id);
int wavePack_1(float *x, float *y, waveFilter *filter, 
	       int twopow, int stage, int type);    /* 1D DWPT */
int wavePack_2(float **x, float **y, waveFilter *filter1, 
        waveFilter *filter2, int twopow1, int twopow2,
	       int stage1, int stage2, int type);   /* 2D DWPT */
int waveTrans_1(float *x, float *y, waveFilter *filter, 
		int twopow, int stage, int type);   /* 1D DWT */
int waveTrans_2(float **x, float **y, waveFilter *filter, 
		int twopow1, int twopow2, int stage1, 
		int stage2, int type);    /* 2D DWPT */
void lpred(float **f, int n1, int n2, int lblock, int hlfilter, int nslope,
           lpFilter *lpf, float **res);   /* lateral prediction */

memBUFF *buffAlloc1(int n);        /* allocate a 1D buffer */
void buffRealloc1(memBUFF *buff, int n);      /* reallocate a 1D buffer */
void buffFree1(memBUFF *buff);                /* free a 1D buffer */
memBUFF **buffAlloc2(int n1, int n2);           /* allocate a 2D buffer */ 
void buffFree2(memBUFF **buff, int n1);       /* free a 2D buffer */
void uniQuant(float *x, int n, float error, 
	      float *ave, float *step, int *qx);  /* uniform quantization */
void uniDequant(float *x, int n, float ave, 
		float step, int *qx);         /* uniform dequantization */
memBUFF* pEncode(int *qx, int n);             /* prefix encoding */
int pDecode(memBUFF *buff, int *qx, int n);   /* prefix decoding */
void codeSilence (void *inb, void *outb);    /* silence encoding */
void codeDesilence ( void *inb, void *outb); /* silence decoding */
int huffCompress(void *inb, void *outb);     /* Huffman encoding */
int huffDecompress(void *inb, void *outb);   /* Huffman decoding */

#endif /* COMP_H */
