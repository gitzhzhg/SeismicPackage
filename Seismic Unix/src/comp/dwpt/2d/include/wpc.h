/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */



#ifndef _WPC_H
#define _WPC_H

#include "par.h"
#include "wpcconfig.h"

/* structure for customized parameters and info */
typedef struct wpcConfigStruct{
	/* for tiling */
	int powt;	/* 2^powt is the tile size along t */	
	int powx;	/* 2^powx is the tile size along x */	
	int tileszt;	/* tile size along t */	
	int tileszx;	/* tile size along x */
	int nblock;	/* # of blocks in each tile */
	int lblock;	/* # of samples in each block */
	int codenblock;	/* # of huffman blocks */
	int codelblock;	/* # of blocks for one huffman block */ 
	/* stages of transform */
	int staget;	/* # of stages for transform along t */	
	int stagex;	/* # of stages for transform along x */	
	/* filter shift, length and coefficients */
	int ishift;	/* ishift of the wavelet */
	int len;	/* length of the filter */	
	float filterh[12];	/* filter coeff for h */
	float filterg[12];	/* filter coeff for g */
} wpcCONFIG;


/* structure for buffer */
typedef struct wpcBuffStruct{
	int mbound;
	int pos;
	unsigned char *code;
} wpcBUFF;

/* definitions */
#define WPC_EOB	-1
#define WPC_BOUND 2

/* structure for the encoded output */
typedef struct wpcPtrStruct{
	int version;
	int n1, n2;
	int size;
	wpcCONFIG *config;
	unsigned char *code;
} wpcPTR;


/* function prototypes */
void *wpcCompress(float *f, int n1, int n2, float error, int *nsize);
int wpcUncompress(void *wpc, float *f);
int wpcN1(void *wpc);
int wpcN2(void *wpc);
int wpcSize(void *wpc);
void wpcFree(void *wpc);
void wpcWrite(void *wpc, FILE *fp);
void *wpcRead(FILE *fp);


/* working functions used */
int wpcEncoder(float **block, float error, void *inconf, void *outbuff, 
        void *interblock);
int wpcDecoder(float **block, void *inconf, void *outbuff,
        void *interblock);

int codeEncoder(void  *inconf, void *qstate, void *obuff, void *interblock);
int codeDecoder(void  *inconf, void *qstate, void *ibuff, void *interblock);

#endif 
