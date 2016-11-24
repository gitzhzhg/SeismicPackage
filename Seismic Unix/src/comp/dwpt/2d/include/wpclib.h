/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */



#ifndef _WPCLIB_H
#define _WPCLIB_H


/* structure for quantization status */
typedef struct wpcQuantStruct{
	unsigned char zeroflag;
	unsigned char *flowflag;
	unsigned char *blockind;
	float ave;
	float step;
	int **qx;
} wpcQUANT;

/* defines for quantization */
#define QUANTZEROFLAGY 1
#define QUANTZEROFLAGN 0
#define QUANTFLOWFLAGY 1
#define QUANTFLOWFLAGN 0
#define QUANTMAX 126 


/* prototypes */
/* tranform */
void wavePack2(float **x, float **y, void *inconf, int type);

/* quantization and dequantization*/
void quantFixerror(float **x, float error, void *inconf, void *qstate);
void quantDe(float **x, void *inconf, void *qstate);

/* en/decoding */
int codeCoder(void  *inconf, void *qstate, void *obuff, void *interblock);

/* silence en/decoding */
void codeSilence (void *inb, void *outb);
void codeDesilence ( void *inb, void *outb);

/* Huffman en/decoding */
int huffCompress(void *inbuff, void *outbuff);
int huffDecompress(void *inbuff, void *outbuff);

/* allocating buffers */
void *buffAlloc1(int n);
void buffRealloc1(void *buff, int n);
void buffFree1(void *buff);
void **buffAlloc2(int n1, int n2);
void buffFree2(void **buff, int n1);

#endif 
