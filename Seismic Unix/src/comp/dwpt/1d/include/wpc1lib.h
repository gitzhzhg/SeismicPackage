/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */



#ifndef _WPCLIB_H
#define _WPCLIB_H

/* structure for buffer */
typedef struct wpcBuffStruct{
	int mbound;
	int pos;
	unsigned char *code;
} wpcBUFF;


#include "wpc1buffer.h"
#include "wpc1bitbuff.h"

/* definitions */
#define WPC_EOB	-1

/* function prototypes */
void wpc1Trans(float *f, void *conf, int type);
void wpc1Quant(float *x, int n, float error, void *inconf, void *qstate);
void wpc1Dequant(float *x, int n, void *inconf, void *qstate);
void wpc1Encoder(void *qstate, int nblock, void *wpc1);

#endif 
