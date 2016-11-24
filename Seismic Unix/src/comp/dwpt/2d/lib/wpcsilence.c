/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCSILENCE: $Revision: 1.5 $ ; $Date: 2011/11/21 16:24:37 $	*/

/*********************** self documentation **********************/
/************************************************************************
WPCSILENCE - routines for in memory silence en/decoding

codeSilence	encode the silence pieces of a stream 
codeDesilence	decode the silence encoded stream

************************************************************************
Function Prototypes:
void codeSilence (void *inb, void *outb)
void codeDesilence ( void *inb, void *outb)

************************************************************************
codeSilence:
Input:
inb	input buffer
Output:
outb	output buffer

codeDesilence:
Input:
inb	input buffer
Output:
outb	output buffer

************************************************************************
codeSilence:
Note:
To avoid the inefficiency in encoding isolated 0's, I start a run
only when there are CODESTARTTHRSHOLD consective 0's. Therefore, 
a look ahead buffer is needed. The run is stopped as soon as there
is a non-0 symbol.  
************************************************************************
Author: CWP: Tong Chen, 1994
************************************************************************/
/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"
#include "wpcbuffer.h"

#define CODESILENCELIMIT 1
#define CODESTARTTHRESHOLD 5
#define CODESTOPTHRESHOLD 1
#define CODESILENCECODE 0x80
#define CODEISSILENCE(c) (((c)>=0&&(c)<(CODESILENCELIMIT)) || ((c)>(0x100-CODESILENCELIMIT)))

#define CODELOOKAHSZ 8
#define CODELOOKAHMASK 7
#define CODEMAXLENGTH 255


/* routines used internally */
static int silence_run(int buffer[], int index);
static int end_of_silence(int buffer[], int index);

void codeSilence (void *inb, void *outb)
/***********************************************************************
Silence encoding, takes care of "silence" pieces 
************************************************************************
inb	input buffer
outb	output buffer
************************************************************************
Note:	To avoid the inefficiency in encoding isolated 0's, I start a run
	only when there are CODESTARTTHRSHOLD consective 0's. Therefore, 
	a look ahead buffer is needed. The run is stopped as soon as there
	is a non-0 symbol.  
***********************************************************************/
{
	wpcBUFF *inbuff = (wpcBUFF *) inb;
	wpcBUFF *outbuff = (wpcBUFF *) outb;
	int look_ahead[CODELOOKAHSZ];
	int index, i, run_length, flag;

	/* preload the look ahead buffer */
	for(i=0; i<CODELOOKAHSZ; i++)
	    if(buffGetc(inbuff, look_ahead[i]) == WPC_EOB)
		break;

	/* if not enough samples, no RLE */
	if(i < CODELOOKAHSZ){
	    buffMerge(outbuff, inbuff);
	    return;
	}

	index = 0;
	for(;;){

	    if(look_ahead[index] == WPC_EOB) break;

	    if(silence_run(look_ahead, index)){
		run_length = 0;

		do{
		    flag = buffGetc(inbuff, look_ahead[index]);
		    if(flag == WPC_EOB) look_ahead[index] = WPC_EOB;
		    index ++;
		    index &= CODELOOKAHMASK;
		    if(++run_length == CODEMAXLENGTH){
			buffPutc(outbuff, CODESILENCECODE);
			buffPutc(outbuff, CODEMAXLENGTH);
			run_length = 0;
		    }
		} while(!end_of_silence(look_ahead, index));

		if(run_length > 0){
		    buffPutc(outbuff, CODESILENCECODE);
		    buffPutc(outbuff, run_length);
		}
	    }

	    if(look_ahead[index] == WPC_EOB) break;

	    /* the silence code in the input get changed */
	    if(look_ahead[index] == CODESILENCECODE) look_ahead[index]++;
	    buffPutc(outbuff, look_ahead[index]);

	    /* get a new code */
	    flag = buffGetc(inbuff, look_ahead[index]);
	    if(flag == WPC_EOB) look_ahead[index] = WPC_EOB;
	    index ++;
	    index &= CODELOOKAHMASK;
	}
}


void codeDesilence ( void *inb, void *outb)
/***********************************************************************
Silence deencoding 
************************************************************************
inb	input buffer
outb	output buffer
***********************************************************************/
{
	wpcBUFF *inbuff = (wpcBUFF *) inb;
	wpcBUFF *outbuff = (wpcBUFF *) outb;
	int c, run_count=0;

	while(buffGetc(inbuff, c) != WPC_EOB){
	    if(c==CODESILENCECODE){
		buffGetc(inbuff, run_count);
		while(run_count-- >0) buffPutc(outbuff, 0x0);
	     }
	    else buffPutc(outbuff, c);
	}
}


static int silence_run(int buffer[], int index)
{
	int i;
	
	for(i=0;i<CODESTARTTHRESHOLD;i++)
	    if(!CODEISSILENCE(buffer[(index+i) & CODELOOKAHMASK]))
		return (0);
	return (1);
}

static int end_of_silence(int buffer[], int index)
{
	int i;

	for(i=0;i<CODESTOPTHRESHOLD;i++)
	    if(CODEISSILENCE(buffer[(index+i) & CODELOOKAHMASK]))
		return (0);
	return (1);
}
