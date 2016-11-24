/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */



/*********************** self documentation **********************/
/************************************************************************
RLE - routines for in memory silence en/decoding

codeSilence - encode the silence pieces of a stream 
codeDesilence	decode the silence encoded stream

*************************************************************************
Function Prototypes:
void codeSilence (void *inb, void *outb);
void codeDesilence ( void *inb, void *outb);

*************************************************************************
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

*************************************************************************
Author:        Tong Chen
************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"

#define SILENCELIMIT 1
#define STARTTHRESHOLD 5
#define STOPTHRESHOLD 1
#define SILENCECODE 0x80
#define ISSILENCE(c) (((c)>=0&&(c)<(SILENCELIMIT)) || ((c)>(0x100-SILENCELIMIT)))

#define LOOKAHSZ 8
#define LOOKAHMASK 7
#define MAXLENGTH 255


/* routines used internally */
static int silence_run(int buffer[], int index);
static int end_of_silence(int buffer[], int index);

void codeSilence (void *inb, void *outb)
/***********************************************************************
Silence encoding, takes care of "silence" pieces 
************************************************************************
inb	input buffer
outb	output buffer
***********************************************************************/
{
	memBUFF *inbuff = (memBUFF *) inb;
	memBUFF *outbuff = (memBUFF *) outb;
	int look_ahead[LOOKAHSZ];
	int index, i, run_length, flag;

	/* preload the look ahead buffer */
	for(i=0; i<LOOKAHSZ; i++)
	    if(buffGetc(inbuff, look_ahead[i]) == MEM_EOB)
		break;

	/* if not enough samples, no RLE */
	if(i < LOOKAHSZ){
	    buffMerge(outbuff, inbuff);
	    return;
	}

	index = 0;
	for(;;){

	    if(look_ahead[index] == MEM_EOB) break;

	    if(silence_run(look_ahead, index)){
		run_length = 0;

		do{
		    flag = buffGetc(inbuff, look_ahead[index]);
		    if(flag == MEM_EOB) look_ahead[index] = MEM_EOB;
		    index ++;
		    index &= LOOKAHMASK;
		    if(++run_length == MAXLENGTH){
			buffPutc(outbuff, SILENCECODE);
			buffPutc(outbuff, MAXLENGTH);
			run_length = 0;
		    }
		} while(!end_of_silence(look_ahead, index));

		if(run_length > 0){
		    buffPutc(outbuff, SILENCECODE);
		    buffPutc(outbuff, run_length);
		}
	    }

	    if(look_ahead[index] == MEM_EOB) break;

	    /* the silence code in the input get changed */
	    if(look_ahead[index] == SILENCECODE) look_ahead[index]++;
	    buffPutc(outbuff, look_ahead[index]);

	    /* get a new code */
	    flag = buffGetc(inbuff, look_ahead[index]);
	    if(flag == MEM_EOB) look_ahead[index] = MEM_EOB;
	    index ++;
	    index &= LOOKAHMASK;
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
	memBUFF *inbuff = (memBUFF *) inb;
	memBUFF *outbuff = (memBUFF *) outb;
	int c, run_count=0;

	while(buffGetc(inbuff, c) != MEM_EOB){
	    if(c==SILENCECODE){
		buffGetc(inbuff, run_count);
		while(run_count-- >0) buffPutc(outbuff, 0x0);
	     }
	    else buffPutc(outbuff, c);
	}
}


static int silence_run(int buffer[], int index)
{
	int i;
	
	for(i=0;i<STARTTHRESHOLD;i++)
	    if(!ISSILENCE(buffer[(index+i) & LOOKAHMASK]))
		return (0);
	return (1);
}

static int end_of_silence(int buffer[], int index)
{
	int i;

	for(i=0;i<STOPTHRESHOLD;i++)
	    if(ISSILENCE(buffer[(index+i) & LOOKAHMASK]))
		return (0);
	return (1);
}
