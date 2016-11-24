/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCCODING: $Revision: 1.5 $ ; $Date: 2011/11/21 16:24:37 $	*/


/*********************** self documentation **********************/
/***************************************************************************
WPCCODING - Routines for in memory coding of the quantized coeffiecients

codeEncoder	encode the quantized coeffiecients into a bit stream	
codeDecoder	decode a bit stream into the quantized coeffiecients

****************************************************************************
Function Prototypes:
int codeEncoder(void  *inconf, void *qstate, void *obuff, void *interblock)
int codeDecoder(void  *inconf, void *qstate, void *ibuff, void *interblock)

****************************************************************************
codeEncoder:
Input:
inconf		configuration info
qstate		quantization status
Output:
outbuff		buffer to hold encoded bit stream
interblock	reserved for inter block communication
Return:
# of bytes after coding

codeDecoder:
Input:
inconf		configuration info
ibuff		buffer to hold encoded symbols
interblock	reserved for inter block communication
Output:
qstate		quantization status
Return:
consistency flag, 1 if wpcCompressed data, 0 otherwise

*************************************************************************
Author:		Tong Chen, 07/29/94
*************************************************************************/

/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"
#include "wpcbuffer.h"

#define CODEMAXLOAD 0x7fff
#define CODEOVERFLOW 127
#define CODEUNDERFLOW -127
#define CODESHORTSIZE 2
#define CODEBYTEMASK 0xff
#define CODEBYTEBIT 8

int codeEncoder(void  *inconf, void *qstate, void *obuff, void *interblock)
/*******************************************************************************
encoding the quantized coefficients 
********************************************************************************
Input:
inconf		configuration info
qstate		quantization status

Output:
outbuff		buffer to hold encoded bit stream
interblock	reserved for inter block communication (unused)

Return:		# of bytes after coding
********************************************************************************
Note:	This routine might seem messy. The essence is to store all the 
	information needed for reconstruction, while using minimal storage.
	Some of the side information is also huffman coded, making the 
	mixed type buffering operation necessary. In short, each block of
	quantized wavelet coefficients are first encoded, using "home brewed"
	prefix coding. This prefix coding takes advantage of the p.d.f. of the 
	wavelet coefficients and proves to be straight forward and efficient. 
	Following this, are the runlength coding and huffman coding. Several
	of the wavelet blocks are first merged to increase the runlength and to
	reduce the overhead of static huffman coding. This merging should be 
	based on the entropy, but here I use deviation instead, which turns out
	to be well correlated to the entropy. Of course, it can also be fixed
	like the zig-zag in the JPEG standard. 
********************************************************************************
Author:		Tong Chen, 07/29/94
*******************************************************************************/
{
	wpcCONFIG *config = (wpcCONFIG *) inconf;
	wpcQUANT *quant = (wpcQUANT *) qstate;
	wpcBUFF *outbuff = (wpcBUFF *) obuff;
	wpcBUFF *pbuff = (wpcBUFF *) interblock; /* dummy */
	wpcBUFF **quantbuff, **rlebuff, **huffbuff;
	int i, j, bind, nsize=0, shortsize, outputsize;
	unsigned int s;
	int iblock, nblock, lblock, codenblock, codelblock;
	int **qx;
	unsigned char zeroflag, *flowflag, *blockind;
	float ave, step;
	int quantmbound, huffmbound;

	/* obtain the configuration info */
	nblock = config->nblock;
	lblock = config->lblock;
	codenblock = config->codenblock;
	codelblock = config->codelblock;

	/* obtain the quantization status */
	zeroflag = quant->zeroflag;
	flowflag = quant->flowflag;
	blockind = quant->blockind;
	ave = quant->ave;
	step = quant->step;
	qx = quant->qx;
	
	outputsize = 0;

	/* if dc signal */
	if(zeroflag==QUANTZEROFLAGY){
	    /* output a zero flag */
	    if(buffPutc(outbuff, QUANTZEROFLAGY) == WPC_EOB)
	    	return WPC_EOB;
	    outputsize ++;	

	    /* output the average as well */ 
	    if(buffWrite(outbuff, sizeof(float), 1, &ave) == WPC_EOB)
	    	return WPC_EOB;
	    outputsize += sizeof(float);

	    return (outputsize);
	}


	/* else, output the zeroflag, average and stepsize, which are
	not huffman coded to preserve the low entroy nature of wavelet
	coefficients  */
	if(buffPutc(outbuff, QUANTZEROFLAGN) == WPC_EOB)
	    return WPC_EOB;
	outputsize ++;	

	/* output the average */
	if(buffWrite(outbuff, sizeof(float), 1, &ave) == WPC_EOB)
	    return WPC_EOB;
	outputsize += sizeof(float);

	/* output the step size */
	if(buffWrite(outbuff, sizeof(float), 1, &step) == WPC_EOB)
	    return WPC_EOB;
	outputsize += sizeof(float);


	/* decide the bounds for the buffers */
	quantmbound = ((int) sizeof(int))*lblock;	 /* guaranteed to be enough */
	huffmbound = ((int) sizeof(int))*codelblock*lblock;/* guaranteed to be enough */

	/* allocate and init buffers for RLE and Huffman */
	quantbuff = (wpcBUFF **) buffAlloc2(nblock, quantmbound); 
	rlebuff = (wpcBUFF **) buffAlloc2(codenblock, huffmbound); 
	huffbuff = (wpcBUFF **) buffAlloc2(codenblock, huffmbound); 


	/* encode each one of the blocks of wavelet coefficients */
	for(iblock = 0; iblock < nblock; iblock ++){

	    	pbuff = quantbuff[iblock];


		/* if no flow, then fast version */
		if(flowflag[iblock] == QUANTFLOWFLAGN){

		    /* output the flow flag, no bound checking needed */
		    buffPutc(pbuff, QUANTFLOWFLAGN);

	    	    /* convert into bytes */ 
	    	    for(i=0;i<lblock;i++)
			buffPutc(pbuff, (unsigned char) qx[iblock][i]);
	    	}
	
		/* else */
		else{

		    /* output the flow flag, no bound checking needed */
		    buffPutc(pbuff, QUANTFLOWFLAGY);

	    	    /* encoding */ 
	    	    for(i=0;i<lblock;i++){
		      if(qx[iblock][i] > 0){

		    	/* if overflow */
		    	if(qx[iblock][i] > QUANTMAX){

	    	            s = (unsigned) qx[iblock][i];

			    /* is there any signal on earth that can make this 
				happen? */
			    if(s > CODEMAXLOAD) s = CODEMAXLOAD; 

			    /* shift the level to avoid silence code */
			    s = (s << 1) + 1;	

			    /* output a prefix code */
		    	    buffPutc(pbuff, CODEOVERFLOW);

			    /* output a short */
			    shortsize = CODESHORTSIZE;
			    while(shortsize-- > 0){
		    	    	buffPutc(pbuff, s & CODEBYTEMASK);
				s >>= CODEBYTEBIT;
			    }	

		    	}

		    	else{
			    buffPutc(pbuff, (unsigned char) qx[iblock][i]);
		    	}
		      }

		      /* negative */
		      else{

		    	/* if underflow */
		    	if(qx[iblock][i] < -QUANTMAX){
	    	            s = (unsigned) (- qx[iblock][i]);

			    /* is there any signal on earth that can make this 
				happen? */
			    if(s > CODEMAXLOAD) s = CODEMAXLOAD; 

			    /* shift the level to avoid silence code */
			    s = (s << 1) + 1;	

			    /* output a prefix code */
		    	    buffPutc(pbuff, CODEUNDERFLOW);

			    /* output a short */
			    shortsize = CODESHORTSIZE;
			    while(shortsize-- > 0){
		    	    	buffPutc(pbuff, s & CODEBYTEMASK);
				s >>= CODEBYTEBIT;
			    }	

		    	}

		    	else{
			    buffPutc(pbuff, (unsigned char) qx[iblock][i]);
		    	}
		      }
		    }
		}

		/* adjust the buffer size */
		buffRealloc1((void *)pbuff, pbuff->pos);
	}


	/* output the block indices and buffer lengths as the header,
	to the Huffman buffer */
	for(iblock = 0; iblock < nblock; iblock ++){

	    /* no bound checking needed */
	    buffPutc(huffbuff[0], blockind[iblock]);
	}
	for(iblock = 0; iblock < nblock; iblock ++){
	    buffWrite(huffbuff[0], sizeof(int), 1, 
		&(quantbuff[iblock]->mbound));
	}


	/* put several blocks together to reduce overhead */
	for(iblock=0; iblock < codenblock; iblock++){

	    pbuff = rlebuff[iblock];

	    for(i=0; i< codelblock; i++){

		/* the block with largest deviation(entropy) is 
		encoded first, so that the header does not damage
		the low entropy blocks */
		j = (codenblock-1-iblock)*codelblock + i;
		bind = blockind[j];

		/* no bound checking needed */
		buffMerge(pbuff, quantbuff[bind]);
	    }

	    /* adjust the buffer sizes */
	    buffRealloc1((void *)pbuff, pbuff->pos);

	    /* rewind the buffers */
	    buffRewind(pbuff);
	}


	/* silence coding blocks, no bound checking necessary */
	for(iblock=0; iblock<codenblock; iblock++){
	    pbuff = huffbuff[iblock];

	    codeSilence(rlebuff[iblock], pbuff);

	    /* adjust the buffer size */
	    buffRealloc1((void *)pbuff, pbuff->pos);

	}


	/* Huffman encoding */
	for(iblock=0; iblock<codenblock; iblock++){
	
	    pbuff = huffbuff[iblock];

	    /* rewind the buffers */
	    buffRewind(pbuff);

	    /* Huffman encoding */
	    nsize = huffCompress(pbuff, outbuff);

	    if( nsize == WPC_EOB) break;
	    outputsize += nsize;
	}


	/* free the spaces */
	buffFree2((void **)quantbuff, nblock);
	buffFree2((void **)rlebuff, codenblock);
	buffFree2((void **)huffbuff, codenblock);

	return ((nsize == WPC_EOB)? WPC_EOB: outputsize);
}


int codeDecoder(void  *inconf, void *qstate, void *ibuff, void *interblock)
/*******************************************************************************
decode
********************************************************************************
Input:
inconf		configuration info
ibuff		buffer to hold encoded symbols
interblock	reserved for inter block communication (unused)

Output:
qstate		quantization status

Return:		consistency flag, 1 if wpcCompressed data, 0 otherwise
********************************************************************************
Author:		Tong Chen, 08/01/94
*******************************************************************************/
{
	wpcCONFIG *config = (wpcCONFIG *) inconf;
	wpcQUANT *quant = (wpcQUANT *) qstate;
	wpcBUFF *inbuff = (wpcBUFF *) ibuff; 
	wpcBUFF *pbuff = (wpcBUFF *) interblock; /* dummy */
	wpcBUFF **quantbuff, **rlebuff, **huffbuff;
	int i, j, bind, retval, shortsize, *bufflen;
	int iblock, nblock, lblock, codenblock, codelblock;
	unsigned int s;
	int **qx;
	unsigned char c=0, zeroflag, *flowflag, *blockind;
	signed char sc;
	float ave, step;
	int huffmbound;

	/* obtain the configuartion info */
	nblock = config->nblock;
	lblock = config->lblock;
	codenblock = config->codenblock;
	codelblock = config->codelblock;

	/* the quantized coefficients and etc */
	flowflag = quant->flowflag;
	blockind = quant->blockind;
	qx = quant->qx;
	
	/* read the zero flag */
	if(buffGetc(inbuff, zeroflag) == WPC_EOB) return 0;

	/* if all zero */
	if(zeroflag == QUANTZEROFLAGY){

	    /* read the average as well */
	    if(buffRead(inbuff, sizeof(float), 1, &ave) == WPC_EOB)
		return 0;

	     return 1;
	}

	/* else if other flag values */
	else if(zeroflag != QUANTZEROFLAGN) return 0;

	/* read the average and stepsize */
	if(buffRead(inbuff, sizeof(float), 1, &ave) == WPC_EOB)
	    return 0;
	if(buffRead(inbuff, sizeof(float), 1, &step) == WPC_EOB)
	    return 0;

	/* put them into the struct */
	quant->ave = ave;
	quant->step = step;

	/* decide the bound for the buffers */
	huffmbound = ((int) sizeof(int))*codelblock*lblock;/* garantee to be enough */

	/* allocate and init buffers for RLE and Huffman */
	bufflen = (int *) malloc(nblock*sizeof(int));
	quantbuff = (wpcBUFF **) malloc(nblock*sizeof(wpcBUFF *));
	rlebuff = (wpcBUFF **) buffAlloc2(codenblock, huffmbound);
	huffbuff = (wpcBUFF **) buffAlloc2(codenblock, huffmbound);


	/* this flag is set to free space even return abnormally */    
	retval = 1;

	/* Huffman decode all the blocks */
	for(iblock=0; iblock < codenblock; iblock ++){

	    pbuff = huffbuff[iblock];

	    if(huffDecompress(inbuff, pbuff) == WPC_EOB){
	    	retval = 0;		
	 	break;
	    }

	    /* adjust the buffers */
	    buffRealloc1((void *)pbuff, pbuff->pos);

	    /* rewind the buffer */
	    buffRewind(pbuff);

	}


	/* get the header */
	if(retval){

	   /* obtain the block indices  */ 
	   for(iblock = 0; iblock < nblock; iblock ++){

		/* consistency check */
/*
		if((buffGetc(huffbuff[0], blockind[iblock]) == WPC_EOB) ||
		      (blockind[iblock] > nblock) || (blockind[iblock] < 0)){
*/
		if((buffGetc(huffbuff[0], blockind[iblock]) == WPC_EOB) ||
		      (blockind[iblock] > nblock)){
		    retval = 0; 
		    break;
		}
	   }

	   if(retval){

	      /* obtain the block lengths */
	      for(iblock = 0; iblock < nblock; iblock ++){

		/* consistency check */
		if((buffRead(huffbuff[0], sizeof(int), 1, bufflen+iblock)
			== WPC_EOB) || bufflen[iblock]<0){
		    retval = 0; 
		    break;
		}
	      }


	      /* silence decoding */
	      if(retval){

		/* decode all the blocks, no checking */
		for(iblock=0; iblock < codenblock; iblock++){

		    pbuff = rlebuff[iblock];

		    /* silence decoding */
		    codeDesilence(huffbuff[iblock], pbuff);

	    	    /* adjust the buffers */
	    	    buffRealloc1((void *)pbuff, pbuff->pos);

		}

		/* allocate spaces for quantization buffers */
	        for(iblock = 0; iblock < nblock; iblock ++)
	 	  quantbuff[iblock] = (wpcBUFF *) buffAlloc1(bufflen[iblock]);


		/* partition into subbands */
		for(iblock=0; iblock<codenblock; iblock++){

		    pbuff = rlebuff[iblock];

		    /* rewind the buffers */
		    buffRewind(pbuff);

            	    for(i=0; i< codelblock; i++){
                    	j = (codenblock-1-iblock)*codelblock + i;
                    	bind = blockind[j];

                    	buffPart(quantbuff[bind],pbuff,bufflen[bind]);
		    }
                }

		/* decode to obtain the quantized coefficients */
		for(iblock = 0; iblock < nblock; iblock ++){

		    pbuff = quantbuff[iblock];

		    /* rewind the buffers */
		    buffRewind(pbuff);
			
		    i = 0;

		    /* get the flow flag */
		    buffGetc(pbuff, flowflag[iblock]); 

		    /* if no overflow, then fast version */
		    if(flowflag[iblock] == QUANTFLOWFLAGN){
			while(buffGetc(pbuff, sc) != WPC_EOB)
			    qx[iblock][i++] = sc; 
		    }

		    /* else, slow version */
		    else{
			while(buffGetc(pbuff, sc) != WPC_EOB){
			
			    /* if overflow */
			    if(sc == CODEOVERFLOW){

				/* get the following short */
				shortsize = CODESHORTSIZE;
				buffGetc(pbuff, c);
				s = c;
				while(--shortsize > 0){
				    buffGetc(pbuff, c);
				    s += c << ((CODESHORTSIZE-shortsize)*
						CODEBYTEBIT);
				}

				/* convert to integer */
				qx[iblock][i++] =  s >> 1;
			    }

			    /* else if underflow */
			    else if(sc == CODEUNDERFLOW){

                                /* get the following short */
                                shortsize = CODESHORTSIZE;
				buffGetc(pbuff, c);
				s = c;
				while(--shortsize > 0){
				    buffGetc(pbuff, c);
				    s += c << ((CODESHORTSIZE-shortsize)*
						CODEBYTEBIT);
				}

                                /* convert to integer */
                                qx[iblock][i++] =  - (s >> 1);
                            }   
			
			    else{
                                qx[iblock][i++] =  sc;
			    }
			}
		    }
		
	 	    if(i != lblock){
			retval = 0;	
			break;
		    }
		}
	    }
	  }
	}

	/* free the spaces */
	free((void *) bufflen);
	buffFree2((void **)quantbuff, nblock);
	buffFree2((void **)rlebuff, codenblock);
	buffFree2((void **)huffbuff, codenblock);

	/* return */
	return (retval);
}
