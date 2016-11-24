/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCENDEC: $Revision: 1.5 $ ; $Date: 2011/11/21 16:24:37 $	*/

/*********************** self documentation **********************/
/***************************************************************************
WPCENDEC -  Wavelet Packet Coding, Encoding and Decoding routines

wpcEncoder - wavelet packet encoder
wpcDencoder - wavelet packet encoder

****************************************************************************
Function Prototypes:
int wpcEncoder(float **block, float error, void *inconf, void *outbuff, 
	void *interblock);
int wpcDecoder(float **block, void *inconf, void *outbuff,
	void *interblock);

****************************************************************************
wpcEncoder:
Input:
block		array[][] of data
error		relative RMS error tolerable
inconf		configuration information

Output:
outbuff		buffer holding the encoded stream 
interblock	reserved for interblock communication 

Return:		# of bytes after encoding

wpcDecoder:
Input:
inconf		configuration information
interblock	reserved for inter block communication

Output:
block		array[][] of data
outbuff		buffer holding the decoded stream 

Return:		consistancy flag, 1 if wpcCompressed, 0 otherwise

***************************************************************************
Author:		Tong Chen, 07/28/94
***************************************************************************/
/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"

int wpcEncoder(float **block, float error, void *inconf, void *outbuff, 
	void *interblock)
/***************************************************************************
the wrapping function for coding one block of data
****************************************************************************
Input:
block		array[][] of data
error		relative RMS error tolerable
inconf		configuration information

Output:
outbuff		buffer holding the encoded stream 
interblock	reserved for interblock communication 

Return:		# of bytes after encoding
****************************************************************************
Author:		Tong Chen, 07/28/94
***************************************************************************/
{
	wpcCONFIG *config = (wpcCONFIG *)inconf;
	wpcBUFF *buff = (wpcBUFF *)outbuff;
	wpcQUANT *quant;
	float **x;
	int nblock, lblock;
	int size;

	/* obtain the configuration info */
	nblock = config->nblock;
	lblock = config->lblock;

	/* allocate spaces */
	x = alloc2float(lblock, nblock);

	/* for quantization */
	quant = (wpcQUANT *) malloc(sizeof(wpcQUANT));
	quant->qx = alloc2int(lblock, nblock);
	quant->flowflag = (unsigned char *) malloc(nblock*sizeof(char));
	quant->blockind = (unsigned char *) malloc(nblock*sizeof(char));	


	/* wavelet packet transform */
	wavePack2(block, x, config, 0);

	/* quantization */
	quantFixerror(x, error, config, quant);

	/* encoding */
	size = codeEncoder(config, quant, buff, interblock);

	/* free the spaces */
	free2float(x);
	free2int(quant->qx);
	free((void *)quant->flowflag);
	free((void *)quant->blockind);
	free((void *)quant);

	return (size);
}


int wpcDecoder(float **block, void *inconf, void *outbuff,
	void *interblock)
/***************************************************************************
the wrapping function for decoding one block of data 
****************************************************************************
Input:
inconf		configuration information
interblock	reserved for inter block communication

Output:
block		array[][] of data
outbuff		buffer holding the decoded stream 

Return:		consistancy flag, 1 if wpcCompressed, 0 otherwise
****************************************************************************
Author:		Tong Chen, 07/28/94
***************************************************************************/
{
	wpcCONFIG *config = (wpcCONFIG *)inconf;
	wpcBUFF *buff = (wpcBUFF *)outbuff;
	wpcQUANT *quant;
	float **x;
	int nblock, lblock, retval;

	
	/* obtain the configuration info */
	nblock = config->nblock;
	lblock = config->lblock;

	/* allocate spaces */
	x = alloc2float(lblock, nblock);

	/* for quantization */
	quant = (wpcQUANT *) malloc(sizeof(wpcQUANT));
	quant->qx = alloc2int(lblock, nblock);
	quant->flowflag = (unsigned char *) malloc(nblock*sizeof(char));
	quant->blockind = (unsigned char *) malloc(nblock*sizeof(char));	

	/* decoding */
	retval = codeDecoder(config, quant, buff, interblock);

	if(retval){

	    /* dequantization */
	    quantDe(x, config, quant);

	    /* wavepacket transform */
	    wavePack2(block, x, config, 1);
	}

	/* free the spaces */
	free2float(x);
	free2int(quant->qx);
	free((void *)quant->flowflag);
	free((void *)quant->blockind);
	free((void *)quant);

	return (retval? 1 : 0);
}
