/* WPC1CODING: $Revision: 1.3 $ ; $Date: 1997/07/30 15:22:59 $	*/

/*********************** self documentation **********************/
/************************************************************************
WPC1CODING - routines for encoding the integer symbols in 1D WPC 

wpc1Encoder - encoder
wpc1Decoder - decoder

************************************************************************
Function Prototypes:
void wpc1Encoder(void *qstate, int nblock, void *wpc1)
int wpc1Decoder(void *qstate, int nblock, void *wpc1)

************************************************************************
wpc1Encoder:
Input:
quant		quantization status
nblock		# of blocks

Output:
wpc1	 	encoded data

wpc1Decoder:
Input:
nblock		# of blocks
wpc1		compressed data	

Output:
quant		quantization status		

Return:		consistency flag, 1 if wpc1Comressed data, 0 otherwise

************************************************************************
Author: CWP: Tong Chen 1995
************************************************************************/
/**************** end self doc ********************************/

#include "wpc1.h"
#include "wpc1lib.h"

#define FMAXINTUS 65535.
#define FMAXINTSN 32767.
#define BITMAXNBIT 4
#define MAXNBIT 16

/* structure used internally */
/* one code */
typedef struct entCodeStruct{
	unsigned int value;
	int bits;
} entCODE;

/* code book */
typedef struct codeBookStruct{
	int maxrun;
	int bitmaxrun;
	int booksize;
	entCODE *code;
	int *pref2bit;
} codeBOOK;

/* code dictionary */ 
typedef struct codeDictStruct{
	int dictsize;
	codeBOOK *book;
} codeDICT;

/* table used internally */
/* this table enables efficient compression for a wide range of
accuracy, namely, 1-16 bit, or with relative RMS error 10^(-5) to 1. */
static int amptbl[MAXNBIT]
    = 	{0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 
	0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000};

/* functions used internally */
void entCoder(void *qstate, int nblock, void *outbuff);
int entDecoder(void *qstate, int nblock, void *outbuff);
void bookCoder(void *bitbuff, void *book, void *qx, int numsmp);
int bookDecoder(void *bitbuff, void *book, void *qx, int numsmp);
static codeDICT *builddict(void);


void wpc1Encoder(void *qstate, int nblock, void *wpc1)
/*************************************************************************
Encode the quantized data 
**************************************************************************
Input:
quant		quantization status
nblock		# of blocks

Output:
wpc1	 	encoded data
**************************************************************************
Author:		Tong Chen, 08/04/94
*************************************************************************/
{
	wpc1QUANT **quant = (wpc1QUANT **) qstate;
	wpc1PTR *w1 = (wpc1PTR *) wpc1;
	wpcBUFF *buff;
	float *step, *ave;
	float atmp, maxstep, maxave, dstep, dave; 
	int *nstep, *nave;
	unsigned char c1, c2;
	int i;

	/* allocate spaces */
	step = (float *) malloc(nblock*sizeof(float));
	ave = (float *) malloc(nblock*sizeof(float));
	nstep = (int *) malloc(nblock*sizeof(int));
	nave = (int *) malloc(nblock*sizeof(int));

	/* allocate the buffer */
	buff = (wpcBUFF *) malloc(sizeof(wpcBUFF));

	/* init the buffer */
	buff->code = w1->code;
	buff->mbound = w1->nsize;
	buff->pos = 0;

	/* obtain the steps and averages */
	maxstep = -1.;
	maxave = -1.;
	for(i=0; i<nblock; i++){
	    step[i] = quant[i]->step;
	    if(maxstep < step[i]) maxstep = step[i];

	    ave[i] = quant[i]->ave;
	    atmp = ABS(ave[i]);
	    if(maxave < atmp) maxave = atmp;
	}

	/* save the step and average */
	w1->overallstep = maxstep;
	w1->overallave = maxave;

	/* if DC trace */
	if(maxstep <= FLT_MIN){

	    /* if not dead trace */
	    if(maxave > FLT_MIN){

		/* quantize the averages */
		dave = FMAXINTSN/maxave;
		for(i=0; i<nblock; i++)
	    	    nave[i] = NINT(ave[i]*dave);

		/* output the averages as signed short, to the buffer */
		for(i=0; i<nblock; i++){
		    /* 2's complement */
	    	    if(nave[i] < 0) nave[i] = 0x10000 + nave[i]; 
	    	    c1 = (nave[i]) & 0xff;
	    	    c2 = (nave[i] >> 8) & 0xff;
	    	    buffPutc(buff, c1); 
	    	    buffPutc(buff, c2); 
		}
	    }
	}

	/* else, normal trace */
	else{

	    /* quantize the steps */
	    dstep = FMAXINTUS/maxstep;
	    for(i=0; i<nblock; i++)
	        nstep[i] = NINT(step[i]*dstep);

	    /* output the steps as unsigned short, to the buffer */
	    for(i=0; i<nblock; i++){
	    	c1 = (nstep[i]) & 0xff;
	    	c2 = (nstep[i] >> 8) & 0xff;
	    	buffPutc(buff, c1); 
	    	buffPutc(buff, c2); 
	    }
	
	    /* if DC not zero */
	    if(maxave > FLT_MIN){

	        /* quantize the steps */
		dave = FMAXINTSN/maxave;
		for(i=0; i<nblock; i++)
	    	    nave[i] = NINT(ave[i]*dave);
	
		/* output the average as signed short, to the buffer */
		for(i=0; i<nblock; i++){ 
		    /* 2's complement */
	    	    if(nave[i] < 0) nave[i] = 0x10000 + nave[i];
	    	    c1 = (nave[i]) & 0xff;
	    	    c2 = (nave[i] >> 8) & 0xff;
	    	    buffPutc(buff, c1); 
	    	    buffPutc(buff, c2); 
		}
	    }


	    /* entropy encoding */	
	    entCoder(quant, nblock, buff);

	}

	/* adjust the size */
	w1->nsize = buff->pos;

	/* free the spaces */
	free((void *) step);
	free((void *) ave);
	free((void *) nstep);
	free((void *) nave);
	free((void *) buff);
}


int wpc1Decoder(void *qstate, int nblock, void *wpc1)
/*************************************************************************
Decode the compressed data 
**************************************************************************
Input:
nblock		# of blocks
wpc1		compressed data	

Output:
quant		quantization status		

Return:		consistency flag, 1 if wpc1Comressed data, 0 otherwise
**************************************************************************
Author:		Tong Chen, 08/08/94
*************************************************************************/
{
	wpc1QUANT **quant = (wpc1QUANT **) qstate;
	wpc1PTR *w1 = (wpc1PTR *) wpc1;
	wpcBUFF *buff;
	float  maxstep, maxave, dstep, dave; 
	int *nstep, *nave;
	unsigned char c1=0, c2=0;
	int i, retval;

	/* allocate spaces */
	nstep = (int *) malloc(nblock*sizeof(int));
	nave = (int *) malloc(nblock*sizeof(int));

	/* allocate the buffer */
	buff = (wpcBUFF *) malloc(sizeof(wpcBUFF));

	/* init the buffer */
	buff->code = w1->code;
	buff->mbound = w1->nsize;
	buff->pos = 0;

	/* max of the step and average */
	maxstep = w1->overallstep;
	maxave = w1->overallave;

	/* if DC trace */
	if(maxstep <= FLT_MIN){

	    /* if not dead trace */
	    if(maxave > FLT_MIN){

		/* input the average as signed short */
		for(i=0; i<nblock; i++){
	    	    buffGetc(buff, c1); 
	    	    buffGetc(buff, c2); 
	    	    nave[i] = c1 + (c2 << 8); 
	    	    if(c2 & 0x80) nave[i] = nave[i] - 0x10000; 
		}

		/* dequantize the averages */
		dave = maxave/FMAXINTSN;
		for(i=0; i<nblock; i++)
	    	    quant[i]->ave = nave[i]*dave;
	    }

	    /* set the steps */
	    for(i=0; i<nblock; i++)
	    	quant[i]->step = FLT_MIN;

	    retval = 1;
	}

	/* else normal trace */
	else{

	    /* input the step as unsigned short */
	    for(i=0; i<nblock; i++){
	    	buffGetc(buff, c1); 
	     	buffGetc(buff, c2); 
	    	nstep[i] = c1 + (c2 << 8);
	    }
	
	    /* dequantize the steps */
	    dstep = maxstep/FMAXINTUS;
	    for(i=0; i<nblock; i++)
	    	quant[i]->step = nstep[i]*dstep;

	    /* if DC not zero */
	    if(maxave > FLT_MIN){

		/* input the average as signed short */
		for(i=0; i<nblock; i++){
	    	    buffGetc(buff, c1); 
	    	    buffGetc(buff, c2); 
	    	    nave[i] = c1 + (c2 << 8); 
	    	    if(c2 & 0x80) nave[i] = nave[i] - 0x10000; 
		}

		/* dequantize the averages */
		dave = maxave/FMAXINTSN;
		for(i=0; i<nblock; i++)
	    	    quant[i]->ave = nave[i]*dave;
	    }

	    /* entropy decoding */	
	    retval = entDecoder(quant, nblock, buff);

	}

	/* free the spaces */
	free((void *) nstep);
	free((void *) nave);
	free((void *) buff);

	return (retval);
}


void entCoder(void *qstate, int nblock, void *outbuff)
/**********************************************************************
entropy encoder using some codebook   
***********************************************************************
Input:
qstate		quantization status
nblock		# of blocks

Output:
outbuff		output memory buffer
**********************************************************************/
{
	wpc1QUANT **quant = (wpc1QUANT **) qstate;
	wpcBUFF *buff = (wpcBUFF *) outbuff;
	wpcBITBUFF *bitbuff;
	codeBOOK codebook;
	static codeDICT *codedict;
	int iblock, flag;
	int numsmp, nbit;
	int *qx;

	/* init the bitwise buffer */
	bitInitbuff(bitbuff, buff);

	/* write # of bits to the buffer */
	for(iblock=0; iblock<nblock; iblock++){
	    nbit = quant[iblock]->nbit;
	    bitOutputbits(bitbuff, nbit, BITMAXNBIT, flag);	
	}

	/* build the code dictionary */
	codedict = builddict();

	/* entropy coding each block */
	for(iblock=0; iblock<nblock; iblock++){

	    nbit = quant[iblock]->nbit;
	    numsmp = quant[iblock]->numsmp;	
	    qx = quant[iblock]->qx;	

	    /* obtain the code book for this nbit */
	    codebook = codedict->book[nbit];
	
	    /* encode according to the code book */
	    bookCoder(bitbuff, &codebook, qx, numsmp);
	}

	nbit += 0*flag; /* dummy */
	/* wind up the buffer */
	bitWindbuff(bitbuff, flag);

	/* close the buffer */
	bitFreebuff(bitbuff);
}


int entDecoder(void *qstate, int nblock, void *inbuff)
/**********************************************************************
entropy decoder using some codebook   
***********************************************************************
Input:
qstate		quantization status
nblock		# of blocks
inbuff		input memory buffer

Output:
qstate		quantization status

Return:		consistency flag, 1 if wpc1Compressed data, 0 otherwise
**********************************************************************/
{
	wpc1QUANT **quant = (wpc1QUANT **) qstate;
	wpcBUFF *buff = (wpcBUFF *) inbuff;
	wpcBITBUFF *bitbuff;
	codeBOOK codebook;
	static codeDICT *codedict;
	int iblock,flag, retval=0;
	int numsmp, nbit;
	int *qx;

	/* init the bitwise buffer */
	bitInitbuff(bitbuff, buff);

	/* read # of bits to the buffer */
	for(iblock=0; iblock<nblock; iblock++){
	    bitInputbits(bitbuff, BITMAXNBIT, nbit, flag);	
	    quant[iblock]->nbit = nbit;
	}

	/* build the code dictionary */
	codedict = builddict();
	
	/* entropy decoding each block */
	for(iblock=0; iblock<nblock; iblock++){

	    nbit = quant[iblock]->nbit;
	    numsmp = quant[iblock]->numsmp;	
	    qx = quant[iblock]->qx;	

	    /* obtain the code book for this nbit */
	    codebook = codedict->book[nbit];

	    /* decode according to the code book */
	    retval = bookDecoder(bitbuff, &codebook, qx, numsmp);

	    if(!retval) break;
	}

	/* close the buffer */
	bitFreebuff(bitbuff);

	retval += 0*flag; /* dummy */

	return (retval);
}


void bookCoder(void *buff, void *book, void *q, int numsmp)
/************************************************************************
entropy coding using a codebook
*************************************************************************
Input:
book		pointer to the codebook
q		array of symbols need encoded
numsmp		# of samples in the array

Output:
buff		output memory buffer (bitwise)	
************************************************************************/
{

        wpcBITBUFF *bitbuff = (wpcBITBUFF *) buff;
        codeBOOK *codebook = (codeBOOK *) book;
        entCODE code; 
        int *qx = (int *) q;
        int symbol, absq;
        int runlen = 0;
        int i, ibit, booksize, flag;

        /* booksize */
        booksize = codebook->booksize;


        /* for each symbol */
        for(i=0; i<numsmp; i++){

            symbol = qx[i];

            /* if zero */
            if(symbol == 0){
                runlen ++;
                continue;
            }

            /* output the runlength */
            if(runlen !=0 ){
                while(runlen > 0){
                    code = codebook->code[0];
                    bitOutputbits(bitbuff, code.value, code.bits, flag);        
   		    if(runlen <= codebook->maxrun){
                        bitOutputbits(bitbuff, runlen-1, 
                                codebook->bitmaxrun, flag);
                            runlen = 0;
                    }

                    else{
                        bitOutputbits(bitbuff, codebook->maxrun - 1, 
                            codebook->bitmaxrun, flag);
                        runlen -= codebook->maxrun;
                    }
                }
            }

            absq = abs(symbol);

            /* # of bits */
            for(ibit=0; ibit<MAXNBIT; ibit++)
                if(absq < amptbl[ibit]) break;

            /* prefix coding */
            code = codebook->code[ibit];
            bitOutputbits(bitbuff, code.value, code.bits, flag);

            /* code */
            if(symbol > 0){
                bitOutputbits(bitbuff, symbol, ibit, flag);
            }
            else{
                symbol += amptbl[ibit] - 1;
                bitOutputbits(bitbuff, symbol, ibit, flag);
            }
        }

        /* if ended with a run */
        if(runlen !=0 ){
            while(runlen > 0){
                code = codebook->code[0];
                bitOutputbits(bitbuff, code.value, code.bits, flag);
                if(runlen <= codebook->maxrun){
                    bitOutputbits(bitbuff, runlen-1, 
                        codebook->bitmaxrun, flag);
                    runlen = 0;
                }

                else{
                    bitOutputbits(bitbuff, codebook->maxrun - 1, 
                        codebook->bitmaxrun, flag);
                    runlen -= codebook->maxrun;
                }
            }
        }


        /* output the end of block symbol */
        code = codebook->code[booksize - 1];
        bitOutputbits(bitbuff, code.value, code.bits, flag);
	runlen = flag; /* dummy */
}


int bookDecoder(void *bbuff, void *book, void *q, int numsmp)
/************************************************************************
entropy decoding using a codebook
*************************************************************************
Input:
bbuff		input memory buffer (bitwise)	
book		pointer to the codebook
numsmp		# of samples in the array

Output:
q		array of symbols decoded

Return:		consistency flag, 1 if wpc1Compressed, 0 otherwise
************************************************************************/
{

        wpcBITBUFF *bitbuff = (wpcBITBUFF *) bbuff;
        codeBOOK *codebook = (codeBOOK *) book;
        entCODE code0; 
        int *qx = (int *) q;
        int symbol, absq;
        int runlen, maxrun, bitmaxrun;
        int i, ibit, pbit, morebit, eobbits, pref, booksize, flag;
        unsigned int value, eobflag, fullpref;

        /* booksize, 0 code and eob */
        booksize = codebook->booksize;
        code0 = codebook->code[0];
        maxrun = codebook->maxrun;
        bitmaxrun = codebook->bitmaxrun +0*maxrun;
        eobflag = codebook->code[booksize-1].value;
        eobbits = codebook->code[booksize-1].bits;
	fullpref = (eobbits == code0.bits);

        i = 0;

        do {

            /* read some bits */ 
            bitInputbits(bitbuff, code0.bits, pref, flag); 
            if(flag == WPC_EOB) return (0);


            /* if it's a zero run */
            if(pref == code0.value){

                /* input the runlength */
                bitInputbits(bitbuff, bitmaxrun, runlen, flag); 
                if(flag == WPC_EOB) return (0);

                /* convert into integers */
                while(runlen >= 0){
                    qx[i++] = 0;
                    runlen --;
                }
            }

            /* else if other prefix */
            else if(codebook->code[pref].bits == code0.bits){

                /* if the end of block symbol */
                if(fullpref && (codebook->code[pref].value == eobflag))
		    break;

		/* conver to # of bits */
		ibit = codebook->pref2bit[pref];

                /* input the code */
                bitInputbits(bitbuff, ibit, absq, flag); 
                if(flag == WPC_EOB) return (0);

                /* if the end of block symbol */
                if((!fullpref) && ((pref << ibit) + absq == eobflag))
			break;

                /* convert to integer */
                if(absq & amptbl[ibit-1]) symbol = absq;
                else symbol = absq - amptbl[ibit] + 1;

                qx[i++] = symbol;
            }

            /* else if not full prefix */
            else{

		pbit = codebook->code[pref].bits; 
		morebit = pbit - code0.bits;

                /* input the remaining prefix */
                bitInputbits(bitbuff, morebit, value, flag); 
                if(flag == WPC_EOB) return (0);

                /* get the full prefix */
                pref = (pref << morebit) + value;

                /* if the end of block symbol */
                if((pbit == eobbits) && (pref == eobflag))
			break;

                /* convert to # of bits */
                ibit = codebook->pref2bit[pref];


                /* input the code */
                bitInputbits(bitbuff, ibit, absq, flag); 
                if(flag == WPC_EOB) return (0);

                /* if the end of block symbol */
                if((pbit+ibit == eobbits)&&((pref<<ibit)+absq == eobflag))
			break;

                /* convert to integer */
                if(absq & amptbl[ibit-1]) symbol = absq;
                else symbol = absq - amptbl[ibit] + 1;

                qx[i++] = symbol;
            }

        } while( i >= 0 /*TRUE*/);

        /* consistency check */
        if(i != numsmp) return (0);
        else return (1);

}
                   

static codeDICT *builddict(void)
/************************************************************************
build the dictionary for entropy coding
*************************************************************************
Return:	pointer to the dictionary
*************************************************************************
Note:	The codebooks here are just a variable form of variable-length
	integer code book.  Each symbol is represented by two parts, 
	a prefix of bit count indicating how many bits are used to encode
	the symbol, and the symbol itself. E.g., for codebook[1], the
	book looks like this:

	bit count	amplitude
	0		0
	1		-1, 1
	2		-3, -2, 2, 3
	3		end_of_block
	
	So, the number 3 is coded as 1011, instead of 0011 (the first 0 is 
	the sign bit), the number 1 on the other hand is coded as 011,
	instead of 0001. The end_of_block symbol is used to indicate the
	end of a block, since the coding here is bitwise, and the result
	might not align up with byte boundaries. To further reduce the 
	overhead, the bit counts are also variable-length coded, though 
	they happen to be the same in codebook[1]. The table pref2bit
	is used to convert the variable-length coded prefix into actually
	bit count.   
************************************************************************/
{
	codeDICT *codedict;
	codeBOOK *codebook;
	int *pref2bit;
	int dictsize, booksize;
	int i;

	/* allocate the code dictionary */
	codedict = (codeDICT *) malloc(sizeof(codeDICT)); 
	codedict->dictsize = dictsize = MAXNBIT - 1;

	/* allocate the code books */
	codedict->book = (codeBOOK *) malloc(dictsize*sizeof(codeBOOK));
	codebook = codedict->book;

	/* for each book */
	for(i=0; i<dictsize; i++){
	    codebook[i].booksize = booksize = i+3;
	    codebook[i].code = (entCODE *) malloc(booksize*sizeof(entCODE));
	}
	
	/* initialize each of the code books */
	/* prefix codes and # of bits */
	codebook[0].maxrun = 16;
	codebook[0].bitmaxrun = 4;
	codebook[0].code[0].value = 0;
	codebook[0].code[0].bits = 1;
	codebook[0].code[1].value = 2;
	codebook[0].code[1].bits = 2;
	codebook[0].code[2].value = 3;
	codebook[0].code[2].bits = 2;
	/* table convert prefix to # of bits */
	codebook[0].pref2bit = (int *) malloc(4*sizeof(int));
	pref2bit = codebook[0].pref2bit;
	pref2bit[0] = 0;
	pref2bit[1] = 0;
	pref2bit[2] = 1;
	pref2bit[3] = 0;


	codebook[1].maxrun = 16;
	codebook[1].bitmaxrun = 4;
	codebook[1].code[0].value = 0;
	codebook[1].code[0].bits = 2;
	codebook[1].code[1].value = 1;
	codebook[1].code[1].bits = 2;
	codebook[1].code[2].value = 2;
	codebook[1].code[2].bits = 2;
	codebook[1].code[3].value = 3;
	codebook[1].code[3].bits = 2;
	codebook[1].pref2bit = (int *) malloc(4*sizeof(int));
	pref2bit = codebook[1].pref2bit;
	for(i=0; i<3; i++) pref2bit[i] = i;
	pref2bit[3] = 0;

	codebook[2].maxrun = 16;
	codebook[2].bitmaxrun = 4;
	codebook[2].code[0].value = 0;
	codebook[2].code[0].bits = 2;
	codebook[2].code[1].value = 1;
	codebook[2].code[1].bits = 2;
	codebook[2].code[2].value = 2;
	codebook[2].code[2].bits = 2;
	codebook[2].code[3].value = 6;
	codebook[2].code[3].bits = 3;
	codebook[2].code[4].value = 7;
	codebook[2].code[4].bits = 3;
	codebook[2].pref2bit = (int *) malloc(8*sizeof(int));
	pref2bit = codebook[2].pref2bit;
	for(i=0; i<3; i++) pref2bit[i] = i;
	for(i=3; i<6; i++) pref2bit[i] = 0;
	pref2bit[6] = 3;
	pref2bit[7] = 0;

	codebook[3].maxrun = 8;
	codebook[3].bitmaxrun = 3;
	codebook[3].code[0].value = 0;
	codebook[3].code[0].bits = 2;
	codebook[3].code[1].value = 1;
	codebook[3].code[1].bits = 2;
	codebook[3].code[2].value = 2;
	codebook[3].code[2].bits = 2;
	codebook[3].code[3].value = 6;
	codebook[3].code[3].bits = 3;
	codebook[3].code[4].value = 7;
	codebook[3].code[4].bits = 3;
	codebook[3].code[5].value = 127;
	codebook[3].code[5].bits = 7;
	codebook[3].pref2bit = (int *) malloc(8*sizeof(int));
	pref2bit = codebook[3].pref2bit;
	for(i=0; i<3; i++) pref2bit[i] = i;
	for(i=3; i<6; i++) pref2bit[i] = 0;
	for(i=6; i<8; i++) pref2bit[i] = i-3;

	codebook[4].maxrun = 8;
	codebook[4].bitmaxrun = 3;
	codebook[4].code[0].value = 0;
	codebook[4].code[0].bits = 2;
	codebook[4].code[1].value = 1;
	codebook[4].code[1].bits = 2;
	codebook[4].code[2].value = 4;
	codebook[4].code[2].bits = 3;
	codebook[4].code[3].value = 5;
	codebook[4].code[3].bits = 3;
	codebook[4].code[4].value = 6;
	codebook[4].code[4].bits = 3;
	codebook[4].code[5].value = 7;
	codebook[4].code[5].bits = 3;
	codebook[4].code[6].value = 255;
	codebook[4].code[6].bits = 8;
	codebook[4].pref2bit = (int *) malloc(8*sizeof(int));
	pref2bit = codebook[4].pref2bit;
	for(i=0; i<2; i++) pref2bit[i] = i;
	for(i=2; i<4; i++) pref2bit[i] = 0;
	for(i=4; i<8; i++) pref2bit[i] = i-2;

	codebook[5].maxrun = 8;
	codebook[5].bitmaxrun = 3;
	codebook[5].code[0].value = 0;
	codebook[5].code[0].bits = 2;
	codebook[5].code[1].value = 2;
	codebook[5].code[1].bits = 3;
	codebook[5].code[2].value = 3;
	codebook[5].code[2].bits = 3;
	codebook[5].code[3].value = 4;
	codebook[5].code[3].bits = 3;
	codebook[5].code[4].value = 5;
	codebook[5].code[4].bits = 3;
	codebook[5].code[5].value = 6;
	codebook[5].code[5].bits = 3;
	codebook[5].code[6].value = 7;
	codebook[5].code[6].bits = 3;
	codebook[5].code[7].value = 511;
	codebook[5].code[7].bits = 9;
	codebook[5].pref2bit = (int *) malloc(8*sizeof(int));
	pref2bit = codebook[5].pref2bit;
	for(i=0; i<1; i++) pref2bit[i] = i;
	for(i=1; i<2; i++) pref2bit[i] = 0;
	for(i=2; i<8; i++) pref2bit[i] = i-1;

	codebook[6].maxrun = 8;
	codebook[6].bitmaxrun = 3;
	codebook[6].code[0].value = 0;
	codebook[6].code[0].bits = 3;
	codebook[6].code[1].value = 1;
	codebook[6].code[1].bits = 3;
	codebook[6].code[2].value = 2;
	codebook[6].code[2].bits = 3;
	codebook[6].code[3].value = 3;
	codebook[6].code[3].bits = 3;
	codebook[6].code[4].value = 4;
	codebook[6].code[4].bits = 3;
	codebook[6].code[5].value = 5;
	codebook[6].code[5].bits = 3;
	codebook[6].code[6].value = 6;
	codebook[6].code[6].bits = 3;
	codebook[6].code[7].value = 7;
	codebook[6].code[7].bits = 3;
	codebook[6].code[8].value = 1023;
	codebook[6].code[8].bits = 10;
	codebook[6].pref2bit = (int *) malloc(8*sizeof(int));
	pref2bit = codebook[6].pref2bit;
	for(i=0; i<8; i++) pref2bit[i] = i;


	codebook[7].maxrun = 8;
	codebook[7].bitmaxrun = 3;
	codebook[7].code[0].value = 0;
	codebook[7].code[0].bits = 3;
	codebook[7].code[1].value = 1;
	codebook[7].code[1].bits = 3;
	codebook[7].code[2].value = 2;
	codebook[7].code[2].bits = 3;
	codebook[7].code[3].value = 3;
	codebook[7].code[3].bits = 3;
	codebook[7].code[4].value = 4;
	codebook[7].code[4].bits = 3;
	codebook[7].code[5].value = 5;
	codebook[7].code[5].bits = 3;
	codebook[7].code[6].value = 6;
	codebook[7].code[6].bits = 3;
	codebook[7].code[7].value = 14;
	codebook[7].code[7].bits = 4;
	codebook[7].code[8].value = 15;
	codebook[7].code[8].bits = 4;
	codebook[7].code[9].value = 4095;
	codebook[7].code[9].bits = 12;
	codebook[7].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[7].pref2bit;
	for(i=0; i<7; i++) pref2bit[i] = i;
	for(i=7; i<14; i++) pref2bit[i] = 0;
	for(i=14; i<16; i++) pref2bit[i] = i-7;


	codebook[8].maxrun = 4;
	codebook[8].bitmaxrun = 2;
	codebook[8].code[0].value = 0;
	codebook[8].code[0].bits = 3;
	codebook[8].code[1].value = 1;
	codebook[8].code[1].bits = 3;
	codebook[8].code[2].value = 2;
	codebook[8].code[2].bits = 3;
	codebook[8].code[3].value = 3;
	codebook[8].code[3].bits = 3;
	codebook[8].code[4].value = 4;
	codebook[8].code[4].bits = 3;
	codebook[8].code[5].value = 5;
	codebook[8].code[5].bits = 3;
	codebook[8].code[6].value = 12;
	codebook[8].code[6].bits = 4;
	codebook[8].code[7].value = 13;
	codebook[8].code[7].bits = 4;
	codebook[8].code[8].value = 14;
	codebook[8].code[8].bits = 4;
	codebook[8].code[9].value = 15;
	codebook[8].code[9].bits = 4;
	codebook[8].code[10].value = 8191;
	codebook[8].code[10].bits = 13;
	codebook[8].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[8].pref2bit;
	for(i=0; i<6; i++) pref2bit[i] = i;
	for(i=6; i<12; i++) pref2bit[i] = 0;
	for(i=12; i<16; i++) pref2bit[i] = i-6;

	codebook[9].maxrun = 4;
	codebook[9].bitmaxrun = 2;
	codebook[9].code[0].value = 0;
	codebook[9].code[0].bits = 3;
	codebook[9].code[1].value = 1;
	codebook[9].code[1].bits = 3;
	codebook[9].code[2].value = 2;
	codebook[9].code[2].bits = 3;
	codebook[9].code[3].value = 3;
	codebook[9].code[3].bits = 3;
	codebook[9].code[4].value = 4;
	codebook[9].code[4].bits = 3;
	codebook[9].code[5].value = 10;
	codebook[9].code[5].bits = 4;
	codebook[9].code[6].value = 11;
	codebook[9].code[6].bits = 4;
	codebook[9].code[7].value = 12;
	codebook[9].code[7].bits = 4;
	codebook[9].code[8].value = 13;
	codebook[9].code[8].bits = 4;
	codebook[9].code[9].value = 14;
	codebook[9].code[9].bits = 4;
	codebook[9].code[10].value = 15;
	codebook[9].code[10].bits = 4;
	codebook[9].code[11].value = 16383;
	codebook[9].code[11].bits = 14;
	codebook[9].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[9].pref2bit;
	for(i=0; i<5; i++) pref2bit[i] = i;
	for(i=5; i<10; i++) pref2bit[i] = 0;
	for(i=10; i<16; i++) pref2bit[i] = i-5;

	codebook[10].maxrun = 4;
	codebook[10].bitmaxrun = 2;
	codebook[10].code[0].value = 0;
	codebook[10].code[0].bits = 3;
	codebook[10].code[1].value = 1;
	codebook[10].code[1].bits = 3;
	codebook[10].code[2].value = 2;
	codebook[10].code[2].bits = 3;
	codebook[10].code[3].value = 3;
	codebook[10].code[3].bits = 3;
	codebook[10].code[4].value = 8;
	codebook[10].code[4].bits = 4;
	codebook[10].code[5].value = 9;
	codebook[10].code[5].bits = 4;
	codebook[10].code[6].value = 10;
	codebook[10].code[6].bits = 4;
	codebook[10].code[7].value = 11;
	codebook[10].code[7].bits = 4;
	codebook[10].code[8].value = 12;
	codebook[10].code[8].bits = 4;
	codebook[10].code[9].value = 13;
	codebook[10].code[9].bits = 4;
	codebook[10].code[10].value = 14;
	codebook[10].code[10].bits = 4;
	codebook[10].code[11].value = 15;
	codebook[10].code[11].bits = 4;
	codebook[10].code[12].value = 32767;
	codebook[10].code[12].bits = 15;
	codebook[10].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[10].pref2bit;
	for(i=0; i<4; i++) pref2bit[i] = i;
	for(i=4; i<8; i++) pref2bit[i] = 0;
	for(i=8; i<16; i++) pref2bit[i] = i-4;

	codebook[11].maxrun = 4;
	codebook[11].bitmaxrun = 2;
	codebook[11].code[0].value = 0;
	codebook[11].code[0].bits = 3;
	codebook[11].code[1].value = 1;
	codebook[11].code[1].bits = 3;
	codebook[11].code[2].value = 2;
	codebook[11].code[2].bits = 3;
	codebook[11].code[3].value = 6;
	codebook[11].code[3].bits = 4;
	codebook[11].code[4].value = 7;
	codebook[11].code[4].bits = 4;
	codebook[11].code[5].value = 8;
	codebook[11].code[5].bits = 4;
	codebook[11].code[6].value = 9;
	codebook[11].code[6].bits = 4;
	codebook[11].code[7].value = 10;
	codebook[11].code[7].bits = 4;
	codebook[11].code[8].value = 11;
	codebook[11].code[8].bits = 4;
	codebook[11].code[9].value = 12;
	codebook[11].code[9].bits = 4;
	codebook[11].code[10].value = 13;
	codebook[11].code[10].bits = 4;
	codebook[11].code[11].value = 14;
	codebook[11].code[11].bits = 4;
	codebook[11].code[12].value = 15;
	codebook[11].code[12].bits = 4;
	codebook[11].code[13].value = 65535;
	codebook[11].code[13].bits = 16;
	codebook[11].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[11].pref2bit;
	for(i=0; i<3; i++) pref2bit[i] = i;
	for(i=3; i<6; i++) pref2bit[i] = 0;
	for(i=6; i<16; i++) pref2bit[i] = i-3;


	codebook[12].maxrun = 4;
	codebook[12].bitmaxrun = 2;
	codebook[12].code[0].value = 0;
	codebook[12].code[0].bits = 3;
	codebook[12].code[1].value = 1;
	codebook[12].code[1].bits = 3;
	codebook[12].code[2].value = 4;
	codebook[12].code[2].bits = 4;
	codebook[12].code[3].value = 5;
	codebook[12].code[3].bits = 4;
	codebook[12].code[4].value = 6;
	codebook[12].code[4].bits = 4;
	codebook[12].code[5].value = 7;
	codebook[12].code[5].bits = 4;
	codebook[12].code[6].value = 8;
	codebook[12].code[6].bits = 4;
	codebook[12].code[7].value = 9;
	codebook[12].code[7].bits = 4;
	codebook[12].code[8].value = 10;
	codebook[12].code[8].bits = 4;
	codebook[12].code[9].value = 11;
	codebook[12].code[9].bits = 4;
	codebook[12].code[10].value = 12;
	codebook[12].code[10].bits = 4;
	codebook[12].code[11].value = 13;
	codebook[12].code[11].bits = 4;
	codebook[12].code[12].value = 14;
	codebook[12].code[12].bits = 4;
	codebook[12].code[13].value = 15;
	codebook[12].code[13].bits = 4;
	codebook[12].code[14].value = 131071;
	codebook[12].code[14].bits = 17;
	codebook[12].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[12].pref2bit;
	for(i=0; i<2; i++) pref2bit[i] = i;
	for(i=2; i<4; i++) pref2bit[i] = 0;
	for(i=4; i<16; i++) pref2bit[i] = i-2;


	codebook[13].maxrun = 4;
	codebook[13].bitmaxrun = 2;
	codebook[13].code[0].value = 0;
	codebook[13].code[0].bits = 3;
	codebook[13].code[1].value = 2;
	codebook[13].code[1].bits = 4;
	codebook[13].code[2].value = 3;
	codebook[13].code[2].bits = 4;
	codebook[13].code[3].value = 4;
	codebook[13].code[3].bits = 4;
	codebook[13].code[4].value = 5;
	codebook[13].code[4].bits = 4;
	codebook[13].code[5].value = 6;
	codebook[13].code[5].bits = 4;
	codebook[13].code[6].value = 7;
	codebook[13].code[6].bits = 4;
	codebook[13].code[7].value = 8;
	codebook[13].code[7].bits = 4;
	codebook[13].code[8].value = 9;
	codebook[13].code[8].bits = 4;
	codebook[13].code[9].value = 10;
	codebook[13].code[9].bits = 4;
	codebook[13].code[10].value = 11;
	codebook[13].code[10].bits = 4;
	codebook[13].code[11].value = 12;
	codebook[13].code[11].bits = 4;
	codebook[13].code[12].value = 13;
	codebook[13].code[12].bits = 4;
	codebook[13].code[13].value = 14;
	codebook[13].code[13].bits = 4;
	codebook[13].code[14].value = 15;
	codebook[13].code[14].bits = 4;
	codebook[13].code[15].value = 262143;
	codebook[13].code[15].bits = 18;
	codebook[13].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[13].pref2bit;
	for(i=0; i<1; i++) pref2bit[i] = i;
	for(i=1; i<2; i++) pref2bit[i] = 0;
	for(i=2; i<16; i++) pref2bit[i] = i-1;


	codebook[14].maxrun = 4;
	codebook[14].bitmaxrun = 2;
	codebook[14].code[0].value = 0;
	codebook[14].code[0].bits = 4;
	codebook[14].code[1].value = 1;
	codebook[14].code[1].bits = 4;
	codebook[14].code[2].value = 2;
	codebook[14].code[2].bits = 4;
	codebook[14].code[3].value = 3;
	codebook[14].code[3].bits = 4;
	codebook[14].code[4].value = 4;
	codebook[14].code[4].bits = 4;
	codebook[14].code[5].value = 5;
	codebook[14].code[5].bits = 4;
	codebook[14].code[6].value = 6;
	codebook[14].code[6].bits = 4;
	codebook[14].code[7].value = 7;
	codebook[14].code[7].bits = 4;
	codebook[14].code[8].value = 8;
	codebook[14].code[8].bits = 4;
	codebook[14].code[9].value = 9;
	codebook[14].code[9].bits = 4;
	codebook[14].code[10].value = 10;
	codebook[14].code[10].bits = 4;
	codebook[14].code[11].value = 11;
	codebook[14].code[11].bits = 4;
	codebook[14].code[12].value = 12;
	codebook[14].code[12].bits = 4;
	codebook[14].code[13].value = 13;
	codebook[14].code[13].bits = 4;
	codebook[14].code[14].value = 14;
	codebook[14].code[14].bits = 4;
	codebook[14].code[15].value = 15;
	codebook[14].code[15].bits = 4;
	codebook[14].code[16].value = 524287;
	codebook[14].code[16].bits = 19;
	codebook[14].pref2bit = (int *) malloc(16*sizeof(int));
	pref2bit = codebook[14].pref2bit;
	for(i=0; i<16; i++) pref2bit[i] = i;

	return (codedict);
}
