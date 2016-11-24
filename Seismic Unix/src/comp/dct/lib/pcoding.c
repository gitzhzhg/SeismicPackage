/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/******************************************************************************
PENCODING - Routines to en/decode the quantized integers for lossless 
		en/decoding

pEncode	        encode the quantized integers	
pDecode	        decode the quantized integers

*******************************************************************************
Function Prototypes:
memBUFF* pEncode(int *qx, int n);
int pDecode(memBUFF *buff, int *qx, int n)

*******************************************************************************
pEncode:
Input:
qx              array[] of input integers
n               length of input 
Returned:
encoded output

pDecode:
Input:
buff            encoded input
qx              array[] of output integers
n               length of output 
Returned:
consistency flag

*******************************************************************************
Author:         Tong Chen, 05/31/95, modified from en/decoding
******************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"


#define MAXLOAD 0x7fff
#define PC_OVERFLOW 127
#define PC_UNDERFLOW -127
#define QUANTMAX 126
#define BYTEMAX 256


memBUFF* pEncode(int *qx, int n)
/******************************************************************************
qx              array[] of input integers
n               length of input 

Returned:
                encoded output
******************************************************************************/
{
   int i;
   size_t nsize;
   unsigned int s;
   unsigned char c1,c2;
   memBUFF *buff;
   
   /* allocate spaces */
   nsize = 2*n*sizeof(char);
   buff = buffAlloc1((int) nsize);
   
   /* encoding */
   for(i=0;i<n;i++){
	 if(qx[i] > 0){

	       /* if overflow */
	       if(qx[i] > QUANTMAX){

		     s = qx[i];

		     /* is there any signal on earth that can make this 
				happen? */
		     if(s > MAXLOAD) s = MAXLOAD; 

		     /* shift the level to avoid silence code */
		     s = (s << 1) + 1;	

		     /* output a prefix code */
		     buffPutc(buff, PC_OVERFLOW);

		     /* output a short, higher byte first */
		     c1 = s/BYTEMAX;
		     buffPutc(buff, c1);
		     c2 = s - c1*BYTEMAX;
		     buffPutc(buff, c2);
		  }	
	       else
	       {
		  c1 = (unsigned char) qx[i];
		  buffPutc(buff, c1);
	       }
	    }
	 /* negative */
	 else{

	       /* if underflow */
	       if(qx[i] < -QUANTMAX){
		     s = - qx[i];

		     /* is there any signal on earth that can make this 
				happen? */
		     if(s > MAXLOAD) s = MAXLOAD; 

		     /* shift the level to avoid silence code */
		     s = (s << 1) + 1;	

		     /* output a prefix code */
		     buffPutc(buff, PC_UNDERFLOW);

		     /* output a short, higher byte first */
		     c1 = s/BYTEMAX;
		     buffPutc(buff, c1);
		     c2 = s - c1*BYTEMAX;
		     buffPutc(buff, c2);
		  }
	       else{
		     c1 = (unsigned char) qx[i];
		     buffPutc(buff, c1);
		  }
	    }
      }

   /* adjust the buffer size */
   buffRealloc1(buff, buff->pos);

   /* return the buffer pointer */
   return(buff);
}



int pDecode(memBUFF *buff, int *qx, int n)
/******************************************************************************
buff            encoded input
qx              array[] of output integers
n               length of output 

Returned:
                consistency flag
******************************************************************************/
{
   int i;
   signed char sc;
   unsigned int s;
   unsigned char c1=0, c2=0;
   
   i = 0;
   while(buffGetc(buff, sc) != MEM_EOB){
			
	 /* if overflow */
	 if(sc == PC_OVERFLOW){

	       /* get the following two bytes */
	       buffGetc(buff, c1);
	       buffGetc(buff, c2);
	       
	       s = c1*BYTEMAX + c2;
				
	       /* convert to integer */
	       qx[i++] =  s >> 1;
	    }

	 /* else if underflow */
	 else if(sc == PC_UNDERFLOW){

	       /* get the following two bytes */
	       buffGetc(buff, c1);
	       buffGetc(buff, c2);

	       s = c1*BYTEMAX + c2;
                                
	       /* convert to integer */
               qx[i++] =  - (s >> 1);
	    }   

	 /* else, no flow */
	 else qx[i++] =  sc;
      }

   /* consistency check */
   if(i != n) return (MEM_EOB);
   else return (0);
}
