/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#ifndef _BITBUFF_H
#define _BITBUFF_H

#include "wpcbuffer.h"

/* structure for bitwise buffering */
typedef struct wpcBitBuffStruct{
	wpcBUFF *buff;
	unsigned char mask;
	unsigned char rack;
} wpcBITBUFF;


/* define the buffering operation as macros to reduce the runtime
   overhead of function calls for every bit operation */

/* bitInitbuff(bitbuff, buff)
	initialize a bit buffer _bitbuff_ from buffer _buff_
   bitFreebuff(bitbuff)
	frees the bit buffer _bitbuff_
   bitWindbuff(bitbuff, flag)
	windup the remaining bits in a byte to the bit buffer _bitbuff_
	_flag_ = 0 if successful, _WPC_EOB_ otherwise
   bitOutputbit(bitbuff, bit, flag)
	write one bit _bit_ to the bit buffer _bitbuff_
	_flag_ = 0 if successful, _WPC_EOB_ otherwise
   bitOutputbits(bitbuff, code, count, flag)
	write _count_ of  bits in code _code_ to the bit buffer _bitbuff_
	_flag_ = 0 if successful, _WPC_EOB_ otherwise
   bitInputbit(bitbuff, flag)
	read one bit from the bit buffer
	_bitbuff_, _flag_ = 0 or 1 if successful, _WPC_EOB_ otherwise
   bitInputbits(bitbuff, bitcount, number, flag)
	read _bitcount_ bits from the bit buffer _bitbuff_, and store
	them in _number_,
	_flag_ = 0 if successful, _WPC_EOB_ otherwise
*/

#define bitInitbuff(bitbuff, buff1){\
	(bitbuff) = (wpcBITBUFF *) malloc(sizeof(wpcBITBUFF));\
	(bitbuff)->buff = (buff1);\
	(bitbuff)->mask = 0x80;\
	(bitbuff)->rack = 0x0;\
}

#define bitFreebuff(bitbuff){\
	free((void *) (bitbuff));\
}

#define bitWindbuff(bitbuff, flag){\
	(flag) = 0;\
	if((bitbuff)->mask != 0x80){\
	    if(buffPutc((bitbuff)->buff, (bitbuff)->rack) == WPC_EOB)\
		(flag) = WPC_EOB;\
	    else (flag) = 1;}\
}

#define bitOutputbit(bitbuff, bit, flag){\
	(flag) = 0;\
	if((bit)) (bitbuff)->rack |= (bitbuff)->mask;\
	(bitbuff)->mask >>= 1;\
	if((bitbuff)->mask == 0){\
	    if(buffPutc((bitbuff)->buff, (bitbuff)->rack) == WPC_EOB)\
		(flag) = WPC_EOB;\
	    (bitbuff)->rack = 0x0;\
	    (bitbuff)->mask = 0x80;\
	}\
}


#define bitOutputbits(bitbuff, code, count, flag){\
	unsigned int lmask;\
	(flag) = 0;\
	lmask = 1 << ((count) - 1);\
	while(lmask){\
	    if(lmask & (code)) (bitbuff)->rack |= (bitbuff)->mask;\
	    (bitbuff)->mask >>= 1;\
	    if((bitbuff)->mask == 0){\
	    	if(buffPutc((bitbuff)->buff, (bitbuff)->rack) == WPC_EOB){\
		    (flag) = WPC_EOB; break;\
		}\
	    	(bitbuff)->rack = 0x0;\
	    	(bitbuff)->mask = 0x80;\
	    }\
	    lmask >>= 1;\
	}\
}

#define bitInputbit(bitbuff, flag){\
	int value;\
	(flag) = 0;\
	if((bitbuff)->mask == 0x80)\
	    if(buffGetc((bitbuff)->buff, (bitbuff)->rack) == WPC_EOB)\
		(flag) = WPC_EOB;\
	if((flag) != WPC_EOB){\
	    value = (bitbuff)->rack & (bitbuff)->mask;\
	    (bitbuff)->mask >>= 1;\
	    if((bitbuff)->mask == 0) (bitbuff)->mask = 0x80;\
	    (flag) = value? 1 : 0;\
	}\
}

#define bitInputbits(bitbuff, bitcount, number, flag){\
	unsigned int lmask;\
	(flag) = 0;\
	lmask = 1 << ((bitcount) - 1);\
	(number) = 0;\
	while(lmask){\
	    if((bitbuff)->mask == 0x80)\
		if(buffGetc((bitbuff)->buff, (bitbuff)->rack) == WPC_EOB){\
		    (flag) = WPC_EOB; break;\
		}\
	    if((bitbuff)->rack & (bitbuff)->mask)\
		(number) |= lmask;\
	    lmask >>= 1;\
	    (bitbuff)->mask >>= 1;\
	    if((bitbuff)->mask == 0) (bitbuff)->mask = 0x80;\
	}\
}

#endif
