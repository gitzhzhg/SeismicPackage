/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*********************************************************************
wrapArray - wrap an array
******************************************************************

******************************************************************
Function prototype:
void wrapArray(void *base,size_t nmemb,size_t size,int f)
*******************************************************************
Author: Potash Corporation: Balasz Nemeth, given to CWP 2008
*******************************************************************/

/**************** end self doc ********************************/

#include "cwp.h"

void wrapArray(void *base, size_t nmemb, size_t size, int f)
/******************************************************************
wrapArray - wrap an array
*******************************************************************
Author: Potash Corporation: Balasz Nemeth, given to CWP 2008
******************************************************************/


{
	char *base2;
	char *base1;
	int shift;
	
	shift = f%(int)nmemb;
	
	if(shift==0) return;
	
	base2 = alloc1(nmemb,size);
	base1 = (char *) base;
	
	if(shift >0) shift=-nmemb+shift; 
	
	memcpy((void *) base2, (const void *) (base1-shift*size),(nmemb+shift)*size);
	memcpy((void *) (base2+(nmemb+shift)*size), (const void *) base1,-shift*size);
	
	memcpy((void *) base1, (const void *) base2,nmemb*size);
	
	free1(base2);


}
