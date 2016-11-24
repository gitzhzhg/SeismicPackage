/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "stdio.h"
#include "math.h"
#define Size 64

/* This program makes test data for mrafxzwt */

void
main()
{
	float f;
	int i,j;

	for (i=0;i<Size;i++){
		for (j=0;j<Size;j++){
			f=sin(-0.0006*((double)(i-Size/2)*(i-Size/2)*i
				+(double)(j-Size/2)*(j-Size/2)*j));
		
			fwrite(&f,sizeof(float),1,stdout);
		}
	}
}
