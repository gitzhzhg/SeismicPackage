/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"
#include "elastic.h"

int rt_SHa_real(float c1212i, float c2323i, float c1223i, float c1212t, 
	float c2323t, float c1223t, float pxi, float pzi, float pxt, 
	float pzt, float pxr, float pzr, int rort, float *coeff, float gz, 	
	float gx)
/*****************************************************************************
Reflection/Transmission coeff for SH propagation in anisotropic media
******************************************************************************
Input:
cijkli		stiffness (incident side)
cijklt		stiffness (transmitted side)
pxi,pzi		incidence slowness
pxt,pzt		transmitted slowness
pxr,pzr		reflected slowness
gz,gx		sin/cos of interface
rort		=0 transmission
		=1 reflection
		=2 free surface
******************************************************************************
Output:
-1		error in routine
1		coefficient found
*coeff		adress of real refl/transm coeff

******************************************************************************
Author: Andreas Rueger, Colorado School of Mines, 02/01/94
******************************************************************************/
{

	float rcoeff,d;
	
	rotvector (&pxt,&pzt,gz,gx);
	rotvector (&pxi,&pzi,gz,gx);
	rotvector (&pxr,&pzr,gz,gx);

	rottensh (&c1212i,&c2323i,&c1223i,gz,gx);
	rottensh (&c1212t,&c2323t,&c1223t,gz,gx);


	/* free surface reflection */
	if(rort == 2){
		d = c1223i*pxr + c2323i* pzr;
		if (ABS(d) < FLT_EPSILON)
			return -1;
		*coeff = -( c1223i*pxi + c2323i* pzi)/d ;
		return 1;
	
	
	} else {
		d = c1223i*pxr + c2323i*pzr - c1223t*pxt - c2323t*pzt;
		if (ABS(d) < FLT_EPSILON)
			return -1;

		rcoeff = (c1223t*pxt + c2323t*pzt - c1223i*pxi - c2323i*pzi)/d;

		/* reflection */
		if(rort == 1)
			*coeff = rcoeff;

		/* transmission */
		else if(rort ==0 )
			*coeff = rcoeff + 1;
		else 
			err(" wrong rort in int rt_SHa_real");
	}

	return 1;
}
