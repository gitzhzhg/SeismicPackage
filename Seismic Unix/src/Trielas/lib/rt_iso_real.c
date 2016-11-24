/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"

int rt_iso_real(float vp1, float vp2, float vs1, float vs2, float rho1,
	float rho2, float pl, int modei, int modet, int rort, float
	*coeff)
/*****************************************************************************
Reflection/Transmission coeff. (see Aki % Richards, pages 145ff)
******************************************************************************
Input:
vp1,vp2		p-wave velocities
vs1,vs2		s-wave velocities
rho1,rho2	densities
modei,modet	incidence and scattered mode
rort		=0 transmission
		=1 reflection
		=2  free surface reflection
******************************************************************************
Output:
-1		error in routine
1		coefficient found
0		coefficient complex
*coeff		address of real refl/transm coeff

******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 02/01/94
******************************************************************************/
{

	float a,b,c,d,del,temp;
	float sj1,sj2,cj1,cj2;
	float si1,si2,ci1,ci2;
	float covi1,covi2,covj1,covj2;
	float D,E,F,G,H;
	float pl2,vs12,vs22;

	si1  = pl*vp1;
	si2  = pl*vp2;
	sj1  = pl*vs1;
	sj2  = pl*vs2;

	/* check for postcritical case */
	if(si1 > 1 || si2 > 1 || sj1 > 1 ||sj2 > 1) 
			return 0;


	/* SH reflection/transmission coeff */
	else if( modei == 2 && modet == 2 ){
		
		cj1 = sqrt(1-sj1*sj1);
		cj2 = sqrt(1-sj2*sj2);

		del = rho1*vs1*cj1+rho2*vs2*cj2;

		/* Pol in ref/trans coeff */
		if(ABS(del) < FLT_EPSILON)
			return -1;
		 
                if(rort == 1)
			*coeff =  1/del *(rho1*vs1*cj1-rho2*vs2*cj2);

		else if(rort == 0)
			*coeff =  1/del *2*rho1*vs1*cj1;

		else if(rort == 2){
			/* needs to be checked */
		        *coeff = 1;
		} else 
			err("ERROR in reftransiso\n");

	} else {
		cj1  = sqrt(1-sj1*sj1);
		cj2  = sqrt(1-sj2*sj2);
		ci1  = sqrt(1-si1*si1);
		ci2  = sqrt(1-si2*si2);

		pl2  = pl*pl;
		
		vs12 = vs1*vs1;
		vs22 = vs2*vs2;

		covi1= ci1/vp1;
		covi2= ci2/vp2;
		covj1= cj1/vs1;
		covj2= cj2/vs2;

		temp = rho2*(1- 2*vs22*pl2);

		a    = temp - rho1*(1 - 2*vs12*pl2);
		b    = temp + 2*rho1*vs12*pl2;
		c    = rho1*(1- 2*vs12*pl2) + 2*rho2*vs22*pl2;
		d    = 2*(rho2*vs22 - rho1*vs12);

		E    = b*covi1 + c*covi2;
		F    = b*covj1 + c*covj2;
		G    = a-d*covi1*covj2;
		H    = a-d*covi2*covj1;
		D    = E*F + G*H*pl2;

		/* Pol in ref/trans coeff */
		if(ABS(D) < FLT_EPSILON)
			return -1;

		/* PP-reflection */
		else if(modei==0 && modet==0 && rort==1)
		    *coeff=((b*covi1 - c*covi2)*F-(a + d*covi1*covj2)*H*pl2)/D;

		/* PP-transmission */
		else if(modei==0 && modet==0 && rort==0)
		    *coeff=2*rho1*ci1*F / (vp2*D);

		/* PS-reflection */
		else if(modei==0 && modet==1 && rort==1)
		    *coeff= -2*covi1*(a*b+c*d*covi2*covj2)*pl*vp1/(vs1*D);

		/* PS-transmission */
		else if(modei==0 && modet==1 && rort==0)
		    *coeff= 2*rho1*ci1*H*pl / (vs2*D);

		/* SP-reflection */
		else if(modei==1 && modet==0 && rort==1)
		    *coeff=-2*covj1*(a*b+c*d*covi2*covj2)*pl*vs1/(vp1*D);

		/* SP-transmission */
		else if(modei==1 && modet==0 && rort==0)
		    *coeff= -2*rho1*cj1*G*pl / (vp2*D);

		/* SS-reflection */
		else if(modei==1 && modet==1 && rort==1)
		    *coeff=-((b*covj1-c*covj2)*E-(a+d*covi2*covj1)*G*pl2)/D;

		/* SS-transmission */
		else if(modei==1 && modet==1 && rort==0)
		    *coeff=2*rho1*cj1*E / (vs2*D);

		/* free surface coefficients */
		else if(rort==2){
			a =1/vs12 - 2*pl2;
			b = 4*pl2*ci1*cj1;
			temp = a*a*vs1*vp1 + b;
			
			/* Pol in the coefficient */
			if(ABS(temp)<FLT_EPSILON)
				return -1;

			/* PP-free surface */
			else if(modei==0 && modet==0)
				*coeff=(-a*a*vs1*vp1 + b)/temp;
	
			/* PS-free surface */
			else if(modei==0 && modet==1)
				*coeff=-4*pl*vp1*ci1*a /temp;

			/* SS-free surface */
			else if(modei==1 && modet==1)
				*coeff=-(a*a*vs1*vp1-b)/temp;

			/* SP-free surface */
			else if(modei==1 && modet==0)
				*coeff=4*vs1*cj1*pl*a/temp;

			else
				return -1;
		} else
			return -1;

	}
	
	return 1;
}

