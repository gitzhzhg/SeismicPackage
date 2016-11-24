/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "elastic.h"

int rt_iso_cmplx(float vp1, float vp2, float vs1, float vs2, float rho1,
	float rho2, float pl, int modei, int modet, int rort, float
	*coeff,float *phase)
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
*coeff		address of refl/transm magnitude
*phase 		address of refl/transm phase
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 02/01/94
******************************************************************************/
{

	float pl2,vs12,vs22,a,b,c,d;
	float sj1,sj2,si1,si2;
	float tempr1,tempr2;
	complex ci1,ci2={0,0},cj1,cj2,invD;
	complex covi1,covi2,covj1,covj2;
	complex D,E,F,G,H,ccoeff;
	complex tempc1,tempc2,del;

	ccoeff = cmplx(0,0);

	si1  = pl*vp1;
	if(si1>1)
		ci1=cmplx(0,-sqrt(si1*si1-1));
	else if(si1<=1)
		ci1=cmplx(sqrt(1-si1*si1),0);

	si2  = pl*vp2;
	if(si2>1)
		ci2=cmplx(0,-sqrt(si2*si2-1));
	else if(si2<=1)
		ci2=cmplx(sqrt(1-si2*si2),0);

	sj1  = pl*vs1;
	if(sj1>1)
		cj1=cmplx(0,-sqrt(sj1*sj1-1));
	else if(sj1<=1)
		cj1=cmplx(sqrt(1-sj1*sj1),0);

	sj2  = pl*vs2;
	if(sj2>1)
		cj2=cmplx(0,-sqrt(sj2*sj2-1));
	else if(sj2<=1)
		cj2=cmplx(sqrt(1-sj2*sj2),0);



	/* SH reflection/transmission coeff */
	if( modei == 2 && modet == 2 ){
		
		tempr1 = rho1*vs1;
		tempr2 = rho2*vs2;

		tempc1 = crmul(cj1,tempr1);
		tempc2 = crmul(cj2,tempr2);

		del = cadd(tempc1,tempc2);
		del =  cinv(del);

                if(rort == 1)
			ccoeff =  cmul(del,csub(tempc1,tempc2));

		else if(rort == 0)
			ccoeff =  cmul(del,crmul(tempc1,2));

		else 
			err("ERROR in reftransiso\n");

	} else {

		pl2  = pl*pl;
		
		vs12 = vs1*vs1;
		vs22 = vs2*vs2;

		covi1= crmul(ci1,1/vp1);
		covi2= crmul(ci2,1/vp2);
		covj1= crmul(cj1,1/vs1);
		covj2= crmul(cj2,1/vs2);

		tempr1 = rho2*(1- 2*vs22*pl2);

		a    = tempr1 - rho1*(1 - 2*vs12*pl2);
		b    = tempr1 + 2*rho1*vs12*pl2;
		c    = rho1*(1- 2*vs12*pl2) + 2*rho2*vs22*pl2;
		d    = 2*(rho2*vs22 - rho1*vs12);

		E    = cadd(crmul(covi1,b),crmul(covi2,c));
		F    = cadd(crmul(covj1,b),crmul(covj2,c));
		tempc1=crmul(cmul(covi1,covj2),d);
		G    = cmplx(a-tempc1.r,tempc1.i);
		tempc1=crmul(cmul(covi2,covj1),d);
		H    = cmplx(a-tempc1.r,tempc1.i);

		D    = cadd(cmul(E,F),crmul(cmul(G,H),pl2));

		if(rcabs(D)<FLT_EPSILON)
			return -1;
		else 
			invD = cinv(D);
		

		/* PP-reflection */
		if(modei==0 && modet==0 && rort==1){
		    tempc1 = cmul(csub(crmul(covi1,b),crmul(covi2,c)),F);
		    tempc2 = crmul(H,pl2);
		    tempc2 = cmul(tempc2,cadd(cmplx(a,0),
				crmul(cmul(covi1,covj2),d)));
		    ccoeff = csub(tempc1,tempc2);
		    tempc1 = invD;
		    ccoeff = cmul(ccoeff,tempc1);

		/* PP-transmission */
		} else if(modei==0 && modet==0 && rort==0){
		    tempc1 = cmul(F,ci1);
		    tempc1 = crmul(tempc1,2*rho1/vp2);
		    tempc2 = invD;
		    ccoeff = cmul(tempc1,tempc2);

		/* PS-reflection */
		}else if(modei==0 && modet==1 && rort==1){
 		    tempr1=a*b;
		    tempr2=c*d;
		    tempc1=crmul(cmul(covi2,covj2),tempr2);
		    tempc1=cadd(cmplx(tempr1,0),tempc1);
		    tempc2=crmul(covi1,-2*pl*vp1/vs1);
		    tempc1=cmul(tempc1,tempc2);
		    tempc2=invD;
		    ccoeff= cmul(tempc1,tempc2);

		/* PS-transmission */
		} else if(modei==0 && modet==1 && rort==0){
		    tempr1=2*rho1*pl/vs2;
		    tempc1=cmul(ci1,H);
		    tempc2=cinv(D);
		    tempc1=crmul(tempc1,tempr1);
		    ccoeff= cmul(tempc1,tempc2);

		/* SP-reflection */
		}else if(modei==1 && modet==0 && rort==1){
 		    tempr1=a*b;
		    tempr2=c*d;
		    tempc1=crmul(cmul(covi2,covj2),tempr2);
		    tempc1=cadd(cmplx(tempr1,0),tempc1);
		    tempc2=crmul(covj1,-2*pl*vs1/vp1);
		    tempc1=cmul(tempc1,tempc2);
		    tempc2=cinv(D);
		    ccoeff= cmul(tempc1,tempc2);

		/* SP-transmission */
		} else if(modei==1 && modet==0 && rort==0){
		    tempr1=-2*rho1*pl/vp2;
		    tempc1=cmul(cj1,G);
		    tempc2=cinv(D);
		    tempc1=crmul(tempc1,tempr1);
		    ccoeff= cmul(tempc1,tempc2);

		/* SS-reflection */
		} else if(modei==1 && modet==1 && rort==1){
		    tempc1 = cmul(csub(crmul(covj2,c),crmul(covj1,b)),E);
		    tempc2 = crmul(G,pl2);
		    tempc2 = cmul(tempc2,cadd(cmplx(a,0),
				crmul(cmul(covi2,covj1),d)));
		    ccoeff = cadd(tempc1,tempc2);
		    tempc1 = cinv(D);
		    ccoeff = cmul(ccoeff,tempc1);

		/* SS-transmission */
		} else if(modei==1 && modet==1 && rort==0){
		    tempc1 = cmul(E,cj1);
		    tempc1 = crmul(tempc1,2*rho1);
		    tempc2 = crmul(D,vs2);
		    tempc2 = cinv(tempc2);
		    ccoeff = cmul(tempc1,tempc2);

		} else 
			return -1;
	    }

	    if(ABS(ccoeff.i) < FLT_EPSILON){
			*coeff = ccoeff.r;
			*phase = 0;
	    } else if(ABS(ccoeff.r) < FLT_EPSILON &&
			  ABS(ccoeff.i) < FLT_EPSILON){
			*coeff = 0;
			*phase = 0;
	    } else if(ABS(ccoeff.r) < FLT_EPSILON &&
			  ccoeff.i > 0){
			*coeff = rcabs(ccoeff);
			*phase = PI/2;
	    } else if(ABS(ccoeff.r) < FLT_EPSILON &&
			  ccoeff.i < 0){
			*coeff = rcabs(ccoeff);
			*phase = -PI/2;
	    } else if(ccoeff.r > 0 && ccoeff.i > 0){
			*coeff = rcabs(ccoeff);
			*phase = atan(ccoeff.i/ccoeff.r);
	    } else if(ccoeff.r < 0 ){
			*coeff = rcabs(ccoeff);
			*phase = PI+atan(ccoeff.i/ccoeff.r);
	    } else if(ccoeff.r > 0 && ccoeff.i < 0){
			*coeff = rcabs(ccoeff);
			*phase = 2*PI+atan(ccoeff.i/ccoeff.r);
	    }


	
	return 1;

}
