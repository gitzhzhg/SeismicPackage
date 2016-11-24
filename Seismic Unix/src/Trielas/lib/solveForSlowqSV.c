/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"


int solveForSlowqSV (float s, float c, float pl, float a1111, float
	 a3333, float a1133,float a1313, float a1113, float a3313, float 
	*pxnew, float *pznew, float rt)
/*****************************************************************************
Continue slowness across interface (PAcrossInterface)
Anisotropic Case, qSV-wave
******************************************************************************/
{
	double oc,os,gamm11,gamm33,gamm13;
	double px2,pz2,pxz,diff1,sqrsq;
	double pz1,pz,px,pxn,pzn,px1;
	int critical,iter;
	double sqr,gm,diff2,err,eps;

	critical=1;
	iter=0;

	if(s*s < 0.5){
		
		oc=1./c;
		eps=0.00000000001;
		sqrsq=1/a1313-pl*pl;
		if(sqrsq<0) sqrsq=0;

		pz=pl*s+rt*sqrt(sqrsq)*c;

		px = (pl-pz*s)*oc;
		px2=px*px;
		pz2=pz*pz;
		pxz=px*pz;
		gamm11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
		gamm33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
		gamm13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
		sqrsq    = (gamm11+gamm33)*(gamm11+gamm33)-
			4*(gamm11*gamm33-gamm13*gamm13);

		if(sqrsq<0) sqrsq=0;

		sqr=sqrt(sqrsq);
		gm    = .5*(gamm11+gamm33-sqr);
		diff1=gm-1;
		err=diff1*diff1;
		pz1=pz;
		if(err>eps) pz=pz1+.0001/sqrt(a1313);
			
		while(err>eps && iter<50){		
			px = (pl-pz*s)*oc;
			px2=px*px;
			pz2=pz*pz;
			pxz=px*pz;
			gamm11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
			gamm33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
			gamm13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
			sqr    = sqrt((gamm11+gamm33)*(gamm11+gamm33)-
				4*(gamm11*gamm33-gamm13*gamm13));
			gm    = .5*(gamm11+gamm33-sqr);
			diff2=gm-1;
			err=diff2*diff2;
			pzn=(diff1*pz-diff2*pz1)/(diff1-diff2);
			diff1=diff2;
			pz1=pz;
			pz=pzn;
			iter++;
		}


	/* everything else */
	} else {
		os=1./s;
		eps=0.00000000001;
		sqrsq=1/a1313-pl*pl;
		if(sqrsq<0) sqrsq=0;

		px=pl*c-rt*sqrt(sqrsq)*s;
		pz = (pl-px*c)*os;
		px2=px*px;
		pz2=pz*pz;
		pxz=px*pz;
		gamm11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
		gamm33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
		gamm13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
		sqrsq    = (gamm11+gamm33)*(gamm11+gamm33)-
			4*(gamm11*gamm33-gamm13*gamm13);

		if(sqrsq<0) sqrsq=0;

		sqr=sqrt(sqrsq);
		gm    = .5*(gamm11+gamm33-sqr);
		diff1=gm-1;
		err=diff1*diff1;
		px1=px;
		px=px1+.0001/sqrt(a1313);
			
		while(err>eps && iter<50){		
			pz = (pl-px*c)*os;
			px2=px*px;
			pz2=pz*pz;
			pxz=px*pz;
			gamm11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
			gamm33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
			gamm13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
			sqr    = sqrt((gamm11+gamm33)*(gamm11+gamm33)-
				4*(gamm11*gamm33-gamm13*gamm13));
			gm    = .5*(gamm11+gamm33-sqr);
			diff2=gm-1;
			err=diff2*diff2;
			pxn=(diff1*px-diff2*px1)/(diff1-diff2);
			diff1=diff2;
			px1=px;
			px=pxn;
			iter++;
		}
	}
	if(iter>=49) critical=-1;

	*pznew = pz;
	*pxnew = px;

	return critical;

}
