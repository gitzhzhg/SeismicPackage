/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"
#include "elastic.h"

int rt_ani_real(float gz, float gx, float a1111i, float a3333i, float 	
	a1133i, float a1313i, float a1113i, float a3313i, float rhoi, 
	float a1111t, float a3333t, float a1133t, float a1313t, float a1113t, 
	float a3313t, float rhot, float pxi, float pzi, float g11i, float g13i,
	float g33i, float pxg,float pzg, float g11g, float g13g, float g33g, 	
	int modei, int modeg, int rort, float *coeff,FILE *ifp)
/*****************************************************************************
Evaluate P-transmission coefficient
******************************************************************************
Input:
a_ijkli		   density normalized stiffness elements incident side
a_ijklt		   density normalized stiffness elements transmitted side
rhoi,rhot	   densities
pxi,pzi		   incident slowness
pxg,pzg	   	   generated slowness
g11i,g13i,g33i     incident polarization
g11g,g13g,g33g     generated wave-polarization
modei		   incident mode
modeg		   generated mode
rort		   reflection(1) or transmission (0) or free surface (2)
gz 		   sin( angle)
gx 		   cos( angle)

Output:
1		   coefficient found
-1 		   error
0 	           root not found
coeff		   ref/trans coefficient

Author:  Andreas Rueger, Colorado School of Mines, 04/24/94
******************************************************************************/
{
	int *ipvt,index=-1;
	float c1111i,c3333i,c1133i,c1313i,c1113i,c3313i;
	float c1111t,c3333t,c1133t,c1313t,c1113t,c3313t;
	float pxrP,pzrP,pxtP,pztP,pxrS,pzrS;
	float pxtS,pztS,g11,g13,g33,vgx,vgz;
	float A,B,C,D,H,I,den;
	float polxi,polzi,polxrP,polzrP,polxtP,polztP,polxrS;
	float polzrS,polxtS,polztS;
	float **a,*b,*rcond,*z;


	/* allocate space for matrix system */
	a=alloc2float(4,4);
	b=alloc1float(4);
	ipvt=alloc1int(4);
	z=alloc1float(4);
	rcond=alloc1float(4);

	/********  find generated wave type-index     ************/
	/* reflect_P (0) reflect_S (1) transm_P (2) transm_S (3) */

	if((modeg == 0 || modeg == 3) && ( rort == 1 || rort == 2 ))
		index = 0;
	else if((modeg == 1 || modeg == 4) && ( rort == 1 || rort == 2 ))
		index = 1;
	else if((modeg == 0 || modeg == 3) && rort == 0)
		index = 2;
	else if((modeg == 1 || modeg == 4) && rort == 0)
		index = 3;
	else 
		err(" wrong mode in rt_ani_real");


	/* get polarization of incident wave */
	polar(pxi,pzi,g11i,g13i,g33i,&polxi,&polzi,modei);   

	/* get tangential slowness-component */
	rotvector (&pxi,&pzi,gz,gx);

	/* find reflected P-root */
	if(index != 0){
		if(findqPqSV(gz,gx,pxi,a1111i,a3333i,a1133i,a1313i,a1113i,
			a3313i,3,&pxrP,&pzrP,&vgx,&vgz,&g11,&g13,&g33,-1,ifp)
			!=1)
			return 0;
	} else {
		pxrP = pxg;
		pzrP = pzg;
		g11  = g11g;
		g13  = g13g;
		g33  = g33g;
	}

	polar(pxrP,pzrP,g11,g13,g33,&polxrP,&polzrP,3);   

	/* find reflected S-root */
	if(index != 1){
		if(findqPqSV(gz,gx,pxi,a1111i,a3333i,a1133i,a1313i,a1113i,
			a3313i,4,&pxrS,&pzrS,&vgx,&vgz,&g11,&g13,&g33,-1,ifp)
			!=1)
			return 0;
	} else {
		pxrS = pxg;
		pzrS = pzg;
		g11  = g11g;
		g13  = g13g;
		g33  = g33g;

	}

	polar(pxrS,pzrS,g11,g13,g33,&polxrS,&polzrS,4);   

	
	rotvector (&pxrP,&pzrP,gz,gx);
	rotvector (&pxrS,&pzrS,gz,gx);
	rotvector (&polxi,&polzi,gz,gx);
	rotvector (&polxrP,&polzrP,gz,gx);
	rotvector (&polxrS,&polzrS,gz,gx);

	c1111i = rhoi*a1111i;
	c3333i = rhoi*a3333i;
	c1313i = rhoi*a1313i;
	c1133i = rhoi*a1133i;
	c1113i = rhoi*a1113i;
	c3313i = rhoi*a3313i;

	/* rotate incidence tensor field */
        rottensor (&c1111i,&c3333i,&c1133i,&c1313i,&c1113i,&c3313i,
		 gz,gx);

	/********* free surface coefficients ********************/
	if(rort == 2){

      	  A = polxrP*pxrP*c1113i+c1313i*(polxrP*pzrP+polzrP*pxrP)+ 	
		c3313i*polzrP*pzrP;
      	  B = polxrS*pxrS*c1113i+c1313i*(polxrS*pzrS+polzrS*pxrS)+ 	
		c3313i*polzrS*pzrS;
      	  C = polxrP*pxrP*c1133i+c3313i*(polxrP*pzrP+polzrP*pxrP)+ 	
		c3333i*polzrP*pzrP;
      	  D = polxrS*pxrS*c1133i+c3313i*(polxrS*pzrS+polzrS*pxrS)+ 	
		c3333i*polzrS*pzrS;
      	  H = polxi*pxi*c1113i+c1313i*(polxi*pzi+polzi*pxi)+ 	
		c3313i*polzi*pzi;
      	  I = polxi*pxi*c1133i+c3313i*(polxi*pzi+polzi*pxi)+ 	
		c3333i*polzi*pzi;
	  den=D*A-C*B;

	  if(ABS(den) < FLT_EPSILON){
		warn(" problems in rt_ani_real (free surface)");
		return -1;
	  } else 
		den = (C*H-I*A)/den;

	  /* reflected qSV (free surface) */
	  if(index == 1){
		*coeff = den;
	  } else if(ABS(A) < FLT_EPSILON){
		warn(" problems in rt_ani_real (free surface)");
		return -1;

	  /* reflected qP (free surface) */
	  } else if(index == 0){
		*coeff = -(H+B*den)/A;
	  } else {
		warn(" problems in rt_ani_real (free surface)");
		return -1;
	  }

	  return 1;

        }



	/* find transmitted P-root */
	if(index != 2){
		if(findqPqSV(gz,gx,pxi,a1111t,a3333t,a1133t,a1313t,a1113t,
			a3313t,3,&pxtP,&pztP,&vgx,&vgz,&g11,&g13,&g33,1,ifp)
			!=1)
			return 0;
	} else {
		pxtP = pxg;
		pztP = pzg;
		g11  = g11g;
		g13  = g13g;
		g33  = g33g;

	}

	polar(pxtP,pztP,g11,g13,g33,&polxtP,&polztP,3);   

	/* find transmitted S-root */
	if(index != 3){
		if(findqPqSV(gz,gx,pxi,a1111t,a3333t,a1133t,a1313t,a1113t,
			a3313t,4,&pxtS,&pztS,&vgx,&vgz,&g11,&g13,&g33,1,ifp)
			!=1)
			return 0;
	} else {
		pxtS = pxg;
		pztS = pzg;
		g11  = g11g;
		g13  = g13g;
		g33  = g33g;

	}

	polar(pxtS,pztS,g11,g13,g33,&polxtS,&polztS,4);   

	/***** rotate rest of the system ************/
	rotvector (&pxtP,&pztP,gz,gx);
	rotvector (&pxtS,&pztS,gz,gx);

	rotvector (&polxtP,&polztP,gz,gx);
	rotvector (&polxtS,&polztS,gz,gx);

	/* adjust polarization according to 
		convention */

	if(pxrP*polxrP+pzrP*polzrP <0){
			polxrP=-polxrP;
			polzrP=-polzrP;
	}
	if(pxtP*polxtP+pztP*polztP <0){
			polxtP=-polxtP;
			polztP=-polztP;
	}
	if(pxrS*polxrS <0){
			polxrS=-polxrS;
			polzrS=-polzrS;
	}
	if(pxtS*polxtS <0){
			polxtS=-polxtS;
			polztS=-polztS;
	}

	c1111t = rhot*a1111t;
	c3333t = rhot*a3333t;
	c1313t = rhot*a1313t;
        c1133t = rhot*a1133t;
	c1113t = rhot*a1113t;
	c3313t = rhot*a3313t;


	/* rotate transmitted tensor field */
        rottensor (&c1111t,&c3333t,&c1133t,&c1313t,&c1113t,&c3313t,
		 gz,gx);


	/* compute matrix elements */
	a[0][0] = polxrP;
	a[1][0] = polxrS;
	a[2][0] = -polxtP;
	a[3][0] = -polxtS;
	a[0][1] = polzrP;
	a[1][1] = polzrS;
	a[2][1] = -polztP;
	a[3][1] = -polztS;

	a[0][2] = c1133i*pxrP*polxrP;
	a[0][2] += c3313i*(pzrP*polxrP+pxrP*polzrP);
	a[0][2] += c3333i*pzrP*polzrP;

	a[1][2] = c1133i*pxrS*polxrS;
	a[1][2] += c3313i*(pzrS*polxrS+polzrS*pxrS);
	a[1][2] += c3333i*pzrS*polzrS;

	a[2][2] = c1133t*pxtP*polxtP;
	a[2][2] += c3313t*(pztP*polxtP+pxtP*polztP);
	a[2][2] += c3333t*pztP*polztP;
	a[2][2] = -a[2][2];

	a[3][2] = c1133t*pxtS*polxtS;
	a[3][2] += c3313t*(pztS*polxtS+pxtS*polztS);
	a[3][2] += c3333t*pztS*polztS;
	a[3][2] = -a[3][2];

	a[0][3] = c1113i*pxrP*polxrP;
	a[0][3] += c1313i*(pzrP*polxrP+pxrP*polzrP);
	a[0][3] += c3313i*pzrP*polzrP;

	a[1][3] = c1113i*pxrS*polxrS;
	a[1][3] += c1313i*(pzrS*polxrS+pxrS*polzrS);
	a[1][3] += c3313i*pzrS*polzrS;

	a[2][3] = c1113t*pxtP*polxtP;
	a[2][3] += c1313t*(pztP*polxtP+pxtP*polztP);
	a[2][3] += c3313t*pztP*polztP;
	a[2][3] = -a[2][3];

	a[3][3] = c1113t*pxtS*polxtS;
	a[3][3] += c1313t*(pztS*polxtS+pxtS*polztS);
	a[3][3] += c3313t*pztS*polztS;
	a[3][3] = -a[3][3];

	/* right hand side vector */
	b[0]   = -polxi;

	b[1]   = -polzi;

	b[2]   = c1133i*polxi*pxi;
	b[2]  += c3313i*(polxi*pzi+polzi*pxi);
	b[2]  += c3333i*polzi*pzi;
	b[2]   = - b[2];

	b[3]   = c1113i*polxi*pxi;
	b[3]  += c1313i*(polxi*pzi+polzi*pxi);
	b[3]  += c3313i*polzi*pzi;
	b[3]   = - b[3];

	/*fprintf(ifp,"a11=%g \t a12=%g \t a13=%g \t a14=%g\n",
		a[0][0],a[0][1],a[0][2],a[0][3]);
	fprintf(ifp,"a21=%g \t a22=%g \t a23=%g \t a24=%g\n",
		a[1][0],a[1][1],a[1][2],a[1][3]);
	fprintf(ifp,"a31=%g \t a32=%g \t a33=%g \t a34=%g\n",
		a[2][0],a[2][1],a[2][2],a[2][3]);
	fprintf(ifp,"a41=%g \t a42=%g \t a43=%g \t a44=%g\n",
		a[3][0],a[3][1],a[3][2],a[3][3]);
	fprintf(ifp,"b1=%g \t b2=%g \t b3=%g \t b4=%g\n",
		b[0],b[1],b[2],b[3]);*/

	/**** solve real n=4 system  *****/
	sgeco(a,4,ipvt,rcond,z);
	sgesl(a,4,ipvt,b,0);

	if(ifp!=NULL){
		fprintf(ifp,"\n Anisotropic Reflection/Transmission coeff\n");
		fprintf(ifp," inc slowness: %f %f\n",pxi,pzi);
		fprintf(ifp," inc polariz.: %f %f\n",polxi,polzi);
		fprintf(ifp," Pref slowness: %f %f\n",pxrP,pzrP);
		fprintf(ifp," Pref polariz.: %f %f\n",polxrP,polzrP);
		fprintf(ifp," Sref slowness %f %f\n",pxrS,pzrS);
		fprintf(ifp," Sref polariz.: %f %f\n",polxrS,polzrS);
		fprintf(ifp," Ptrans slowness %f %f\n",pxtP,pztP);
		fprintf(ifp," Ptrans polariz.: %f %f\n",polxtP,polztP);
		fprintf(ifp," Strans slowness %f %f\n",pxtS,pztS);
		fprintf(ifp," Strans polariz.: %f %f\n",polxtS,polztS);
		fprintf(ifp," Values with respect to rotated system\n");
		fprintf(ifp," \n *****coefficients ******* \n ");
		fprintf(ifp," Pref=%g \n",b[0]);
		fprintf(ifp," Sref=%g \n",b[1]);
		fprintf(ifp," Ptrans=%g \n",b[2]);
		fprintf(ifp," Strans=%g \n",b[3]);
	}

	/* write coefficient **/
	*coeff=b[index];
	

	return 1;

}
