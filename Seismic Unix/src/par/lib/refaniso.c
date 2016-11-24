/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "anisotropy.h"

/*********************** self documentation **********************/
/***********************************************************************
REFANISO - Reflection coefficients for Anisotropic media

graebner - Reflection/Transmission coeff. (for VTI and TIH media)
           Coefficients are based on Graebner's paper.
gvelpolSH - compute SH group velocity and polarization for TI medium
gvelpolTI - compute P/SV group velocity and polarization for TI medium
p_hor2DTI - compute horizontal slowness for in-plane propagation in TI medium
p_vert2DVTI - Given the horizontal slowness, compute vertical slowness component
rottens2D - rotate 2-D elastic stiffness tensor
stiff2thomVTI - convert density normalized stiffness components 
		parameters into Thomsen's for transversely isotropic 
		material with vertical axes of symmetry
stiff2tv - Convert stiffnesses into Thomsen's parameter of the equivalent
          VTI model, P-wave reflections and azimuthal AVO in TI-media
thom2stiffTI - convert Thomsen's parameters into density normalized 
		stiffness components for transversely isotropic material
		with in-plane-tilted axes of symmetry
thom2tv - Convert generic Thomsen parameters into Thomsen's parameter 
          of the equivalent VTI model
v_phase2DVTI - Given phase angle, compute phase-velocity for TI media

******************************************************************************
graebner:
Input:
spar1,spar2      stiffnesses for both media
rho1,rho2        densities
pl               horizontal slowness
modei            incident mode (0=P 1=SV)
modet            scattered mode
rort             =1 reflection =0 transmission

Output:
coeff            ref/trans coeff

******************************************************************************
gvelpolSH:
Input:
aijkl		stiffness elements
px,pz		slowness elements

Output:
*vgx, *vgz	group velocities
*g11,*g13,*g33  polarizations

******************************************************************************
gvelpolTI:
Input:
aijkl		stiffness elements
px,pz		slowness elements

Output:
*vgx, *vgz	group velocities
*g11,*g13,*g33  polarizations

******************************************************************************
p_hor2DTI: 
Input:
spar		stiffness elements
s		sin(incidence angle)
mode		0=qP-Wave, 1=qSV-wave

Output:
*p	horizontal slowness component p_x

******************************************************************************
p_vert2DVTI:
Input:
spar1            (density normalized) stiffnesses
pl               horizontal slowness
modei            mode (0=P 1=SV)

Output:
p_vert           vertical slowness component

******************************************************************************
rottens2D:
Input:
aijkl		input/output stiffness elements
phi 		rotation angle (counterclock wise)

******************************************************************************
stiff2thomVTI :
Input:
*aijkl	density normalized stiffness components

Output:
vp,vs	vertical P and S wave velocity
eps	Thomsen's parameter 
delta	Thomsen's parameter 
gamma	Thomsen's parameter 

******************************************************************************
stiff2tv - Convert stiffnesses into Thomsen's parameter of the equivalent
           VTI model, P-wave reflections and azimuthal AVO in TI-media
Input:
spar            stiffnesses (normalized or non-normal.)

Output:
alpha           fracture plane compressional velocity
beta            S_parallel vertical velocity
ev              eps of equiv. VTI
dv              delta of ..
gv              gamma of ..

******************************************************************************
thom2tv:
Input:
vp              symm. axis compressional velocity
vs              symm. axis shear velocity
eps             Thomsen's generic parameter as defined with resp. to sym. axis
delta           Thomsen's ..
gamma           Thomsen's ..

Output:
alpha           fracture plane compressional velocity
beta            S_parallel vertical velocity
ev              eps of equiv. VTI
dv              delta of ..
gv              gamma of ..

******************************************************************************
v_phase2DVTI:
Input:
spar1            (density normalized) stiffnesses
sangle           sin(phase_angle)
mode             mode (0=P 1=SV)

Output:
v_phase          phase-velocity for angle
******************************************************************************


************************************************************************
Function prototypes:
int graebner2D(Stiff2D *spar1, double rho1, Stiff2D *spar2, double rho2,
	 double pl, int modei, int modet, int rort, double *coeff);
void gvelpolSH(double a1212, double a2323, double a1223, double px, 
		double pz, double *vgx, double *vgz, double *g11, 
		double *g13, double *g33)
int gvelpolTI (double a1111, double a3333, double a1133, double a1313,
	double a1113, double a3313, double px, double pz, double *vgx,
	double *vgz, double *g11n, double *g13n, double *g33n);
int p_hor2DTI (Stiff2D *spar, double s, int mode, double *p);
int p_vert2DVTI(Stiff2D *spar1, double pl, int modei, double *p_vert);
void rottens2D (Stiff2D *spar, double phi);
int stiff2thomVTI (double a1111, double a3333, double a1133, double a1313, 	
	double a1212, double *vp, double *vs, double *eps,
        double *delta, double *gamma);
int stiff2tv(Stiff2D *spar,double *alpha,double *beta,double *ev,
			double *dv,double *gv);
int thom2stiffTI (double vp, double vs, double eps, double delta, double gamma,
	double phi, Stiff2D *spar, int sign)
int thom2tv(double vp,double vs,double eps,double delta,double gamma,
	    double *alpha,double *beta,double *ev,double *dv,double *gv);
int v_phase2DVTI(Stiff2D *spar1, double sangle, int mode, double *v_phase);

************************************************************************
Author: CWP: Andreas Rueger, 1994-1996, Colorado School of Mines
***********************************************************************/
/**************** end self doc ********************************/



#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)


int graebner2D(Stiff2D *spar1, double rho1, Stiff2D *spar2, double rho2,
	 double pl, int modei, int modet, int rort, double *coeff)
/*****************************************************************************
graebner - Reflection/Transmission coeff. (for VTI and TIH media)
           Coefficients are based on Graebner's paper.
******************************************************************************
Input:
spar1,spar2      stiffnesses for both media
rho1,rho2        densities
pl               horizontal slowness
modei            incident mode (0=P 1=SV)
modet            scattered mode
rort             =1 reflection =0 transmission

Output:
coeff            ref/trans coeff

******************************************************************************
Notes:
	routine returns (-1) if evanescent energy present
	it is assumed that a1111,a3333,a1313 >0 
	Currently no SH scattering supported

 Note the mistype in the equation for K1. The algorithm can be 
 used for VTI and TIH media on the incidence and scattering side

******************************************************************************
 Technical reference: Graebner, M.; Geophysics, Vol 57, No 11:
		Plane-wave reflection and transmission coefficients
		for a transversely isotropic solid.


******************************************************************************
Author: CWP: Andreas Rueger, Colorado School of Mines, 02/01/94
******************************************************************************/
{
	double K1,K2,K3;
	double qa1,qa2,qb1,qb2,sqr,p2;
	double la1,la2,lb1,lb2;
	double ma1,ma2,mb1,mb2;
	double a1,a2,b1,b2;
	double c1,c2,d1,d2;
	double qasq,qbsq;
	double a1111,a3333,a1133;
	double a1313,oa3333,oa1313;
	double m11,m12,m13,m14,m21,m22,m23,m24;
	double m31,m32,m33,m34,m41,m42,m43,m44;

	p2=pl*pl;

	/* note: we need only density normalized
	coefficients to get vertical slowness and
	eigenvectors */

	/* diprint(modei);
	diprint(modet);
	diprint(rort);
	ddprint(spar1->a1111);
	ddprint(spar1->a3333);
	ddprint(spar1->a1133);
	ddprint(spar1->a1313);
	ddprint(spar2->a1111);
	ddprint(spar2->a3333);
	ddprint(spar2->a1133);
	ddprint(spar2->a1313); */


	/**********************  Medium 1  ***********************************/
	a1111=spar1->a1111;
	a3333=spar1->a3333;
	a1133=spar1->a1133;
	a1313=spar1->a1313;

	oa3333=1./a3333;
	oa1313=1./a1313;

	sqr=a1111*oa1313+a1313*oa3333-(a1313+a1133)*(a1313+a1133)*oa3333*oa1313;
	K1=oa3333+oa1313 - sqr*p2;

	K2=a1111*oa3333*p2 -oa3333;

	K3=p2-oa1313;

	sqr= K1*K1 -4.*K2*K3;
	if(ABS(sqr)<FLT_EPSILON)
		sqr=0.;

	if(sqr < 0){
		fprintf(stderr,"ERROR in slowness medium1 \n");
		return (-1);
	}

	/* vertical slowness in medium 1 */
	qasq=0.5*((K1 - sqrt(sqr)));
	qa1=sqrt(qasq);
	qbsq=0.5*(K1 + sqrt(sqr));
	qb1=sqrt(qbsq);

	/* ddprint(qa1);ddprint(qb1); */

	sqr=a1111*p2 + a1313*qasq + a3333*qasq + a1313*p2 -2.;
	if (ABS(sqr) < FLT_EPSILON){
		fprintf(stderr,"ERROR P eigenvector medium 1 \n");
		return (-1);
	}

	sqr=1./sqr;

	la1=a3333*qasq + a1313*p2 -1.;
	
	la1=la1*sqr;
	if(ABS(la1)<FLT_EPSILON)
		la1=0.;

	if(la1 < 0){
		fprintf(stderr,"ERROR la1 < 0\n");
		return (-1);
	}

	/* P-eigenvector component medium 1*/
	la1=sqrt(la1);

	ma1=a1313*qasq + a1111*p2 -1.;
	ma1=ma1*sqr;
	if(ABS(ma1)<FLT_EPSILON)
		ma1=0.;

	if(ma1 <0){
		fprintf(stderr,"ERROR ma1 <0\n");
		return (-1);
	}

	/* P-eigenvector component medium 1*/
	ma1=sqrt(ma1);

	sqr=a1111*p2 + a1313*qbsq + a3333*qbsq + a1313*p2 -2.;
	if (ABS(sqr) < FLT_EPSILON){
		fprintf(stderr,"ERROR SV eigenvector medium 1\n");
		return (-1);
	}

	sqr=1./sqr;

	lb1=a1313*qbsq + a1111*p2 -1.;
	
	lb1=lb1*sqr;

	if(ABS(lb1)<FLT_EPSILON)
		lb1=0.;

	if(lb1 <0){
		fprintf(stderr,"ERROR lb1 <0\n");
		return (-1);
	}

	/* SV-eigenvector component medium 1*/
	lb1=sqrt(lb1);

	mb1=a3333*qbsq + a1313*p2 -1.;
	mb1=mb1*sqr;

	if(ABS(mb1)<FLT_EPSILON)
		mb1=0.;

	if(mb1 <0){
		fprintf(stderr,"ERROR mb1 <0\n");
		return (-1);
	}

	/* SV-eigenvector component medium 1*/
	mb1=sqrt(mb1);

	/* ddprint(la1);ddprint(lb1);
	   ddprint(ma1);ddprint(mb1);*/

	/* convert into stiffness */
	a1313=a1313*rho1;
	a3333=a3333*rho1;
	a1133=a1133*rho1;

	a1=a1313*(qa1*la1 + pl*ma1);
	b1=a1313*(qb1*mb1 - pl*lb1);
	c1=pl*la1*a1133 + qa1*ma1*a3333;
	d1=pl*mb1*a1133 - qb1*lb1*a3333;

	/* ddprint(a1);ddprint(b1);
	   ddprint(c1);ddprint(d1);*/


	/**********************  Medium 2  ***********************************/

	a1111=spar2->a1111;
	a3333=spar2->a3333;
	a1133=spar2->a1133;
	a1313=spar2->a1313;

	oa3333=1./a3333;
	oa1313=1./a1313;

	sqr=a1111*oa1313+a1313*oa3333-(a1313+a1133)*(a1313+a1133)*oa3333*oa1313;
	K1=oa3333+oa1313 - sqr*p2;

	K2=a1111*oa3333*p2 -oa3333;

	K3=p2-oa1313;

	sqr= K1*K1 -4.*K2*K3;
	if(ABS(sqr)<FLT_EPSILON)
		sqr=0.;

	if(sqr < 0){
		fprintf(stderr,"ERROR slowness medium 2 \n");
		return (-1);
	}

	/* vertical slowness in medium 2 */
	qasq=0.5*((K1 - sqrt(sqr)));

	if(ABS(qasq)<FLT_EPSILON)
		qasq=0.;

	if(qasq < 0){
		fprintf(stderr,"\n COMPLEX vertical P- slowness medium 2 \n");
		return (-1);
	}

	qa2=sqrt(qasq);
	qbsq=0.5*(K1 + sqrt(sqr));
	qb2=sqrt(qbsq);

	/* ddprint(qa2);ddprint(qb2); */

	sqr=a1111*p2 + a1313*qasq + a3333*qasq + a1313*p2 -2.;
	if (ABS(sqr) < FLT_EPSILON){
		fprintf(stderr,"ERROR P eigenvector medium 2 \n");
		return (-1);
	}

	sqr=1./sqr;

	la2=a3333*qasq + a1313*p2 -1;
	
	la2=la2*sqr;
	if(ABS(la2)<FLT_EPSILON)
		la2=0.;

	if(la2 <0){
		fprintf(stderr,"ERROR la2<0\n");
		return (-1);
	}

	/* P eigenvector component medium 2*/
	la2=sqrt(la2);

	ma2=a1313*qasq + a1111*p2 -1;
	ma2=ma2*sqr;

	if(ABS(ma2)<FLT_EPSILON)
		ma2=0.;

	if(ma2 <0){
		fprintf(stderr,"ERROR ma2<0\n");
		return (-1);
	}

	/* P eigenvector component medium 2*/
	ma2=sqrt(ma2);

	sqr=a1111*p2 + a1313*qbsq + a3333*qbsq + a1313*p2 -2.;
	if (ABS(sqr) < FLT_EPSILON){
		fprintf(stderr,"ERROR SV eigenvector medium 2 \n");
		return (-1);
	}

	sqr=1./sqr;

	lb2=a1313*qbsq + a1111*p2 -1.;
	lb2=lb2*sqr;
	if(ABS(lb2)<FLT_EPSILON)
		lb2=0.;

	if(lb2 < 0){
		fprintf(stderr,"ERROR lb2<0\n");
		return (-1);
	}

	/* SV-eigenvector component medium 2*/
	lb2=sqrt(lb2);

	mb2=a3333*qbsq + a1313*p2 -1.;
	mb2=mb2*sqr;
	if(ABS(mb2)<FLT_EPSILON)
		mb2=0.;

	if(mb2 <0){
		fprintf(stderr,"ERROR mb2<0\n");
		return (-1);
	}

	/* SV-eigenvector component medium 1*/
	mb2=sqrt(mb2);

	/* ddprint(la2);ddprint(lb2);
	   ddprint(ma2);ddprint(mb2);*/

	/* convert into stiffness */
	a1313=a1313*rho2;
	a3333=a3333*rho2;
	a1133=a1133*rho2;

	a2=a1313*(qa2*la2 + pl*ma2);
	b2=a1313*(qb2*mb2 - pl*lb2);
	c2=pl*la2*a1133 + qa2*ma2*a3333;
	d2=pl*mb2*a1133 - qb2*lb2*a3333;

	/* ddprint(a2);ddprint(b2);
	   ddprint(c2);ddprint(d2); */

	m11=la1; m12=mb1; m13=-la2; m14=-mb2;
	m21=c1;  m22=d1;  m23=-c2;  m24=-d2;
	m31=ma1; m32=-lb1;m33=ma2;  m34=-lb2;
	m41=a1;  m42=b1;  m43=a2;   m44=b2; 

	/*ddprint(m11); ddprint(m12); ddprint(m13); ddprint(m14);
	ddprint(m21); ddprint(m22); ddprint(m23); ddprint(m24);
	ddprint(m31); ddprint(m32); ddprint(m33); ddprint(m34);
	ddprint(m41); ddprint(m42); ddprint(m43); ddprint(m44);*/

	sqr=(m14*m23*m32*m41 - m13*m24*m32*m41 - m14*m22*m33*m41 + 
      	     m12*m24*m33*m41 + m13*m22*m34*m41 - m12*m23*m34*m41 - 
     	     m14*m23*m31*m42 + m13*m24*m31*m42 + m14*m21*m33*m42 - 
     	     m11*m24*m33*m42 - m13*m21*m34*m42 + m11*m23*m34*m42 + 
     	     m14*m22*m31*m43 - m12*m24*m31*m43 - m14*m21*m32*m43 + 
      	     m11*m24*m32*m43 + m12*m21*m34*m43 - m11*
      	     m22*m34*m43 -m13*m22*m31*m44 + m12*m23*m31*m44 + 
	     m13*m21*m32*m44 - m11*m23*m32*m44 - m12*m21*m33*m44 + 	
	     m11*m22*m33*m44);
	/* ddprint(sqr); */

	if(ABS(sqr)<FLT_EPSILON){
		fprintf(stderr,"ERROR Denominator == 0 \n");
		return (-1);
	}

	*coeff=1./sqr;


	/* PP transmission*/
	if(modei==0 && modet==0 && rort==0)
		*coeff *=2*(-(m14*m21*m32*m41)+m11*m24*m32*m41+
			m12*m21*m34*m41-m11*m22*m34*m41+m14*m21*m31*m42-
			m11*m24*m31*m42-m12*m21*m31*m44+m11*m22*m31*m44);

	/* PP reflection*/
	else if(modei==0 && modet==0 && rort==1)
		*coeff *=(m14*m23*m32-m13*m24*m32-m14*m22*m33+m12*m24*m33+
       			  m13*m22*m34-m12*m23*m34)*m41+m31*(-(m14*m23*m42)+
		   	  m13*m24*m42+m14*m22*m43-m12*m24*m43-m13*m22*m44+
 			  m12*m23*m44)+m21*(-(m14*m33*m42)+m13*m34*m42+
			  m14*m32*m43-m12*m34*m43-m13*m32*m44+m12*m33*m44)+
    			  m11*(m24*m33*m42-m23*m34*m42-m24*m32*m43+
			  m22*m34*m43+m23*m32*m44-m22*m33*m44);

	/* PS transmission*/
	else if(modei==0 && modet==1 && rort==0)
		*coeff *=2*(m13*m21*m32*m41-m11*m23*m32*m41-m12*m21*m33*m41+
      			 m11*m22*m33*m41-m13*m21*m31*m42+m11*m23*m31*m42+
			 m12*m21*m31*m43-m11*m22*m31*m43);

	/* PS reflection*/
	else if(modei==0 && modet==1 && rort==1)
		*coeff *=2*(m14*m21*m33*m41-m11*m24*m33*m41-m13*m21*m34*m41+
		         m11*m23*m34*m41-m14*m21*m31*m43+m11*m24*m31*m43+
			 m13*m21*m31*m44-m11*m23*m31*m44);

	/* SP transmission*/
	else if(modei==1 && modet==0 && rort==0)
		*coeff *=2*(-(m14*m22*m32*m41)+m12*m24*m32*m41+
			 m14*m22*m31*m42-m12*m24*m31*m42+m12*m21*m34*m42-
			 m11*m22*m34*m42-m12*m21*m32*m44+m11*m22*m32*m44);

	/* SP reflection*/
	else if(modei==1 && modet==0 && rort==1)
		*coeff *=2*(-(m14*m22*m33*m42)+m12*m24*m33*m42+
			 m13*m22*m34*m42-m12*m23*m34*m42+m14*m22*m32*m43-
			 m12*m24*m32*m43-m13*m22*m32*m44+m12*m23*m32*m44);

	/* SS transmission*/
	else if(modei==1 && modet==1 && rort==0)
		*coeff *=2*(m13*m22*m32*m41-m12*m23*m32*m41-m13*m22*m31*m42+		
			 m12*m23*m31*m42-m12*m21*m33*m42+m11*m22*m33*m42+
			 m12*m21*m32*m43-m11*m22*m32*m43);

	/* SS reflection*/
	else if(modei==1 && modet==1 && rort==1)
		*coeff *=(-(m14*m23*m31)+m13*m24*m31+m14*m21*m33-
		         m11*m24*m33-m13*m21*m34+m11*m23*m34)*m42+
			 m32*(m14*m23*m41-m13*m24*m41-m14*m21*m43+		
			 m11*m24*m43+m13*m21*m44-m11*m23*m44)+
			 m22*(m14*m33*m41-m13*m34*m41-m14*m31*m43+
			 m11*m34*m43+m13*m31*m44-m11*m33*m44)+	
			 m12*(-(m24*m33*m41)+m23*m34*m41+m24*m31*m43-		
			 m21*m34*m43 - m23*m31*m44 + m21*m33*m44);
	else 
		return (-1);

	sqr=*coeff;

	/* ddprint(sqr); */
	return 1;
}


void gvelpolSH(double a1212, double a2323, double a1223, double px, 
		double pz, double *vgx, double *vgz, double *g11, 
		double *g13, double *g33)
/*****************************************************************************
gvelpolSH - compute SH group velocity and polarization for TI medium
******************************************************************************
Input:
aijkl		stiffness elements
px,pz		slowness elements

Output:
*vgx, *vgz	group velocities
*g11,*g13,*g33  polarizations

******************************************************************************
Author:  Andreas Rueger, Colorado School of Mines, 01/09/95
******************************************************************************/
{

	*vgx = a1212*px + a1223*pz;
	*vgz = a1223*px + a2323*pz;

	*g11=0.;
	*g13=0.;
	*g33=0.;
}

int gvelpolTI (double a1111, double a3333, double a1133, double a1313,
	double a1113, double a3313, double px, double pz, double *vgx,
	double *vgz, double *g11n, double *g13n, double *g33n)
/*****************************************************************************
gvelpolTI - compute P/SV group velocity and polarization for TI medium
******************************************************************************
Input:
aijkl		stiffness elements
px,pz		slowness elements

Output:
*vgx, *vgz	group velocities
*g11,*g13,*g33  polarizations

******************************************************************************
Notes:
Input px/pz values determine ray mode.
Subroutine returns:
(-1) if error in g11, g33 detected,
(1) otherwise
******************************************************************************
Author:  Andreas Rueger, Colorado School of Mines, 01/09/95
******************************************************************************/
{
	double gamm11,gamm13,gamm33;
	double px2,pz2,pxz,den,g11,g13,g33;

	px2   = px*px;
	pz2   = pz*pz;
	pxz   = px*pz;


	/*anisotropy parameters*/
	gamm11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
	gamm33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
	gamm13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
	den     = 1/(gamm11+gamm33-2);
	g11     = (gamm33-1)*den;
	g33     = (gamm11-1)*den;
	g13     = -gamm13*den;

	/* computing ray (group) velocities */
	*vgx =  (a1111*px*g11+(a1133+a1313)*pz*g13+a3313*pz*g33+
		a1113*(pz*g11+2*px*g13)+a1313*g33*px);
	*vgz =  (a3333*pz*g33+(a1133+a1313)*px*g13+a1113*px*g11+
		+a3313*(px*g33+2*pz*g13)+a1313*g11*pz);

	/* kill round-off */
	if( g11 < - FLT_EPSILON || g33 < - FLT_EPSILON) 
		return (-1);

	else if( g11 < 0.0 )
		g11 = 0;

	else if( g33 < 0.0 )
		g33 = 0;


	*g11n=g11;
	*g13n=g13;
	*g33n=g33;

	return (1);

}


int p_hor2DTI (Stiff2D *spar, double s, int mode, double *p)
/*****************************************************************************
p_hor2DTI - compute horizontal slowness for in-plane propagation in TI medium
******************************************************************************
Input:
spar		stiffness elements
s		sin(incidence angle)
mode		0=qP-Wave, 1=qSV-wave

Output:
*p	horizontal slowness component p_x

******************************************************************************
Notes:
(-1) if error detected,
(1) otherwise
******************************************************************************
Author:  Andreas Rueger, Colorado School of Mines, 01/26/95
******************************************************************************/
{

        double cc,c,ss,sc,gamm11,gamm33,gamm13,sqr,vp2;

	if(mode != 0 && mode !=1)
		return (-1);	/* wrong mode */

	cc = 1.-s*s;

	/* ddprint(s); */

	if(ABS(cc)<FLT_EPSILON)
		cc=0.;
	if(cc <0)
		return (-1);	/* evanescent energy generated */


 	c  = sqrt(cc);
	ss = s*s;
	sc = s*c;

	/*computing phase velocity for angle */
	gamm11 = spar->a1111*ss+spar->a1313*cc +2*spar->a1113*sc;
	gamm33 = spar->a3333*cc +spar->a1313*ss+2*spar->a3313*sc;
	gamm13 = (spar->a1133+spar->a1313)*sc+spar->a1113*ss+ 
			spar->a3313*cc;
	sqr    = sqrt((gamm11+gamm33)*(gamm11+gamm33)-
			4*(gamm11*gamm33-gamm13*gamm13));
	vp2    = gamm11+gamm33;
	       vp2    =((mode == 0)? vp2+sqr : vp2-sqr);

	/* error check */
	if(vp2 < 0) 
		err("\n ERROR: ray initialization \n");
	/* p_hor = sin(pangle)/ABS(phasevel) */

	*p     = s/sqrt(vp2*.5);

	return (1);
	
}

int p_vert2DVTI(Stiff2D *spar1, double pl, int modei, double *p_vert)
/*****************************************************************************
p_vert2DVTI - Given the horizontal slowness, compute vertical slowness component
******************************************************************************
Input:
spar1            (density normalized) stiffnesses
pl               horizontal slowness
modei            mode (0=P 1=SV)

Output:
p_vert           vertical slowness component

******************************************************************************
Notes:
	routine returns (-1) if evanescent energy present
	it is assumed that a1111,a3333,a1313 >0 
	Currently no SH scattering supported

******************************************************************************
Technical reference: Graebner, M.; Geophysics, Vol 57, No 11:
		Plane-wave reflection and transmission coefficients
		for a transversely isotropic solid.

******************************************************************************
Author: CWP: Andreas Rueger, Colorado School of Mines, 01/26/96
******************************************************************************/
{
	double K1,K2,K3;
	double qa1,qb1,sqr,p2;
	double qasq,qbsq;
	double a1111,a3333,a1133;
	double a1313,oa3333,oa1313;

	p2=pl*pl;

	/* note: we need only density normalized
	coefficients to get vertical slowness and
	eigenvectors */


	/**********************  Medium 1  ***********************************/
	a1111=spar1->a1111;
	a3333=spar1->a3333;
	a1133=spar1->a1133;
 	a1313=spar1->a1313;

	oa3333=1./a3333;
	oa1313=1./a1313;

	sqr=a1111*oa1313+a1313*oa3333-(a1313+a1133)*(a1313+a1133)*oa3333*oa1313;
	K1=oa3333+oa1313 - sqr*p2;

	K2=a1111*oa3333*p2 -oa3333;

	K3=p2-oa1313;

	sqr= K1*K1 -4.*K2*K3;
	if(ABS(sqr)<FLT_EPSILON)
		sqr=0.;

	if(sqr < 0){
		fprintf(stderr," evanescent energy generated  \n");
		return (-1);
	}

	/* vertical slowness in medium 1 */
	qasq=0.5*((K1 - sqrt(sqr)));
        if(qasq < 0){
		fprintf(stderr,"evanescent energy generated  \n");
		return (-1);
	}

	qa1=sqrt(qasq);
	qbsq=0.5*(K1 + sqrt(sqr));
	qb1=sqrt(qbsq);

	if(modei==0)
	   *p_vert =qa1;
	else
           *p_vert =qb1;
	
	/* ddprint(sqr); */
	return 1;
}

void rottens2D (Stiff2D *spar, double phi)
/*****************************************************************************
rottens2D - rotate 2-D elastic stiffness tensor
******************************************************************************
Input:
aijkl		input/output stiffness elements
phi 		rotation angle (counterclock wise)

******************************************************************************
Notes:
Warning!: double precision required for this routine!

******************************************************************************
Author:  Andreas Rueger, Colorado School of Mines, Jan 07, 1995
******************************************************************************/
{
		double ss,cc,sc,a,c,f,l,m,n,o,p,q;
		double si=sin(phi),co=cos(phi);

		ss=si*si; cc=co*co;
		sc=si*co;
		a=spar->a1111;
		c=spar->a3333;
		f=spar->a1133;
		l=spar->a1313;
		m=spar->a1113;
		n=spar->a3313;
		o=spar->a1212;
		p=spar->a2323;
		q=spar->a1223;


		spar->a1111=a*cc*cc+c*ss*ss+2*f*cc*ss+4*l*cc*ss
			-4*m*cc*sc-4*n*ss*sc;
		spar->a3333=a*ss*ss+c*cc*cc+2*f*cc*ss+4*l*cc*ss+
			4*m*ss*sc+4*n*cc*sc;
		spar->a1133=a*ss*cc+c*ss*cc+f*(ss*ss+cc*cc)-4*l*ss*cc-2*m*
			(ss*sc-cc*sc)-2*n*(cc*sc- ss*sc);
		spar->a1313=a*ss*cc+c*ss*cc-2*f*ss*cc+l*(cc*cc+ss*ss-2*ss*cc)
			-2*m*(ss*sc-cc*sc)-2*n*(cc*sc- ss*sc);
		spar->a1113=a*cc*sc-c*ss*sc+f*(ss*sc-cc*sc)+2*l*(ss*sc-cc*sc)+
			m*(cc*cc -3*cc*ss) +(3*cc*ss - ss*ss )*n;
		spar->a3313=a*ss*sc-c*cc*sc+f*(cc*sc-ss*sc)+2*l*(cc*sc-ss*sc)+
			m*( 3*cc*ss - ss*ss ) + n*( cc*cc - 3*ss*cc);
		spar->a1212=cc*o-2*sc*q+ss*p;
		spar->a2323=ss*o+2*sc*q+cc*p;
		spar->a1223=sc*o+(cc-ss)*q-sc*p;
}

int thom2stiffTI (double vp, double vs, double eps, double delta, double gamma,
	double phi, Stiff2D *spar, int sign)
/*****************************************************************************
thom2stiffTI - convert Thomsen's parameters into density normalized 
		stiffness components for transversely isotropic material
		with in-plane-tilted axes of symmetry
******************************************************************************
Input:
vp,vs	P and S wave velocity along symmetry axes
eps	Thomsen's parameter with respect to symmetry axes
delta	Thomsen's parameter with respect to symmetry axes
gamma	Thomsen's parameter with respect to symmetry axes
phi	angle(rad) vertical --> symmetry axes (clockwise) 
sign	sign of c11+c44 ( for most materials sign=1)

Output:
*aijkl	density normalized stiffness components
*******************************************************************************
Notes:
subroutine returns (-1) if unphysical parameter detected, otherwise
returns (1).

(1)-axes horizontal (2)-axes out-of-plane (3)-axis vertical

For vertical axes of symmetry see thom2stiffVTI.c
*******************************************************************************
Author: Andreas Rueger, Colorado School of Mines, Jan 07, 1995
******************************************************************************/
{
	double temp1,temp2;

	if(vp<=0. || vs<0. )
		return (-1);
	
	spar->a3333 = temp1 = vp*vp;
	spar->a1313 = temp2 = vs*vs;
	spar->a1111 = 2.* temp1*eps + temp1;

	temp1 = 2.* delta *temp1 *(temp1-temp2)
	        +(temp1-temp2)*(temp1-temp2);

	if(temp1 < 0.)
		return (-1);
	 
	if (sign == 1)
		spar->a1133 = sqrt(temp1)-temp2;
	else if(sign == -1)
		spar->a1133 = - sqrt(temp1)-temp2;
	else
		return (-1);

	spar->a1212 = 2.*temp2*gamma + temp2;

	spar->a1113 = spar->a3313 = spar->a1223 = 0.0;

	spar->a2323 = temp2; /* a1313=a2323 */

	rottens2D(spar,phi);


	return (1);

}



int stiff2thomVTI (double a1111, double a3333, double a1133, double a1313, 	
	double a1212, double *vp, double *vs, double *eps,
        double *delta, double *gamma)
/*****************************************************************************
stiff2thomVTI - convert density normalized stiffness components 
		parameters into Thomsen's for transversely isotropic 
		material with vertical axes of symmetry
******************************************************************************
Input:
*aijkl	density normalized stiffness components

Output:
vp,vs	vertical P and S wave velocity
eps	Thomsen's parameter 
delta	Thomsen's parameter 
gamma	Thomsen's parameter 

*******************************************************************************
Notes:
subroutine returns (-1) if unphysical parameter detected, otherwise
returns (1).

(1)-axes horizontal (2)-axes out-of-plane (3) axis vertical

For tilted in-plane axes of symmetry see stiff2thomTI.c
or libTest/stiff2thoma.ma
*******************************************************************************
Author: Andreas Rueger, Colorado School of Mines, Jan 08, 1995
******************************************************************************/
{
	double temp;

	if(a1111<=0. || a1313<0.)
		return (-1);
	 
	
	*vp  = sqrt(a3333);
	*vs  = sqrt(a1313);
	*eps = (a1111-a3333)*0.5/a3333;

	temp   = (a1133+a1313)*(a1133+a1313)-(a3333-a1313)*(a3333-a1313);
	*delta = temp/(a3333*(a3333-a1313)) * 0.5;
	*gamma = (a1212-a1313)/a1313*0.5;

	return (1);

}

int stiff2tv(Stiff2D *spar,double *alpha,double *beta,double *ev,
double *dv,double *gv)
/*****************************************************************************
stiff2tv - Convert stiffnesses into Thomsen's parameter of the equivalent
VTI model, P-wave reflections and azimuthal AVO in TI-media
******************************************************************************
Input:
spar            stiffnesses (normalized or non-normal.)

Output:
alpha           fracture plane compressional velocity
beta            S_parallel vertical velocity
ev              eps of equiv. VTI
dv              delta of ..
gv              gamma of ..

******************************************************************************
Author: CWP: Andreas Rueger, Colorado School of Mines, 01/27/96
******************************************************************************/
{



   	   *alpha=sqrt(spar->a3333);
	   *beta=sqrt(spar->a2323);
	   *ev=(spar->a1111 - spar-> a3333)/(2.*spar->a3333);
	   *dv=((spar->a1133+spar->a1313)*(spar->a1133+spar->a1313)-
		   (spar->a3333-spar->a1313)*(spar->a3333-spar->a1313))/
		  (2.*spar->a3333*(spar->a3333-spar->a1313));
	   *gv=(spar->a1212 - spar->a2323)/(2.*spar->a2323);
	   
	   return 1;
	   
	   
}

int thom2tv(double vp,double vs,double eps,double delta,double gamma,
	    double *alpha,double *beta,double *ev,double *dv,double *gv)
/*****************************************************************************
thom2tv - Convert generic Thomsen parameters into Thomsen's parameter 
          of the equivalent VTI model
******************************************************************************
Input:

vp              symm. axis compressional velocity
vs              symm. axis shear velocity
eps             Thomsen's generic parameter as defined with resp. to sym. axis
delta           Thomsen's ..
gamma           Thomsen's ..

Output:
alpha           fracture plane compressional velocity
beta            S_parallel vertical velocity
ev              eps of equiv. VTI
dv              delta of ..
gv              gamma of ..

******************************************************************************
Notes:
Technical Reference:
Andreas Rueger, P-wave reflections and azimuthal AVO in TI-media
******************************************************************************
Author: Andreas Rueger, Colorado School of Mines, 01/27/96
******************************************************************************/
{
   double f;
   
   *ev    = -eps/(1.+2.*eps);

   *gv    = -gamma/(1.+2.*gamma);

   *alpha = vp*sqrt(1.+2.*eps);
   
   *beta  = vs*sqrt(1.+2.*gamma);
    
   f = vs/vp;

   f = 1 - f*f;
   
   *dv    = (delta -2.*eps*(1. + eps/f))/((1.+2.*eps)*(1.+2.*eps/f));

   return 1;
   
}

int v_phase2DVTI(Stiff2D *spar1, double sangle, int mode, double *v_phase)
/*****************************************************************************
v_phase2DVTI - Given phase angle, compute phase-velocity for TI media
******************************************************************************
Input:
spar1            (density normalized) stiffnesses
sangle           sin(phase_angle)
mode             mode (0=P 1=SV)

Output:
v_phase          phase-velocity for angle
******************************************************************************
Notes: Currently no SH mode supported
******************************************************************************
Technical reference: White, J. E..; Underground Sound.

******************************************************************************
Author: Andreas Rueger, Colorado School of Mines, 01/26/96
******************************************************************************/
{
	double a1111,a3333,a1133,a1313;
	double s2,c2,sqr,dummy;
	
	s2=sangle*sangle;
        c2=1.-s2;
	

	/***density normalized stiffnesses *************/
	a1111=spar1->a1111;
	a3333=spar1->a3333;
	a1133=spar1->a1133;
 	a1313=spar1->a1313;

	sqr=(a1111-a1313)*s2 - (a3333-a1313)*c2;
	sqr *=sqr;
	sqr +=4.*(a1133+a1313)*(a1133+a1313)*s2*c2;
	
	if(ABS(sqr)<FLT_EPSILON)
		sqr=0.;

	else if(sqr < 0){
		fprintf(stderr," error in v_phase \n");
		return (-1);
	}

        sqr=sqrt(sqr);
	
        dummy=(a1111+a1313)*s2+(a3333+a1313)*c2;
	
	if(mode==0)
	   *v_phase = sqrt(0.5*(dummy+sqr));
	else
           *v_phase = sqrt(0.5*(dummy-sqr));
	
	return 1;
}
