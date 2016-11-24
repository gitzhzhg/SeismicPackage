/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTIVEL: $Revision: 1.5 $ ; $Date: 2011/11/16 23:40:27 $        */

#include "su.h"

/*********************** self documentation **********************/
char *sdoc[] = { 
"									",
"  SUTIVEL -  SU Transversely Isotropic velocity table builder		",
"	computes vnmo or vphase as a function of Thomsen's parameters and",
"	theta and optionally interpolate to constant increments in slowness",
"									",
" Optional Parameters:							",
" a=2500.		alpha (vertical p velocity)			",
" b=1250.		beta (vertical sv velocity)			",
" e=.20			epsilon (horiz p-wave anisotropy)		",
" d=.10			delta (strange parameter)			",
" maxangle=90.0		max angle in degrees				",
" nangle=9001		number of angles to compute			",
" verbose=0		set to 1 to see full listing			",
" np=8001		number of slowness values to output		",
" option=1		1=output vnmo(p) (result used for TI DMO)	",
"			2=output vnmo(theta) in degrees			",
"			3=output vnmo(theta) in radians			",
"			4=output vphase(p)				",
"			5=output vphase(theta) in degrees		",
"			6=output vphase(theta) in radians		",
"			7=output first derivative vphase(p)		",
"			8=output first derivative vphase(theta) in degrees",
"			9=output first derivative vphase(theta) in radians",
"			10=output second derivative vphase(p)		",
"			11=output second derivative vphase(theta) in degrees",
"			12=output second derivative vphase(theta) in radians",
"			13=( 1/vnmo(0)^2 -1/vnmo(theta)^2 )/p^2 test vs theta",
"			   (result should be zero for all theta for d=e)",
"			14=return vnmo(p) for weak anisotropy		",
" normalize=0		=1 means scale vnmo by cosine and scale vphase by",
" 			    1/sqrt(1+2*e*sin(theta)*sin(theta)		",
"	 		   (only useful for vphase when d=e for constant",
"				result)					",
"			=0 means output vnmo or vphase unnormalized	",
"									",
" Output on standard output is ascii text with:				",
" line   1: number of values						",
" line   2: abscissa increment (p or theta increment, always starts at zero)",
" line 3-n: one value per line						",
"									",
NULL};

/*
 * Author: (visitor to CSM form Mobil) John E. Anderson, Spring 1994
 */
/**************** end self doc ********************************/

void vget( float a, float b, float e, float d, float theta, float *vel);
float vnmoweakTI( float a, float b, float e, float d, float p);
float DweakTI( float a, float b, float e, float d, float p);

int main (int argc, char **argv)
{
	float a,b,e,d,maxangle,dangle,angle,theta,p,v,vnmo;
	float dv,d2v,pmax,dp,test;
	float vel[6];
	int nangle,iangle,verbose,np,ip,option,normalize;
	float *pin;
	float *vin;
	float *pout;
	float *vout;

	initargs(argc,argv);
	requestdoc(1);

	if (!getparfloat("a",&a)) a = 2500.;
	if (!getparfloat("b",&b)) b = 1250.;
	if (!getparfloat("e",&e)) e = .20;
	if (!getparfloat("d",&d)) d = .10;
	if (!getparfloat("maxangle",&maxangle)) maxangle=90.;
	if (!getparint("nangle",&nangle)) nangle = 9001;
	if (!getparint("np",&np)) np=8001;
	if (!getparint("verbose",&verbose)) verbose=0;
	if (!getparint("option",&option)) option=1;
	if (!getparint("normalize",&normalize)) normalize=0;
	fprintf(stderr,"normalize=%d\n",normalize);

        checkpars();
	pin = (float *)malloc(nangle*sizeof(float));
	vin = (float *)malloc(nangle*sizeof(float));
	pout = (float *)malloc(np*sizeof(float));
	vout = (float *)malloc(np*sizeof(float));

	if(option==14) {
		pmax=1./(a*sqrt(1+2*e));
		dp=pmax/(np-1);
		for(ip=0;ip<np;ip++) {
			vnmo=vnmoweakTI(a,b,e,d,ip*dp);
			if(vnmo<0.) {
				pmax=(ip-1)*dp;
				goto newmax;
			}
		}
newmax:
		dp=pmax/(np-1);
		fprintf(stdout,"%d\n",np);
		fprintf(stdout,"%e\n",dp);
	   	for(ip=0;ip<np;ip++) {
			vnmo=vnmoweakTI(a,b,e,d,ip*dp);
			fprintf(stdout,"%e\n",vnmo);
		}
		return(CWP_Exit());
	}

	if(nangle<2) nangle=2;
	dangle = maxangle/(nangle-1);
	if(verbose) {
		fprintf(stdout," alpha = a = %f\n",a);
		fprintf(stdout," beta = b = %f\n",b);
		fprintf(stdout," epsilon = e = %f\n",e);
		fprintf(stdout," delta = d = %f\n",d);
		fprintf(stdout," a*sqrt(1+2*d) = %f\n",a*sqrt(1+2*d));
		fprintf(stdout," a*sqrt(1+2*e) = %f\n",a*sqrt(1+2*e));
	}

	if(option==2 || option==5 || option==8 || option==11 || option==13 ) {
		if(verbose) fprintf(stdout," The number of angles =");
		fprintf(stdout,"%d\n",nangle);
		if(verbose) fprintf(stdout," The angle increment in degrees =");
		fprintf(stdout,"%e\n",dangle);
		if(verbose)
		fprintf(stdout,
			" Following are velocity values for constant increments in angle\n");
		}
	if(option==3 || option==6 || option==9 || option==12 ) {
		if(verbose) fprintf(stdout," The number of angles =");
		fprintf(stdout,"%d\n",nangle);
		if(verbose) fprintf(stdout," The angle increment in radians =");
		fprintf(stdout,"%e\n",dangle*PI/180.);
		if(verbose)
		fprintf(stdout,
			" Following are velocity values for constant increments in angle\n");
		}

	for(iangle=0;iangle<nangle;iangle++) {

		angle=iangle*dangle;
		theta=angle*PI/180.;

		vget(a,b,e,d,theta,vel);
		if(normalize) {
			vel[0] /= sqrt(1 + 2*e*sin(theta)*sin(theta));
			vel[3] *= cos(theta);
			}
		v=vel[0];
		dv=vel[1];
		d2v=vel[2];
		vnmo=vel[3];
		p=vel[4];

		pin[iangle]=p;
		vin[iangle]=vnmo;
		if(option==4) vin[iangle]=v;
		if(option==7) vin[iangle]=dv;
		if(option==10) vin[iangle]=d2v; 
		if(option==2 || option==3) fprintf(stdout,"%e\n",vnmo);
		if(option==5 || option==6) fprintf(stdout,"%e\n",v);
		if(option==8 || option==9) fprintf(stdout,"%e\n",dv);
		if(option==11 || option==12) fprintf(stdout,"%e\n",d2v);
		if(option==13) {
			test=p*p+1./(vnmo*vnmo)-1./(vin[0]*vin[0]);
			fprintf(stdout,"%e\n",test);
			}
		if(verbose) {
			fprintf(stdout," angle (in degrees) = %6.2f",angle);
			fprintf(stdout," vphase = %9.2f",v);
			fprintf(stdout," vnmo = %15.6e",vnmo);
			fprintf(stdout," p = %15.6e\n",p);
			fprintf(stdout,"	  dv/dtheta = %15.6e",dv);
			fprintf(stdout," d2v/dtheta2 = %15.6e\n",d2v);
			}
		}
	if(option ==1 || option==4) {
	   pmax=pin[0];
	   for(iangle=0;iangle<nangle;iangle++) {
		pmax=MAX(pmax,pin[iangle]);
	   }
	   dp=pmax/(np-4);
	   for(ip=0;ip<np;ip++)
		pout[ip]=ip*dp;
	   intlin(nangle,pin,vin,vin[0],vin[nangle-1],np,pout,vout);
	   if(verbose) fprintf(stdout," np = ");
	   fprintf(stdout,"%d\n",np);
	   if(verbose) fprintf(stdout," dp = ");
	   fprintf(stdout,"%e\n",dp);
	   if(verbose) fprintf(stdout,
		" Following are velocity values for constant increments in p\n");
	   for(ip=0;ip<np;ip++) {
		fprintf(stdout,"%e\n",vout[ip]);
		}
	}
	free(pin);
	free(vin);
	free(pout);
	free(vout);
	return(CWP_Exit());
}

void vget( float a, float b, float e, float d, float theta, float *vel)
/*

This routine returns phase and NMO velocity information given
an input angle and Thomsen's parameters for a TI medium.

input parameters:
----------------
a      = alpha = vertical p-wave phase velocity = vrms/sqrt(1+2*d)    
b      = beta = vertical shear wave phase velocity
e      = epsilon = anisotropy factor for horizontally propagating p waves  
d      = delta = 0.5*(e + ds/(1-(b*b)/(a*a)))	
theta  = angle in radians 

returned parameters:
-------------------
vel[0] = p-wave phase velocity
vel[1] = first derivative of p-wave phase velocity with respect to theta
vel[2] = second derivative of p-wave phase velocity with respect to theta
vel[3] = NMO velocity
vel[4] = ray parameter p = sin(theta)/vphase
vel[5] = NMO velocity using weak anisotropy assumption

*/
{
	float p, psi, sint, cost, sin2t, cos2t, tant, eps, f, g;
	float vnmo,v,dv,d2v,gamma,dgamma,d2gamma,sqgamma;
	eps=1.0e-7;
	sint=sin(theta);
	cost=cos(theta);
	sin2t=sint*sint;
	cos2t=cost*cost;
	psi=1.-(b*b)/(a*a);
	f = psi*(2*d-e);
	g = (psi+e)*e;
	gamma=0.25*psi*psi+f*sin2t*cos2t+g*sin2t*sin2t;
	dgamma=f*2*sint*cost*cos2t +
		( 4*g - 2*f )*sint*sin2t*cost;
	d2gamma=f*2*cos2t*cos2t +
		( g - f )*12*cos2t*sin2t +
		( 2*f - 4*g )*sin2t*sin2t;	
	sqgamma=sqrt(gamma);
	v=a*sqrt(1.+e*sin2t-0.5*psi+sqgamma);
	dv=0.5*a*a*(2*e*sint*cost + 0.5*dgamma/sqgamma)/v;
	d2v=0.5*a*a*(2*e*(cos2t-sin2t)+0.5*d2gamma/sqgamma
		-0.25*dgamma*dgamma/(sqgamma*gamma) )/v - dv*dv/v;
	if(cost<eps) cost=eps;
	tant=sint/cost;
	vnmo = ( v*sqrt( 1 + d2v/v) )/( cost * (1-tant*dv/v) );
	p=sint/v;
	vel[0]=v;
	vel[1]=dv;
	vel[2]=d2v;
	vel[3]=vnmo;
	vel[4]=p;
	return;
}
float DweakTI( float a, float b, float e, float d, float p)
/*
	returns D(p) for the weak anisotropy approximation

input parameters:
----------------
a      = alpha = vertical p-wave phase velocity = vrms/sqrt(1+2*d)    
b      = beta = vertical shear wave phase velocity
e      = epsilon = anisotropy factor for horizontally propagating p waves  
d      = delta = 0.5*(e + ds/(1-(b*b)/(a*a)))	
p      = slowness = sin(theta)/v

*/
{
	float y;
	float vnmo0;
	float D;

	vnmo0=a*sqrt(1+2*d);
	y=p*vnmo0;
	y=y*y;
	D=p*p*( 1 + 2*(e-d)*(4*y*y-9*y+6) );
	return (D);
}

float vnmoweakTI( float a, float b, float e, float d, float p)
/*
	returns vnmo(p) for the weak anisotropy approximation
	or a value of -1 for the evanescent region

input parameters:
----------------
a      = alpha = vertical p-wave phase velocity = vrms/sqrt(1+2*d)    
b      = beta = vertical shear wave phase velocity
e      = epsilon = anisotropy factor for horizontally propagating p waves  
d      = delta = 0.5*(e + ds/(1-(b*b)/(a*a)))	
p      = slowness = sin(theta)/v

*/
{
	float y;
	float D;
	float vnmo;

	D=DweakTI(a,b,e,d,p);
	y=a*a*(1+2*d);
	y=1/y;
	if(y>D) {
		vnmo=1./sqrt(y-D);
	}else{
		vnmo=-1;
	}
	return (vnmo);
}

