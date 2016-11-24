/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* REFRELAAZIHTI: $Revision: 1.3 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"
#include "anisotropy.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)

#define EPS 0.0001

/* prototype of subroutine used internally */

int phasevel3DTIH(Stiff2D *spar, int mode, double d1, double d2, 	
	       double d3, double *v);
int p_hor3DTIH(Stiff2D *spar1, int mode, double sangle, double cangle, 	
	       double sazi, double cazi, double *p);
void polconv(Vector3D *d, int m, int rort);
int eigVect3DTIH(Stiff2D *spar, int mode, double p1, double p2, 
		double p3, Vector3D *d);
int p_vert3DTIH(Stiff2D *spar, int mode, double p, double s, double c, 
		int rort, double *q, Vector3D *d);
int graebner3D(Stiff2D *spar1, Stiff2D *spar2, double rho1, double rho2, 
	      int modei, int modet, int rort, double sazi, double cazi,
	      double p, double *b, double **a, int *ipvt, double *z,
	      double *rcond);
int testEikonal(Stiff2D *spar, int mode, double p1, double p2, 
		double p3, Vector3D *d);

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" REFREALAZIHTI -  REAL AZImuthal REFL/transm coeff for HTI media 	",
"									",
" refRealAziHTI  [optional parameters]	>coeff.data 			",
" 									",
" Optional parameters: 							",
" vp1=2         p-wave velocity medium 1 (with respect to symm.axes)	",
" vs1=1         s-wave velocity medium 1 (with respect to symm.axes)	",
" eps1=0        epsilon medium1						",
" delta1=0	delta medium 1						",
" gamma1=0	gamma medium 1						",
" rho1=2.7	density medium 1 					",
" vp2=2         p-wave velocity medium 2 (with respect to symm.axes)	",
" vs2=1         s-wave velocity medium 2 (with respect to symm.axes)	",
" eps2=0        epsilon medium 2					",
" delta2=0	delta medium 2						",
" gamma2=0	gamma medium 2						",
" rho2=2.7	density medium 2 					",
" modei=0 	incident mode is qP					",
"		=1 incident mode is qSV					",
"		=2 incident mode is SP					",
" modet=0 	scattered mode						",
" rort=1 	reflection(1)	or transmission (0)			",
" azimuth=0	azimuth with respect to x1-axis (clockwise)		",
" fangle=0	first incidence angle					",
" langle=45	last incidence angle					",
" dangle=1	angle increment						",
" iscale=0      default: angle in degrees				",
"		=1 angle-axis in rad                                    ",
"               =2 axis  horizontal slowness                            ",
"               =3 sin^2 of incidence angle                             ",
" ibin=1 	binary output 						",
"		=0 Ascci output						",
" outparfile	=outpar parameter file for plotting			",
" coeffile	=coeff.data coefficient-output file			",
" test=1 	activate testing routines in code			",
" info=0 	output intermediate results				",
"									",
" Notes:								",
" Axes of symmetry have to coincide in both media.  This code computes	",
" all 6 REAL reflection/transmissions coefficients on the fly. However,	",
" the set-up is such Real reflection/transmission coefficients in 	",
" HTI-media with coinciding symmetry axes.				",
" However, the set-up is such that currently only one coefficient is	",
" dumped into the output. This is easily changed.  The solution of the	",
" scattering problem is obtained numerically and involves the Gaussian	",
" elimination of a 6X6 matrix.						", 
"									",
NULL};

/*
 * AUTHOR:: Andreas Rueger, Colorado School of Mines, 02/10/95
 *                original name of code <graebnerTIH.c>
 *           modified, extended version of this code <refTIH3D>
 * 
 *  Technical references:
 *
 * 	Sebastian Geoltrain: Asymptotic solutions to direct
 *		and inverse scattering in anisotropic elastic media;
 *		CWP 082.
 *	Graebner, M.; Geophysics, Vol 57, No 11:
 *		Plane-wave reflection and transmission coefficients
 *		for a transversely isotropic solid.
 *	Cerveny, V., 1972, Seismic rays and ray intensities in inhomogeneous 	
 *		anisotropic media: Geophys. J. R. astr. Soc., 29, 1-13.
 *
 *	.. and some derivations by Andreas Rueger.
 *
 * If propagation is perpendicular or 
 * parallel to the symmetry axis, the solution is analytic (see 		",
 * graebner2D.c and rtRealIso.c		
 */
/**************** end self doc ********************************/

int test;
int info;

/* the main program */
int main (int argc, char **argv)
{
	double vp1,vp2,vs1,vs2,rho1,rho2;
	double eps1,eps2,delta1,delta2;
	double gamma1,gamma2,azimuth;
	float fangle,langle,dangle,angle;
	double *coeff,p=0;
	double sangle,cangle,sazi,cazi;
	float anglef,dummy;
	FILE *outparfp=NULL, *coeffp=NULL;
	int ibin,modei,modet,rort,iangle,iscale,index;
	char *outparfile=NULL,*coeffile=NULL;
	Stiff2D *spar1, *spar2;
	double **a,*rcond,*z;
	int *ipvt;

	/* allocate space for stiffness elements */
	spar1=(Stiff2D*)emalloc(sizeof(Stiff2D));
	spar2=(Stiff2D*)emalloc(sizeof(Stiff2D));

	/* allocate space for matrix system */
	a = alloc2double(6,6);
	coeff = alloc1double(6);
	ipvt=alloc1int(6);
	z = alloc1double(6);
	rcond=alloc1double(6);

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	if (!getparint("ibin",&ibin)) ibin = 1;
	if (!getparint("modei",&modei)) modei = 0;
	if (!getparint("modet",&modet)) modet = 0;
	if (!getparint("rort",&rort)) rort = 1;
        if (!getparint("iscale",&iscale)) iscale = 0;
	if (!getparint("test",&test)) test = 1;
	if (!getparint("info",&info)) info = 0;

	if(modei != 0 && modei !=1 && modei !=2){
		fprintf(stderr," \n ERROR wrong incidence mode \n");
		return (-1);	/* wrong mode */
	}

	if(modet != 0 && modet !=1 && modet !=2){
		fprintf(stderr," \n ERROR wrong scattering mode \n");
		return (-1);	/* wrong mode */
	}

        if(rort != 0 && rort !=1){
                fprintf(stderr," ERROR wrong rort parameter \n");
                return (-1);    /* wrong mode */
        }

        if(iscale != 0 && iscale !=1 && iscale !=2 && iscale!=3 ){
                fprintf(stderr," ERROR wrong iscale parameter \n");
                return (-1);    /* wrong mode */
        }


	if (!getparfloat("fangle",&fangle)) fangle = 0.0;
	if (!getparfloat("langle",&langle)) langle = 45.0;
	if (!getparfloat("dangle",&dangle)) dangle = 1.0;

	if (!getpardouble("azimuth",&azimuth)) azimuth = 0.;
	if (!getpardouble("vp1",&vp1)) vp1 = 2.0;
	if (!getpardouble("vp2",&vp2)) vp2 = 2.0;
	if (!getpardouble("vs1",&vs1)) vs1 = 1.0;
	if (!getpardouble("vs2",&vs2)) vs2 = 1.0;
	if (!getpardouble("rho1",&rho1)) rho1 = 2.7;
	if (!getpardouble("rho2",&rho2)) rho2 = 2.7;

	if (!getpardouble("eps1",&eps1)) eps1 = 0.;
	if (!getpardouble("eps2",&eps2)) eps2 = 0.;

	if (!getpardouble("delta1",&delta1)) delta1 = 0.;
	if (!getpardouble("delta2",&delta2)) delta2 = 0.;
	if (!getpardouble("gamma1",&gamma1)) gamma1 = 0.;
	if (!getpardouble("gamma2",&gamma2)) gamma2 = 0.;

	if (getparstring("outparfile",&outparfile)) {
                 outparfp = efopen(outparfile,"w");
        } else {
                 outparfp = efopen("outpar","w");
        }

	if (getparstring("coeffile",&coeffile)) {
                 coeffp = efopen(coeffile,"w");
        } else {
                 coeffp = efopen("coeff.data","w");
        }

        checkpars();


	/******   some debugging information ******************/
	if(info){
		ddprint(azimuth);
		ddprint(vp1); ddprint(vs1); ddprint(rho1);
		ddprint(eps1); ddprint(delta1); ddprint(gamma1);

		ddprint(vp2); ddprint(vs2); ddprint(rho2);
		ddprint(eps2); ddprint(delta2); ddprint(gamma2);
	}


	/* convert into rad */
	azimuth=azimuth*PI /180.;
	sazi=sin(azimuth);
	cazi=cos(azimuth);

	/******   convertion into cij's ************************/
	if (!thom2stiffTI(vp1,vs1,eps1,delta1,gamma1,PI/2.,spar1,1) ){
		fprintf(stderr," \n ERROR in thom2stiffTI (1) \n");
		return (-1);
	}

	if (!thom2stiffTI(vp2,vs2,eps2,delta2,gamma2,PI/2.,spar2,1) ){
		fprintf(stderr,"\n ERROR in thom2stiffTI (2) \n");
		return (-1);
	}

	/*****    more debugging output ************************/
	
	if(info){
		diprint(modei);
		diprint(modet);
		diprint(rort);
		ddprint(spar1->a1111);
		ddprint(spar1->a3333);
		ddprint(spar1->a1133);
		ddprint(spar1->a1313);
		ddprint(spar1->a2323);
		ddprint(spar1->a1212);

		ddprint(spar2->a1111);
		ddprint(spar2->a3333);
		ddprint(spar2->a1133);
		ddprint(spar2->a1313);
		ddprint(spar2->a2323);
		ddprint(spar2->a1212);
	}

	/********  find generated wave type-index     ************/
	/* reflect_P (0) reflect_S (1) transm_P (2) transm_S (3) */

	if(modet == 0 && rort==1)
		index = 0;
	else if(modet == 1 && rort==1)
		index = 1;
	else if(modet == 2 && rort==1)
		index = 2;
	else if(modet == 0 && rort==0)
		index = 3;
	else if(modet == 1 && rort==0)
		index = 4;
	else if(modet == 2 && rort==0)
		index = 5;
	else {
		fprintf(stderr,"\n ERROR wrong (index) \n ");
		return (-1);
	}


	/***************** LOOP OVER ANGLES ************************/
	for(angle=fangle,iangle=0;angle<=langle;angle+=dangle){
		
		if(info) ddprint(angle);
	
		sangle=(double) angle*PI/180;
		cangle=cos(sangle);
		sangle=sin(sangle);


		/* get horizontal slowness */
		if(p_hor3DTIH(spar1,modei,sangle,cangle,sazi,cazi,&p)!=1){
			fprintf(stderr,"\n ERROR in p_hor3DTIH \n ");
			return (-1);
		}

		/* compute reflection/transmission coefficient */
		if(graebner3D(spar1,spar2,rho1,rho2,modei,modet,rort,
		   sazi,cazi,p,coeff,a,ipvt,z,rcond)!=1){
			fprintf(stderr,"\n ERROR in p_hor3DTIH \n ");
			return (-1);
		}

		++iangle;

                if(iscale==0)
                     anglef=(float) angle;
                else if(iscale==1)
                     anglef=(float) angle*PI/180.;
                else if(iscale==2)
                     anglef=(float) p;
                else if(iscale==3) 
                     anglef=(float) sangle*sangle;
                  
                dummy= (float)coeff[index];
                  

                /* Binary output for x_t */
                if(ibin==1){

                        fwrite(&anglef,sizeof(float),1,coeffp);
                        fwrite(&dummy,sizeof(float),1,coeffp);

                /* ASCII output  */
                } else if(ibin==0){

                        fprintf(coeffp,"%f      %f\n",anglef,dummy);
		}
	}

	/*********  No of output pairs for plotting ********/
	if(ibin) fprintf(outparfp,"%i\n",iangle);

	return 1;
}


int graebner3D(Stiff2D *spar1, Stiff2D *spar2, double rho1, double rho2, 
	      int modei, int modet, int rort, double sazi, double cazi,
	      double p, double *b, double **a, int *ipvt, double *z,
	      double *rcond)
/*****************************************************************************
	Real reflection/transmission coefficients in
	TIH-media  with coinciding symmetry axes.
Input:
	spar1	density normalized stiffness components medium 1
	spar2	density normalized stiffness components medium 2
	modei	incident wave mode (=0 P; =1 SV; =2 SP)
	modet	scattered wave mode (=0 P; =1 SV; =2 SP)
	rort    reflection or transmission
	sazi	sin(azimuth from symmetry axis)
	cazi	cos(azimuth from symmetry axis)
	p	horizontal slowness component
Output:
	coeff	reflection/transmission coefficient

Technical references:
	Sebastian Geoltrain: Asymptotic solutions to direct
		and inverse scattering in anisotropic elastic media;
		CWP 082.
 	Graebner, M.; Geophysics, Vol 57, No 11:
		Plane-wave reflection and transmission coefficients
		for a transversely isotropic solid.
	Cerveny, V., 1972, Seismic rays and ray intensities in inhomogeneous 	
		anisotropic media: Geophys. J. R. astr. Soc., 29, 1-13.

	.. and some own derivations.

***************************************************************************
AUTHOR: Andreas Rueger, Colorado School of Mines, 01/29/95
***************************************************************************/
{
	double q_in,q_Pr,q_Pt,q_SVr,q_SVt,q_SPr,q_SPt;
	double c55i,c55t,c44i,c44t,c13i,c13t,c33i,c33t;
	Vector3D d_in,d_Pr,d_Pt,d_SVr,d_SVt,d_SPr,d_SPt;

	double p1=cazi*p;
	double p2=sazi*p;

	c55i=spar1->a1313 * rho1;
	c55t=spar2->a1313 * rho2;
	c44i=spar1->a2323 * rho1;
	c44t=spar2->a2323 * rho2;
	c13i=spar1->a1133 * rho1;
	c13t=spar2->a1133 * rho2;
	c33i=spar1->a3333 * rho1;
	c33t=spar2->a3333 * rho2;




	/* compute vertical slowness components and polarization vectors */

	/* incident wave */
	if(p_vert3DTIH(spar1,modei,p,sazi,cazi,0,&q_in,&d_in) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (incident wave) \n ");
		return (-1);
	}
	if(test && testEikonal(spar1,modei,p1,p2,q_in,&d_in)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (incident wave)");
		return (-1);
	}

	/* reflected qP */
	if(p_vert3DTIH(spar1,0,p,sazi,cazi,1,&q_Pr,&d_Pr) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (reflected qP) \n ");
		return (-1);
	}
	if(test && testEikonal(spar1,0,p1,p2,q_Pr,&d_Pr)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (reflected qP)");
		return (-1);
	}

	/* reflected SV */
	if(p_vert3DTIH(spar1,1,p,sazi,cazi,1,&q_SVr,&d_SVr) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (reflected SV) \n ");
		return (-1);
	}
	if(test && testEikonal(spar1,1,p1,p2,q_SVr,&d_SVr)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (reflected SV) \n");
		return (-1);
	}

	/* reflected SP */
	if(p_vert3DTIH(spar1,2,p,sazi,cazi,1,&q_SPr,&d_SPr) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (reflected SP) \n ");
		return (-1);
	}
	if(test && testEikonal(spar1,2,p1,p2,q_SPr,&d_SPr)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (reflected SP) \n");
		return (-1);
	}

	/* transmitted qP */
	if(p_vert3DTIH(spar2,0,p,sazi,cazi,0,&q_Pt,&d_Pt) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (transmitted qP) \n ");
		return (-1);
	}
	if(test && testEikonal(spar2,0,p1,p2,q_Pt,&d_Pt)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (transmitted qP) \n");
		return (-1);
	}

	/* transmitted SV */
	if(p_vert3DTIH(spar2,1,p,sazi,cazi,0,&q_SVt,&d_SVt) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (transmitted SV) \n ");
		return (-1);
	}
	if(test && testEikonal(spar2,1,p1,p2,q_SVt,&d_SVt)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (transmitted SV) \n");
		return (-1);
	}

	/* transmitted SP */
	if(p_vert3DTIH(spar2,2,p,sazi,cazi,0,&q_SPt,&d_SPt) !=1){
		fprintf(stderr,"\n ERROR in p_vert3DTIH (transmitted SP) \n ");
		return (-1);
	}
	if(test && testEikonal(spar2,2,p1,p2,q_SPt,&d_SPt)!=1){
		fprintf(stderr,"\n ERROR in testEikonal (transmitted SP) \n");
		return (-1);
	}


	/* compute matrix elements */
	a[0][0] = d_Pr.x; 	a[1][0] = d_SVr.x;	a[2][0] = d_SPr.x;
	a[3][0] = -d_Pt.x;	a[4][0] = -d_SVt.x;	a[5][0] = -d_SPt.x;
	
	a[0][1] = d_Pr.y; 	a[1][1] = d_SVr.y;	a[2][1] = d_SPr.y;
	a[3][1] = -d_Pt.y;	a[4][1] = -d_SVt.y;	a[5][1] = -d_SPt.y;

	a[0][2] = d_Pr.z; 	a[1][2] = d_SVr.z;	a[2][2] = d_SPr.z;
	a[3][2] = -d_Pt.z;	a[4][2] = -d_SVt.z;	a[5][2] = -d_SPt.z;

	a[0][3] =  c55i*(d_Pr.z*p1 +  d_Pr.x*q_Pr);
	a[1][3] =  c55i*(d_SVr.z*p1+d_SVr.x*q_SVr);
	a[2][3] =  c55i*(d_SPr.z*p1+d_SPr.x*q_SPr);
	a[3][3] = -c55t*(d_Pt.z*p1 +  d_Pt.x*q_Pt);
	a[4][3] = -c55t*(d_SVt.z*p1+d_SVt.x*q_SVt);
	a[5][3] = -c55t*(d_SPt.z*p1+d_SPt.x*q_SPt);

	a[0][4] =  c44i*(d_Pr.z*p2 +  d_Pr.y*q_Pr);
	a[1][4] =  c44i*(d_SVr.z*p2+d_SVr.y*q_SVr);
	a[2][4] =  c44i*(d_SPr.z*p2+d_SPr.y*q_SPr);
	a[3][4] = -c44t*(d_Pt.z*p2 + d_Pt.y*q_Pt);
	a[4][4] = -c44t*(d_SVt.z*p2+d_SVt.y*q_SVt);
	a[5][4] = -c44t*(d_SPt.z*p2+d_SPt.y*q_SPt);

	a[0][5] =  c13i*d_Pr.x*p1+(c33i-2*c44i)*d_Pr.y*p2+c33i*d_Pr.z*q_Pr;
	a[1][5] =  c13i*d_SVr.x*p1+(c33i-2*c44i)*d_SVr.y*p2+c33i*d_SVr.z*q_SVr;
	a[2][5] =  c13i*d_SPr.x*p1+(c33i-2*c44i)*d_SPr.y*p2+c33i*d_SPr.z*q_SPr;
	a[3][5] = -c13t*d_Pt.x*p1-(c33t-2*c44t)*d_Pt.y*p2-c33t*d_Pt.z*q_Pt;
	a[4][5] = -c13t*d_SVt.x*p1-(c33t-2*c44t)*d_SVt.y*p2-c33t*d_SVt.z*q_SVt;
	a[5][5] = -c13t*d_SPt.x*p1-(c33t-2*c44t)*d_SPt.y*p2-c33t*d_SPt.z*q_SPt;

	/* right hand side vector */
	b[0]   = -d_in.x;
	b[1]   = -d_in.y;
	b[2]   = -d_in.z;
	b[3]   = -c55i*(d_in.z*p1 +  d_in.x*q_in);
	b[4]   = -c44i*(d_in.z*p2 +  d_in.y*q_in);
	b[5]   = -(c13i*d_in.x*p1+(c33i-2*c44i)*d_in.y*p2+c33i*d_in.z*q_in);

	if(info){
	fprintf(stderr,"a00=%g  a01=%g  a02=%g  a03=%g  a04=%g  a05=%g \n",
		a[0][0],a[0][1],a[0][2],a[0][3],a[0][4],a[0][5]);
	fprintf(stderr,"a10=%g  a11=%g  a12=%g  a13=%g  a14=%g  a15=%g \n",
		a[1][0],a[1][1],a[1][2],a[1][3],a[1][4],a[1][5]);
	fprintf(stderr,"a20=%g  a21=%g  a22=%g  a23=%g  a24=%g  a25=%g \n",
		a[2][0],a[2][1],a[2][2],a[2][3],a[2][4],a[2][5]);
	fprintf(stderr,"a30=%g  a31=%g  a32=%g  a33=%g  a34=%g  a35=%g \n",
		a[3][0],a[3][1],a[3][2],a[3][3],a[3][4],a[3][5]);
	fprintf(stderr,"a40=%g  a41=%g  a42=%g  a43=%g  a44=%g  a45=%g \n",
		a[4][0],a[4][1],a[4][2],a[4][3],a[4][4],a[4][5]);
	fprintf(stderr,"a50=%g  a51=%g  a52=%g  a53=%g  a54=%g  a55=%g \n",
		a[5][0],a[5][1],a[5][2],a[5][3],a[5][4],a[5][5]); 
	fprintf(stderr,"b1=%g  b2=%g  b3=%g  b4=%g  b5=%g  b6=%g \n",
		b[0],b[1],b[2],b[3],b[4],b[5]); 
	}

	/**** solve real n=4 system  *****/
	dgeco(a,6,ipvt,rcond,z);
	dgesl(a,6,ipvt,b,0);

	if(info){
	fprintf(stderr,"\n TIH Reflection/Transmission coeff\n");
	fprintf(stderr,"inc polar: %g %g %g\n",d_in.x,d_in.y,d_in.z);
	fprintf(stderr,"inc vert slown.: %g \n",q_in);
	fprintf(stderr,"Pr polar: %g %g %g\n",d_Pr.x,d_Pr.y,d_Pr.z);
	fprintf(stderr,"Pr vert slown.: %g \n",q_Pr);
	fprintf(stderr,"SVr polar: %g %g %g\n",d_SVr.x,d_SVr.y,d_SVr.z);
	fprintf(stderr,"SVr vert slown.: %g \n",q_SVr);
	fprintf(stderr,"SPr polar: %g %g %g\n",d_SPr.x,d_SPr.y,d_SPr.z);
	fprintf(stderr,"SPr vert slown.: %g \n",q_SPr);
	fprintf(stderr,"Pt polar: %g %g %g\n",d_Pt.x,d_Pt.y,d_Pt.z);
	fprintf(stderr,"Pt vert slown.: %g \n",q_Pt);
	fprintf(stderr,"SVt polar: %g %g %g\n",d_SVt.x,d_SVt.y,d_SVt.z);
	fprintf(stderr,"SVt vert slown.: %g \n",q_SVt);
	fprintf(stderr,"SPt polar: %g %g %g\n",d_SPt.x,d_SPt.y,d_SPt.z);
	fprintf(stderr,"SPt vert slown.: %g \n",q_SPt); 
	fprintf(stderr,"Pr coefficient: %g \n",b[0]);
	fprintf(stderr,"SVr coefficient: %g \n",b[1]);
	fprintf(stderr,"SPr coefficient: %g \n",b[2]);
	fprintf(stderr,"Pt coefficient: %g \n",b[3]);
	fprintf(stderr,"SVt coefficient: %g \n",b[4]);
	fprintf(stderr,"SPt coefficient: %g \n",b[5]);
	}

	return (1);

}

	

int p_hor3DTIH(Stiff2D *spar1, int mode, double sangle, double cangle, 	
	       double sazi, double cazi, double *p)
/*****************************************************************************
Given incidence angle and azimuth, compute the horizontal slowness for 
media of TIH symmetry and azimuthal propagation. 

Input
	spar1	density normalized stiffness components
	mode	wave mode (=0 P; =1 SV; =2 SP)
	sangle	sin(incidence angle)
	cangle  cos(incidence angle)
	sazi	sin(azimuth from symmetry axis)
	cazi	cos(azimuth from symmetry axis)

Output 
	p	horizontal slowness
Notes:
	routine returns (-1) if evanescent energy present
	it is assumed that density normalized stiffnesses are
	physically reasonable. 
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 01/29/95
******************************************************************************/
{
	double d1=cazi*sangle;
	double d2=sazi*sangle;
	double d3=cangle;
	double v;

	/* get phase velocity for propagation angle */
	if(phasevel3DTIH(spar1,mode,d1,d2,d3,&v) != 1){
		fprintf(stderr,"\n  ERROR in phasevel3DTIH \n ");
		return (-1);
	}

	if(info){
		ddprint(d1);
		ddprint(d2);
		ddprint(d3);
		ddprint(v);
	}

	*p=sangle/v;
	
	return (1);
}

int phasevel3DTIH(Stiff2D *spar, int mode, double d1, double d2, 	
	       double d3, double *v)
/*****************************************************************************
Given incidence angle and azimuth, compute phase velocity for 
media of TIH symmetry and azimuthal propagation. 

Input
	spar1	density normalized stiffness components
	mode	wave mode (=0 P; =1 SV; =2 SP)
	d_j	normalized direction component	
Notes:
	it is assumed that density normalized stiffnesses are
	physically reasonable. 
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 01/29/95
******************************************************************************/
{
	double dr=d2*d2+d3*d3;
	double da=d1*d1;

	double alpha,beta;

	if(mode == 2){
		alpha = spar->a1212 * da + spar->a2323 *dr;
		*v    = sqrt(alpha);
		return(1);
	} else {
		alpha = (spar->a3333+spar->a1313)*dr+
			(spar->a1111+spar->a1313)*da;

		beta=(spar->a3333-spar->a1313)*dr;
		beta -= ((spar->a1111-spar->a1313)*da);
		beta *= beta;
		beta += (4.*da*dr*(spar->a1313+spar->a1133)* 
			(spar->a1313+spar->a1133));
	
		if(beta <0) return (-1);
		beta = sqrt(beta);

		if(alpha -beta <0) return (-1);

		alpha =((mode==0)?sqrt((alpha+beta)*.5):sqrt((alpha-beta)*.5));
		*v=alpha;
	}
	return (1);
}


int p_vert3DTIH(Stiff2D *spar, int mode, double p, double s, double c, 
		int rort, double *q, Vector3D *d)
/*****************************************************************************
Given azimuth and horizontal slowness compute vertical slowness and
displacement vectors for media of TIH symmetry and azimuthal propagation. 

Input
	spar	density normalized stiffness components
	mode	wave mode (=0 P; =1 SV; =2 SP)
	rort	reflection=1 or transmission=0
	s	sin(azimuth from symmetry axis)
	c	cos(azimuth from symmetry axis)

Output
	q	vertical slowness
	d	displacement vector

Notes:
	it is assumed that density normalized stiffnesses are
	physically reasonable. 
	I decided not to distinguish between	
	special cases such as in-plane propagation. This can be done
	at a later stage, here it is helpful to check the consistency
	with graebner 2D and the isotropic routines.
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 01/31/95
******************************************************************************/
{
        double K1,K2,oa44;
   
	double a55 = spar->a1313;
	double a44 = spar->a2323;

	double cc  = c*c;
	double ss  = s*s;
	double pp  = p*p;
	double p1  = p*c;
	double p2  = p*s;
	double a33 = spar->a3333;
	double a11 = spar->a1111;
	double a13 = spar->a1133;

	double oa33   = 1./a33;
	double oa55   = 1./a55;
	double oa3355 = oa33*oa55;
	double pppp   = pp*pp;

	double sqr;

	/* SP mode*************************************** */
	if(mode==2){
		oa44=1./a44;

		sqr=oa44 - pp*(oa44*a55*cc + ss);
		if(sqr < 0)
			return (-1);
		
		/* vertical slowness */
		*q = (rort==0)? sqrt(sqr) : -sqrt(sqr);

		/* eigenvectors  */
		d->x = 0.;

		if(ABS(p2)<EPS){
			d->y = 1.;
			d->z = 0.;
		} else {
			sqr  = (*q)/p2;
			d->z = -1.;
			d->y = sqr;
		}

		polconv(d,mode,(int)SGN(*q));
		return (1);
	}

	/* qP qSV modes  *************************************** */


	K1 = pp*(cc*(a13*a13*oa3355-a11*oa55+2.*a13*oa33) -2.*ss);
	       K1 += oa55+oa33;

	K2 = oa3355 + pp*(-a11*oa3355*cc-oa55*ss-oa33);
	       K2 += pppp*(cc*cc*oa33*a11 + ss*ss +
		     cc*ss*(a11*oa55-2.*a13*oa33-a13*a13*oa3355));

	sqr = K1*K1-4.*K2;
	if(sqr <0) 
		return (-1);

	sqr = sqrt(sqr);
	
	sqr = (mode == 0)? K1-sqr : K1+sqr;
	if(sqr<0)
		return (-1);

	*q = (rort==0) ? sqrt(0.5*sqr) :-sqrt(0.5*sqr);

	if(eigVect3DTIH(spar,mode,p1,p2,*q,d) !=1){
		fprintf(stderr,"\n  ERROR in eigVect3DTIH \n");
		return (-1);
	}
	
	return (1);
}


int eigVect3DTIH(Stiff2D *spar, int mode, double p1, double p2, 
		double p3, Vector3D *d)
/*****************************************************************************
Given vertical, horizontal slowness and wave mode, compute 
displacement vectors for media of TIH symmetry and azimuthal propagation. 

Input
	spar	density normalized stiffness components
	mode	wave mode (=0 P; =1 SV; =2 SP)
	p1	horizontal slowness component
	p2	out-of plane slowness component
	p3	vertical slowness

Output
	d	displacement vector

Notes:
	it is assumed that density normalized stiffnesses are
	physically reasonable. 
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 01/31/95
******************************************************************************/
{
	float dummy;
	double a55 = spar->a1313;
	double a33 = spar->a3333;
	double a11 = spar->a1111;
	double a13 = spar->a1133;

	double a552 = a55*a55;
	double a112 = a11*a11;
	double a332 = a33*a33;
	double a132 = a13*a13;
	double a115 = a11*a55;
	double a335 = a33*a55;
	double a113 = a33*a11;
	double a135 = a13*a55;

	double p12 = p1*p1;
	double p14 = p12*p12;
	double p32 = p3*p3;
	double p34 = p32*p32;
	double p22 = p2*p2;
	double p24 = p22*p22;
	double p122= p12*p22;
	double p123= p12*p32;
	double p23=  p22*p32;

	/* Propagation normal to interface */
	if(ABS(p1) < EPS && ABS(p2) < EPS){
		d->x = (mode==1)? 1. : 0.;
		d->y = 0;
		d->z = (mode==1)? 0. : 1.;

	/* Propagation in fracture plane P */
	} else if(ABS(p1) < EPS && ABS(p2) > EPS){
		dummy=p2/p3;
		d->y = (mode ==1) ? 0. : dummy;
		d->x = (mode ==1) ? 1. : 0.;
		d->z = (mode ==1) ? 0. : 1.;

	/* Propagation  P/S */
	}  else {
		d->z =1;
		d->y =p2/p3;

		dummy = (-2.*a113 + 2.*a115 + 4.*a132 + 8.*a135 + 2.*a335 + 
     			2.*a552)*p122 + (-2.*a113 + 2.*a115 + 4.*a132 + 
    			 8.*a135 + 2.*a335 + 2.*a552)*p123 + 
  			(a112 - 2.*a115 + a552)*p14 + 
  			(2.*a332 - 4.*a335 + 2.*a552)*p23 + 
  			(a332 - 2.*a335 + a552)*p24 + 
  			(a332 - 2.*a335 + a552)*p34;

		if(dummy <0)
			return (-1);

		dummy = SGN(1.-2.*mode)*sqrt(dummy);
		dummy = a11*p12 - a55*p12 - a33*p22 + a55*p22 - 
     			a33*p32 + a55*p32 +dummy;
		d->x  = dummy/(2.*(a13 + a55)*p1*p3);
	}

	polconv(d,mode,(int) SGN(p3));

        return (1);
	
}

void polconv(Vector3D *d, int m, int rort)
/*****************************************************************************
Normalize and adopt a common sign convention 

Input
	d	displacement vector
	m	wave mode
	rort	reflection (-1) or transmission (1)

Output
	d	normalized displacement vector

Convention according to Aki&Richards p. 148. 
For S-wave:	(a) x is positive
		(b) if x==0 y is positive
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 01/31/95
******************************************************************************/
{
	double sign=1.;
	double norm = sqrt(1./(d->x*d->x+d->y*d->y+d->z*d->z));

	/* downgoing P wave */
	if(m==0 && rort && d->z <0 )
		sign = -1;

	/* upgoing P wave */
	else if(m==0 && rort==-1 && d->z >0 )
		sign = -1;

	/* S-wave for x==0 */
	else if(m !=0 && ABS(d->x) < EPS)
		sign = SGN(d->y);

	/* S-wave for x!=0 */
	else if(m !=0 && d->x < 0.)
		sign = -1;

	
	d->x 	*= sign * norm;
	d->y 	*= sign * norm;
	d->z 	*= sign * norm;
}

int testEikonal(Stiff2D *spar, int mode, double p1, double p2, 
		double p3, Vector3D *d)
/*****************************************************************************
Check if Eikonal equation is satisfied 
Input
	spar	density normalized stiffness components
	mode	wave mode (=0 P; =1 SV; =2 SP)
	p1	horizontal slowness component
	p2	out-of plane slowness component
	p3	vertical slowness
	d	displacement vector

Notes:
	it is assumed that density normalized stiffnesses are
	physically reasonable. 
******************************************************************************
Author:
	  Andreas Rueger, Colorado School of Mines, 02/02/95
******************************************************************************/
{
	double p12=p1*p1;
	double p32=p3*p3;	
	double p23=p2*p2+p32;
	double deter;

	/************* check for eigenvectors ****************** */
	if (mode !=2){
		deter  = spar->a1111*p12 + spar->a1313*p23 -1.;
		deter *= (spar->a1313*p12 + spar->a3333*p23 -1.);
		deter -=(spar->a1133+spar->a1313)*(spar->a1133+spar->a1313)*
			p12*p23;
	} else if (mode == 2)
		deter = -1.+spar->a1313*p12 + spar->a2323*p23;

	if(ABS(deter)>EPS)
		return (-1);

	/************* check for eigenvectors ****************** */

	deter=(-1.+spar->a1111*p12+spar->a1313*p23)*d->x +
	       (spar->a1133+spar->a1313)*p1*p2*d->y +
	       (spar->a1133+spar->a1313)*p1*p3*d->z;

	if(ABS(deter)>EPS){
		fprintf(stderr,"deter=%g EPS= %g ",ABS(deter),EPS);
		return (-1);
	}
	deter=(spar->a1133+spar->a1313)*p1*p2*d->x +
	      (-1.+spar->a1313*p12+spar->a3333*p2*p2+spar->a2323*p32)*d->y +
	      (spar->a3333-spar->a2323)*p2*p3*d->z;

	if(ABS(deter)>EPS)
		return (-1);

 	deter=(spar->a1133+spar->a1313)*p1*p3*d->x +
	      (spar->a3333-spar->a2323)*p2*p3*d->y +
	      (-1.+spar->a1313*p12 +spar->a2323*p2*p2+spar->a3333*p32)*d->z;

	if(ABS(deter)>EPS)
		return (-1);

	return (1);
}	
