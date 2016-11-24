/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/* REFREALVTI: $Revision: 1.3 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"
#include "anisotropy.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)


/* prototype of subroutine used internally */

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" REFREALVTI -  REAL REFL/transm coeff for VTI media and symmetry-axis	",
"                 planes of HTI media 					",
"                                                                       ",
" refRealVTI  [Optional parameters]	   			        ",
" 									",
" Optional parameters:							",
" vp1=2          p-wave velocity medium 1 (along symm.axes)	        ",
" vs1=1          s-wave velocity medium 1 (along symm.axes)	        ",
" eps1=0         Thomsen's epsilon medium 1				",
" delta1=0	 Thomsen's delta medium 1	         		",
" rho1=2.7	 density medium 1 					",
" axis1=0	 medium 1 is VTI					",
"		 =1 medium 1 is HTI					",
" vp2=2.5         p-wave velocity medium 2 (along symm.axes)	        ",
" vs2=1.2          s-wave velocity medium 2 (along symm.axes)	        ",
" eps2=0	 epsilon medium2					",
" delta2=0	delta medium 2						",
" rho2=3.0	density medium 2 					",
" axis2=0	medium 2 is VTI						",
"		=2 medium 2 is HTI					",
" modei=0 	incident mode is qP					",
"		=1 incident mode is qSV					",
" modet=0 	scattered mode						",
" rort=1 	reflection(1)	or transmission (0)			",
" fangle=0	first angle						",
" langle=45	last angle						",
" dangle=1	angle increment						",
" iscale=0       =1 angle-axis in rad					",
"                =2 axis  horizontal slowness                           ",
"                =3 sin^2 of incidence angle                            ",
" ibin=1 	binary output 						",
"		=0 Ascci output						",
" outparfile	=outpar parameter file for plotting			",
" coeffile	=coeff.data coefficient-output file			",
"									",
" Notes:								",
" Coefficients are based on Graebner's 1992 Geophysics paper. Note the	",
" mistype in the equation for K1. The algorithm can be used for VTI	",
" and HTI media on the incidence and scattering side.			",
"									",
NULL};

/*
 * AUTHOR:: Andreas Rueger, Colorado School of Mines, 01/20/95
 *       original name of algorithm: graebner1.c
 *
 *  Technical reference: Graebner, M.; Geophysics, Vol 57, No 11:
 *		Plane-wave reflection and transmission coefficients
 *		for a transversely isotropic solid.
 *		Rueger, A.; Geophysics 1996 (accepted):
 *		P-wave reflection coefficients ...
 *
 */
/**************** end self doc ***********************************/

/* the main program */
int
main (int argc, char **argv)
{
	double vp1,vp2,vs1,vs2,rho1,rho2;
	double eps1,eps2,delta1,delta2,sangle;
	float fangle,langle,dangle,angle;
	double coeff,p=0;
	float anglef,dummy;
	FILE *outparfp=NULL, *coeffp=NULL;
	int ibin,modei,modet,rort,test,iangle,iscale;
	int axes1,axes2;
	char *outparfile=NULL,*coeffile=NULL;
	Stiff2D *spar1, *spar2;

	spar1=(Stiff2D*)emalloc(sizeof(Stiff2D));
	spar2=(Stiff2D*)emalloc(sizeof(Stiff2D));

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	if (!getparint("ibin",&ibin)) ibin = 1;
	if (!getparint("modei",&modei)) modei = 0;
	if (!getparint("modet",&modet)) modet = 0;
	if (!getparint("rort",&rort)) rort = 1;
	if (!getparint("iscale",&iscale)) iscale = 0;
	if (!getparint("axis1",&axes1)) axes1 = 0;
	if (!getparint("axis2",&axes2)) axes2 = 0;

	if(modei != 0 && modei !=1){
		fprintf(stderr," ERROR wrong incidence mode \n");
		return (-1);	/* wrong mode */
	}

	if(modet != 0 && modet !=1){
		fprintf(stderr," ERROR wrong scattering mode \n");
		return (-1);	/* wrong mode */
	}
        if(rort != 0 && rort !=1){
		fprintf(stderr," ERROR wrong rort parameter \n");
		return (-1);	/* wrong mode */
        }
	
	if(iscale != 0 && iscale !=1 && iscale !=2 && iscale!=3 ){
		fprintf(stderr," ERROR wrong iscale parameter \n");
		return (-1);	/* wrong mode */
        }
	
	if (!getparfloat("fangle",&fangle)) fangle = 0.0;
	if (!getparfloat("langle",&langle)) langle = 45.0;
	if (!getparfloat("dangle",&dangle)) dangle = 1.0;

	if (!getpardouble("vp1",&vp1)) vp1 = 2.0;
	if (!getpardouble("vp2",&vp2)) vp2 = 2.5;
	if (!getpardouble("vs1",&vs1)) vs1 = 1.0;
	if (!getpardouble("vs2",&vs2)) vs2 = 1.2;
	if (!getpardouble("rho1",&rho1)) rho1 = 2.7;
	if (!getpardouble("rho2",&rho2)) rho2 = 3.0;

	if (!getpardouble("eps1",&eps1)) eps1 = 0.;
	if (!getpardouble("eps2",&eps2)) eps2 = 0.;

	if (!getpardouble("delta1",&delta1)) delta1 = 0.;
	if (!getpardouble("delta2",&delta2)) delta2 = 0.;

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

	/*
	ddprint(vp1); ddprint(vs1); ddprint(rho1);
	ddprint(eps1); ddprint(delta1); diprint(axes1);

	ddprint(vp2); ddprint(vs2); ddprint(rho2);
	ddprint(eps2); ddprint(delta2); diprint(axes2); */

	if (!thom2stiffTI(vp1,vs1,eps1,delta1,0.,axes1*PI/2.,spar1,1) ){
		fprintf(stderr," ERROR in thom2stiffTI (1)");
		return (-1);
	}

	if (!thom2stiffTI(vp2,vs2,eps2,delta2,0.,axes2*PI/2.,spar2,1) ){
		fprintf(stderr," ERROR in thom2stiffTI (2)");
		return (-1);
	}

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

	for(angle=fangle,iangle=0;angle<=langle;angle+=dangle){
		
		sangle = (double) sin(angle*PI/180.);

		/* determine horizontal slowness */
		if(p_hor2DTI(spar1,sangle,modei,&p) == -1){
			fprintf(stderr," ERROR in p_hoz2DTI \n");
			return (-1);
		}

		/* compute reflection/transmission coefficient */
		test=graebner2D(spar1,rho1,spar2,rho2,p,
			 modei,modet,rort,&coeff);

		if(test==1){

		  ++iangle;

		  if(iscale==0)
		     anglef=(float) angle;
		  else if(iscale==1)
		     anglef=(float) angle*PI/180.;
		  else if(iscale==2)
		     anglef=(float) p;
		  else if(iscale==3) 
		     anglef=(float) sangle*sangle;
		  
		  dummy= (float)coeff;
		  

    		 /* Binary output for x_t */
     		 if(ibin==1){

		       	fwrite(&anglef,sizeof(float),1,coeffp);
		   	fwrite(&dummy,sizeof(float),1,coeffp);

    		 /* ASCII output  */
     		 } else if(ibin==0){
			
		        fprintf(coeffp,"%f	%f\n",anglef,dummy);

	      	 }
		} else if(test==-1)
			fprintf(stderr,"ERROR in graebner1 \n");
	}

	if(ibin) fprintf(outparfp,"%i\n",iangle);

	return 1;
}


