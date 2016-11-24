/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* LINRORT: $Revision: 1.6 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include <par.h>		/* definuje getchar, sdoc a error funkci */


/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" LINRORT - linearized P-P, P-S1 and P-S2 reflection coefficients 	",
"		for a horizontal interface separating two of any of the	",
"		following halfspaces: ISOTROPIC, VTI, HTI and ORTHORHOMBIC. ",
"									",
"   linrort [optional parameters]					",
"									",
" hspace1=ISO	medium type of the incidence halfspace:		 	",
"		=ISO ... isotropic					",
"		=VTI ... VTI anisotropy				 	",
"		=HTI ... HTI anisotropy				 	",
"		=ORT ... ORTHORHOMBIC anisotropy			",
" for ISO:								",
" vp1=2	 	P-wave velocity, halfspace1				",
" vs1=1		S-wave velocity, halfspace1				",
" rho1=2.7	density, halfspace1					",
"									",
" for VTI:								",
" vp1=2		P-wave vertical velocity (V33), halfspace1		",
" vs1=1		S-wave vertical velocity (V44=V55), halfspace1		",
" rho1=2.7	density, halfspace1					",
" eps1=0	Thomsen's generic epsilon, halfspace1			",
" delta1=0	Thomsen's generic delta, halfspace1			",
" gamma1=0	Thomsen's generic gamma, halfspace1			", 
"									",
" for HTI:								",
" vp1=2	 P-wave vertical velocity (V33), halfspace1			",
" vs1=1	 \"fast\" S-wave vertical velocity (V44), halfspace1		",
" rho1=2.7	density, halfspace1					",
" eps1_v=0	Tsvankin's \"vertical\" epsilon, halfspace1		",
" delta1_v=0	Tsvankin's \"vertical\" delta, halfspace1		",
" gamma1_v=0	Tsvankin's \"vertical\" gamma, halfspace1		",	
"									",
" for ORT:								",
" vp1=2		P-wave vertical velocity (V33), halfspace1		",
" vs1=1	 x2-polarized S-wave vertical velocity (V44), halfspace1 	",
" rho1=2.7	density, halfspace1					",
" eps1_1=0	Tsvankin's epsilon in [x2,x3] plane, halfspace1	 	",
" delta1_1=0	Tsvankin's delta in [x2,x3] plane, halfspace1		",
" gamma1_1=0	Tsvankin's gamma in [x2,x3] plane, halfspace1	  	",
" eps1_2=0	Tsvankin's epsilon in [x1,x3] plane, halfspace1		",
" delta1_2=0	Tsvankin's delta in [x1,x3] plane, halfspace1		",
" gamma1_2=0	Tsvankin's gamma in [x1,x3] plane, halfspace1	  	",
" delta1_3=0	Tsvankin's delta in [x1,x2] plane, halfspace1		",
"									",
" hspace2=ISO	medium type of the reflecting halfspace (the same	",
"		convention as above)					",
"									",
" medium parameters of the 2nd halfspace follow the same convention	",
" as above:								",
"									",
" vp2=2.5		 vs2=1.2		rho2=3.0		",
" eps2=0		  delta2=0					",
" eps2_v=0		delta2_v=0		gamma2_v=0		",
" eps2_1=0		delta2_1=0		gamma2_1=0		",
" eps2_2=0		delta2_2=0		gamma2_2=0		",
" delta2_3=0								",
"									",
"	(note you do not need \"gamma2\" parameter for evaluation	",
"	of weak-anisotropy reflection coefficients)			",
"									",
" a_file=-1	the string '-1' ... incidence and azimuth angles are	",
"		generated automatically using the setup values below	",
"		a_file=file_name ... incidence and azimuth angles are	",
"		read from a file \"file_name\"; the program expects a	",
"		file of two columns [inc. angle, azimuth]		",
"									",
" in the case of a_file=-1:						",
" fangle=0	first incidence phase angle				",
" langle=30	last incidence angle					",
" dangle=1	incidence angle increment				",
" fazim=0	first azimuth (in deg)				  	",
" lazim=0	last azimuth  (in deg)				  	",
" dazim=1	azimuth increment (in deg)				",
"									",
" kappa=0.	azimuthal rotation of the lower halfspace2 (e.t. a	",
"		symmetry axis plane for HTI, or a symmetry plane for	",
"		ORTHORHOMBIC) with respect to the x1-axis		",
"									",
" out_inf=info.out	information output file				",
" out_P=Rpp.out	file with Rpp reflection coefficients			",
" out_S=Rps.out	file with Rps reflection coefficients			",
" out_SVSH=Rsvsh.out  file with SV and SH projections of reflection	",
"			coefficients					",
" out_Error=error.out file containing error estimates evaluated during  ",
"			the computation of the reflection coefficients;	",
"									",
"									",
" Output:								",
" out_P:								",
" inc. phase angle, azimuth, reflection coefficient; for a_file=-1, the ",
" inc. angle is the fast dimension					",
" out_S:								",
" inc. phase angle, azimuth, Rps1, Rps2, cos(PHI), sin(PHI); for	",
" a_file=-1, the inc. angle is the fast dimension			", 
" out_SVSH:								",
" inc. phase angle, azimuth, Rsv, Rsh, cos(PHI), sin(PHI); for	  ",
" a_file=-1, the inc. angle is the fast dimension			",
" out_Error:								",
" error estimates of Rpp, Rpsv and Rpsh approximations; global error is ",
" analysed as well as partial contributions to the error due to the	",
" isotropic velocity contrasts, and due to anisotropic  upper and lower ",
" halfspaces. The error file is self-explanatory, see also descriptions ",
" of subroutines P_err_2nd_order, SV_err_2nd_order and SH_err_2nd_order.",
"									",
"									",
" Adopted Convention:							",
"									",
" The right-hand Cartesian coordinate system with the x3-axis pointing  ",
" upward has been chosen. The upper halfspace (halfspace1)		",
" contains the incident P-wave. Incidence angles can vary from <0,PI/2),",
" azimuths are unlimited, +azimuth sense counted from x1->x2 axes	",
" (azimuth=0 corresponds to the direction of x1-axis). In the current	",
" version, the coordinate system is attached to the halfspace1 (e.t.	",
" the symmetry axis plane of HTI halfspace1, or one of symmetry planes  ",
" of ORTHORHOMBIC halfspace1, is aligned with the x1-axis), however, the",
" halfspace2 can be arbitrarily rotated along the x3-axis with respect  ",
" to the halfspace1. The positive weak-anisotropy polarization of the	",
" reflected P-P wave (e.t. positive P-P reflection coefficient) is close",
" to the direction of isotropic slowness vector of the wave (pointing	",
" outward the interface). Similarly, weak-anisotropy S-wave reflection  ",
" coefficients are described in terms of \"SV\" and \"SH\" isotropic	",
" polarizations, \"SV\" and \"SH\" being unit vectors in the plane	",
" perpendicular to the isotropic slowness vector. Then, the positive	",
" \"SV\" polarization vector lies in the incidence plane and points	",
" towards the interface, and positive \"SH\" polarization vector is	",
" perpendicular to the incidence plane, aligned with the positive	",
" x2-axis, if azimuth=0. Rotation angle \"PHI\", characterizing a	",
" rotation of \"the best projection\" of the S1-wave polarization	",
" vector in the isotropic SV-SH plane in the incidence halfspace1, is	",
" counted in the positive sense from \"SV\" axis (PHI=0) towards the	",
" \"SH\" axis (PHI=PI/2). Of course, S2 is perpendicular to S1, and	",
" the projection of S1 and S2 polarizations onto the SV-SH plane	",
" coincides with SV and SH directions, respectively, for PHI=0.		",
"									",
" The units for velocities are km/s, angles I/O are in degrees		",
"									",
" Additional Notes:							",
"	The coefficients are computed as functions of phase incidence	",
"	angle and azimuth (determined by the incidence slowness vector).",
"	Vertical symmetry planes of the HTI and				",
"	ORTHORHOMBIC halfspaces can be arbitrarily rotated along the	",
"	x3-axis. The linearization is based on the assumption of weak	", 
"	contrast in elastic medium parameters across the interface,	",
"	and the assumption of weak anisotropy in both halfspaces.	",
"	See the \"Adopted Convention\" paragraph below for a proper	",
"	input.								",
"									",
NULL};

/* 
 *  Author: Petr Jilek, CSM-CWP, December 1999.
 */

/**************** end self doc ********************************/

/* this sets up the S-background; optimum for HTI1/HTI2 is HTIback1=a55, */
/* HTIback2=a44, optimum for ORT unknown yet at this point*/

#define HTIback1 55	/* a55 or a44 in HTI1 (44 keeps correct values	*/
			/*  at 90 deg of azimuth)			*/
#define HTIback2 55	/* a55 or a44 in HTI2				*/
#define ORTback1 55	/* a55 or a44 in ORT1 , a55 is standart		*/
#define ORTback2 55	/* a55 or a44 in ORT2, a55 is standart		*/

float vp1,vp2,vs1,vs2,rho1,rho2;
float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_1,gamma1_44,gamma1_55;
float eps2_1,eps2_2,delta2_1,delta2_2,delta2_3,gamma2_44,gamma2_55;

int main(int argc, char **argv)
{

	extern float vp1,vp2,vs1,vs2,rho1,rho2;
	extern float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_1,gamma1_44,gamma1_55;
  extern float eps2_1,eps2_2,delta2_1,delta2_2,delta2_3,gamma2_44,gamma2_55;

  float eps1,delta1,eps2,delta2,gamma1;
  float eps1_v,delta1_v,gamma1_v,eps2_v,delta2_v,gamma2_v;
  float gamma1_2,gamma2_1,gamma2_2;
  float fangle,langle,dangle,fazim,lazim,dazim,kappa,ang,azim;

  float RPP,RPS1,RPS2,SV,SH,cosPHI,sinPHI;
  float errp,errs, Max, Iso_e[5]={0.,0.,0.,0.,0.};

  int HSP1=-1,HSP2=-1,count=1,n,back=0;
  
  char *hspace1=NULL;
  char *hspace2=NULL;
  char *a_file=NULL;
  char *out_inf=NULL; 
  char *out_P=NULL;
  char *out_S=NULL; 
  char *out_SVSH=NULL;
  char *out_Error=NULL;
  FILE *a_file_p=NULL;
  FILE *out_inf_p=NULL;
  FILE *out_P_p=NULL;
  FILE *out_S_p=NULL;
  FILE *out_SVSH_p=NULL;
  FILE *out_Error_p=NULL;

  ErrorFlag Rp_1st, Rp_2nd, Rsv_1st, Rsv_2nd, Rsh_1st, Rsh_2nd;
  
  
  float lincoef_Rp(float ang, float azim, float kappa, float *rpp, ErrorFlag *rp_1st,
		ErrorFlag *rp_2nd, int count);
  float lincoef_Rs(float ang, float azim, float kappa, float *rps1, 
		float *rps2, float *sv, float *sh, float *cphi, float *sphi, int i_hsp,
		ErrorFlag *rsv_1st, ErrorFlag *rsv_2nd, ErrorFlag *rsh_1st, ErrorFlag *rsh_2nd, int count);


  /*auxiliar quantities for inversion info */
  float beta, alpha, Da_a, Db_b, DG_G, DZ_Z, PS2,PSC,PABS,SS2,SSC,SABS;


  

/* hook up getpar to handle the parameters */
  
  initargs(argc,argv);
  requestdoc(0);

  if (!getparstring("hspace1",&hspace1)) hspace1 = "ISO";
  if (!getparfloat("vp1",&vp1)) vp1 = 2.0;
  if (!getparfloat("vs1",&vs1)) vs1 = 1.0;
  if (!getparfloat("rho1",&rho1)) rho1 = 2.7;
  if (!getparfloat("eps1",&eps1)) eps1 = 0;
  if (!getparfloat("delta1",&delta1)) delta1 = 0.;
  if (!getparfloat("gamma1",&gamma1)) gamma1 = 0.;
  if (!getparfloat("eps1_v",&eps1_v)) eps1_v = 0;
  if (!getparfloat("delta1_v",&delta1_v)) delta1_v = 0.;
  if (!getparfloat("gamma1_v",&gamma1_v)) gamma1_v = 0.;
  if (!getparfloat("eps1_1",&eps1_1)) eps1_1 = 0;
  if (!getparfloat("delta1_1",&delta1_1)) delta1_1 = 0.;
  if (!getparfloat("gamma1_1",&gamma1_1)) gamma1_1 = 0.;
  if (!getparfloat("eps1_2",&eps1_2)) eps1_2 = 0;
  if (!getparfloat("delta1_2",&delta1_2)) delta1_2 = 0.;
  if (!getparfloat("gamma1_2",&gamma1_2)) gamma1_2 = 0.;
  if (!getparfloat("delta1_3",&delta1_3)) delta1_3 = 0.;

  if (!getparstring("hspace2",&hspace2)) hspace2 = "ISO";
  if (!getparfloat("vp2",&vp2)) vp2 = 2.5;
  if (!getparfloat("vs2",&vs2)) vs2 = 1.2;
  if (!getparfloat("rho2",&rho2)) rho2 = 3.0;
  if (!getparfloat("eps2",&eps2)) eps2 = 0;
  if (!getparfloat("delta2",&delta2)) delta2 = 0.;
  if (!getparfloat("eps2_v",&eps2_v)) eps2_v = 0;
  if (!getparfloat("delta2_v",&delta2_v)) delta2_v = 0.;
  if (!getparfloat("gamma2_v",&gamma2_v)) gamma2_v = 0.;
  if (!getparfloat("eps2_1",&eps2_1)) eps2_1 = 0;
  if (!getparfloat("delta2_1",&delta2_1)) delta2_1 = 0.;
  if (!getparfloat("gamma2_1",&gamma2_1)) gamma2_1 = 0.;
  if (!getparfloat("eps2_2",&eps2_2)) eps2_2 = 0;
  if (!getparfloat("delta2_2",&delta2_2)) delta2_2 = 0.;
  if (!getparfloat("gamma2_2",&gamma2_2)) gamma2_2 = 0.;
  if (!getparfloat("delta2_3",&delta2_3)) delta2_3 = 0.;
  
  if (!getparstring("a_file",&a_file)) a_file = "-1";  
  if (!getparfloat("fangle",&fangle)) fangle = 0.;
  if (!getparfloat("langle",&langle)) langle = 30.;
  if (!getparfloat("dangle",&dangle)) dangle = 1.;
  if (!getparfloat("fazim",&fazim)) fazim = 0.;
  if (!getparfloat("lazim",&lazim)) lazim = 0.;
  if (!getparfloat("dazim",&dazim)) dazim = 1.;
  if (!getparfloat("kappa",&kappa)) kappa = 0.;
  
  if (!getparstring("out_inf", &out_inf))  out_inf = "info.out" ;
  if (!getparstring("out_P", &out_P))  out_P = "Rpp.out" ;
  if (!getparstring("out_S", &out_S))  out_S = "Rps.out" ; 
  if (!getparstring("out_SVSH", &out_SVSH))  out_SVSH = "Rsvsh.out" ; 
  if (!getparstring("out_Error", &out_Error))  out_Error = "error.out" ; 

  checkpars();

  /* open the files and "ID" the halfspaces*/

  if (strcmp(a_file,"-1")) 
	a_file_p = efopen(a_file, "r");
  out_inf_p = efopen(out_inf, "w");
  out_P_p = efopen(out_P, "w");
  out_S_p = efopen(out_S, "w");
  out_SVSH_p = efopen(out_SVSH, "w");
  out_Error_p = efopen(out_Error, "w");


  if((!strcmp(hspace1,"ISO"))||(!strcmp(hspace1,"iso")))HSP1=0;
  if((!strcmp(hspace1,"VTI"))||(!strcmp(hspace1,"vti")))HSP1=1;
  if((!strcmp(hspace1,"HTI"))||(!strcmp(hspace1,"hti")))HSP1=2;
  if((!strcmp(hspace1,"ORT"))||(!strcmp(hspace1,"ort")))HSP1=3;
  if((!strcmp(hspace2,"ISO"))||(!strcmp(hspace2,"iso")))HSP2=0;
  if((!strcmp(hspace2,"VTI"))||(!strcmp(hspace2,"vti")))HSP2=1;
  if((!strcmp(hspace2,"HTI"))||(!strcmp(hspace2,"hti")))HSP2=2;
  if((!strcmp(hspace2,"ORT"))||(!strcmp(hspace2,"ort")))HSP2=3;


  fprintf(stdout,"LinRORT working ...\n");
  /* necessary checks */

  if((HSP1==-1)||(HSP2==-1))
	{
	err(" wrong halfspace denotation; error in: hspace1 or hspace2 parameter.  Program terminated!");
	return(-1);
	}  
  if ((fangle < 0.|| fangle >= 90.) || (langle <= 0.|| langle >= 90.)
	|| (fangle > langle) || (dangle > 1+(langle-fangle)) || (dangle == 0) ) 
	{
	err(" incidence angle from <0;90) only; error in fangle or langle or dangle.  Program terminated!");
	return (-1);
	}
  if (fazim > lazim)
	{
	err(" an error in fazim or lazim.  Program terminated!");
	return (-1);
	}
	

  /* dumping some info into the file info.out  */

  fprintf(out_inf_p," ************************ \n *** INFORMATION FILE *** \n ************************ \n \n INCIDENCE HALFSPACE 1: %s \n",hspace1);
  if (HSP1==0) fprintf(out_inf_p," Vp1=%6.3f \t Vs1=%6.3f \t rho1=%6.3f",vp1,vs1,rho1);
  if (HSP1==1) fprintf(out_inf_p," Vp1(Vp33)=%6.3f \t Vs1(Vs44)=%6.3f \t rho1=%6.3f eps1=%6.3f \t\t delta1=%6.3f ",vp1,vs1,rho1,eps1,delta1);

  if (HSP1==2) fprintf(out_inf_p," Vp1(Vp33)=%6.3f \t Vs1(Vs44)=%6.3f \t rho1=%6.3f eps1_v=%6.3f \t\t delta1_v=%6.3f \t gamma1_v=%6.3f",vp1,vs1,rho1,eps1_v,delta1_v,gamma1_v);
  if (HSP1==3) fprintf(out_inf_p," Vp1(Vp33)=%6.3f \t Vs1(Vs44)=%6.3f \t rho1=%6.3f eps1_1=%6.3f \t\t delta1_1=%6.3f \t gamma1_1=%6.3f eps1_2=%6.3f \t\t delta1_2=%6.3f \t gamma1_2=%6.3f delta1_3=%6.3f",vp1,vs1,rho1,eps1_1,delta1_1,gamma1_1,eps1_2,delta1_2,gamma1_2,delta1_3);

  fprintf(out_inf_p,"\n\n REFLECTING HALFSPACE 2: %s \n", hspace2);
  if (HSP2==0) fprintf(out_inf_p," Vp2=%6.3f \t Vs2=%6.3f \t rho2=%6.3f",vp2,vs2,rho2);
  if (HSP2==1) fprintf(out_inf_p," Vp2(Vp33)=%6.3f \t Vs2(Vs44)=%6.3f \t rho2=%6.3f eps2=%6.3f \t\t delta2=%6.3f ",vp2,vs2,rho2,eps2,delta2);
  if (HSP2==2) fprintf(out_inf_p," Vp2(Vp33)=%6.3f \t Vs2(Vs44)=%6.3f \t rho2=%6.3f eps2_v=%6.3f \t\t delta2_v=%6.3f \t gamma2_v=%6.3f",vp2,vs2,rho2,eps2_v,delta2_v,gamma2_v);
  if (HSP2==3) fprintf(out_inf_p," Vp2(Vp33)=%6.3f \t Vs2(Vs44)=%6.3f \t rho2=%6.3f eps2_1=%6.3f \t\t delta2_1=%6.3f \t gamma2_1=%6.3f eps2_2=%6.3f \t\t delta2_2=%6.3f \t gamma2_2=%6.3f delta2_3=%6.3f",vp2,vs2,rho2,eps2_1,delta2_1,gamma2_1,eps2_2,delta2_2,gamma2_2,delta2_3); 

  fprintf(out_inf_p,"\n \n INCIDENCE ANGLE AND AZIMUTH RANGE:\n");
  if (strcmp(a_file,"-1")) 
	fprintf(out_inf_p," Incidence angles and azimuths are specified in the file \"%s\", \n kappa=%7.3f \n",a_file,kappa);
 else
	fprintf(out_inf_p," fangle=%7.3f \t langle=%7.3f \t dangle=%7.3f \t fazim=%7.3f	\t lazim=%7.3f	\t dazim=%7.3f \t rotation of the halfspace 2:  kappa=%7.3f \n", fangle,langle,dangle,fazim,lazim,dazim,kappa); 


  /* initialization of anisotropic parameters */

  /* ISO media */
  if (HSP1==0)
	{
	eps1_1=0.;
	eps1_2=0.;
	delta1_1=0.;
	delta1_2=0.;
	delta1_3=0.;
	gamma1_44=0.;
	gamma1_55=0.;
	gamma1_1=0.;	
	}
  if (HSP2==0)
	{
	eps2_1=0.;
	eps2_2=0.;
	delta2_1=0.;
	delta2_2=0.;
	delta2_3=0.;
	gamma2_44=0.;
	gamma2_55=0.;
	}


  /* VTI media */
  if (HSP1==1)
	{
	eps1_1=eps1;
	eps1_2=eps1;
	delta1_1=delta1;
	delta1_2=delta1;
	delta1_3=0.;
	gamma1_44=0.;
	gamma1_55=0.;
	gamma1_1=gamma1;
	}
  if (HSP2==1)
	{
	eps2_1=eps2;
	eps2_2=eps2;
	delta2_1=delta2;
	delta2_2=delta2;
	delta2_3=0.;
	gamma2_44=0.;
	gamma2_55=0.;
	}
  

  /* HTI media */
  if (HSP1==2)
	{
	eps1_1=0.;
	eps1_2=eps1_v;
	delta1_1=0.;
	delta1_2=delta1_v;
	delta1_3=delta1_v-2*eps1_v;
	/*  exact delta_3 gives usually worse results: 
	delta1_3=(delta1_v-2*eps1_v)/(1+2*eps1_v); */
	gamma1_1=0.;
	if (HTIback1==44)
	{
	  gamma1_44=0.;
	  gamma1_55=gamma1_v;
	  count+=1; 
	  back=1;	  
	}
	if (HTIback1==55)
	{
	  gamma1_44=-gamma1_v/(1+2*gamma1_v);
	  gamma1_55=0.;
	  vs1=vs1*sqrt(1+2*gamma1_v);	  
	}
	}
  if (HSP2==2)
	{
	eps2_1=0.;
	eps2_2=eps2_v;
	delta2_1=0.;
	delta2_2=delta2_v;
	delta2_3=delta2_v-2*eps2_v;
	/*exact delta_3 gives usually worse results: 
	delta2_3=(delta2_v-2*eps2_v)/(1+2*eps2_v); */
	if (HTIback2==44)
	{
	  gamma2_44=0.;
	  gamma2_55=gamma2_v;
	  count+=1;	  
	  back=1;
	}
	if (HTIback2==55)
	{
	  gamma2_44=-gamma2_v/(1+2*gamma2_v);
	  gamma2_55=0.;
	  vs2=vs2*sqrt(1+2*gamma2_v);
	}
	}
  
  /* ORT media */
  if (HSP1==3)
	{
	if (ORTback1==44)
	{
	  gamma1_44=0.;
	  gamma1_55=gamma1_2-(gamma1_1*(1+2*gamma1_2)/(1+2*gamma1_1));
	  count+=1;	
	  back=1;	  
	}
	if (ORTback1==55)
	{
	  gamma1_44=gamma1_1-(gamma1_2*(1+2*gamma1_1)/(1+2*gamma1_2));
	  gamma1_55=0.;
	  vs1=vs1*sqrt((1+2*gamma1_2)/(1+2*gamma1_1));
	}
	}
  if(HSP2==3)
	{
	if (ORTback2==44)
	{
	  gamma2_44=0.;
	  gamma2_55=gamma2_2-(gamma2_1*(1+2*gamma2_2)/(1+2*gamma2_1));
	  count+=1;
	  back=1;
	}
	if (ORTback2==55)
	{
	  gamma2_44=gamma2_1-(gamma2_2*(1+2*gamma2_1)/(1+2*gamma2_2));
	  gamma2_55=0.;
	  vs2=vs2*sqrt((1+2*gamma2_2)/(1+2*gamma2_1));
	}
	}
  

  /* some more dumping into the file info.out  */
  
  fprintf(out_inf_p,"\n INPUT PARAMETERS FOR THE GENERAL Rpp AND Rps SUBROUTINES: \n");
  fprintf(out_inf_p," alpha1=%6.3f \t\t beta1=%6.5f \t\t rho1=%6.3f eps1_1=%6.3f \t\t delta1_1=%6.3f eps1_2=%6.3f \t\t delta1_2=%6.3f delta1_3=%6.3f gamma1_44=%6.5f \t gamma1_55=%6.5f \t gamma1_1=%6.5f \n",vp1,vs1,rho1,eps1_1, delta1_1,eps1_2,delta1_2,delta1_3,gamma1_44,gamma1_55,gamma1_1);

  fprintf(out_inf_p,"\n alpha2=%6.3f \t\t beta2=%6.5f \t\t rho2=%6.3f eps2_1=%6.3f \t\t delta2_1=%6.3f eps2_2=%6.3f \t\t delta2_2=%6.3f delta2_3=%6.3f gamma2_44=%6.5f \t gamma2_55=%6.5f \n",vp2,vs2,rho2,eps2_1, delta2_1,eps2_2,delta2_2,delta2_3,gamma2_44,gamma2_55);

  fprintf(out_inf_p,"\n ERROR MESSAGES: \n");
  
  /* some useful combinations for inversion */
  Da_a=(vp2-vp1)/(0.5*(vp2+vp1));
  DG_G=(pow(vs2,2)*rho2-pow(vs1,2)*rho1)/(0.5*(pow(vs2,2)*rho2+pow(vs1,2)*rho1));
  DZ_Z=(vp2*rho2-vp1*rho1)/(0.5*(vp2*rho2+vp1*rho1));
  alpha=0.5*(vp2+vp1);
  beta=0.5*(vs2+vs1);
  Db_b=(vs2-vs1)/(0.5*(vs2+vs1));

  PS2=0.5*((delta1_2-delta1_1+8*pow(beta/alpha,2)*gamma1_44) -
	(delta2_2-delta2_1+8*pow(beta/alpha,2)*gamma2_44)*cos(2*kappa*PI/180));	
  
		PSC=0.5*(delta2_2-delta2_1+8*pow(beta/alpha,2)*gamma2_44)*sin(2*kappa*PI/180);
  PABS=0.5*(-(delta2_2-delta2_1+8*pow(beta/alpha,2)*gamma2_44)*pow(sin(kappa*PI/180),2) +
		delta2_2-delta1_2+ Da_a - pow(2*beta/alpha,2)*DG_G);

  SS2=alpha/(2*(alpha+beta))*(delta1_2-delta1_1) + 2*beta/alpha*gamma1_44 - 
	(alpha/(2*(alpha+beta))*(delta2_2-delta2_1)+ 2*beta/alpha*gamma2_44)*cos(2*kappa*PI/180);
  SSC=(alpha/(2*(alpha+beta))*(delta2_2-delta2_1)+ 2*beta/alpha*gamma2_44)*sin(2*kappa*PI/180);
  /*  SABS=-(alpha/(2*(alpha+beta))*(delta2_2-delta2_1)+ 2*beta/alpha*gamma2_44)*pow(sin(kappa*PI/180),2) +
	alpha/(2*(alpha+beta))*(delta2_2-delta1_2)-(alpha+2*beta)/(2*alpha)*(rho2-rho1)/(0.5*(rho2+rho1))-
	2*beta/alpha*Db_b;
  */
  SABS=-(alpha/(2*(alpha+beta))*(delta2_2-delta2_1)+ 2*beta/alpha*gamma2_44)*pow(sin(kappa*PI/180),2) +
	alpha/(2*(alpha+beta))*(delta2_2-delta1_2)-0.5*(rho2-rho1)/(0.5*(rho2+rho1))-
	beta/alpha*DG_G;

  /*  some auxiliary outputs, may be useful

fprintf(stderr,"\n b_a=%f \t Da_a=%f \t Dr_r=%f \t Db_b=%f \n DG_G=%f \t 0.5*DZ_Z=%f \n PS2=%f \t PSC=%f \t PABS=%f \n SS2=%f \t SSC=%f \t SABS=%f \n", beta/alpha,Da_a,(rho2-rho1)/(0.5*(rho2+rho1)),Db_b,
DG_G,0.5*DZ_Z,PS2,PSC,PABS,SS2,SSC,SABS);
  */

  /* finally, the evaluation of the coefficients */
  
  if (!strcmp(a_file,"-1"))
	{
	for( azim = fazim; azim <= lazim; azim += dazim){
	 for( ang = fangle; ang <= langle; ang += dangle)
		{	  
		if((errp=lincoef_Rp(ang*PI/180, azim*PI/180, kappa*PI/180, &RPP, &Rp_1st, &Rp_2nd, count))!=0)
		fprintf(out_inf_p,"\n !!! WARNING: incidence ANGLE=%4.2fdeg probably approaches, or may have exceeded, the crytical angle ANGL_CR=%4.2fdeg (this is an approximate value only).\n", ang, errp);
		errs=lincoef_Rs(ang*PI/180, azim*PI/180, kappa*PI/180, &RPS1, &RPS2, &SV, &SH, &cosPHI, &sinPHI, HSP1,
			&Rsv_1st, &Rsv_2nd, &Rsh_1st, &Rsh_2nd, count);		
		fprintf(out_P_p," %f \t %f \t %f \n",ang,azim,RPP);
 count+=1;
		if (errs >= 0)
		{
		 fprintf(out_S_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,RPS1,RPS2,cosPHI,sinPHI);
		 fprintf(out_SVSH_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,SV,SH,cosPHI,sinPHI);	
		}
		else if (errs==-1)
		{
		fprintf(out_S_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,RPS1,RPS2,cosPHI,sinPHI);
		fprintf(out_SVSH_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,SV,SH,cosPHI,sinPHI);		
		fprintf(out_inf_p,"\n !!! WARNING: incidence ANGLE=%4.2fdeg is essentially at a singular point of S-wave propagation. However, the Rps computation is completed. \n", ang);
		}				
		else if (errs==-2)
		{
		 fprintf(out_inf_p,"\n !!! WARNING: incidence ANGLE=%4.2f is essentially at a singular point of S-wave propagation, the computation of Rps is terminated for this angle (ORT inc. halfspace).\n", ang);
		}
		}	 
	}
	}  
	else
	{
	while(fscanf(a_file_p,"%f %f",&ang,&azim)!=EOF)
	 {
		if (ang >= 90.) continue;
		if ((errp=lincoef_Rp(ang*PI/180, azim*PI/180, kappa*PI/180, &RPP, &Rp_1st, &Rp_2nd, count))!=0.)
		fprintf(out_inf_p,"\n !!! WARNING: incidence ANGLE=%4.2fdeg probably approaches, or may have exceeded, the crytical angle ANGL_CR=%4.2fdeg (this is an approximate value only).\n", ang, errp);
		errs=lincoef_Rs(ang*PI/180, azim*PI/180, kappa*PI/180, &RPS1, &RPS2, &SV, &SH, &cosPHI, &sinPHI, HSP1, &Rsv_1st, &Rsv_2nd, &Rsh_1st, &Rsh_2nd, count);
		fprintf(out_P_p," %f \t %f \t %f \n",ang,azim,RPP);
		count+=1;
		if (errs >= 0.)
		{
		fprintf(out_S_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,RPS1,RPS2,cosPHI,sinPHI);
		fprintf(out_SVSH_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,SV,SH,cosPHI,sinPHI);
		}
		else if (errs==-1)
		{
		fprintf(out_S_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,RPS1,RPS2,cosPHI,sinPHI);
		fprintf(out_SVSH_p," %f \t %f \t %f \t %f \t %f \t %f \n",ang,azim,SV,SH,cosPHI,sinPHI);		
		fprintf(out_inf_p,"\n !!! WARNING: incidence ANGLE=%4.2fdeg is essentially at a singular point of S-wave propagation. However, the Rps computation is completed. \n", ang);
		}				
		else if (errs==-2)
		{
		fprintf(out_inf_p,"\n !!! WARNING: incidence ANGLE=%4.2fdeg azimuth AZIM=%4.2fdeg is essentially at a singular point of S-wave propagation, the computation of Rps is terminated for this angle (ORT inc. halfspace). \n", ang,azim);
		}		
	 }
	}
  

  /* Almost done - filling up the out_Error file */

  fprintf(out_Error_p,"************************************************************* \n");
  fprintf(out_Error_p,"******************* FAST TEST OF ACCURACY ******************* \n");
  fprintf(out_Error_p,"************************************************************* \n");
  fprintf(out_Error_p,"(The test is based on the comparison of 1st and 2nd order terms.) \n");
  fprintf(out_Error_p,"(The test applies for the S-wave projection \"back=0\" only.) \n");


  if(back == 0){	


	/* P COEFFICIENT FIRST */

	fprintf(out_Error_p,"\n Rpp COEFFICIENT:  \n");
	fprintf(out_Error_p,"****************** \n");

	/* P isotropic */
	fprintf(out_Error_p,"\n A) Isotropic component (due to velocity and density contrasts):  \n");
	fprintf(out_Error_p,"\n Inc. Angle: \t \t	15deg	20deg	25deg	30deg	35deg \n");
	fprintf(out_Error_p," ----------------------------------------------------------------------------- \n");
	fprintf(out_Error_p," Exact: \t \t %7.4f	%7.4f	%7.4f	%7.4f	%7.4f \n",
		Rp_2nd.iso[0],Rp_2nd.iso[1],Rp_2nd.iso[2],Rp_2nd.iso[3], Rp_2nd.iso[4]);
	fprintf(out_Error_p," Approximation: \t %7.4f	%7.4f	%7.4f	%7.4f	%7.4f	\n",
		Rp_1st.iso[0],Rp_1st.iso[1],Rp_1st.iso[2],Rp_1st.iso[3], Rp_1st.iso[4]);
	fprintf(out_Error_p," ------------------------------------------------------------------------------ \n");

	Max=0.;  
	for(n=0; n < 5 ; n++){
	if(fabs(Rp_2nd.iso[n]) > 0.0001){
	Iso_e[n]=(Rp_1st.iso[n]/Rp_2nd.iso[n]-1)*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f	%6.1f	%6.1f	\n", Iso_e[0], Iso_e[1], Iso_e[2], Iso_e[3]);
	/*if(fabs(Max) >= 15.) 
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpp: isotropic component: maximum error %6.1f(in percent) exceeds the desired accuracy \n", Max);
	}*/
	
	/* P upper halfspace */
	fprintf(out_Error_p,"\n B) Anisotropic incidence halfspace component: (tested in the vicinity of inc. angle=%6.2fdeg)  \n", Rp_1st.angle[0]*180/PI);
	fprintf(out_Error_p,"\n Azimuth: \t \t	0deg	90deg \n");
	fprintf(out_Error_p," --------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f \n", Rp_1st.upper[0],Rp_1st.upper[1]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f  \n", Rp_2nd.upper[0],Rp_2nd.upper[1]);
	fprintf(out_Error_p," --------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 2 ; n++){
	if(fabs(Rp_1st.upper[n]) > 0.0001){
	Iso_e[n]=(Rp_2nd.upper[n]/fabs(Rp_1st.upper[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f  \n \n", Iso_e[0], Iso_e[1]);
 /*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpp: aniso component: inc. hlfspc: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
 }*/

	/* P lower */
	fprintf(out_Error_p,"\n C) Anisotropic reflecting halfspace component: (tested in the vicinity of inc. angle=%6.2fdeg)  \n", Rp_1st.angle[0]*180/PI);
	fprintf(out_Error_p,"\n Azimuth: \t \t	0deg	90deg \n");
	fprintf(out_Error_p," --------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f \n", Rp_1st.lower[0],Rp_1st.lower[1]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f  \n", Rp_2nd.lower[0],Rp_2nd.lower[1]);
	fprintf(out_Error_p," --------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 2 ; n++){
	if(fabs(Rp_1st.lower[n]) > 0.0001){
	Iso_e[n]=(Rp_2nd.lower[n]/fabs(Rp_1st.lower[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in  percent): \t  %6.1f	%6.1f  \n \n", Iso_e[0], Iso_e[1]); 
/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpp: aniso component: refl. hlfspc: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
	}*/

	/* P global */
	fprintf(out_Error_p,"\n D) Global error due to anisotropy:  (tested in the vicinity of inc. angle=%6.2fdeg and for kappa=%6.2fdeg )\n", Rp_1st.angle[0]*180/PI, kappa);
	fprintf(out_Error_p,"\n Azimuth: \t \t	0deg	30deg	60deg	90deg  \n");
	fprintf(out_Error_p," ---------------------------------------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f	%8.5f	%8.5f \n", 
		Rp_1st.global[0],Rp_1st.global[2],Rp_1st.global[3],Rp_1st.global[1]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f	%8.5f	%8.5f  \n", 
		Rp_2nd.global[0],Rp_2nd.global[2],Rp_2nd.global[3],Rp_2nd.global[1]);
	fprintf(out_Error_p," ---------------------------------------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 4 ; n++){
	if(fabs(Rp_1st.global[n]) > 0.0001){
	Iso_e[n]=(Rp_2nd.global[n]/fabs(Rp_1st.global[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f	%6.1f	%6.1f  \n \n", 
		Iso_e[0], Iso_e[2],Iso_e[3],Iso_e[1]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpp: aniso component: global: maximum error %6.1f(in percent) exceeds the desired accuracy
	\n \n", Max);
	}*/


	
	/* SV COMPONENT SECOND */

	fprintf(out_Error_p,"\n Rpsv COEFFICIENT:  \n");
	fprintf(out_Error_p,"******************* \n");

	/* SV isotropic */
	fprintf(out_Error_p,"\n A) Isotropic component (due to velocity and density contrasts):  \n");
	fprintf(out_Error_p,"\n Inc. Angle: \t \t	15deg	20deg	25deg	30deg	35deg \n");
	fprintf(out_Error_p," ----------------------------------------------------------------------------- \n");
	fprintf(out_Error_p," Exact: \t \t %7.4f	%7.4f	%7.4f	%7.4f	%7.4f \n",
		Rsv_2nd.iso[0],Rsv_2nd.iso[1],Rsv_2nd.iso[2],Rsv_2nd.iso[3], Rsv_2nd.iso[4]);
	fprintf(out_Error_p," Approximation: \t %7.4f	%7.4f	%7.4f	%7.4f	%7.4f	\n",
		Rsv_1st.iso[0],Rsv_1st.iso[1],Rsv_1st.iso[2],Rsv_1st.iso[3], Rsv_1st.iso[4]);
	fprintf(out_Error_p," ------------------------------------------------------------------------------ \n");

	Max=0.;  
	for(n=0; n < 5 ; n++){
	if(fabs(Rsv_2nd.iso[n]) > 0.0001){
	Iso_e[n]=(Rsv_1st.iso[n]/Rsv_2nd.iso[n]-1)*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f	%6.1f	%6.1f	%6.1f \n \n",
		Iso_e[0], Iso_e[1], Iso_e[2], Iso_e[3], Iso_e[4]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsv: isotropic component: maximum error %6.1f(in percent) exceeds the desired accuracy 
	\n", Max);
	}*/
	
 
	/* SV upper halfspace */
	fprintf(out_Error_p,"\n B) Anisotropic incidence halfspace component: (tested in the vicinity of inc. angle=%6.2fdeg)  \n", Rsv_1st.angle[0]*180/PI);
	fprintf(out_Error_p,"\n Azimuth: \t \t	0deg	90deg \n");
	fprintf(out_Error_p," --------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f \n", Rsv_1st.upper[0],Rsv_1st.upper[1]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f  \n", Rsv_2nd.upper[0],Rsv_2nd.upper[1]);
	fprintf(out_Error_p," --------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 2 ; n++){
	if(fabs(Rsv_1st.upper[n]) > 0.0001){
	Iso_e[n]=(Rsv_2nd.upper[n]/fabs(Rsv_1st.upper[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f  \n \n", Iso_e[0], Iso_e[1]);
 /*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsv: aniso component: inc. hlfspc: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
 }*/

	/* SV lower */
	fprintf(out_Error_p,"\n C) Anisotropic reflecting halfspace component: (tested in the vicinity of inc. angle=%6.2fdeg)  \n", Rsv_1st.angle[0]*180/PI);
	fprintf(out_Error_p,"\n Azimuth: \t \t	0deg	90deg \n");
	fprintf(out_Error_p," --------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f \n", Rsv_1st.lower[0],Rsv_1st.lower[1]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f  \n", Rsv_2nd.lower[0],Rsv_2nd.lower[1]);
	fprintf(out_Error_p," --------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 2 ; n++){
	if(fabs(Rsv_1st.lower[n]) > 0.0001){
	Iso_e[n]=(Rsv_2nd.lower[n]/fabs(Rsv_1st.lower[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f  \n \n", Iso_e[0], Iso_e[1]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsv: aniso component: refl. hlfspc: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
	}*/

	/* SV global */
	fprintf(out_Error_p,"\n D) Global error due to anisotropy:  (tested in the vicinity of inc. angle=%6.2fdeg and for kappa=%6.2fdeg )\n", Rsv_1st.angle[0]*180/PI, kappa);
	fprintf(out_Error_p,"\n Azimuth: \t \t	0deg	30deg	60deg	90deg  \n");
	fprintf(out_Error_p," ---------------------------------------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f	%8.5f	%8.5f \n", 
		Rsv_1st.global[0],Rsv_1st.global[2],Rsv_1st.global[3],Rsv_1st.global[1]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f	%8.5f	%8.5f  \n", 
		Rsv_2nd.global[0],Rsv_2nd.global[2],Rsv_2nd.global[3],Rsv_2nd.global[1]);
	fprintf(out_Error_p," ---------------------------------------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 4 ; n++){
	if(fabs(Rsv_1st.global[n]) > 0.0001){
	Iso_e[n]=(Rsv_2nd.global[n]/fabs(Rsv_1st.global[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f	%6.1f	%6.1f  \n \n", Iso_e[0], Iso_e[2],Iso_e[3],Iso_e[1]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsv: aniso component: global: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
	}*/



	/*SH COMPONENT*/

	fprintf(out_Error_p,"\n Rpsh COEFFICIENT:  \n");
	fprintf(out_Error_p,"******************* \n");	

	/* SH upper halfspace */
	fprintf(out_Error_p,"\n A) Anisotropic incidence halfspace component: (tested in the vicinity of inc. angle=%6.2fdeg)  \n", Rsh_1st.angle[0]*180/PI);
	fprintf(out_Error_p,"\n Azimuth: \t \t	45deg	\n");
	fprintf(out_Error_p," ----------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f  \n", Rsh_1st.upper[0]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f \n", Rsh_2nd.upper[0]);
	fprintf(out_Error_p," ----------------------------------\n");

	Max=0.;  
	for(n=0; n < 1 ; n++){
	if(fabs(Rsh_1st.upper[n]) > 0.0001){
	Iso_e[n]=(Rsh_2nd.upper[n]/fabs(Rsh_1st.upper[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	\n \n", Iso_e[0]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsh: aniso component: inc. hlfspc: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
  }*/

	/* SH lower */
	fprintf(out_Error_p,"\n B) Anisotropic reflecting halfspace component: (tested in the vicinity of inc. angle=%6.2fdeg)  \n", Rsh_1st.angle[0]*180/PI);
	fprintf(out_Error_p,"\n Azimuth: \t \t	45deg	\n");
	fprintf(out_Error_p," ----------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f  \n", Rsh_1st.lower[0]);
	fprintf(out_Error_p," Second Order: \t \t %8.5f \n", Rsh_2nd.lower[0]);
	fprintf(out_Error_p," ----------------------------------\n");

	Max=0.;  
	for(n=0; n < 1 ; n++){
	if(fabs(Rsh_1st.lower[n]) > 0.0001){
	Iso_e[n]=(Rsh_2nd.lower[n]/fabs(Rsh_1st.lower[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f  \n \n", Iso_e[0]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsh: aniso component: refl. hlfspc: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
	}*/

	/* SH global */
	fprintf(out_Error_p,"\n C) Global error due to anisotropy:  (tested in the vicinity of inc. angle=%6.2fdeg and for kappa=%6.2fdeg )\n", Rsh_1st.angle[0]*180/PI, kappa);
	fprintf(out_Error_p,"\n Azimuth: \t \t	20deg	45deg	70deg	\n");
	fprintf(out_Error_p," -----------------------------------------------------------\n");
	fprintf(out_Error_p," First Order: \t \t %8.5f	%8.5f	%8.5f	\n", Rsh_1st.global[0], Rsh_1st.global[1], Rsh_1st.global[2] );
 fprintf(out_Error_p," Second Order: \t \t %8.5f	%8.5f	%8.5f  \n", Rsh_2nd.global[0], Rsh_2nd.global[1], Rsh_2nd.global[2]);
	fprintf(out_Error_p," -----------------------------------------------------------\n");

	Max=0.;  
	for(n=0; n < 3 ; n++){
	if(fabs(Rsh_1st.global[n]) > 0.0001){
	Iso_e[n]=(Rsh_2nd.global[n]/fabs(Rsh_1st.global[n]))*100.;
	Max = fabs(Max) > fabs(Iso_e[n]) ? Max : Iso_e[n];
	}
	else{
	Iso_e[n]=999.9;
	}
	}
	fprintf(out_Error_p," Rel. Error (in percent): \t  %6.1f	%6.1f	%6.1f	\n \n", Iso_e[0], Iso_e[1], Iso_e[2]);
	/*if(fabs(Max) >= 15.) {
	fprintf(stdout,"\n !!! ACCURACY-ERROR MESSAGE !!! \n");
	fprintf(stdout," Rpsh: aniso component: global: maximum error %6.1f(in percent) exceeds the desired accuracy \n \n", Max);
	}*/
	fprintf(out_Error_p,"\n \n END OF FILE \n");	

  }
  else{
	fprintf(out_Error_p,"\n THE ERROR ESTIMATES CANNOT BE EVALUATED FOR THE S-WAVE BACKGROUND VELOCITIES CORRESPONDING TO A44 (APPLIES TO BOTH HALFSPACES) \n");
  }  
  /* done with the out_Error error file */


  /* last step - closing the files */

  if (strcmp(a_file,"-1"))  
	fclose(a_file_p);
  fclose(out_inf_p);
  fclose(out_P_p);
  fclose(out_S_p);
  fclose(out_SVSH_p);
  fclose(out_Error_p);

  fprintf(stdout,"		...LinRORT done.\n");
  
  /* we are done */
 
	return(CWP_Exit());
}

/************************************************************************/
/************************ END OF THE SOURCE FILE ************************/
/************************************************************************/


