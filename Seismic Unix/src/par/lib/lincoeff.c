/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"
/*********************** self documentation **********************/
/************************************************************************
LINCOEFF - subroutines to create linearized reflection coefficients
          for ISO, VTI, HTI, or ORTHO media.

lincoef_Rp - weak constrast-weak-anisotropy Rpp reflection coefficient
              for an arbitrary combination of ISO, VTI, HTI, and ORTHO
              halfspaces
lincoef_Rs - weak constrast-weak-anisotropy Rps reflection coefficient
              for an arbitrary combination of ISO, VTI, HTI, and ORTHO
              halfspaces

Function prototypes:
float lincoef_Rp(float ang, float azim, float kappa, float *rpp, ErrorFlag *rp_1st, ErrorFlag *rp_2nd, int count)

***********************************************************************
lincoef_Rp:
Input:
ang	phase incidence angle (in rad)
azim	phase azimuthal angle (in rad)
kappa	mutual rotation of the reflecting halfspace with respect
        to the incidence halfspace (in rad)
count ... calling number of the subroutine Rp. If, and only if, count=1 
        (first call) then the error quantities *rp_1st and *rp_2nd are
	evaluated.

Output:
*rpp		Rpp(ang,azim,kappa, el.param.) reflection coefficient
*rp_1st ... ErrorFlag structure: contains information on first-order
            Rpp reflection coefficient evaluated for the purpose of
	    error analysis.
*rp_2nd ... ErrorFlag structure: contains information on semi 
            second-order Rpp reflection coefficient evaluated for 
	    the purpose of error analysis. 
err 	0...usual output
    	ANG_CR...incidence angle approaches the crytical angle ANG_CR
***********************************************************************
Notes:

***********************************************************************
Author: CWP: Petr Jilek,  December 1999.
**********************************************************************/
/**************** end self doc ********************************/

#define err_ang 30.*PI/180
#define LC_TINY FLT_EPSILON

int P_err_2nd_order(ErrorFlag *rp_1st, ErrorFlag *rp_2nd, 
				float true_kappa, int index);

float lincoef_Rp(float ang, float azim, float kappa, float *rpp, ErrorFlag *rp_1st, ErrorFlag *rp_2nd, int count)

     /* The subroutine for computation of weak-contrast-weak-anisotropy
	Rpp reflection coefficient for an arbitrary combination
	of ISO, VTI, HTI or ORTHORHOMBIC halfspaces, see adopted
	convention described in the main LinRORT.

	INPUT:
	ang = phase incidence angle (in rad)
	azim = phase azimuthal angle (in rad)
	kappa = mutual rotation of the reflecting halfspace with respect
	        to the incidence halfspace (in rad)
	count ... calling number of the subroutine Rp. If, and only if, count=1 
	          (first call) then the error quantities *rp_1st and *rp_2nd are
		  evaluated.

	External medium parameters of the both halfspaces: they must be
	pre-processed, i.e. correctly assigned for particular halfspaces. 
	There is no an appropriate check guard in this subroutine.

	OUTPUT:
	*rpp = Rpp(ang,azim,kappa, el.param.) reflection coefficient
	*rp_1st ... ErrorFlag structure: contains information on first-order
	            Rpp reflection coefficient evaluated for the purpose of
		    error analysis.
	*rp_2nd ... ErrorFlag structure: contains information on semi 
	            second-order Rpp reflection coefficient evaluated for 
		    the purpose of error analysis. 

	The error estimate is based on the comparison of the 1st and 2nd order 
	quantities from the structures *rp_1st and *rp_2nd.

	struct ErrorFlag
	        {
		   float iso[5];      ... isotropic part of Rpp for 5 different inc. angles
	           float upper[2];    ... ANISO/ISO Rpp for azimuths 0deg and 90 deg - upper hsp contribution
	           float lower[2];    ... ISO/ANISO Rpp for azimuths 0deg and 90 deg - lower hsp contribution
	           float global[4];   ... ANISO/ANISO Rpp for four azimuths - global model
	           float angle[4];    ... inc. angle, two azimuths and kappa for which the quantities
		                          above are evaluated
	        }; 


	err = 0...usual output
            = ANG_CR...incidence angle approaches the crytical angle ANG_CR
	      (approximately ang>=0.7*ANG_CR)
	      */
{

  extern float vp1,vp2,vs1,vs2,rho1,rho2;
  extern float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_44,gamma1_55;
  extern float eps2_1,eps2_2,delta2_1,delta2_2,delta2_3,gamma2_44,gamma2_55;
  
  float err=0., ANG_CR;                               /* to control thinks */
  float Da_a, DG_G, alpha, beta, a_k;     /* to simplify thinks */
  float abs_lincoeff, S2, S2T2;                                /* to compute thinks */
 

  
  /* first, an approximate check for the crytical angle */

  if (vp1 <= vp2)
    {
      if (ang >= 0.7*(ANG_CR=asin(vp1/vp2)))
	err=ANG_CR;
    }

  /* some quantities in advance */

  Da_a=(vp2-vp1)/(0.5*(vp2+vp1));
  DG_G=(pow(vs2,2)*rho2-pow(vs1,2)*rho1)/(0.5*(pow(vs2,2)*rho2+pow(vs1,2)*rho1));
  /*-- DG_G=(rho2-rho1)/(0.5*(rho2+rho1)) + 2*(vs2-vs1)/2.85; --*/
  alpha=0.5*(vp2+vp1);
  beta=0.5*(vs2+vs1);
  

  /* first call => error quantities determination */
  if(count==1) {
    	P_err_2nd_order(rp_1st, rp_2nd, kappa, 1);
    	if(kappa != 0.) P_err_2nd_order(rp_1st, rp_2nd, kappa, 2);
    }


  /*----- tests for different ratios alpha/beta -------
    fprintf(stderr,"alpha_orig=%f \t beta_orig=%f \t a/b_orig=%f \n",alpha,beta,beta/alpha);
    alpha=3.5;
    beta=1.9214265;
    vole
    fprintf(stderr,"alpha=%f \t beta=%f \t a/b=%f \n",alpha,beta,beta/alpha);
    ----------------------------------------------------*/


  /* now, the individual parts of the coefficient */

  a_k=azim-kappa;

  abs_lincoeff = (vp2*rho2 - vp1*rho1)/(vp2*rho2 + vp1*rho1);
  
  S2 = 0.5*(
	    Da_a - pow(2*beta/alpha,2)*DG_G + delta2_1*pow(sin(a_k),2) + delta2_2*pow(cos(a_k),2) -
	    delta1_1*pow(sin(azim),2) - delta1_2*pow(cos(azim),2) -  8*pow(beta/alpha,2)*(
				 gamma2_55*pow(cos(a_k),2) + gamma2_44*pow(sin(a_k),2) -
				 gamma1_55*pow(cos(azim),2) - gamma1_44*pow(sin(azim),2))
				 );
  
  /* -----------  no iso terms here 
     S2 = 0.5*(
	    delta2_1*pow(sin(a_k),2) + delta2_2*pow(cos(a_k),2) -
	    delta1_1*pow(sin(azim),2) - delta1_2*pow(cos(azim),2) -  8*pow(beta/alpha,2)*(
				 gamma2_55*pow(cos(a_k),2) + gamma2_44*pow(sin(a_k),2) -
				 gamma1_55*pow(cos(azim),2) - gamma1_44*pow(sin(azim),2))
				 );
  ----------------------------------*/

  S2T2 = 0.5*(
	      Da_a + 
	      eps2_1*pow(sin(a_k),4) + eps2_2*(pow(cos(a_k),4)+2*pow(cos(a_k),2)*pow(sin(a_k),2)) +
	      delta2_3*pow(cos(a_k),2)*pow(sin(a_k),2) -
	      eps1_1*pow(sin(azim),4) - eps1_2*(pow(cos(azim),4)+2*pow(sin(azim),2)*pow(cos(azim),2)) -
	      delta1_3*pow(cos(azim),2)*pow(sin(azim),2)
	      );
  

  /* finally, make the coefficient */

  /*  fprintf(stderr,"IN_ANG=%f AZIM=%f abs_lincoeff=%f \t S2=%f \t S2T2=%f \n",ang*180./PI,azim*180./PI,abs_lincoeff,S2,S2T2);
   */

  /*fprintf(stderr,"ang=%f azim=%f S3/S1=%f \n",ang*180/PI,azim*180/PI,S2T2/S2);*/

  *rpp = abs_lincoeff + S2*pow(sin(ang),2) + S2T2*pow(sin(ang),2)*pow(tan(ang),2);
  

  return(err*180/PI);
}

/********************************************/
/****************** the end *****************/
/********************************************/


float lincoef_Rs(float ang, float azim, float kappa, float *rps1, float *rps2, 
	 float *sv, float *sh, float *cphi, float *sphi, int i_hsp,
	 ErrorFlag *rsv_1st, ErrorFlag *rsv_2nd, ErrorFlag *rsh_1st, ErrorFlag *rsh_2nd, int count)

     /* The subroutine for computation of weak-contrast-weak-anisotropy
	Rps reflection coefficients for an arbitrary combination
	of ISO, VTI, HTI or ORTHORHOMBIC halfspaces, see adopted
	convention described in the main LinRORT.

	INPUT:
	ang = phase incidence angle (in rad)
	azim = phase azimuthal angle (in rad)
	kappa = mutual rotation of the reflecting halfspace with respect
	        to the incidence halfspace (in rad)
	i_hsp=0...iso, =1...VTI, =2...HTI, =3...ORT; the type of upper (incidence)
	      halfspace (strictly, this parameter would not be necesary, but knowing
	      the type of the halfspace in advance simplifies this subroutines)
	count ... calling number of the subroutine Rs. If, and only if, count=1 
	          (first call) then the error quantities *rsv_1st, *rsv_2nd, *rsh_1st  
		  and *rsh_2nd are evaluated.

	external medium parameters of the both halfspaces: they must be
	pre-processed, i.e. correctly assigned for all possible halfspaces. 
	There is no a check for that in this subroutine.

	OUTPUT:
	*rps1 = Rps1(ang,azim,kappa,el.parameters) reflection coefficient
	*rps2 = Rps2(ang,azim,kappa,el.parameters) reflection coefficient
	*sv = Rpsv(ang,azim,kappa,el.parameters) reflection coefficient
	*sh = Rpsh(ang,azim,kappa,el.parameters) reflection coefficient
	*cphi = cosine of rotation angle PHI, characterizing an approximate
	        rotation of *rps1 from *sv direction 
	*sphi = sine of rotation angle PHI, characterizing an approximate
	        rotation of *rps1 from *sv direction

	           the following holds:
	           rps1 = sv*cos(phi) + sh*sin(phi)
	           rps2 = - sv*sin(phi) + sh*cos(phi)


	*rsv_1st, *rsh_1st ... ErrorFlag structure: contains information on first-order
	                       Rpsv, Rpsh reflection coefficients evaluated for the purpose of
		               error analysis.
	*rsv_2nd, *rsh_2nd ... ErrorFlag structure: contains information on semi 
	                       second-order Rpsv, Rpsh reflection coefficient evaluated for 
		               the purpose of error analysis. 

	The error estimate is based on the comparison of the 1st and 2nd order 
	quantities from the structures *rsv_1st and *rsv_2nd, and *rsh_1st and *rsh_2nd.

	struct ErrorFlag
	        {
		   float iso[5];      ... isotropic part of Rpsv for 5 different inc. angles (for Rpsh, iso[n]=0)
	           float upper[2];    ... ANISO/ISO Rpsv (Rpsh) for azimuths 0deg and 90 deg (45deg) 
		                           - upper halfspace contribution
	           float lower[2];    ... ISO/ANISO Rpsv (Rpsh) for azimuths 0deg and 90 deg (45deg)
		                          - lower halfspace contribution
	           float global[4];   ... ANISO/ANISO Rpsv (Rpsh) for four (three) azimuths
		                          - global model
	           float angle[4];    ... inc. angle, azimuths and kappa for which the quantities
		                          above are evaluated
	        }; 


	err = 0...usual output
            = ANG_CR...inc. ang approaches the crytical angle ANG_CR 
	      (approximately ang>=0.7*ANG_CR)
	    =-1...inc.angle is a singular point (or very close to it: Vs1^2-Vs2^2<=1E-6 Km/s).
	          However, the incidence halfspace is ISO or VTI or HTI, so the 
		  computation of Rps is completed.
            =-2...inc.angle is a singular point (or very close to it: Vs1^2-Vs2^2<=1E-6 Km/s).
	          Incidence halfspace is ORT, so the computation of Rps is terminated
		  (-1 and -2 have priority over ANG_CR)
      */

{

  extern float vp1,vp2,vs1,vs2,rho1,rho2;
  extern float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_44,gamma1_55;
  extern float eps2_1,eps2_2,delta2_1,delta2_2,delta2_3,gamma2_44,gamma2_55;

  float err=0.,ANG_CR;  
  int perr;

  float alpha, beta, Da_a, Dr_r, Db_b, CS, S_C, CS3, S3_C, S5_C, S, CS_C, CS3_C, S3;
  float cos2a, sin2a, cos2a_k, sin2a_k, DG_G;
  
  
  int polar(float ang, float azim, float *cphi, float *sphi, int i_hsp);
  int S_err_2nd_order(ErrorFlag *rsv_1st, ErrorFlag *rsv_2nd, ErrorFlag *rsh_1st, ErrorFlag *rsh_2nd, 
		       float true_kappa, int index);

  /* first, an approximate check for the crytical angle */

  if (vp1 <= vp2)
    {
      if (ang >= 0.7*(ANG_CR=asin(vp1/vp2)))
	err=ANG_CR*180/PI;
    }

  /* compute cosine and sine of the rotation angle PHI */

  perr=polar(ang, azim, cphi, sphi, i_hsp);
  if (perr) err=perr;

  if (err == -2) return(err);
  
  /* now, determine the SV and SH terms in Rps coefficients */

  alpha = 0.5*(vp2+vp1);
  beta = 0.5*(vs2+vs1);
  Dr_r = (rho2-rho1)/(0.5*(rho2+rho1));
  Db_b = (vs2-vs1)/beta;
  Da_a = (vp2-vp1)/alpha;  
  cos2a_k = pow(cos(azim-kappa),2);
  sin2a_k = pow(sin(azim-kappa),2);
  cos2a = pow(cos(azim),2);
  sin2a = pow(sin(azim),2);

  /*** for DG_G insted of Dr_R + 2Db_b ****/
  DG_G=(pow(vs2,2)*rho2-pow(vs1,2)*rho1)/(0.5*(pow(vs2,2)*rho2+pow(vs1,2)*rho1)); 

  if(count==1) {
    S_err_2nd_order(rsv_1st, rsv_2nd, rsh_1st, rsh_2nd, kappa, 1);
    if(kappa != 0.) S_err_2nd_order(rsv_1st, rsv_2nd, rsh_1st, rsh_2nd, kappa, 2);
     ;
  }
  
  /*------ tests for different ratios alpha/beta -------
    fprintf(stderr,"alpha_orig=%f \t beta_orig=%f \t a/b_orig=%f \n",alpha,beta,beta/alpha);
  alpha=3.5;
  beta=1.9214265;
  vole
  delta2_3=-0.22;
  fprintf(stderr,"alpha=%f \t beta=%f \t a/b=%f \n",alpha,beta,beta/alpha);
      ------------------------------------------------------------------*/


  /*------------------      
  CS = -beta/alpha*Dr_r - 
    (alpha*beta)/(2*(pow(alpha,2)-pow(beta,2)))*(delta2_2*cos2a_k +
						 delta2_1*sin2a_k -
						 delta1_1*sin2a -
						 delta1_2*cos2a )
    - 2*beta/alpha*(gamma2_55*cos2a_k + gamma2_44*sin2a_k -
		    gamma1_55*cos2a - gamma1_44*sin2a +	Db_b);
  -----------------*/

  /*  for DG_G insted of Dr_R + 2Db_b: */
  CS = -beta/alpha*DG_G - 
      (alpha*beta)/(2*(pow(alpha,2)-pow(beta,2)))*(delta2_2*cos2a_k +
      delta2_1*sin2a_k -
      delta1_1*sin2a -
      delta1_2*cos2a )
      -2*beta/alpha*(gamma2_55*cos2a_k + gamma2_44*sin2a_k -
      gamma1_55*cos2a - gamma1_44*sin2a );
  

  
  S_C = -0.5*Dr_r +
    pow(alpha,2)/(2*(pow(alpha,2)-pow(beta,2)))*(delta2_2*cos2a_k +
						 delta2_1*sin2a_k -
						 delta1_2*cos2a -
						 delta1_1*sin2a);

  CS3 = -alpha*beta/(pow(alpha,2)-pow(beta,2))*(delta1_1*sin2a + 
						delta1_2*cos2a -
						delta1_3*cos2a*sin2a -
						eps1_2*(cos2a*cos2a+2*cos2a*sin2a) -
						eps1_1*sin2a*sin2a +
						eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) +
						eps2_1*sin2a_k*sin2a_k -
						delta2_1*sin2a_k -
						delta2_2*cos2a_k +
						delta2_3*cos2a_k*sin2a_k);
   
  /*
    S3_C = pow(beta,2)/pow(alpha,2)*Dr_r +
    pow(beta,2)/(2*(pow(alpha,2)-pow(beta,2)))*(delta1_1*sin2a +
						delta1_2*cos2a -
						delta2_1*sin2a_k -
						delta2_2*cos2a_k) +
    pow(alpha,2)/(pow(alpha,2)-pow(beta,2))*(delta1_1*sin2a +
					     delta1_2*cos2a -
					     delta1_3*cos2a*sin2a -
					     eps1_1*sin2a*sin2a -
					     eps1_2*(cos2a*cos2a+2*cos2a*sin2a) +
					     eps2_1*sin2a_k*sin2a_k +
					     eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) -
					     delta2_1*sin2a_k -
					     delta2_2*cos2a_k +
					     delta2_3*cos2a_k*sin2a_k) +
    2*pow(beta/alpha,2)*(gamma2_55*cos2a_k +
			 gamma2_44*sin2a_k -
			 gamma1_55*cos2a -
			 gamma1_44*sin2a + Db_b); 
  
  ************* for DG_G insted of Dr_R + 2Db_b:  */
 
  
  S3_C = pow(beta,2)/pow(alpha,2)*DG_G +
    pow(beta,2)/(2*(pow(alpha,2)-pow(beta,2)))*(delta1_1*sin2a +
						delta1_2*cos2a -
						delta2_1*sin2a_k -
						delta2_2*cos2a_k) +
    pow(alpha,2)/(pow(alpha,2)-pow(beta,2))*(delta1_1*sin2a +
					     delta1_2*cos2a -
					     delta1_3*cos2a*sin2a -
					     eps1_1*sin2a*sin2a -
					     eps1_2*(cos2a*cos2a+2*cos2a*sin2a) +
					     eps2_1*sin2a_k*sin2a_k +
					     eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) -
					     delta2_1*sin2a_k -
					     delta2_2*cos2a_k +
					     delta2_3*cos2a_k*sin2a_k) +
    2*pow(beta/alpha,2)*(gamma2_55*cos2a_k +
			 gamma2_44*sin2a_k -
			 gamma1_55*cos2a -
			 gamma1_44*sin2a);
  
  

  S5_C = -pow(beta,2)/(pow(alpha,2)-pow(beta,2))*(delta1_1*sin2a +
						  delta1_2*cos2a -
						  delta1_3*cos2a*sin2a -
						  eps1_1*sin2a*sin2a -
						  eps1_2*(cos2a*cos2a+2*cos2a*sin2a) +
						  eps2_1*sin2a_k*sin2a_k +
						  eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) -
						  delta2_1*sin2a_k -
						  delta2_2*cos2a_k +
						  delta2_3*cos2a_k*sin2a_k);
  
  S = pow(alpha,2)/(4*(pow(alpha,2)-pow(beta,2)))*((delta2_2-delta2_1)*sin(2*(azim-kappa)) +
						   (delta1_1-delta1_2)*sin(2*azim));

  CS_C = alpha*beta/(4*(pow(alpha,2)-pow(beta,2)))*((delta2_1-delta2_2)*sin(2*(azim-kappa)) +
						    (delta1_2-delta1_1)*sin(2*azim)) +
    beta/alpha*((gamma2_44-gamma2_55)*sin(2*(azim-kappa)) -
		(gamma1_44-gamma1_55)*sin(2*azim));
  
  CS3_C = alpha*beta/(2*(pow(alpha,2)-pow(beta,2)))*((delta1_1-delta1_2-delta1_3*(cos2a-sin2a) +
						      2*(eps1_2-eps1_1)*sin2a)*sin(azim)*cos(azim) -
						     (delta2_1-delta2_2-delta2_3*(cos2a_k-sin2a_k) +
						      2*(eps2_2-eps2_1)*sin2a_k)*sin(azim-kappa)*cos(azim-kappa));
  
  S3 = pow(alpha,2)/(2*(pow(alpha,2)-pow(beta,2)))*((-delta1_1+delta1_2+delta1_3*(cos2a-sin2a) -
					       2*(eps1_2-eps1_1)*sin2a)*sin(azim)*cos(azim) +
					      (delta2_1-delta2_2-delta2_3*(cos2a_k-sin2a_k) +
					       2*(eps2_2-eps2_1)*sin2a_k)*sin(azim-kappa)*cos(azim-kappa));


  *sv= CS * cos(ang) * sin(ang)
    + S_C * sin(ang)
    + CS3 * cos(ang)*pow(sin(ang),3)
    + (S3_C + S_C*0.5*pow(beta/alpha,2)) * pow(sin(ang),3)
    + (S5_C + S3_C*0.5*pow(beta/alpha,2)) * pow(sin(ang),5)
    + S5_C*0.5*pow(beta/alpha,2) * pow(sin(ang),7);
  

  *sh= S * sin(ang)
    + CS_C * cos(ang)*sin(ang)
    + (CS3_C + CS_C*0.5*pow(beta/alpha,2)) * cos(ang)*pow(sin(ang),3)
    + S3 * pow(sin(ang),3)
    + CS3_C*0.5*pow(beta/alpha,2) * cos(ang)*pow(sin(ang),5);
    
  /*  fprintf(stderr,"vp=%f vs=%f \n",alpha,beta); 
  fprintf(stderr,"ang=%f azim=%f k=%f sv=%f S=%f CSCj=%f S3=%f S3CCj=%f \n",ang*180/PI,azim*180/PI,kappa*180/PI,*sv,S,CS_C,S3,CS3_C);
  
  
    fprintf(stderr,"ang=%f azim=%f k=%f sv=%f CS=%f SCj=%f CS3=%f S3Cj=%f S5Cj=%f \n",ang*180/PI,azim*180/PI,kappa*180/PI,*sv,CS,S_C,CS3,S3_C,S5_C);  
  */

  /*    fprintf(stderr,"IN_ANG=%f AZIM=%f Da=%f \t Dr=%f \t Db=%f \t a/b=%f \n",ang*180./PI,azim*180./PI,Da_a,Dr_r,Db_b,beta/alpha);
   */
  

  /* finnaly, complete the Rps1, Rps2 */
  
  /*  fprintf(stderr,"ang=%f azim=%f S3/S1=%f \n",ang*180/PI,azim*180/PI,(CS3+S3_C+S_C*0.5*pow(beta/alpha,2)-0.5*CS)/(CS+S_C));  
   */

  *rps1 = (*sv)*(*cphi) + (*sh)*(*sphi);
  *rps2 = -(*sv)*(*sphi) + (*sh)*(*cphi);
  
  return(err);
}

/******************************/
/*******    the end   *********/
/******************************/



int P_err_2nd_order(ErrorFlag *rp_1st, ErrorFlag *rp_2nd, float true_kappa, int index)


     /* The subroutine evaluates first order (exactly) and second order (approximately)
	Rpp coefficients for a given incidence angle and azimuths. The second order evaluation
	is based on the numerical modeling and comparing of the exact Rpp and its first order 
	approximation (so called semi-2nd order). The comparison has been done for the incidence 
	angle of 30 deg and it should be most accurate for this angle. However, such an estimated 
	semi-2nd order Rpp usually resulted in a global increase of accuracy of the first-order Rpp, 
	so the subroutine can be used for other angles, as a rough error estimate, as well.

	INPUT:
	true_kappa ... mutual rotation of the upper and lower halfspaces in rad. This angle affects
	               the global error evaluation only.
	index ... =1, =2: if the true_kappa=0, then the subroutine should be called just once
		          with index=1. If the true_kappa is not zero, then the
		          subroutine needs to be called the second time (index=2) in
		          order to properly evaluate the GLOBAL error. No other values of
			  the "index" parameter are allowed. If some other value is on the
			  entry, the subroutine is terminated immediately with the value -1.
	err_ang ... as #define. The incidence angle for which the error analysis is
	                        performed. Not recommended to change the current value of
				30deg by a large ammount.
	lkappa, kkappa ... in declaration. Two azimuths for which the global error is evaluated
	                                   besides 0deg and 90deg; their values can be changed arbitrary, 
					   but do not forget to change your output information as well 
					   (i.e. give proper azimuths for the output file corresponding to
					   *rp_1st(2nd).global[2],[3]).

	OUTPUT:
	*rp_1st ... ErrorFlag structure: contains information on first-order
	            Rpp reflection coefficient evaluated for the purpose of
		    error analysis.
	*rp_2nd ... ErrorFlag structure: contains information on semi 
	            second-order Rpp reflection coefficient evaluated for 
		    the purpose of error analysis. 

        	struct ErrorFlag
	         {
		    float iso[5];      ... isotropic part of Rpp for 5 different inc. angles
	            float upper[2];    ... ANISO/ISO Rpp for azimuths 0deg and 90 deg - upper hsp contribution
	            float lower[2];    ... ISO/ANISO Rpp for azimuths 0deg and 90 deg - lower hsp contribution
	            float global[4];   ... ANISO/ANISO Rpp for azimuths 0deg and 90 deg - global model
	            float angle[4];    ... inc. angle, two azimuths and kappa for which the quantities
		                           above are evaluated
	          }, 
 	 where  
         iso[0]-iso[4] ... isotropic part of Rpp for the following inc. angles (due to velocity
	                   and density contrasts): 15deg, 20deg, 25deg, 30deg and 35deg. 
		 	   If a change is required, seek the paragraph "ISO part" in this subroutine.
        upper[0],upper[1] ... Rpp coefficients for azimuths 0deg and 90deg, respectively, computed
                              for the ANISO/ISO configuration of the halfspaces.
        lower[0],lower[1] ... Rpp coefficients for azimuths 0deg and 90deg, respectively, computed
                              for the ISO/ANISO configuration of the halfspaces.
        global[0]-[3] ... Rpp coefficients for ANISO/ANISO configuration evaluated in the following
                          azimuths, respectively: 0deg, 90deg, lkappa, kkappa.
        angle[0]-[3] ... inc. angle (err_ang), 0deg, 90deg, kappa.

      P_err_2nd_order = 0 ... usual termination
                      =-1 ... unusual termination (wrong input)
       
     */
{

  extern float vp1,vp2,vs1,vs2,rho1,rho2;
  extern float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_44;
  extern float eps2_1,eps2_2,delta2_1,delta2_2,delta2_3,gamma2_44;
  
  float Da_a, Dz_z, DG_G, Db_b, Dr_r, alpha, beta, azim;
  float Delta1_1, Delta1_2, Delta2_1, Delta2_2, Gamma1_44, Gamma2_44;  
  float D1_1, D1_2, D2_1, D2_2, GS_1, GS_2;
  float ISO_2nd;
  float d11_plus=0., d11_min=0., d12_plus=0., d12_min=0., gs1_plus=0., gs1_min=0.;
  float d21_plus=0., d21_min=0., d22_plus=0., d22_min=0., gs2_plus=0., gs2_min=0.;
  float a_plus=0., a_min=0, b_plus=0., b_min=0., r_plus=0., r_min=0. ;

  float Fst0_0, Fst0_90, Fst90_0, Fst90_90, FstK_0, FstK_90, FstKK_0, FstKK_90;
  float Fst0_kappa, Fst90_kappa, FstK_kappa, FstKK_kappa;
  float Snd0_0, Snd0_90, Snd90_0, Snd90_90, SndK_0, SndK_90, SndKK_0, SndKK_90;
  float Snd0_kappa, Snd90_kappa, SndK_kappa, SndKK_kappa;  
  float err_kappa=0.0, lkappa= 30.*PI/180, kkappa= 60.*PI/180, D;
  
  int N=0;  
    
  float P_term(float Da_a, float Dz_z, float DG_G, 
	       float Delta1_1, float Delta1_2, float Delta1_3,
	       float Delta2_1, float Delta2_2, float Delta2_3,  
	       float Eps1_1, float Eps1_2, float Eps2_1, float Eps2_2, 
	       float Gamma1_44, float Gamma2_44, float alpha, float beta, 
	       float ang, float azim, float a_k);

  float Iso_exact(int type, float vp1, float vs1, float rho1, 
		  float vp2, float vs2, float rho2, float ang);  
  
  

  /* some useful quantities in advance */

  Da_a=(vp2-vp1)/(0.5*(vp2+vp1));
  Db_b=(vs2-vs1)/(0.5*(vs2+vs1));
  Dz_z=(vp2*rho2 - vp1*rho1)/(vp2*rho2 + vp1*rho1);
  DG_G=(pow(vs2,2)*rho2-pow(vs1,2)*rho1)/(0.5*(pow(vs2,2)*rho2+pow(vs1,2)*rho1));
  Dr_r=(rho2-rho1)/(0.5*(rho2+rho1));
  alpha=0.5*(vp2+vp1);
  beta=0.5*(vs2+vs1);
    
  if(index == 1) err_kappa=0.;
  if(index == 2) err_kappa=PI/2.;
  if((index != 1) && (index != 2)) return(-1);

  if(delta1_1) d11_plus=(fabs(delta1_1)+delta1_1)/(2*fabs(delta1_1));  
  if(delta1_1) d11_min=(fabs(delta1_1)-delta1_1)/(2*fabs(delta1_1));
  if(delta1_2) d12_plus=(fabs(delta1_2)+delta1_2)/(2*fabs(delta1_2));
  if(delta1_2) d12_min=(fabs(delta1_2)-delta1_2)/(2*fabs(delta1_2));
  if(gamma1_44) gs1_plus=(fabs(gamma1_44)+gamma1_44)/(2*fabs(gamma1_44));
  if(gamma1_44) gs1_min=(fabs(gamma1_44)-gamma1_44)/(2*fabs(gamma1_44));

  if(delta2_1) d21_plus=(fabs(delta2_1)+delta2_1)/(2*fabs(delta2_1));  
  if(delta2_1) d21_min=(fabs(delta2_1)-delta2_1)/(2*fabs(delta2_1));
  if(delta2_2) d22_plus=(fabs(delta2_2)+delta2_2)/(2*fabs(delta2_2));
  if(delta2_2) d22_min=(fabs(delta2_2)-delta2_2)/(2*fabs(delta2_2));
  if(gamma2_44) gs2_plus=(fabs(gamma2_44)+gamma2_44)/(2*fabs(gamma2_44));
  if(gamma2_44) gs2_min=(fabs(gamma2_44)-gamma2_44)/(2*fabs(gamma2_44));

  if(Da_a) a_plus=(fabs(Da_a)+Da_a)/(2*fabs(Da_a));
  if(Da_a) a_min=(fabs(Da_a)-Da_a)/(2*fabs(Da_a));
  if(Db_b) b_plus=(fabs(Db_b)+Db_b)/(2*fabs(Db_b));
  if(Db_b) b_min=(fabs(Db_b)-Db_b)/(2*fabs(Db_b));
  if(Dr_r) r_plus=(fabs(Dr_r)+Dr_r)/(2*fabs(Dr_r));
  if(Dr_r) r_min=(fabs(Dr_r)-Dr_r)/(2*fabs(Dr_r));


  /* the angles used for the anisotropic accuracy evaluation */

  (*rp_1st).angle[0]=(*rp_2nd).angle[0] = err_ang;        /* incidence angle */
  (*rp_1st).angle[1]=(*rp_2nd).angle[1] = 0.*PI/180;      /* first azimuth */
  (*rp_1st).angle[2]=(*rp_2nd).angle[2] = 90.*PI/180;     /* second azimuth */ 
  (*rp_1st).angle[3]=(*rp_2nd).angle[3] = true_kappa;      /* kappa */



  /* ISO part first */
  
  for(N = 0; N < 5; N++)
    {
      (*rp_1st).iso[N]=P_term(Da_a, Dz_z, DG_G, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 
			      0., 0., alpha, beta, (15+5*N)*PI/180, 0., 0.);
      (*rp_2nd).iso[N]=Iso_exact(0, vp1, vs1, rho1, vp2, vs2, rho2, (15+5*N)*PI/180);
    }

  /* get the ISO semi-2nd order for the inc. angle err_ang */
  ISO_2nd = Iso_exact(0, vp1, vs1, rho1, vp2, vs2, rho2, err_ang) - 
            P_term(Da_a, Dz_z, DG_G, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 
		   0., 0., alpha, beta, err_ang, 0., 0.);


  /* UPPER halfspace contribution */

  Delta1_1=(-0.45*d11_plus  -0.50*d11_min)*pow(delta1_1,2) + 
    (-0.22*d11_plus*gs1_plus+0*d11_plus*gs1_min-0.93*d11_min*gs1_plus-0.33*d11_min*gs1_min)*delta1_1*gamma1_44 + 
    (0.74*d11_plus*a_plus + 0.59*d11_plus*a_min + 2.56*d11_min*a_plus + 0.31*d11_min*a_min)*delta1_1*Da_a + 
    (0.19*d11_plus*b_plus + 0*d11_plus*b_min + 1.27*d11_min*b_plus + 0.31*d11_min*b_min)*delta1_1*Db_b + 
    (0*d11_plus*r_plus + 0.18*d11_plus*r_min + 1.27*d11_min*r_plus + 0.95*d11_min*r_min)*delta1_1*Dr_r;

  D1_1=Delta1_1;
  
  Delta1_2=(-0.45*d12_plus -0.50*d12_min)*pow(delta1_2,2) +
    (0.74*d12_plus*a_plus + 0.59*d12_plus*a_min + 2.56*d12_min*a_plus + 0.31*d12_min*a_min)*delta1_2*Da_a + 
    (0.19*d12_plus*b_plus + 0*d12_plus*b_min + 1.27*d12_min*b_plus + 0.31*d12_min*b_min)*delta1_2*Db_b + 
    (0*d12_plus*r_plus + 0.18*d12_plus*r_min + 1.27*d12_min*r_plus + 0.95*d12_min*r_min)*delta1_2*Dr_r;
							  

  D1_2=Delta1_2;  

  Delta2_1=0.;
  Delta2_2=0.;

  Gamma1_44=((1./3.)*gs1_plus + 0.55*gs1_min)*pow(gamma1_44,2) +
    (0.92*gs1_plus*a_plus + 0.9*gs1_plus*a_min + 1.27*gs1_min*a_plus + 0.98*gs1_min*a_min)*gamma1_44*Da_a +
    (-1.74*gs1_plus*b_plus -2.1*gs1_plus*b_min -1.84*gs1_min*b_plus -1.91*gs1_min*b_min)*gamma1_44*Db_b +
    (-1.02*gs1_plus*r_plus -1.04*gs1_plus*r_min -0.9*gs1_min*r_plus -0.9*gs1_min*r_min)*gamma1_44*Dr_r;
								
  GS_1=Gamma1_44;
  
  Gamma2_44=0.;

  if(index == 1)
    {      
      azim=0.;
      (*rp_1st).upper[0]=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, 0., 0., 0.,
				eps1_1, eps1_2, 0., 0.,
				gamma1_44, 0., alpha, beta, err_ang, azim, 0.);
      (*rp_2nd).upper[0]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0., 0., 0., 0., 0., 
				Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.) +
	                 ISO_2nd;
      
      azim=90.*PI/180;  
      (*rp_1st).upper[1]=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, 0., 0., 0.,
				eps1_1, eps1_2, 0., 0.,
				gamma1_44, 0., alpha, beta, err_ang, azim, 0.);
      (*rp_2nd).upper[1]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0., 0., 0., 0., 0.,
				Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.) +
                         ISO_2nd;
      
    }
  


  /* LOWER halfspace contribution */

  Delta1_1=0.;
  Delta1_2=0.;

  Delta2_1=(-0.44*d21_plus -0.89*d21_min)*pow(delta2_1,2) + 
    (-(1./3.)*d21_plus*gs2_plus+0*d21_plus*gs2_min-0.65*d21_min*gs2_plus+0*d21_min*gs2_min)*delta2_1*gamma2_44+ 
    (1.83*d21_plus*a_plus +1.43*d21_plus*a_min +2.56*d21_min*a_plus +1.35*d21_min*a_min)*delta2_1*Da_a + 
    (-0.37*d21_plus*b_plus -0.59*d21_plus*b_min -0.55*d21_min*b_plus -0.07*d21_min*b_min)*delta2_1*Db_b + 
    (0*d21_plus*r_plus +0*d21_plus*r_min -0.13*d21_min*r_plus +0.10*d21_min*r_min)*delta2_1*Dr_r;


  D2_1=Delta2_1;
  
  Delta2_2=(-0.44*d22_plus -0.89*d22_min)*pow(delta2_2,2) +
    (1.83*d22_plus*a_plus +1.43*d22_plus*a_min +2.56*d22_min*a_plus +1.35*d22_min*a_min)*delta2_2*Da_a + 
    (-0.37*d22_plus*b_plus -0.59*d22_plus*b_min -0.55*d22_min*b_plus -0.07*d22_min*b_min)*delta2_2*Db_b + 
    (0*d22_plus*r_plus + 0*d22_plus*r_min -0.13*d22_min*r_plus + 0.10*d22_min*r_min)*delta2_2*Dr_r;
							   
  D2_2=Delta2_2;
  
  Gamma1_44=0.;
  Gamma2_44=(-0.37*gs2_plus -0.5*gs2_min)*pow(gamma2_44,2) +
    (0*gs2_plus*a_plus +1.05*gs2_plus*a_min +0.83*gs2_min*a_plus +0.83*gs2_min*a_min)*gamma2_44*Da_a +
    (-0.46*gs2_plus*b_plus +0.23*gs2_plus*b_min +0.24*gs2_min*b_plus +0.38*gs2_min*b_min)*gamma2_44*Db_b +
    (-0.13*gs2_plus*r_plus +0.07*gs2_plus*r_min +0*gs2_min*r_plus +0.24*gs2_min*r_min)*gamma2_44*Dr_r;
							    
  GS_2=Gamma2_44;
  
  if(index == 1)
    {     
      azim=0.;
      (*rp_1st).lower[0]=P_term(Da_a, Dz_z, DG_G, 0., 0., 0., delta2_1, delta2_2, delta2_3,
				0., 0., eps2_1, eps2_2,
				0., gamma2_44, alpha, beta, err_ang, 0., azim);
      (*rp_2nd).lower[0]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				0., 0., 0., 0.,
				Gamma1_44, Gamma2_44, alpha, beta, err_ang, 0., azim) +
	                 ISO_2nd;
      
      
      azim=90.*PI/180;  
      (*rp_1st).lower[1]=P_term(Da_a, Dz_z, DG_G, 0., 0., 0., delta2_1, delta2_2, delta2_3,
				0., 0., eps2_1, eps2_2,
				0., gamma2_44, alpha, beta, err_ang, 0., azim);
      (*rp_2nd).lower[1]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				0., 0., 0., 0.,
				Gamma1_44, Gamma2_44, alpha, beta, err_ang, 0., azim) +
	                 ISO_2nd;
      
      
    }
  
  /* GLOBAL contribution*/
  
  /* The following terms are derived on the assumption that Rpp coefficients for
     two identicaly anisotropic halfspaces are zero (with 0 velocity contrats)
  */
  
  Delta1_1=D1_1 +
           (0.45*d11_plus*d21_plus -0.45*d11_plus*d21_min -0.50*d11_min*d21_plus +0.50*d11_min*d21_min)*
                                                             delta1_1*delta2_1*pow(cos(err_kappa),2) + 
           (0.45*d11_plus*d22_plus -0.45*d11_plus*d22_min -0.50*d11_min*d22_plus +0.50*d11_min*d22_min)* 
                                                             delta1_1*delta2_2*pow(sin(err_kappa),2) +
           (0.22*d11_plus*gs2_plus +0.*d11_plus*gs2_min +0.93*d11_min*gs2_plus +0.33*d11_min*gs2_min)*
	                                                     delta1_1*gamma2_44*pow(cos(err_kappa),2);
  
  Delta1_2=D1_2 + 
           (0.45*d12_plus*d21_plus -0.45*d12_plus*d21_min -0.50*d12_min*d21_plus +0.50*d12_min*d21_min)*
                                                             delta1_2*delta2_1*pow(sin(err_kappa),2) + 
           (0.45*d12_plus*d22_plus -0.45*d12_plus*d22_min -0.50*d12_min*d22_plus +0.50*d12_min*d22_min)* 
                                                             delta1_2*delta2_2*pow(cos(err_kappa),2) +
           (0.22*d12_plus*gs2_plus +0.*d12_plus*gs2_min +0.93*d12_min*gs2_plus +0.33*d12_min*gs2_min)*
                                                             delta1_2*gamma2_44*pow(sin(err_kappa),2); 

  Delta2_1=D2_1 + 
           (0.44*d21_plus*d11_plus -0.44*d21_plus*d11_min -0.89*d21_min*d11_plus +0.89*d21_min*d11_min)*
                                                             delta2_1*delta1_1*pow(cos(err_kappa),2) + 
           (0.44*d21_plus*d12_plus -0.44*d21_plus*d12_min -0.89*d21_min*d12_plus +0.89*d21_min*d12_min)* 
                                                             delta2_1*delta1_2*pow(sin(err_kappa),2) +
           ((1./3.)*d21_plus*gs1_plus + 0.*d21_plus*gs1_min +0.65*d21_min*gs1_plus +0.*d21_min*gs1_min)*
                                                             delta2_1*gamma1_44*pow(cos(err_kappa),2);
  

  Delta2_2=D2_2 + 
           (0.44*d22_plus*d11_plus -0.44*d22_plus*d11_min -0.89*d22_min*d11_plus +0.89*d22_min*d11_min)*
                                                             delta2_2*delta1_1*pow(sin(err_kappa),2) + 
           (0.44*d22_plus*d12_plus -0.44*d22_plus*d12_min -0.89*d22_min*d12_plus +0.89*d22_min*d12_min)* 
                                                             delta2_2*delta1_2*pow(cos(err_kappa),2) +
           ((1./3.)*d22_plus*gs1_plus + 0.*d22_plus*gs1_min +0.65*d22_min*gs1_plus +0.*d22_min*gs1_min)*
                                                             delta2_2*gamma1_44*pow(sin(err_kappa),2);


  Gamma1_44=GS_1 + 
            (0.0*gs1_plus*d21_plus +0.0*gs1_plus*d21_min +0.0*gs1_min*d21_plus +0.0*gs1_min*d21_min)*
                                                             gamma1_44*delta2_1*pow(cos(err_kappa),2) +
            (0.0*gs1_plus*d22_plus +0.0*gs1_plus*d22_min +0.0*gs1_min*d22_plus +0.0*gs1_min*d22_min)*
                                                             gamma1_44*delta2_2*pow(sin(err_kappa),2) +
            (-(1./3.)*gs1_plus*gs2_plus -0.75*gs1_plus*gs2_min -0.95*gs1_min*gs2_plus -0.55*gs1_min*gs2_min)*
                                                             gamma1_44*gamma2_44*pow(cos(err_kappa),2);
  
  Gamma2_44=GS_2 +
            (0.37*gs2_plus*gs1_plus +0.*gs2_plus*gs1_min +0.*gs2_min*gs1_plus +0.5*gs2_min*gs1_min)*
                                                            gamma2_44*gamma1_44*pow(cos(err_kappa),2);

  /* The following terms are based on error minimization for 2-parameter model only.
     Notice that such 2nd order terms do not result in Rpp=0 for two identically
     anisotropic halfspaces (probably not convenient to use).
  
  
   Delta1_1=D1_1 +
           (0.45*d11_plus*d21_plus -0.45*d11_plus*d21_min -0.50*d11_min*d21_plus +0.50*d11_min*d21_min)*
                                                             delta1_1*delta2_1*pow(cos(err_kappa),2) + 
           (0.45*d11_plus*d22_plus -0.45*d11_plus*d22_min -0.50*d11_min*d22_plus +0.50*d11_min*d22_min)* 
                                                             delta1_1*delta2_2*pow(sin(err_kappa),2) +
           (0*d11_plus*gs2_plus +0.1*d11_plus*gs2_min +0.8*d11_min*gs2_plus +1.1*d11_min*gs2_min)*
	                                                     delta1_1*gamma2_44*pow(cos(err_kappa),2);
  
  Delta1_2=D1_2 + 
           (0.45*d12_plus*d21_plus -0.45*d12_plus*d21_min -0.50*d12_min*d21_plus +0.50*d12_min*d21_min)*
                                                             delta1_2*delta2_1*pow(sin(err_kappa),2) + 
           (0.45*d12_plus*d22_plus -0.45*d12_plus*d22_min -0.50*d12_min*d22_plus +0.50*d12_min*d22_min)* 
                                                             delta1_2*delta2_2*pow(cos(err_kappa),2) +
           (0*d12_plus*gs2_plus +0.1*d12_plus*gs2_min +0.8*d12_min*gs2_plus +1.1*d12_min*gs2_min)*
                                                             delta1_2*gamma2_44*pow(sin(err_kappa),2); 

  Delta2_1=D2_1 + 
           (0.44*d21_plus*d11_plus -0.44*d21_plus*d11_min -0.89*d21_min*d11_plus +0.89*d21_min*d11_min)*
                                                             delta2_1*delta1_1*pow(cos(err_kappa),2) + 
           (0.44*d21_plus*d12_plus -0.44*d21_plus*d12_min -0.89*d21_min*d12_plus +0.89*d21_min*d12_min)* 
                                                             delta2_1*delta1_2*pow(sin(err_kappa),2);
  

  Delta2_2=D2_2 + 
           (0.44*d22_plus*d11_plus -0.44*d22_plus*d11_min -0.89*d22_min*d11_plus +0.89*d22_min*d11_min)*
                                                             delta2_2*delta1_1*pow(sin(err_kappa),2) + 
           (0.44*d22_plus*d12_plus -0.44*d22_plus*d12_min -0.89*d22_min*d12_plus +0.89*d22_min*d12_min)* 
                                                             delta2_2*delta1_2*pow(cos(err_kappa),2);


  Gamma1_44=GS_1 + 
            (0.1*gs1_plus*d21_plus +0.22*gs1_plus*d21_min +0.2*gs1_min*d21_plus +0.55*gs1_min*d21_min)*
                                                             gamma1_44*delta2_1*pow(cos(err_kappa),2) +
            (0.1*gs1_plus*d22_plus +0.22*gs1_plus*d22_min +0.2*gs1_min*d22_plus +0.55*gs1_min*d22_min)*
                                                             gamma1_44*delta2_2*pow(sin(err_kappa),2) +
            (-(1./3.)*gs1_plus*gs2_plus -0.75*gs1_plus*gs2_min -0.95*gs1_min*gs2_plus -0.55*gs1_min*gs2_min)*
                                                             gamma1_44*gamma2_44*pow(cos(err_kappa),2);
  
  Gamma2_44=GS_2 +
            (0.37*gs2_plus*gs1_plus +0*gs2_plus*gs1_min +0*gs2_min*gs1_plus +0.5*gs2_min*gs1_min)*
                                                            gamma2_44*gamma1_44*pow(cos(err_kappa),2);
  
  */
  
  if(index == 1)

    /* here, err_kappa=0: evaluate the error for alligned halfspaces */

    {
      azim=0.;
      (*rp_1st).global[0]=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      (*rp_2nd).global[0]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	                  ISO_2nd;
      

      azim=90.*PI/180;
      (*rp_1st).global[1]=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      (*rp_2nd).global[1]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	                  ISO_2nd;
      
      azim=lkappa;
      (*rp_1st).global[2]=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      (*rp_2nd).global[2]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	                  ISO_2nd;
      
      
      azim=kkappa;
      (*rp_1st).global[3]=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      (*rp_2nd).global[3]=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	                  ISO_2nd;
      
    }
  
  Fst0_0=(*rp_1st).global[0];
  Snd0_0=(*rp_2nd).global[0];
  Fst90_0=(*rp_1st).global[1];
  Snd90_0=(*rp_2nd).global[1];
  FstK_0=(*rp_1st).global[2];
  SndK_0=(*rp_2nd).global[2];
  FstKK_0=(*rp_1st).global[3];
  SndKK_0=(*rp_2nd).global[3];

  if(index == 2)

    /* here, err_kappa = 90.: evaluate the error for the halfspaces rotated by 90 deg */

    {    
      azim=0.;  
      Fst0_90=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      Snd0_90=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	     ISO_2nd;
      Fst0_kappa=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-true_kappa);
      
      
      azim=90.*PI/180;  
      Fst90_90=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);  
      Snd90_90=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	       ISO_2nd;
      Fst90_kappa=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-true_kappa);


      azim=lkappa;
      FstK_90=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      SndK_90=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	      ISO_2nd;
      FstK_kappa=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-true_kappa);
       

      azim=kkappa;
      FstKK_90=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa);
      SndKK_90=P_term(0., 0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				 0., 0., 0., 0.,
				 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, azim-err_kappa) +
	       ISO_2nd;
      FstKK_kappa=P_term(Da_a, Dz_z, DG_G, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				 eps1_1, eps1_2, eps2_1, eps2_2,
				 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, azim-true_kappa);
       
      /* interpolate the error for the halfspaces rotated by the angle true_kappa */

      if(fabs(D=Fst0_90-Fst0_0) < LC_TINY){
	Snd0_kappa = 0.5*(Snd0_0 + Snd0_90);
      }
      else {
	Snd0_kappa = Snd0_0 + ((Snd0_90-Snd0_0)/D)*(Fst0_kappa-Fst0_0);
      }

      if(fabs(D=Fst90_90-Fst90_0) < LC_TINY) {
	Snd90_kappa = 0.5*(Snd90_0 + Snd90_90);
      }
      else {
	Snd90_kappa = Snd90_0 + ((Snd90_90-Snd90_0)/D)*(Fst90_kappa-Fst90_0);
      }

      if(fabs(D=FstK_90-FstK_0) < LC_TINY) {
	SndK_kappa =0.5*(SndK_90 + SndK_0);
      }
      else {
	SndK_kappa = SndK_0 + ((SndK_90-SndK_0)/D)*(FstK_kappa-FstK_0);
      }
      
      if(fabs(D=FstKK_90-FstKK_0) < LC_TINY) {
	SndKK_kappa = 0.5*(SndKK_90 + SndKK_0);
      }
      else {
	SndKK_kappa = SndKK_0 + ((SndKK_90-SndKK_0)/D)*(FstKK_kappa-FstKK_0);
      }
      
      (*rp_1st).global[0]=Fst0_kappa;
      (*rp_2nd).global[0]=Snd0_kappa;
      (*rp_1st).global[1]=Fst90_kappa;
      (*rp_2nd).global[1]=Snd90_kappa;
      (*rp_1st).global[2]=FstK_kappa;
      (*rp_2nd).global[2]=SndK_kappa;
      (*rp_1st).global[3]=FstKK_kappa;
      (*rp_2nd).global[3]=SndKK_kappa;      

    }
  
			     
  return (0);
  
}

/*****************************/
/********* the end ***********/
/*****************************/


float P_term(float Da_a, float Dz_z, float DG_G, 
	     float Delta1_1, float Delta1_2, float Delta1_3,
	     float Delta2_1, float Delta2_2, float Delta2_3,  
	     float Eps1_1, float Eps1_2, float Eps2_1, float Eps2_2, 
	     float Gamma1_44, float Gamma2_44, float alpha, float beta, 
	     float ang, float azim, float a_k)

     /* This subroutine evaluates individual angular terms (inc. angle)  
	for Rpp 1st-order approximation. The input represents the medium parameters 
	(Thomsen's type aniso parameters (GammaX_44 represents GammaX_s), plus 
	S-wave impedance contrast ratio DG_G, density contrast ratio Dr_r and 
	background P- and S-wave velocities), and incidence angle "ang",
	azimuth "azim" and rotation angle "kappa", all being phase angles in radians.
     */

{
  float absolute, sn_2, sn_2_tg_2;
  
  absolute = Dz_z;

  sn_2 = 0.5*(
	    Da_a - pow(2*beta/alpha,2)*DG_G + Delta2_1*pow(sin(a_k),2) + Delta2_2*pow(cos(a_k),2) -
            Delta1_1*pow(sin(azim),2) - Delta1_2*pow(cos(azim),2) -  8*pow(beta/alpha,2)*(
	    Gamma2_44*pow(sin(a_k),2) - Gamma1_44*pow(sin(azim),2)));

  sn_2_tg_2 = 0.5*(Da_a +
		   Eps2_1*pow(sin(a_k),4) + Eps2_2*(pow(cos(a_k),4)+2*pow(cos(a_k),2)*pow(sin(a_k),2)) +
		   Delta2_3*pow(cos(a_k),2)*pow(sin(a_k),2) -
		   Eps1_1*pow(sin(azim),4) - Eps1_2*(pow(cos(azim),4)+2*pow(sin(azim),2)*pow(cos(azim),2)) -
		   Delta1_3*pow(cos(azim),2)*pow(sin(azim),2));
  
  /*fprintf(stderr,"dela1_1=%f delta1_2=%f gamma1s=%f delta2_1=%f delta2_2=%f gamma2s=%f \n",
    Delta1_1,Delta1_2,Gamma1_44,Delta2_1,Delta2_2,Gamma2_44);*/
  

  return(absolute + sn_2*pow(sin(ang),2) + sn_2_tg_2*pow(sin(ang),2)*pow(tan(ang),2));

}

/*****************************/
/******* the end *************/
/*****************************/



  float Iso_exact(int type, float vp1, float vs1, float rho1, 
		  float vp2, float vs2, float rho2, float ang)  

     /* The subroutine returns the exact Rpp (type=0) or Rps (type=1) 
	real-valued reflection coefficient evaluated for a horizontal 
	interface separating two isotropic halfspaces. If the coefficient 
	is to be complex, the value -999.9 is returned. The subroutine 
	returns the value 1000 if the wave type "type" is chosen incorrectly.

	The other quantities are:
	vp1 ... P-wave velocity in the incidence halfspace
	vs1 ... S-wave velocity in the incidence halfspace
	rho1 ... density of the incidence halfspace
	vp2 ... P-wave velocity in the reflecting halfspace
	vs2 ... S-wave velocity in the reflecting halfspace
	rho2 ... density of the reflecting halfspace
	ang ... incidence angle in radians
     */

{
  float p, P1, P2, P3, P4;
  float q, X, Y, Z, D;
  float COEF;
  
  
  /***** some auxiliary quantities ***/

  p=sin(ang)/vp1;
  if((P1=1-pow(vp1*p,2)) < 0.) return(-999.9);
  if((P2=1-pow(vs1*p,2)) < 0.) return(-999.9);
  if((P3=1-pow(vp2*p,2)) < 0.) return(-999.9);
  if((P4=1-pow(vs2*p,2)) < 0.) return(-999.9);
  P1=sqrt(P1);
  P2=sqrt(P2);
  P3=sqrt(P3);
  P4=sqrt(P4);

  q=2*(rho2*pow(vs2,2) - rho1*pow(vs1,2));
  X=rho2 - q*pow(p,2);
  Y=rho1 + q*pow(p,2);
  Z=rho2 - rho1 - q*pow(p,2);
  
  D=pow(q*p,2)*P1*P2*P3*P4 +
    rho1*rho2*(vp1*vs2*P2*P3 + vs1*vp2*P1*P4) +
    vp1*vs1*P3*P4*pow(Y,2) +
    vp2*vs2*P1*P2*pow(X,2) +
    vp1*vp2*vs1*vs2*pow(p,2)*pow(Z,2);
  
  /*** the coefficient ***/

  if(type == 0)
    {
      COEF = (pow(q*p,2)*P1*P2*P3*P4 +
	      rho1*rho2*(vs1*vp2*P1*P4 - vp1*vs2*P2*P3) -
	      vp1*vs1*P3*P4*pow(Y,2) +
	      vp2*vs2*P1*P2*pow(X,2) -
	      vp1*vp2*vs1*vs2*pow(p,2)*pow(Z,2))/D;    
    }
  else if(type == 1) 
    {
      COEF = (-2*vp1*p*P1*(q*P3*P4*Y + vp2*vs2*X*Z))/D;
    }
  else 
    {
      COEF = 1000.;
    }
  
  return (COEF);
  
}


int polar(float ang, float azim, float *cphi, float *sphi, int i_hsp)

     /* cos(PHI) and sin(PHI) of rotation angle PHI describing
	a rotation of the polarization vector of S1 (S2)  wave
	in the isotropic slowness plane:

	ang = INCIDENCE phase angle (from the vertical) in rad
	azim = azimuth in rad
	cphi=cos(PHI)
	sphi=sin(PHI)
	i_hsp=0,1,2,3 for ISO,VTI,HTI,ORT respectively
	Phi si counted in right-hand Cartesian c.s. SV-SH,
	SV pointing downward. Positive Phi is from SV->SH.

	the subroutine requires the generalized medium parameters
	as the function LinRps, plus the parameter gamma1_1, which 
	enter the function as extern 

        perr=0 O.K.
	=-1 a singular point in VTI or HTI, computation completed
	=-2 a singular point in ORT, computation terminated 
	=-100 if a wrong choice of the inc. halfspace */


{
  extern float vp1,vs1;
  extern float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_1,gamma1_44,gamma1_55;

  int c,perr=0;
  float B12, B11_B22, D;
  float A55Gs, A55G1, A33;
  float VS,sin_j, cos_j, sin_k, cos_k;
  
  
  /* do not do anything, if ISO inc. halfspace */

  if (i_hsp == 0)
    {
      *cphi=1.;
      *sphi=0.;
      return(perr=0);
    }  

  /* auxiliary quantities */

  A55Gs = pow(vs1,2)*(gamma1_44 - gamma1_55);
  A55G1 = pow(vs1,2)*(1 + 2*gamma1_55)*gamma1_1;
  A33 = pow(vp1,2);

  /* get the reflection angle of the iso S-wave (in rad), and azimuth angles */

  VS = sqrt( (2*pow(vs1,2)*(1+2*gamma1_44) + pow(vs1,2)*(1+2*gamma1_55)*(1+gamma1_1))/3 );
  
  sin_j = (VS/vp1)*sin(ang);
  cos_j = 1 - 0.5*pow(sin_j,2);
  cos_k = cos(azim);
  sin_k = sin(azim);

  /*fprintf(stderr,"vp=%f vs=%f ang=%f azim=%f a3=%f a55gs=%f a55g1=%f Sj=%f Cj=%f Saz=%f Caz=%f \n",vp1,vs1,ang,azim,A33,
	  A55Gs,A55G1,sin_j,cos_j,sin_k,cos_k);
  */
  
  /* get the polarization coefficients */

  B11_B22 = 2*A33*(eps1_2*pow(cos_k,2)-delta1_2)*pow(cos_k,2)*pow(cos_j,2)*pow(sin_j,2)
          + 2*A33*(eps1_1*pow(sin_k,2)-delta1_1)*pow(sin_k,2)*pow(cos_j,2)*pow(sin_j,2)
    + 2*A33*(1+2*eps1_2)*delta1_3*pow(cos_k,2)*pow(sin_k,2)*pow(sin_j,2)*(pow(cos_j,2)+1)
    + 4*A33*eps1_2*pow(cos_k,2)*pow(sin_k,2)*pow(cos_j,2)*pow(sin_j,2) 
    + 2*A33*(eps1_2-eps1_1)*pow(cos_k,2)*pow(sin_k,2)*pow(sin_j,2)
    - 2*A55G1*pow(sin_j,2)
    - 2*A55Gs*(pow(cos_k,2)-pow(sin_k,2))
    + 2*A55Gs*pow(cos_k,2)*pow(sin_j,2);
  
  B12 = A33*(1+2*eps1_2)*delta1_3*cos_k*sin_k*(pow(cos_k,2)-pow(sin_k,2))*cos_j*pow(sin_j,2)
    + A33*(delta1_2-delta1_1)*cos_k*sin_k*cos_j*pow(sin_j,2)
    + 2*A33*(eps1_1-eps1_2)*cos_k*pow(sin_k,3)*cos_j*pow(sin_j,2)
    + 2*A55Gs*cos_k*sin_k*cos_j;
  
  D = sqrt(pow(B11_B22,2) + 4*pow(B12,2));

  /*  fprintf(stderr,"B1122=%f B12=%f \n",B11_B22,B12);
   */

  if (D <= LC_TINY) perr=-1;
  
  /* do the logic for different halfspaces */

  if (i_hsp == 1){
    *cphi=1.;
    *sphi=0.;
    return(perr);     
  }
  if (i_hsp == 2){
    *cphi=(cos_j*cos_k)/sqrt(1-pow(sin_j*cos_k,2));
    *sphi=-sin_k/sqrt(1-pow(sin_j*cos_k,2));
    return(perr);
  }
  if (i_hsp == 3){
    if (perr == -1) return(perr=-2);
    if (fabs(B12) <= LC_TINY)
      {
      if (fabs(B11_B22) <= LC_TINY) return(perr=-2);
      switch (c=floor(B11_B22/fabs(B11_B22))) 
	{
	case 1: case 2:
	  *cphi=1;
	  *sphi=0;
	  break;
	case 0: case -1: case -2:
	  *cphi=0;
	  *sphi=1;
	  break;
	default : return(perr=-2);
	}
      }
    else {
      *cphi=sqrt( 0.5*(1+B11_B22/D)  );
      *sphi=(B12/fabs(B12))*sqrt( 0.5*(1-B11_B22/D) );
    }
    
    return(0);
   
  }
  
  return(-100);

}

/********************************/
/******** the end ***************/
/********************************/


int S_err_2nd_order(ErrorFlag *rsv_1st, ErrorFlag *rsv_2nd, ErrorFlag *rsh_1st, ErrorFlag *rsh_2nd, 
		    float true_kappa, int index)


     /* The subroutine evaluates first order (exactly) and second order (approximately)
	Rpsv and Rpsh coefficients for a given incidence angle and azimuths. The second order evaluation
	is based on the numerical modeling and comparing of the exact Rpsv (Rpsh) and its first order 
	approximation (so called semi-2nd order). The comparison has been done for the incidence 
	angle of 30 deg and it should be most accurate for this angle. However, such an estimated 
	semi-2nd order Rpsv (Rpsh) usually resulted in a global increase of accuracy of the first-order Rpsv 
	(Rpsh) so the subroutine can be used for other angles, as a rough error estimate, as well.

	INPUT:
	true_kappa ... mutual rotation of the upper and lower halfspaces in rad. This angle affects
	               the global error evaluation only.
	index ... =1, =2: if the true_kappa=0, then the subroutine should be called just once
		          with index=1. If the true_kappa is not zero, then the
		          subroutine needs to be called the second time (index=2) in
		          order to properly evaluate the GLOBAL error. No other values of
			  the "index" parameter are allowed. If some other value is on the
			  entry, the subroutine is terminated immediately with the value -1.
	err_ang ... as #define. The incidence angle for which the error analysis is
	                        performed. Not recommended to change the current value of
				30deg by a large ammount.
	lkappa, kkappa ... in declaration. Two azimuths for which the global error for Rpsv is evaluated,
	                                   besides 0deg and 90deg; their values can be changed arbitrary, 
					   but do not forget to change your output information as well 
					   (i.e. give proper azimuths for the output file corresponding to
					   *rp_1st(2nd).global[2],[3]). In the case of Rpsh, the angles are
					   given as 20deg, 45deg and 70deg, and can be changed in the
					   paragraph GLOBAL -> SH -> err_kappa=90deg section.

	OUTPUT:
	*rsv_1st (*rsh_1st) ... ErrorFlag structure: contains information on first-order
	                        Rpsv (Rpsh) reflection coefficient evaluated for the purpose of
				error analysis.
	*rsv_2nd (*rsv_2nd) ... ErrorFlag structure: contains information on semi 
	                        second-order Rpsv (Rpsh) reflection coefficient evaluated for 
				the purpose of error analysis. 

        	struct ErrorFlag
	         {
		    float iso[5];     
	            float upper[2];   
	            float lower[2];   
	            float global[4]; 
	            float angle[4]; 
		                    
	          }, 
 	 where  
         iso[0]-iso[4] ... Rpsv: isotropic part for the following inc. angles (due to velocity
	                   and density contrasts): 15deg, 20deg, 25deg, 30deg and 35deg. 
		 	   If a change is required, seek the paragraph "ISO part" in this subroutine.
			   Rpsh: iso[n]=0 for all n.
        upper[0],upper[1] ... Rpsv: coefficient for azimuths 0deg and 90deg, respectively, computed
                              for the ANISO/ISO configuration of the halfspaces. If required,
			      changes can be made in UPPER -> SV paragraph.
			      Rpsh: the same as above. However, only upper[0] is activated for 45deg.
			      If required, changes can be made in UPPER -> SH paragraph.
        lower[0],lower[1] ... Rpsv: coefficients for azimuths 0deg and 90deg, respectively, computed
                              for the ISO/ANISO configuration of the halfspaces. If required,
			      changes can be made in LOWER -> SV paragraph. 
			      Rpsh: the same as above. However, only lower[0] is activated for 45deg.
			      If required, changes can be made in LOWER -> SH paragraph.
        global[0]-[3] ... Rpsv: coefficients for ANISO/ANISO configuration evaluated in the following
                          azimuths, respectively: 0deg, 90deg, lkappa, kkappa. If required,
			  changes can be made using lkappa and kkappa variables.  
			  Rpsh: the same meaning as above. However, only the global[0]-global[2] are
			  activated for the angles 20deg, 45deg and 70deg. If required,
			  changes can be made in GLOBAL -> SH -> err_kappa=PI/2. paragraph. 
        angle[0]-[3] ... inc. angle (err_ang), 0deg, 90deg, kappa.

      S_err_2nd_order = 0 ... usual termination
                      =-1 ... unusual termination (wrong input)
     */
     

{

  extern float vp1,vp2,vs1,vs2,rho1,rho2;
  extern float eps1_1,eps1_2,delta1_1,delta1_2,delta1_3,gamma1_44;
  extern float eps2_1,eps2_2,delta2_1,delta2_2,delta2_3,gamma2_44;
  
  float Da_a, Db_b, Dr_r, DG_G, alpha, beta;   
  float Delta1_1, Delta1_2, Delta2_1, Delta2_2, Gamma1_44, Gamma2_44;  
  float D1_1, D1_2, D2_1, D2_2, GS_1, GS_2, SHD1_1, SHD1_2, SHD2_1, SHD2_2, SHGS_1, SHGS_2;  
  float ISO_2nd, azim;               
  float d11_plus=0., d11_min=0., d12_plus=0., d12_min=0., gs1_plus=0., gs1_min=0.;
  float d21_plus=0., d21_min=0., d22_plus=0., d22_min=0., gs2_plus=0., gs2_min=0.;
  float a_plus=0., a_min=0, b_plus=0., b_min=0., r_plus=0., r_min=0. ;

  float Fst0_0, Fst0_90, Fst90_0, Fst90_90, FstK_0, FstK_90, FstKK_0, FstKK_90;
  float Fst0_kappa, Fst90_kappa, FstK_kappa, FstKK_kappa;
  float Snd0_0, Snd0_90, Snd90_0, Snd90_90, SndK_0, SndK_90, SndKK_0, SndKK_90;
  float Snd0_kappa, Snd90_kappa, SndK_kappa, SndKK_kappa;  
  float SHFst20_0, SHFst20_90, SHFst45_0, SHFst45_90, SHFst70_0, SHFst70_90;
  float SHFst20_kappa, SHFst45_kappa, SHFst70_kappa;
  float SHSnd20_0, SHSnd20_90, SHSnd45_0, SHSnd45_90, SHSnd70_0, SHSnd70_90;
  float SHSnd20_kappa, SHSnd45_kappa, SHSnd70_kappa; 
  float err_kappa=0.0, lkappa=30.*PI/180, kkappa=60.*PI/180, D;

  int N=0;  

  float SV_term(float DG_G, float Dr_r, float Delta1_1, float Delta1_2, float Delta1_3,
		float Delta2_1, float Delta2_2, float Delta2_3,
		float Eps1_1, float Eps1_2, float Eps2_1, float Eps2_2,
		float Gamma1_44, float Gamma2_44, float alpha, float beta, 
		float ang, float azim, float kappa);
  
  float SH_term(float Delta1_1, float Delta1_2, float Delta1_3, 
		float Delta2_1, float Delta2_2, float Delta2_3,
		float Eps1_1, float Eps1_2, float Eps2_1, float Eps2_2,
		float Gamma1_44, float Gamma2_44, float alpha, float beta, 
		float ang, float azim, float kappa);

  float Iso_exact(int type, float vp1, float vs1, float rho1, 
		  float vp2, float vs2, float rho2, float ang);  

  /* some quantities in advance */

  Da_a=(vp2-vp1)/(0.5*(vp2+vp1));
  Db_b=(vs2-vs1)/(0.5*(vs2+vs1));
  Dr_r=(rho2-rho1)/(0.5*(rho2+rho1));  
  DG_G=(pow(vs2,2)*rho2-pow(vs1,2)*rho1)/(0.5*(pow(vs2,2)*rho2+pow(vs1,2)*rho1));
  alpha=0.5*(vp2+vp1);
  beta=0.5*(vs2+vs1);
  /*kkappa=PI/2.-true_kappa;*/

  if(index == 1) err_kappa=0.;
  if(index == 2) err_kappa=PI/2.;
  if((index != 1) && (index != 2)) return(-1);

  if(delta1_1) d11_plus=(fabs(delta1_1)+delta1_1)/(2*fabs(delta1_1));  
  if(delta1_1) d11_min=(fabs(delta1_1)-delta1_1)/(2*fabs(delta1_1));
  if(delta1_2) d12_plus=(fabs(delta1_2)+delta1_2)/(2*fabs(delta1_2));
  if(delta1_2) d12_min=(fabs(delta1_2)-delta1_2)/(2*fabs(delta1_2));
  if(gamma1_44) gs1_plus=(fabs(gamma1_44)+gamma1_44)/(2*fabs(gamma1_44));
  if(gamma1_44) gs1_min=(fabs(gamma1_44)-gamma1_44)/(2*fabs(gamma1_44));

  if(delta2_1) d21_plus=(fabs(delta2_1)+delta2_1)/(2*fabs(delta2_1));  
  if(delta2_1) d21_min=(fabs(delta2_1)-delta2_1)/(2*fabs(delta2_1));
  if(delta2_2) d22_plus=(fabs(delta2_2)+delta2_2)/(2*fabs(delta2_2));
  if(delta2_2) d22_min=(fabs(delta2_2)-delta2_2)/(2*fabs(delta2_2));
  if(gamma2_44) gs2_plus=(fabs(gamma2_44)+gamma2_44)/(2*fabs(gamma2_44));
  if(gamma2_44) gs2_min=(fabs(gamma2_44)-gamma2_44)/(2*fabs(gamma2_44));

  if(Da_a) a_plus=(fabs(Da_a)+Da_a)/(2*fabs(Da_a));
  if(Da_a) a_min=(fabs(Da_a)-Da_a)/(2*fabs(Da_a));
  if(Db_b) b_plus=(fabs(Db_b)+Db_b)/(2*fabs(Db_b));
  if(Db_b) b_min=(fabs(Db_b)-Db_b)/(2*fabs(Db_b));
  if(Dr_r) r_plus=(fabs(Dr_r)+Dr_r)/(2*fabs(Dr_r));
  if(Dr_r) r_min=(fabs(Dr_r)-Dr_r)/(2*fabs(Dr_r));
  
  /* the angles used for the anisotropic accuracy evaluation */

  (*rsv_1st).angle[0]=(*rsv_2nd).angle[0] = err_ang;        /* incidence angle */
  (*rsv_1st).angle[1]=(*rsv_2nd).angle[1] = 0.;             /* first azimuth */
  (*rsv_1st).angle[2]=(*rsv_2nd).angle[2] = 90.;            /* second azimuth */ 
  (*rsv_1st).angle[3]=(*rsv_2nd).angle[3] = true_kappa;      /* kappa */

  (*rsh_1st).angle[0]=(*rsh_2nd).angle[0] = err_ang;        /* incidence angle */
  (*rsh_1st).angle[1]=(*rsh_2nd).angle[1] = 0.;             /* first azimuth */
  (*rsh_1st).angle[2]=(*rsh_2nd).angle[2] = 90.;            /* second azimuth */ 
  (*rsh_1st).angle[3]=(*rsh_2nd).angle[3] = true_kappa;      /* kappa */


  /* ISO part first */

  /* SV */

  for(N = 0; N < 5; N++)
    {
      (*rsv_1st).iso[N]=SV_term(DG_G, Dr_r, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
				0., 0., alpha, beta, (15+5*N)*PI/180, 0., 0.);
      (*rsv_2nd).iso[N]=Iso_exact(1, vp1, vs1, rho1, vp2, vs2, rho2, (15+5*N)*PI/180);
    }

  /* evaluate the ISO 2nd order for the inc. angle err_ang */

  ISO_2nd = Iso_exact(1, vp1, vs1, rho1, vp2, vs2, rho2, err_ang) -
            SV_term(DG_G, Dr_r, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
		    0., 0., alpha, beta, err_ang, 0., 0.);
  
  /* SH */
  (*rsh_1st).iso[0]=(*rsh_2nd).iso[0]=0.;
  (*rsh_1st).iso[1]=(*rsh_2nd).iso[1]=0.;
  (*rsh_1st).iso[2]=(*rsh_2nd).iso[2]=0.;
  (*rsh_1st).iso[3]=(*rsh_2nd).iso[3]=0.;
  (*rsh_1st).iso[4]=(*rsh_2nd).iso[4]=0.;


  /* UPPER halfspace contribution */

  /* SV */
  Delta1_1=(-1*d11_plus -2.5*d11_min)*pow(delta1_1,2) + 
    (-0.22*d11_plus*gs1_plus+0*d11_plus*gs1_min-0.5*d11_min*gs1_plus-0.39*d11_min*gs1_min)*delta1_1*gamma1_44 + 
    (1.28*d11_plus*a_plus +0.75*d11_plus*a_min +1.93*d11_min*a_plus +2.93*d11_min*a_min)*delta1_1*Da_a + 
    (0.64*d11_plus*b_plus +0.23*d11_plus*b_min +0.18*d11_min*b_plus +1.87*d11_min*b_min)*delta1_1*Db_b + 
    (1.15*d11_plus*r_plus +0.66*d11_plus*r_min +3.51*d11_min*r_plus +2.69*d11_min*r_min)*delta1_1*Dr_r;
  

  D1_1=Delta1_1;
  
  Delta1_2=(-1*d12_plus -2.5*d12_min)*pow(delta1_2,2) +
    (1.28*d12_plus*a_plus +0.75*d12_plus*a_min +1.93*d12_min*a_plus +2.93*d12_min*a_min)*delta1_2*Da_a + 
    (0.64*d12_plus*b_plus +0.23*d12_plus*b_min +0.18*d12_min*b_plus +1.87*d12_min*b_min)*delta1_2*Db_b + 
    (1.15*d12_plus*r_plus +0.66*d12_plus*r_min +3.51*d12_min*r_plus +2.69*d12_min*r_min)*delta1_2*Dr_r;
						       

  D1_2=Delta1_2;  

  Delta2_1=0.;
  Delta2_2=0.;

  Gamma1_44=(-0.6*gs1_plus -1.1*gs1_min)*pow(gamma1_44,2) +
    (-0.385*gs1_plus*a_plus -0.36*gs1_plus*a_min -0.55*gs1_min*a_plus -0.225*gs1_min*a_min)*gamma1_44*Da_a +
    (0*gs1_plus*b_plus +0*gs1_plus*b_min +0*gs1_min*b_plus -0.3*gs1_min*b_min)*gamma1_44*Db_b +
    (-0.33*gs1_plus*r_plus -0.32*gs1_plus*r_min -0.64*gs1_min*r_plus -0.57*gs1_min*r_min)*gamma1_44*Dr_r;
							   
  GS_1=Gamma1_44;
  
  Gamma2_44=0.;

  if(index == 1)
    {     
      azim=0.;
      (*rsv_1st).upper[0]=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, 0., 0., 0.,
				  eps1_1, eps1_2, 0., 0.,
				  gamma1_44, 0., alpha, beta, err_ang, azim, 0.); 
      (*rsv_2nd).upper[0]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				  0., 0., 0., 0.,
				  Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.) +
	                  ISO_2nd;
      
      
      azim=90.*PI/180;  
      (*rsv_1st).upper[1]=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, 0., 0., 0.,
				  eps1_1, eps1_2, 0., 0.,
				  gamma1_44, 0., alpha, beta, err_ang, azim, 0.);
      (*rsv_2nd).upper[1]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				  0., 0., 0., 0.,
				  Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.) +
	                  ISO_2nd;
      
    }
  
  /* SH */
  Delta1_1=(-0.8*d11_plus -2.0*d11_min)*pow(delta1_1,2) + 
    (-0.22*d11_plus*gs1_plus+0*d11_plus*gs1_min-0.4*d11_min*gs1_plus-0.22*d11_min*gs1_min)*delta1_1*gamma1_44 + 
    (0*d11_plus*d12_plus + 0*d11_plus*d12_min + 0*d11_min*d12_plus + 0*d11_min*d12_min)*delta1_1*delta1_2 +
    (1.32*d11_plus*a_plus +1.09*d11_plus*a_min +2.93*d11_min*a_plus +2.4*d11_min*a_min)*delta1_1*Da_a + 
    (0.95*d11_plus*b_plus +0.42*d11_plus*b_min +0.73*d11_min*b_plus +0.75*d11_min*b_min)*delta1_1*Db_b + 
    (1.67*d11_plus*r_plus +1.68*d11_plus*r_min +3.71*d11_min*r_plus +3.12*d11_min*r_min)*delta1_1*Dr_r;
  

  SHD1_1=Delta1_1;
  
  Delta1_2=(-0.8*d12_plus -2.0*d12_min)*pow(delta1_2,2) +
    (0.22*d12_plus*gs1_plus-0.33*d12_plus*gs1_min+0.22*d12_min*gs1_plus+0.32*d12_min*gs1_min)*delta1_2*gamma1_44 +
    (1.32*d12_plus*a_plus +1.09*d12_plus*a_min +2.93*d12_min*a_plus +2.4*d12_min*a_min)*delta1_2*Da_a + 
    (0.95*d12_plus*b_plus +0.42*d12_plus*b_min +0.73*d12_min*b_plus +0.75*d12_min*b_min)*delta1_2*Db_b + 
    (1.67*d12_plus*r_plus +1.68*d12_plus*r_min +3.71*d12_min*r_plus +3.12*d12_min*r_min)*delta1_2*Dr_r;
  

  SHD1_2=Delta1_2;  

  Delta2_1=0.;
  Delta2_2=0.;

  Gamma1_44=(-0.45*gs1_plus -0.78*gs1_min)*pow(gamma1_44,2) +
    (-0.17*gs1_plus*a_plus -0.16*gs1_plus*a_min -0.28*gs1_min*a_plus -0.09*gs1_min*a_min)*gamma1_44*Da_a +
    (-0.27*gs1_plus*b_plus -0.45*gs1_plus*b_min -0.55*gs1_min*b_plus -0.52*gs1_min*b_min)*gamma1_44*Db_b +
    (-0.38*gs1_plus*r_plus -0.48*gs1_plus*r_min -0.57*gs1_min*r_plus -0.61*gs1_min*r_min)*gamma1_44*Dr_r;
							     

  SHGS_1=Gamma1_44;
  
  Gamma2_44=0.;


  if(index == 1 )
    {
      azim=45.*PI/180;  
      (*rsh_1st).upper[0]=SH_term(delta1_1, delta1_2, delta1_3, 0., 0., 0.,
				  eps1_1, eps1_2, 0., 0.,
				  gamma1_44, 0., alpha, beta, err_ang, azim, 0.);
      (*rsh_2nd).upper[0]=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				  0., 0., 0., 0.,
				  Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.);  
      
      (*rsh_2nd).upper[1]=0.0;
      (*rsh_1st).upper[1]=0.0;
    }
  
 
  /* LOWER halfspace contribution */

  /* SV */
  Delta1_1=0.;
  Delta1_2=0.;

  Delta2_1=(-0.72*d21_plus -1.89*d21_min)*pow(delta2_1,2) + 
    (0.61*d21_plus*gs2_plus+1.26*d21_plus*gs2_min+0.83*d21_min*gs2_plus+2.28*d21_min*gs2_min)*delta2_1*gamma2_44+
    (-0.28*d21_plus*a_plus +0*d21_plus*a_min -1.1*d21_min*a_plus -1.05*d21_min*a_min)*delta2_1*Da_a + 
    (0.82*d21_plus*b_plus +0.83*d21_plus*b_min +2.93*d21_min*b_plus +1.73*d21_min*b_min)*delta2_1*Db_b + 
    (-0.07*d21_plus*r_plus +0.1*d21_plus*r_min +0*d21_min*r_plus +0*d21_min*r_min)*delta2_1*Dr_r;
  

  D2_1=Delta2_1;
  
  Delta2_2=(-0.72*d22_plus -1.89*d22_min)*pow(delta2_2,2) +
    (-0.28*d22_plus*a_plus +0*d22_plus*a_min -1.1*d22_min*a_plus -1.05*d22_min*a_min)*delta2_2*Da_a + 
    (0.82*d22_plus*b_plus +0.83*d22_plus*b_min +2.93*d22_min*b_plus +1.73*d22_min*b_min)*delta2_2*Db_b + 
    (-0.07*d22_plus*r_plus +0.1*d22_plus*r_min +0*d22_min*r_plus + 0*d22_min*r_min)*delta2_2*Dr_r;
							   

  D2_2=Delta2_2;
  
  Gamma1_44=0.;
  Gamma2_44=(-0.28*gs2_plus -(2./3.)*gs2_min)*pow(gamma2_44,2) +
    (-0.37*gs2_plus*a_plus -0.3*gs2_plus*a_min -0.55*gs2_min*a_plus -0.45*gs2_min*a_min)*gamma2_44*Da_a +
    (1.1*gs2_plus*b_plus +0.69*gs2_plus*b_min +0.92*gs2_min*b_plus +0.6*gs2_min*b_min)*gamma2_44*Db_b +
    (0*gs2_plus*r_plus +0*gs2_plus*r_min +0.06*gs2_min*r_plus +0.20*gs2_min*r_min)*gamma2_44*Dr_r;
								

  GS_2=Gamma2_44;

  if(index == 1)
    {
      azim=0.;
      (*rsv_1st).lower[0]=SV_term(DG_G, Dr_r, 0., 0., 0., delta2_1, delta2_2, delta2_3,
				  0., 0., eps2_1, eps2_2,
				  0., gamma2_44, alpha, beta, err_ang, azim, 0.);
      (*rsv_2nd).lower[0]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0., 
				  0., 0., 0., 0.,
				  Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.) +
	                  ISO_2nd;
      
      
      azim=90.*PI/180;  
      (*rsv_1st).lower[1]=SV_term(DG_G, Dr_r, 0., 0., 0., delta2_1, delta2_2, delta2_3,
				  0., 0., eps2_1, eps2_2,
				  0., gamma2_44, alpha, beta, err_ang, azim, 0.);
      (*rsv_2nd).lower[1]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				  0., 0., 0., 0.,
				  Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.) +
	                  ISO_2nd;
      
    }
  
  /* SH */
  Delta1_1=0.;
  Delta1_2=0.;

  Delta2_1=(-0.61*d21_plus -1.56*d21_min)*pow(delta2_1,2) + 
    (0.28*d21_plus*gs2_plus+1.1*d21_plus*gs2_min+0.44*d21_min*gs2_plus+1.56*d21_min*gs2_min)*delta2_1*gamma2_44 + 
    (0*d21_plus*d22_plus + 0*d21_plus*d22_min + 0*d21_min*d22_plus + 0*d21_min*d22_min)*delta2_1*delta2_2 +
    (0.09*d21_plus*a_plus +0.38*d21_plus*a_min -0.47*d21_min*a_plus -0.74*d21_min*a_min)*delta2_1*Da_a + 
    (0.55*d21_plus*b_plus +0.45*d21_plus*b_min +2.01*d21_min*b_plus +1.28*d21_min*b_min)*delta2_1*Db_b + 
    (-0.07*d21_plus*r_plus +0.05*d21_plus*r_min +0*d21_min*r_plus +0.14*d21_min*r_min)*delta2_1*Dr_r;
  


  SHD2_1=Delta2_1;
  
  Delta2_2=(-0.61*d22_plus -1.56*d22_min)*pow(delta2_2,2) +
    (0.11*d22_plus*gs2_plus+0.11*d22_plus*gs2_min+0.*d22_min*gs2_plus+0.64*d22_min*gs2_min)*delta2_2*gamma2_44+
    (0.09*d22_plus*a_plus +0.38*d22_plus*a_min -0.47*d22_min*a_plus -0.74*d22_min*a_min)*delta2_2*Da_a + 
    (0.55*d22_plus*b_plus +0.45*d22_plus*b_min +2.01*d22_min*b_plus +1.28*d22_min*b_min)*delta2_2*Db_b + 
    (-0.07*d22_plus*r_plus +0.05*d22_plus*r_min +0*d22_min*r_plus +0.14*d22_min*r_min)*delta2_2*Dr_r;
  

  SHD2_2=Delta2_2;
  
  Gamma1_44=0.;
  Gamma2_44=(-(1./3.)*gs2_plus -(2./3.)*gs2_min)*pow(gamma2_44,2)+
    (-0.18*gs2_plus*a_plus -0.08*gs2_plus*a_min -0.18*gs2_min*a_plus -0.15*gs2_min*a_min)*gamma2_44*Da_a +
    (0.64*gs2_plus*b_plus +0.52*gs2_plus*b_min +0.55*gs2_min*b_plus +0.52*gs2_min*b_min)*gamma2_44*Db_b +
    (-0.06*gs2_plus*r_plus +0*gs2_plus*r_min +0.06*gs2_min*r_plus +0.23*gs2_min*r_min)*gamma2_44*Dr_r;
								   

  SHGS_2=Gamma2_44;

  if(index == 1) 
    {
    azim=45.*PI/180;
    (*rsh_1st).lower[0]=SH_term(0., 0., 0., delta2_1, delta2_2, delta2_3,
				0., 0., eps2_1, eps2_2,
				0., gamma2_44, alpha, beta, err_ang, azim, 0.);
    (*rsh_2nd).lower[0]=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				0., 0., 0., 0.,
				Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, 0.);  
    (*rsh_1st).lower[1]=0.0;
    (*rsh_2nd).lower[1]=0.0;
    }
  

  /* GLOBAL model contribution */

  /* SV */
  /* The following terms are derived on the assumption that Rps coefficients for
     two identicaly anisotropic halfspaces are zero (with 0 velocity contrats)
  */

  Delta1_1=D1_1 +
           (1.*d11_plus*d21_plus -1.*d11_plus*d21_min -2.5*d11_min*d21_plus +2.5*d11_min*d21_min)*
                                                             delta1_1*delta2_1*pow(cos(err_kappa),2) +
           (1.*d11_plus*d22_plus -1.*d11_plus*d22_min -2.5*d11_min*d22_plus +2.5*d11_min*d22_min)* 
                                                             delta1_1*delta2_2*pow(sin(err_kappa),2) +
           (0.22*d11_plus*gs2_plus +0.*d11_plus*gs2_min +0.5*d11_min*gs2_plus +0.39*d11_min*gs2_min)*
	                                                     delta1_1*gamma2_44*pow(cos(err_kappa),2);
  
  Delta1_2=D1_2 + 
           (1.*d12_plus*d21_plus -1.*d12_plus*d21_min -2.5*d12_min*d21_plus +2.5*d12_min*d21_min)*
                                                             delta1_2*delta2_1*pow(sin(err_kappa),2) +
           (1.*d12_plus*d22_plus -1.*d12_plus*d22_min -2.5*d12_min*d22_plus +2.5*d12_min*d22_min)* 
                                                             delta1_2*delta2_2*pow(cos(err_kappa),2) +
           (0.22*d12_plus*gs2_plus +0.*d12_plus*gs2_min +0.5*d12_min*gs2_plus +0.39*d12_min*gs2_min)*
                                                             delta1_2*gamma2_44*pow(sin(err_kappa),2); 

  Delta2_1=D2_1 + 
           (0.72*d21_plus*d11_plus -0.72*d21_plus*d11_min -1.89*d21_min*d11_plus +1.89*d21_min*d11_min)*
                                                             delta2_1*delta1_1*pow(cos(err_kappa),2) + 
           (0.72*d21_plus*d12_plus -0.72*d21_plus*d12_min -1.89*d21_min*d12_plus +1.89*d21_min*d12_min)* 
                                                             delta2_1*delta1_2*pow(sin(err_kappa),2) +
           (-0.61*d21_plus*gs1_plus -1.26*d21_plus*gs1_min -0.83*d21_min*gs1_plus -2.28*d21_min*gs1_min)*
                                                             delta2_1*gamma1_44*pow(cos(err_kappa),2);
  

  Delta2_2=D2_2 + 
           (0.72*d22_plus*d11_plus -0.72*d22_plus*d11_min -1.89*d22_min*d11_plus +1.89*d22_min*d11_min)*
                                                             delta2_2*delta1_1*pow(sin(err_kappa),2) + 
           (0.72*d22_plus*d12_plus -0.72*d22_plus*d12_min -1.89*d22_min*d12_plus +1.89*d22_min*d12_min)* 
                                                             delta2_2*delta1_2*pow(cos(err_kappa),2) +
           (-0.61*d22_plus*gs1_plus -1.26*d22_plus*gs1_min -0.83*d22_min*gs1_plus -2.28*d22_min*gs1_min)*
                                                             delta2_2*gamma1_44*pow(sin(err_kappa),2);


  Gamma1_44=GS_1 + 
            (0.0*gs1_plus*d21_plus +0.0*gs1_plus*d21_min +0.0*gs1_min*d21_plus +0.0*gs1_min*d21_min)*
                                                             gamma1_44*delta2_1*pow(cos(err_kappa),2) +
            (0.0*gs1_plus*d22_plus +0.0*gs1_plus*d22_min +0.0*gs1_min*d22_plus +0.0*gs1_min*d22_min)*
                                                             gamma1_44*delta2_2*pow(sin(err_kappa),2) +
            (0.6*gs1_plus*gs2_plus +0.3*gs1_plus*gs2_min +0.3*gs1_min*gs2_plus +1.1*gs1_min*gs2_min)*
                                                             gamma1_44*gamma2_44*pow(cos(err_kappa),2);
  
  Gamma2_44=GS_2 +
            (0.28*gs2_plus*gs1_plus +0.*gs2_plus*gs1_min +0.*gs2_min*gs1_plus +(2./3.)*gs2_min*gs1_min)*
                                                            gamma2_44*gamma1_44*pow(cos(err_kappa),2);



  /* The following terms are based on error minimization for 2-parameter model only.
     Notice that such 2nd order terms do not result in Rps=0 for two identically
     anisotropic halfspaces (probably not good to use in most cases).
  */

  /*  Delta1_1=D1_1 +
           (1.*d11_plus*d21_plus -1.*d11_plus*d21_min -2.5*d11_min*d21_plus +2.5*d11_min*d21_min)*
                                                             delta1_1*delta2_1*pow(cos(err_kappa),2) + 
           (1.*d11_plus*d22_plus -1.*d11_plus*d22_min -2.5*d11_min*d22_plus +2.5*d11_min*d22_min)* 
                                                             delta1_1*delta2_2*pow(sin(err_kappa),2) +
           (-0.15*d11_plus*gs2_plus +0*d11_plus*gs2_min +0.6*d11_min*gs2_plus -1*d11_min*gs2_min)*
                                                             delta1_1*gamma2_44*pow(cos(err_kappa),2);
  
  Delta1_2=D1_2 + 
           (1.*d12_plus*d21_plus -1.*d12_plus*d21_min -2.5*d12_min*d21_plus +2.5*d12_min*d21_min)*
                                                             delta1_2*delta2_1*pow(sin(err_kappa),2) + 
           (1.*d12_plus*d22_plus -1.*d12_plus*d22_min -2.5*d12_min*d22_plus +2.5*d12_min*d22_min)* 
                                                             delta1_2*delta2_2*pow(cos(err_kappa),2) +
           (-0.15*d12_plus*gs2_plus +0*d12_plus*gs2_min +0.6*d12_min*gs2_plus -1*d12_min*gs2_min)*
                                                             delta1_2*gamma2_44*pow(sin(err_kappa),2); 

  Delta2_1=D2_1 + 
           (0.72*d21_plus*d11_plus -0.72*d21_plus*d11_min -1.89*d21_min*d11_plus +1.89*d21_min*d11_min)*
                                                             delta2_1*delta1_1*pow(cos(err_kappa),2) + 
           (0.72*d21_plus*d12_plus -0.72*d21_plus*d12_min -1.89*d21_min*d12_plus +1.89*d21_min*d12_min)* 
                                                             delta2_1*delta1_2*pow(sin(err_kappa),2);
  

  Delta2_2=D2_2 + 
           (0.72*d22_plus*d11_plus -0.72*d22_plus*d11_min -1.89*d22_min*d11_plus +1.89*d22_min*d11_min)*
                                                             delta2_2*delta1_1*pow(sin(err_kappa),2) + 
           (0.72*d22_plus*d12_plus -0.72*d22_plus*d12_min -1.89*d22_min*d12_plus +1.89*d22_min*d12_min)* 
                                                             delta2_2*delta1_2*pow(cos(err_kappa),2);


  Gamma1_44=GS_1 + 
            (-0.3*gs1_plus*d21_plus -0.4*gs1_plus*d21_min -0.4*gs1_min*d21_plus -0.7*gs1_min*d21_min)*
                                                             gamma1_44*delta2_1*pow(cos(err_kappa),2) +
            (-0.3*gs1_plus*d22_plus -0.4*gs1_plus*d22_min -0.4*gs1_min*d22_plus -0.7*gs1_min*d22_min)*
                                                             gamma1_44*delta2_2*pow(sin(err_kappa),2) +
            (0.6*gs1_plus*gs2_plus +0.3*gs1_plus*gs2_min +0.3*gs1_min*gs2_plus +1.1*gs1_min*gs2_min)*
                                                            gamma1_44*gamma2_44*pow(cos(err_kappa),2);
  
  Gamma2_44=GS_2 +
            (0.28*gs2_plus*gs1_plus +0*gs2_plus*gs1_min +0*gs2_min*gs1_plus +(2./3.)*gs2_min*gs1_min)*
                                                            gamma2_44*gamma1_44*pow(cos(err_kappa),2);
  */

  if(index == 1)

    /* here, err_kappa=0: evaluate the error for alligned halfspaces */

    {
      azim=0.;
      (*rsv_1st).global[0]=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsv_2nd).global[0]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0.,
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	                   ISO_2nd;
      
      azim=90.*PI/180;
      (*rsv_1st).global[1]=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsv_2nd).global[1]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0.,
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	                   ISO_2nd;

      azim=lkappa;  
      (*rsv_1st).global[2]=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsv_2nd).global[2]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0.,
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	                   ISO_2nd;
      
      azim=kkappa;  
      (*rsv_1st).global[3]=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsv_2nd).global[3]=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0.,
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	                   ISO_2nd;
      
    }
  
  Fst0_0=(*rsv_1st).global[0];
  Snd0_0=(*rsv_2nd).global[0];
  Fst90_0=(*rsv_1st).global[1];
  Snd90_0=(*rsv_2nd).global[1];
  FstK_0=(*rsv_1st).global[2];
  SndK_0=(*rsv_2nd).global[2];
  FstKK_0=(*rsv_1st).global[3];
  SndKK_0=(*rsv_2nd).global[3];

  if(index == 2)

    /* here, err_kappa = 90.: evaluate the error for the halfspaces rotated by 90 deg */

    {
      azim=0.;
      Fst0_90=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		      eps1_1, eps1_2, eps2_1, eps2_2,
		      gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      Snd0_90=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
		      0., 0., 0., 0.,
		      Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	      ISO_2nd;
      Fst0_kappa=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		      eps1_1, eps1_2, eps2_1, eps2_2,
		      gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);
       
      
      azim=90.*PI/180;
      Fst90_90=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		       eps1_1, eps1_2, eps2_1, eps2_2,
		       gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      Snd90_90=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
		       0., 0., 0., 0.,
		       Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	       ISO_2nd;
      Fst90_kappa=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		       eps1_1, eps1_2, eps2_1, eps2_2,
		       gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);

      
      azim=lkappa;  
      FstK_90=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		      eps1_1, eps1_2, eps2_1, eps2_2,
		      gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SndK_90=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
		      0., 0., 0., 0.,
		      Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	      ISO_2nd;
      FstK_kappa=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		      eps1_1, eps1_2, eps2_1, eps2_2,
		      gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);

      
      azim=kkappa;  
      FstKK_90=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
		       eps1_1, eps1_2, eps2_1, eps2_2,
		       gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SndKK_90=SV_term(0., 0., Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
		       0., 0., 0., 0.,
		       Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa) +
	       ISO_2nd;
      FstKK_kappa=SV_term(DG_G, Dr_r, delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			  eps1_1, eps1_2, eps2_1, eps2_2,
			  gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);
      
      /* interpolate the error for the halfspaces rotated by the angle true_kappa */

      if(fabs(D=Fst0_90-Fst0_0) < LC_TINY){
	Snd0_kappa = 0.5*(Snd0_0 + Snd0_90);
      }
      else {
	Snd0_kappa = Snd0_0 + ((Snd0_90-Snd0_0)/D)*(Fst0_kappa-Fst0_0);
      }

      if(fabs(D=Fst90_90-Fst90_0) < LC_TINY) {
	Snd90_kappa = 0.5*(Snd90_0 + Snd90_90);
      }
      else {
	Snd90_kappa = Snd90_0 + ((Snd90_90-Snd90_0)/D)*(Fst90_kappa-Fst90_0);
      }

      if(fabs(D=FstK_90-FstK_0) < LC_TINY) {
	SndK_kappa =0.5*(SndK_90 + SndK_0);
      }
      else {
	SndK_kappa = SndK_0 + ((SndK_90-SndK_0)/D)*(FstK_kappa-FstK_0);
      }
      
      if(fabs(D=FstKK_90-FstKK_0) < LC_TINY) {
	SndKK_kappa = 0.5*(SndKK_90 + SndKK_0);
      }
      else {
	SndKK_kappa = SndKK_0 + ((SndKK_90-SndKK_0)/D)*(FstKK_kappa-FstKK_0);
      }

      (*rsv_1st).global[0]=Fst0_kappa;
      (*rsv_2nd).global[0]=Snd0_kappa;
      (*rsv_1st).global[1]=Fst90_kappa;
      (*rsv_2nd).global[1]=Snd90_kappa;
      (*rsv_1st).global[2]=FstK_kappa;
      (*rsv_2nd).global[2]=SndK_kappa;
      (*rsv_1st).global[3]=FstKK_kappa;
      (*rsv_2nd).global[3]=SndKK_kappa;  
      
    }
  
  
  
  /* SH */
  /* The following terms are derived on the assumption that Rps coefficients for
     two identicaly anisotropic halfspaces are zero (with 0 velocity contrats)
  */

  Delta1_1=SHD1_1 +
           (0.8*d11_plus*d21_plus +0.33*d11_plus*d21_min +0.33*d11_min*d21_plus +2.0*d11_min*d21_min)*
                                                             delta1_1*delta2_1*pow(cos(1*err_kappa),2) + 
           (0.8*d11_plus*d22_plus +0.33*d11_plus*d22_min +0.33*d11_min*d22_plus +2.0*d11_min*d22_min)* 
                                                             delta1_1*delta2_2*pow(sin(1*err_kappa),2) +
           (0.22*d11_plus*gs2_plus +0.*d11_plus*gs2_min +0.4*d11_min*gs2_plus +0.22*d11_min*gs2_min)*
                                                            delta1_1*gamma2_44*pow(cos(1*err_kappa),2);
  
  Delta1_2=SHD1_2 + 
           (0.8*d12_plus*d21_plus +0.33*d12_plus*d21_min +0.33*d12_min*d21_plus +2.0*d12_min*d21_min)*
                                                             delta1_2*delta2_1*pow(sin(1*err_kappa),2) + 
           (0.8*d12_plus*d22_plus +0.33*d12_plus*d22_min +0.33*d12_min*d22_plus +2.0*d12_min*d22_min)* 
                                                             delta1_2*delta2_2*pow(cos(1*err_kappa),2) +
           (-0.22*d12_plus*gs2_plus +0.33*d12_plus*gs2_min -0.22*d12_min*gs2_plus -0.32*d12_min*gs2_min)*
                                                            delta1_2*gamma2_44*pow(cos(1*err_kappa),2); 

  Delta2_1=SHD2_1 + 
           (0.61*d21_plus*d11_plus +0.*d21_plus*d11_min +0.*d21_min*d11_plus +1.56*d21_min*d11_min)*
                                                             delta2_1*delta1_1*pow(cos(1*err_kappa),2) + 
           (0.61*d21_plus*d12_plus + 0.*d21_plus*d12_min + 0.*d21_min*d12_plus +1.56*d21_min*d12_min)* 
                                                             delta2_1*delta1_2*pow(sin(1*err_kappa),2) +
           (-0.28*d21_plus*gs1_plus -1.1*d21_plus*gs1_min -0.44*d21_min*gs1_plus -1.56*d21_min*gs1_min)*
                                                            delta2_1*gamma1_44*pow(cos(1*err_kappa),2);
  

  Delta2_2=SHD2_2 + 
           (0.61*d22_plus*d11_plus + 0.*d22_plus*d11_min + 0.*d22_min*d11_plus +1.56*d22_min*d11_min)*
                                                             delta2_2*delta1_1*pow(sin(1*err_kappa),2) + 
           (0.61*d22_plus*d12_plus +0*d22_plus*d12_min +0*d22_min*d12_plus +1.56*d22_min*d12_min)* 
                                                             delta2_2*delta1_2*pow(cos(1*err_kappa),2) +
           (-0.11*d22_plus*gs1_plus -0.11*d22_plus*gs1_min -0.0*d22_min*gs1_plus -0.64*d22_min*gs1_min)*
                                                            delta2_2*gamma1_44*pow(cos(1*err_kappa),2);


  Gamma1_44=SHGS_1 + 
            (-0.0*gs1_plus*d21_plus -0.0*gs1_plus*d21_min -0.0*gs1_min*d21_plus -0.0*gs1_min*d21_min)*
                                                             gamma1_44*delta2_1*pow(cos(1*err_kappa),2) +
            (0.*gs1_plus*d22_plus +0.*gs1_plus*d22_min +0.*gs1_min*d22_plus +0.*gs1_min*d22_min)*
                                                             gamma1_44*delta2_2*pow(cos(2*err_kappa),2) +
            (0.45*gs1_plus*gs2_plus +0.*gs1_plus*gs2_min +0.1*gs1_min*gs2_plus +0.78*gs1_min*gs2_min)*
                                                            gamma1_44*gamma2_44*pow(cos(1*err_kappa),2);
  
  Gamma2_44=SHGS_2+
            ((1./3.)*gs2_plus*gs1_plus +0*gs2_plus*gs1_min +0*gs2_min*gs1_plus +(2./3.)*gs2_min*gs1_min)*
                                                            gamma2_44*gamma1_44*pow(cos(1*err_kappa),2); 

  /* The following terms are based on error minimization for 2-parameter model only.
     Notice that such 2nd order terms do not result in Rps=0 for two identically
     anisotropic halfspaces (probably not good to use for most cases).
  
  
  Delta1_1=SHD1_1 +
           (0.8*d11_plus*d21_plus +0.33*d11_plus*d21_min +0.33*d11_min*d21_plus +2.0*d11_min*d21_min)*
                                                             delta1_1*delta2_1*cos(2*err_kappa) + 
           (0.11*d11_plus*d22_plus -0.11*d11_plus*d22_min +0.11*d11_min*d22_plus +0.11*d11_min*d22_min)* 
                                                             delta1_1*delta2_2*cos(2*err_kappa) +
           (0*d11_plus*gs2_plus +0.5*d11_plus*gs2_min +0.4*d11_min*gs2_plus +0.7*d11_min*gs2_min)*
                                                            delta1_1*gamma2_44*cos(2*err_kappa);
  
  Delta1_2=SHD1_2 + 
           (0.11*d12_plus*d21_plus -0.11*d12_plus*d21_min +0.11*d12_min*d21_plus +0.11*d12_min*d21_min)*
                                                             delta1_2*delta2_1*cos(2*err_kappa) + 
           (0.8*d12_plus*d22_plus +0.33*d12_plus*d22_min +0.33*d12_min*d22_plus +2.0*d12_min*d22_min)* 
                                                             delta1_2*delta2_2*cos(2*err_kappa) +
           (0.11*d12_plus*gs2_plus +0.22*d12_plus*gs2_min +0*d12_min*gs2_plus +0.11*d12_min*gs2_min)*
                                                            delta1_2*gamma2_44*cos(2*err_kappa); 

  Delta2_1=SHD2_1 + 
           (0.61*d21_plus*d11_plus +0*d21_plus*d11_min +0*d21_min*d11_plus +1.56*d21_min*d11_min)*
                                                             delta2_1*delta1_1*cos(2*err_kappa) + 
           (0*d21_plus*d12_plus + 0*d21_plus*d12_min + 0*d21_min*d12_plus + 0*d21_min*d12_min)* 
                                                             delta2_1*delta1_2*cos(2*err_kappa);
  

  Delta2_2=SHD2_2 + 
           (0*d22_plus*d11_plus + 0*d22_plus*d11_min + 0*d22_min*d11_plus + 0*d22_min*d11_min)*
                                                             delta2_2*delta1_1*cos(2*err_kappa) + 
           (0.61*d22_plus*d12_plus +0*d22_plus*d12_min +0*d22_min*d12_plus +1.56*d22_min*d12_min)* 
                                                             delta2_2*delta1_2*cos(2*err_kappa);


  Gamma1_44=SHGS_1 + 
            (-0.15*gs1_plus*d21_plus -0.3*gs1_plus*d21_min -0.25*gs1_min*d21_plus -0.4*gs1_min*d21_min)*
                                                             gamma1_44*delta2_1*cos(2*err_kappa) +
            (0*gs1_plus*d22_plus +0*gs1_plus*d22_min +0*gs1_min*d22_plus +0*gs1_min*d22_min)*
                                                             gamma1_44*delta2_2*cos(2*err_kappa) +
            (0.45*gs1_plus*gs2_plus +0*gs1_plus*gs2_min +0.1*gs1_min*gs2_plus +0.78*gs1_min*gs2_min)*
                                                            gamma1_44*gamma2_44*cos(2*err_kappa);
  
  Gamma2_44=SHGS_2+
            ((1./3.)*gs2_plus*gs1_plus +0*gs2_plus*gs1_min +0*gs2_min*gs1_plus +(2./3.)*gs2_min*gs1_min)*
                                                            gamma2_44*gamma1_44*cos(2*err_kappa); 
  */

  if(index == 1)
    {
      /* here, err_kappa=0: evaluate the error for alligned halfspaces */

      azim=20.*PI/180;
      (*rsh_1st).global[0]=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsh_2nd).global[0]=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0., 
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa);
 
      
      azim=45.*PI/180;
      (*rsh_1st).global[1]=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsh_2nd).global[1]=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0., 
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      
      azim=70.*PI/180;
      (*rsh_1st).global[2]=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
				   eps1_1, eps1_2, eps2_1, eps2_2,
				   gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      (*rsh_2nd).global[2]=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
				   0., 0., 0., 0.,
				   Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      
      (*rsh_1st).global[3]=0.0;
      (*rsh_2nd).global[3]=0.0;

    }

  SHFst20_0=(*rsh_1st).global[0];
  SHSnd20_0=(*rsh_2nd).global[0];
  SHFst45_0=(*rsh_1st).global[1];
  SHSnd45_0=(*rsh_2nd).global[1];
  SHFst70_0=(*rsh_1st).global[2];
  SHSnd70_0=(*rsh_2nd).global[2];

  if(index == 2)

    {
    /* here, err_kappa = 90.: evaluate the error for the halfspaces rotated by 90 deg */

      azim=20.*PI/180;
      SHFst20_90=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			 eps1_1, eps1_2, eps2_1, eps2_2,
			 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SHSnd20_90=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
			 0., 0., 0., 0., 
			 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SHFst20_kappa=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			 eps1_1, eps1_2, eps2_1, eps2_2,
			 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);
 
      
      azim=45.*PI/180;
      SHFst45_90=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			 eps1_1, eps1_2, eps2_1, eps2_2,
			 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SHSnd45_90=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
			 0., 0., 0., 0., 
			 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SHFst45_kappa=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			 eps1_1, eps1_2, eps2_1, eps2_2,
			 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);
 
      
      azim=70.*PI/180;
      SHFst70_90=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			 eps1_1, eps1_2, eps2_1, eps2_2,
			 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SHSnd70_90=SH_term(Delta1_1, Delta1_2, 0., Delta2_1, Delta2_2, 0.,
			 0., 0., 0., 0.,
			 Gamma1_44, Gamma2_44, alpha, beta, err_ang, azim, err_kappa);
      SHFst70_kappa=SH_term(delta1_1, delta1_2, delta1_3, delta2_1, delta2_2, delta2_3,
			 eps1_1, eps1_2, eps2_1, eps2_2,
			 gamma1_44, gamma2_44, alpha, beta, err_ang, azim, true_kappa);

      /* interpolate the error for the halfspaces rotated by the angle true_kappa */

      if(fabs(D=SHFst20_90-SHFst20_0) < LC_TINY){
	SHSnd20_kappa = 0.5*(SHSnd20_0 + SHSnd20_90);
      }
      else {
	SHSnd20_kappa = SHSnd20_0 + ((SHSnd20_90-SHSnd20_0)/D)*(SHFst20_kappa-SHFst20_0);
      }

      if(fabs(D=SHFst45_90-SHFst45_0) < LC_TINY) {
	SHSnd45_kappa = 0.5*(SHSnd45_0 + SHSnd45_90);
      }
      else {
	SHSnd45_kappa = SHSnd45_0 + ((SHSnd45_90-SHSnd45_0)/D)*(SHFst45_kappa-SHFst45_0);
      }
      if(fabs(D=SHFst70_90-SHFst70_0) < LC_TINY) {
	SHSnd70_kappa = 0.5*(SHSnd70_0 + SHSnd70_90);
      }
      else {
	SHSnd70_kappa = SHSnd70_0 + ((SHSnd70_90-SHSnd70_0)/D)*(SHFst70_kappa-SHFst70_0);
      }

      (*rsh_1st).global[0]=SHFst20_kappa;
      (*rsh_2nd).global[0]=SHSnd20_kappa;
      (*rsh_1st).global[1]=SHFst45_kappa;
      (*rsh_2nd).global[1]=SHSnd45_kappa;
      (*rsh_1st).global[2]=SHFst70_kappa;
      (*rsh_2nd).global[2]=SHSnd70_kappa;
      
    }
  
  return(0);
  
}

/**********************************/
/********** the end ***************/
/**********************************/



float SV_term(float DG_G, float Dr_r, 
	      float Delta1_1, float Delta1_2, float Delta1_3,
	      float Delta2_1, float Delta2_2, float Delta2_3, 
	      float Eps1_1, float Eps1_2, float Eps2_1, float Eps2_2,
	      float Gamma1_44, float Gamma2_44, 
	      float alpha, float beta, float ang, float azim, float kappa)

     /* This subroutine evaluates individual angular terms (inc. angle)  
	for Rpsv 1st-order approximation. The input represents the medium parameters 
	(Thomsen's type aniso parameters (GammaX_44 represents GammaX_s), plus 
	S-wave impedance contrast ratio DG_G, density contrast ratio Dr_r and 
	background P- and S-wave velocities), and incidence angle "ang",
	azimuth "azim" and rotation angle "kappa", all being phase angles in radians.
     */

{
  float cos2a, sin2a, cos2a_k, sin2a_k;
  float CS, S_C, CS3, S3_C, S5_C, result=0.;
  

  cos2a_k = pow(cos(azim-kappa),2);
  sin2a_k = pow(sin(azim-kappa),2);
  cos2a = pow(cos(azim),2);
  sin2a = pow(sin(azim),2);
 
  CS = -beta/alpha*DG_G - 
      (alpha*beta)/(2*(pow(alpha,2)-pow(beta,2)))*(Delta2_2*cos2a_k +
      Delta2_1*sin2a_k -
      Delta1_1*sin2a -
      Delta1_2*cos2a )
      -2*beta/alpha*(Gamma2_44*sin2a_k - Gamma1_44*sin2a );


  
  S_C = -0.5*Dr_r +
    pow(alpha,2)/(2*(pow(alpha,2)-pow(beta,2)))*(Delta2_2*cos2a_k +
						 Delta2_1*sin2a_k -
						 Delta1_2*cos2a -
						 Delta1_1*sin2a);

  CS3 = -alpha*beta/(pow(alpha,2)-pow(beta,2))*(Delta1_1*sin2a + 
						Delta1_2*cos2a  -
						Delta2_1*sin2a_k -
						Delta2_2*cos2a_k -
						Delta1_3*cos2a*sin2a -
						Eps1_2*(cos2a*cos2a+2*cos2a*sin2a) -
						Eps1_1*sin2a*sin2a +
						Eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) +
						Eps2_1*sin2a_k*sin2a_k +
						Delta2_3*cos2a_k*sin2a_k					  
						);
  
  S3_C = pow(beta,2)/pow(alpha,2)*DG_G +
    pow(beta,2)/(2*(pow(alpha,2)-pow(beta,2)))*(Delta1_1*sin2a +
						Delta1_2*cos2a -
						Delta2_1*sin2a_k -
						Delta2_2*cos2a_k) +
    pow(alpha,2)/(pow(alpha,2)-pow(beta,2))*(Delta1_1*sin2a +
					     Delta1_2*cos2a -
					     Delta2_1*sin2a_k -
					     Delta2_2*cos2a_k -
					     Delta1_3*cos2a*sin2a -
					     Eps1_1*sin2a*sin2a -
					     Eps1_2*(cos2a*cos2a+2*cos2a*sin2a) +
					     Eps2_1*sin2a_k*sin2a_k +
					     Eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) +
					     Delta2_3*cos2a_k*sin2a_k) +
    2*pow(beta/alpha,2)*(Gamma2_44*sin2a_k -
			 Gamma1_44*sin2a);
  

  S5_C = -pow(beta,2)/(pow(alpha,2)-pow(beta,2))*(Delta1_1*sin2a +
						  Delta1_2*cos2a -
						  Delta2_1*sin2a_k -
						  Delta2_2*cos2a_k -
						  Delta1_3*cos2a*sin2a -
						  Eps1_1*sin2a*sin2a -
						  Eps1_2*(cos2a*cos2a+2*cos2a*sin2a) +
						  Eps2_1*sin2a_k*sin2a_k +
						  Eps2_2*(cos2a_k*cos2a_k+2*sin2a_k*cos2a_k) +
						  Delta2_3*cos2a_k*sin2a_k);

  
  result = CS * cos(ang) * sin(ang)
         + S_C * sin(ang)
         + CS3 * cos(ang)*pow(sin(ang),3)
         + (S3_C + S_C*0.5*pow(beta/alpha,2)) * pow(sin(ang),3)
         + (S5_C + S3_C*0.5*pow(beta/alpha,2)) * pow(sin(ang),5)
         + S5_C*0.5*pow(beta/alpha,2) * pow(sin(ang),7);


 return(result);
 
}

/*****************************************/
/************** the end ******************/
/*****************************************/



float SH_term(float Delta1_1, float Delta1_2, float Delta1_3, 
	      float Delta2_1, float Delta2_2, float Delta2_3,
	      float Eps1_1, float Eps1_2, float Eps2_1, float Eps2_2,
	      float Gamma1_44, float Gamma2_44, float alpha, float beta, 
	      float ang, float azim, float kappa)

     /* This subroutine evaluates individual angular terms (inc. angle)  
	for Rpsh 1st-order approximation. The input represents the medium parameters 
	(Thomsen's type aniso parameters (GammaX_44 represents GammaX_s), plus 
	S-wave impedance contrast ratio DG_G, density contrast ratio Dr_r and 
	background P- and S-wave velocities), and incidence angle "ang",
	azimuth "azim" and rotation angle "kappa", all being phase angles in radians.
     */

{
  float cos2a, sin2a, cos2a_k, sin2a_k;
  float S, CS_C, CS3_C, S3, result;
  

  cos2a_k = pow(cos(azim-kappa),2);
  sin2a_k = pow(sin(azim-kappa),2);
  cos2a = pow(cos(azim),2);
  sin2a = pow(sin(azim),2);
 
  
  S = pow(alpha,2)/(4*(pow(alpha,2)-pow(beta,2)))*((Delta2_2-Delta2_1)*sin(2*(azim-kappa)) +
						   (Delta1_1-Delta1_2)*sin(2*azim));

  CS_C = alpha*beta/(4*(pow(alpha,2)-pow(beta,2)))*((Delta2_1-Delta2_2)*sin(2*(azim-kappa)) +
						    (Delta1_2-Delta1_1)*sin(2*azim)) +
    beta/alpha*((Gamma2_44)*sin(2*(azim-kappa)) -
		(Gamma1_44)*sin(2*azim));
  
  CS3_C = alpha*beta/(2*(pow(alpha,2)-pow(beta,2)))*((Delta1_1-Delta1_2-Delta1_3*(cos2a-sin2a) +
						      2*(Eps1_2-Eps1_1)*sin2a)*sin(azim)*cos(azim) -
						     (Delta2_1-Delta2_2-Delta2_3*(cos2a_k-sin2a_k) +
						      2*(Eps2_2-Eps2_1)*sin2a_k)*sin(azim-kappa)*cos(azim-kappa));

  
  S3 = pow(alpha,2)/(2*(pow(alpha,2)-pow(beta,2)))*((-Delta1_1+Delta1_2+Delta1_3*(cos2a-sin2a) -
						     2*(Eps1_2-Eps1_1)*sin2a)*sin(azim)*cos(azim) +
						    (Delta2_1-Delta2_2-Delta2_3*(cos2a_k-sin2a_k) +
						     2*(Eps2_2-Eps2_1)*sin2a_k)*sin(azim-kappa)*cos(azim-kappa));

	     
  result = S * sin(ang)
    + CS_C * cos(ang)*sin(ang)
    + (CS3_C + CS_C*0.5*pow(beta/alpha,2)) * cos(ang)*pow(sin(ang),3)
    + S3 * pow(sin(ang),3)
    + CS3_C*0.5*pow(beta/alpha,2) * cos(ang)*pow(sin(ang),5);


 return(result);
 
}


#define stretch 0  /*1=perform stretching, 0=do not. 
		    The stretching should not be performed if the purpose of this subroutine is
		    to obtain Rpsv and Rpsh components from data since it slightly changes the 
		    original (and correct) mutual directions of Rps1 and Rps2 vector amplitudes 
		    to be projected, although such changes are usually negligable (even for 
		    relatively strong anisotropies). 
		    The stretching should be performed if the purpose of this 
		    subroutine is to obtain the common polarization angle Phi from data (and,
		    possibly, to compare it with a theoretical value) since the stretching is the 
		    optimal way to unify the polarization angles Phi1 and Phi2 corresponding to the 
		    two amplitude vectors Rps1 and Rps2. */

int Phi_rot(float *rs1,float *rs2,int iso_plane,float pb_x,float pb_y,float pb_z,float gs1_x,float gs1_y,
	    float gs1_z,float gs2_x,float gs2_y,float gs2_z,float *CPhi1,float *SPhi1, float *CPhi2,float *SPhi2)

     /*

       The subroutine evaluates cosinus and sinus of polarizarization
       rotation angle Phi of s1 and s2 waves from real data: based on a projection of
       (gs1_x,gs1_y,gs1_z) and (gs2_x,gs2_y,gs2_z) polarization
       vectors S1 and S2 waves onto an isotropic plane of rotation
       with the normal (pb_x,pb_y,pb_z), followed by a perpendicular
       correction (optional) of projected polarization vectors, obtaining g10 and
       g20 pol. vectors in the background medium. Then angle Phi is
       found as the angle between g10 and gsv vectors, CPhi is simply
       g10_sv component, SPhi is g10_sh component. Finally, signs of
       background polarizations as well as rs1, rs2 coefficients are
       further adjusted; S1 and S2 are righ-hand oriented, S1 is
       always located in the 1st or 4th quadrant of the SV-SH
       coordinate system.  

       Input: rs1, rs2 ... PS1, PS2 reflection
       coefficients from data vec{pb} ... a normal constraining
       isotropic plane of pol. angle rotation 
       vec{gs1}, vec{gs2}... S1 and S2 polarization vectors from data 
       iso_plane ...0=projection in an isotropic plane defined by background slowness 
                    (better for RSH),
                    1=projection in s1-s2 anisotropic plane defined by S-wave polarizations 
		    (better for RSV) 
 
       Output: 
       CPhi1, SPhi1... cosinus and sinus of Phi (rotation polarization angle) of 
                       s1 wave with rs1 refl. coefficient,
       CPhi2, SPhi2... cosinus and sinus of Phi (rotation polarization angle) of 
                       s2 wave with rs2 refl. coefficient,
       s1 and s2 polarizations are right-handed and supposedly almost perpendicular, 
       s1 is always located in the 1th or 4th quadrant, s2 in 2nd or 1st quadrant, 
       respectively 

       All vectors should be given in right-hand coordinate system with z-axis pointing
       upwards.

     */

{

  float azim,gv_x=0.0,gv_y=0.0,gv_z=0.0,gh_x=0.0,gh_y=0.0,gh_z=0.0;
  float p_h,norm,denom,alpha,temp;
  float ax_x,ax_y,ax_z,n_x,n_y,n_z,prl_x,prl_y,prl_z;
  float s1_prl,s1_ax,s2_prl,s2_ax,s1_v,s1_h,s2_v,s2_h,a11,a12;
  float g10_v,g10_h,g20_v,g20_h;
  
  float az_n,an_n,sv_n_x,sv_n_y,sv_n_z,sh_n_x,sh_n_y,sh_n_z;
    
  /* let's start: just to make sure  */
  
  norm = sqrt(pow(gs1_x,2)+pow(gs1_y,2)+pow(gs1_z,2));
  gs1_x = gs1_x/norm;
  gs1_y = gs1_y/norm;
  gs1_z = gs1_z/norm;

  norm = sqrt(pow(gs2_x,2)+pow(gs2_y,2)+pow(gs2_z,2));
  gs2_x = gs2_x/norm;
  gs2_y = gs2_y/norm;
  gs2_z = gs2_z/norm;
  
  if (iso_plane != 1) 
    {
      if (pb_x != 0.) 
	{
	  if (pb_y == 0.) {
	    if (pb_x > 0.) azim = 0.;
	    if (pb_x < 0.) azim = PI;
	  }
	  else{
	    if (pb_x > 0. && pb_y > 0.) azim = atan2(pb_y,pb_x);
	    if (pb_x < 0. && pb_y > 0.) azim = PI + atan2(pb_y,pb_x);
	    if (pb_x < 0. && pb_y < 0.) azim = PI + atan2(pb_y,pb_x);
	    if (pb_x > 0. && pb_y < 0.) azim = 2*PI + atan2(pb_y,pb_x);
	  }
	}
      
      if (pb_x == 0.)
	{
	  if (pb_y == 0.) azim = 0.;
	  if (pb_y > 0.) azim = PI/2;
	  if (pb_y < 0.) azim = 3*PI/2;
	}
      
  
  /* now, determine gsv and gsh polarization vectors in the background isotropy plane */
      
      p_h = sqrt(pow(pb_x,2)+pow(pb_y,2));
      
      if (p_h != 0.)
	{
	  gh_x = -pb_y/p_h;
	  if (pb_x != 0.) gh_y = (pb_x/fabs(pb_x))*sqrt(1-pow(gh_x,2));
	  if (pb_x == 0.) gh_y = 0.;
	}
      else
	{
	  gh_x = 0.;
	  gh_y = 1.;
	}
      gh_z = 0.;
      
      gv_x = gh_y*pb_z - gh_z*pb_y;
      gv_y = gh_z*pb_x - gh_x*pb_z;
      gv_z = gh_x*pb_y - gh_y*pb_x;
      norm = sqrt(pow(gv_x,2)+pow(gv_y,2)+pow(gv_z,2));
      gv_x = gv_x/norm;
      gv_y = gv_y/norm;
      gv_z = gv_z/norm;
      
      /*  fprintf(stderr,"gv_x=%f gv_y=%f gv_z=%f gh_x=%f gh_y=%f gh_z=%f\n",gv_x,gv_y,gv_z,gh_x,gh_y,gh_z);
	fprintf(stderr,"pb_x=%f pb_y=%f pb_z=%f norm=%f \n ",pb_x,pb_y,pb_z,norm);
      */
      
    }
  

  /* normal vector to the gs1-gs2 plane */

  n_x = gs1_y*gs2_z - gs2_y*gs1_z;
  n_y = gs1_z*gs2_x - gs2_z*gs1_x;
  n_z = gs1_x*gs2_y - gs2_x*gs1_y;
  norm = sqrt(pow(n_x,2)+pow(n_y,2)+pow(n_z,2));
  if (norm == 0)
    {
      fprintf(stderr,"! Paralel S-wave polarizations !");
      fprintf(stderr,"  Either wrong data or extreme anisotropy or a vicinity of S-wave singularity! \n" );
      return(-1);
    }  
  n_x = n_x/norm;
  n_y = n_y/norm;
  n_z = n_z/norm;

  if ((pb_x*n_x+pb_y*n_y+pb_z*n_z) < 0.)
    {
      n_x = -n_x;
      n_y = -n_y;
      n_z = -n_z;

      temp = gs1_x;
      gs1_x = gs2_x;
      gs2_x = temp;

      temp = gs1_y;
      gs1_y = gs2_y;
      gs2_y = temp;

      temp = gs1_z;
      gs1_z = gs2_z;
      gs2_z = temp;

      temp = *rs1;
      *rs1 = *rs2;
      *rs2 = temp;
      
    }
  else if ((pb_x*n_x+pb_y*n_y+pb_z*n_z) == 0.)
    {
      fprintf(stderr," ! S-wave polarizations in a plane paralel to the slowness vector!");
      fprintf(stderr,"   Either wrong data or extreme anisotropy \n");
      return(-1);
    }  

  
  /* the last choice if the plane of projection: iso or aniso ? */

  if (iso_plane == 1)
    {
      az_n = atan2(n_y,n_x);
      an_n = atan2(n_z,sqrt(pow(n_x,2)+pow(n_y,2)));
      sv_n_x = sin(an_n)*cos(az_n);
      sv_n_y = sin(an_n)*sin(az_n);
      sv_n_z = -cos(an_n);
      sh_n_x = -sin(az_n);
      sh_n_y = cos(az_n);
      sh_n_z = 0;
      
      s1_v = gs1_x*sv_n_x + gs1_y*sv_n_y + gs1_z*sv_n_z;
      s1_h = gs1_x*sh_n_x + gs1_y*sh_n_y + gs1_z*sh_n_z;
      s2_v = gs2_x*sv_n_x + gs2_y*sv_n_y + gs2_z*sv_n_z;
      s2_h = gs2_x*sh_n_x + gs2_y*sh_n_y + gs2_z*sh_n_z;
      
      goto jump;
      
    }

  /* axis of rotation of the planes gs1-gs2 -> gs10-gs20 */
      
  ax_x = n_y*pb_z - n_z*pb_y;
  ax_y = n_z*pb_x - n_x*pb_z;
  ax_z = n_x*pb_y - n_y*pb_x;

  norm = sqrt(pow(ax_x,2)+pow(ax_y,2)+pow(ax_z,2));
  if (norm == 0)
    {
      ax_x = gv_x;
      ax_y = gv_y;
      ax_z = gv_z;
    }
  else
    {
      ax_x = ax_x/norm;
      ax_y = ax_y/norm;
      ax_z = ax_z/norm;
    }
  
  /* paralel vector with the plane gs1-gs2, perpendicular to ax and n:
     (n,ax,prl) complete right-hand orthogonal basis */

  prl_x = n_y*ax_z - n_z*ax_y;
  prl_y = n_z*ax_x - n_x*ax_z;
  prl_z = n_x*ax_y - n_y*ax_x;
  norm = sqrt(pow(prl_x,2)+pow(prl_y,2)+pow(prl_z,2));
  prl_x = prl_x/norm;
  prl_y = prl_y/norm;
  prl_z = prl_z/norm;


  /* coordinates of gs1, gs2 in (ax,prl) plane */

  s1_prl =  gs1_x*prl_x + gs1_y*prl_y + gs1_z*prl_z;
  s1_ax = gs1_x*ax_x + gs1_y*ax_y + gs1_z*ax_z;
  s2_prl =  gs2_x*prl_x + gs2_y*prl_y + gs2_z*prl_z;
  s2_ax = gs2_x*ax_x + gs2_y*ax_y + gs2_z*ax_z;


  /* rotation of gs1, gs2 onto sv-sh plane */

  a11 = ax_x*gv_x + ax_y*gv_y + ax_z*gv_z;
  a12 = ax_x*gh_x + ax_y*gh_y + ax_z*gh_z;
  s1_v = a11*s1_ax - a12*s1_prl;
  s1_h = a12*s1_ax + a11*s1_prl;
  s2_v = a11*s2_ax - a12*s2_prl;
  s2_h = a12*s2_ax + a11*s2_prl;

 jump:

  /* the closest stretched perpendicular polarization vectors g10, g20*/

  if (!(denom = s1_v*s2_h - s1_h*s2_v)){
        fprintf(stderr,"this should not happen \n");
	return(-1);
  }
  alpha = 0.5*(PI/2-acos(s1_v*s2_v + s1_h*s2_h));
  
  /*this is for stretching of vectors up to a common angle Phi */

  if (stretch == 1)
    {
      g10_v = (cos(alpha)*s2_h - sin(alpha)*s1_h)/denom;
      g10_h = -(cos(alpha)*s2_v - sin(alpha)*s1_v)/denom;
      g20_v = -(cos(alpha)*s1_h - sin(alpha)*s2_h)/denom;
      g20_h = (cos(alpha)*s1_v - sin(alpha)*s2_v)/denom;
      
      s1_v = g10_v;
      s1_h = g10_h;
      s2_v = g20_v;
      s2_h = g20_h;
    }
  

  /* put s1 wave into 1 or 4 quadrant */

  if(s1_v < 0.)
    {
      s1_v = -s1_v;
      s1_h = -s1_h;
      *rs1 = -(*rs1);
    }
  
  /* put s2 wave into 1 or 2 quadrant */

  if(s2_h < 0.)
    {
      s2_v = -s2_v;
      s2_h = -s2_h;
      *rs2 = -(*rs2);
    }

  /* make sure s1->s2 is right-handed */
  
  if ((s1_v*s2_h - s1_h*s2_v) < 0.)
    {
      temp = s1_v;
      s1_v = s2_v;
      s2_v = temp;
      temp = s1_h;
      s1_h = s2_h;
      s2_h = temp;
      temp = *rs1;
      *rs1 = *rs2;
      *rs2 = temp;
      
    } 

  /* cos and sin of Phi */

  *CPhi1= s1_v;
  *SPhi1= s1_h;
  *CPhi2= s2_v;
  *SPhi2= s2_h;
  
  
  /* O.K. this is it */

  return(0);
  
}
