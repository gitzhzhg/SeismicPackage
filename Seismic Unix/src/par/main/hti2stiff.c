/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* HTI2STIFF: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"
#include "anisotropy.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" HTI2STIFF - convert HTI parameters alpha, beta, d(V), e(V), gamma	",
"		into stiffness tensor					",
"									",
"    hti2stiff  [optional parameter] (output is to   outpar)		",
"									",
" Optional Parameters							",
" alpha=2	    isotropy-plane p-wave velocity		   	",
" beta=1	     fast isotropy-plan s-wave velocity			",
" ev=0		e(V) 							",
" dv=0		d(V)							",
" gamma=0	    shear-wave splitting parameter			",
" rho=1		density							",
" sign		     sign of c13+c55 ( for most materials sign=1)	",
" outpar=/dev/tty    output parameter file				",
"									",
" Output:								",
"  c_ijkl	    stiffness components for x1=symmetry axis		",
"		    x3= vertical					",
NULL};

/*
 *
 * Credits:   Andreas Rueger, CWP Aug 01, 1996
 *
 * Reference: Andreas Rueger, P-wave reflection coefficients for
 *    transverse isotropy with vertical and horizontal axis
 *	    of symmetry,  GEOPHYSICS
 */

/**************** end self doc ********************************/


int main (int argc, char **argv)
{
	char *outpar=NULL;	/* name of file holding output  */
	FILE *outparfp=NULL;	/* ... its file pointer		*/

	float temp1,temp2,temp3,rho;
	float alpha,beta,ev,dv,gamma;
	
	int sign;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);


	/* Get parameters */
	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");

	if (!getparfloat("alpha", &alpha))	alpha=2.0;
	if (!getparfloat("beta", &beta))	beta=1.0;
	if (!getparfloat("dv", &dv))		dv=0.0;
	if (!getparfloat("gamma", &gamma))	gamma=0.0;
	if (!getparfloat("rho", &rho))		rho=1.0;
	if (!getparfloat("ev", &ev))		ev=0.0;
	if (!getparint("sign", &sign))		sign=1;
	

        checkpars();

	/* check the values */
	if(alpha<=0. || beta<=0. )
	{
	   err(" \n NEGATIVE VELOCITIES ");
	   return (-1);
	}

	fprintf(outparfp,"\n hti2stiff : \n \n INPUT parameters \n\n");
	fprintf(outparfp,"alpha = %f\n",alpha);
	fprintf(outparfp,"beta = %f\n",beta);
	fprintf(outparfp,"dv = %f\n",dv);
	fprintf(outparfp,"ev = %f\n",ev);
	fprintf(outparfp,"gamma = %f\n",gamma);
	fprintf(outparfp,"rho = %f\n",rho);
	fprintf(outparfp,"sign = %i\n",sign);
	
	
	/* temporary variables */
	temp1 = alpha*alpha;
	temp2 = beta*beta;
	temp3 = rho*(temp1 - 2.* temp2);
	
	fprintf(outparfp," OUTPUT \n\n");
	fprintf(outparfp," c1111 = c11 = %f\n",(2.* temp1*ev + temp1)*rho );
	fprintf(outparfp," c2222 = c22 = %f\n",temp1*rho);
	fprintf(outparfp," c3333 = c33 = %f\n",temp1*rho);
	fprintf(outparfp," c2323 = c44 = %f\n",temp2*rho);

	/* C55 is slow shear-wave velocity */
	temp2 = temp2/(2.*gamma + 1.0);
	
	fprintf(outparfp," c1313 = c55 = %f\n",temp2*rho );
	fprintf(outparfp," c1212 = c66 = %f\n",temp2*rho );

	temp2 = temp2*rho;
	temp1 = temp1*rho;
	
	temp1 = 2.* dv *temp1 *(temp1-temp2)
		+(temp1-temp2)*(temp1-temp2);

	if(temp1 < 0.) {
	   err(" temp1 < 0 , unphysical ");
	   return (-1);
	}
	
	if(sign ==1)
		temp2 = sqrt(temp1)-temp2;
	else if(sign == -1)
		temp2 = - sqrt(temp1)-temp2;
	else
		return (-1);
	

 	fprintf(outparfp," c1122 = c12 = %f\n",temp2);
	fprintf(outparfp," c1133 = c13 = %f\n",temp2);
	fprintf(outparfp," c2233 = c23 = %f\n",temp3);

	return (1);

}

