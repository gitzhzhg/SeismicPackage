/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* THOM2HTI: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include <par.h>
#include "anisotropy.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" THOM2HTI - Convert Thompson parameters V_p0, V_s0, eps, gamma,	",
"              to the HTI parameters alpha, beta, epsilon(V), delta(V), ",
"	       gamma							",
"									",
" thom2hti  [optional parameter]                           		",
"									",
"									",
" vp=2         symm.axis p-wave velocity                        	",
" vs=1         symm.axis s-wave velocity                        	",
" eps=0        Thomsen's (generic) epsilon 	         		",
" gamma=0      Thomsen's generic gamma                                  ",
" weak=1       compute weak approximation                               ",
" outpar=/dev/tty	output parameter file				",
"									",
" Outputs:								",
"   alpha, beta, e(V), d(V), gamma					",
"									",
" Notes:								",
" Output is dumped to the screen and, if selected to outpar		",
"									",
" Code can be used to find models that satisfy the constraints		",
" that are imposed on HTI models caused by vertically fractured		",
" layers. For definition and use of the HTI parameter set see CWP-235.	",
NULL};

/* 
 *  Credits: Andreas Rueger, CWP
 *  For definition and use of the HTI parameter set see CWP-235.
 */

/**************** end self doc ********************************/

/* internal subroutines */
int moonshine(double a, double b, double c, double *x1, double *x2);
int testconstraint(double ev,double dv,double gamma,double alpha,double vs);
int testweak(double ev,double *dv,double gamma,double alpha,double vs);


int main (int argc, char **argv)
{

	double vp,vs,eps,dv,gamma,alpha,beta;
        double ev,x1=0.0,x2=0.0,gv;
	double a,b,c;
	char *outpar=NULL;	/* name of file holding output parfile  */
	FILE *outparfp=NULL;	/* ... its file pointer			*/

	int weak ;
	
	Stiff2D *spar1;

	spar1=(Stiff2D*)emalloc(sizeof(Stiff2D));

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	if (!getpardouble("vp",&vp)) vp = 2.0;
	if (!getpardouble("vs",&vs)) vs = 1.0;
	if (!getpardouble("eps",&eps)) eps = 0.;
	if (!getpardouble("gamma",&gamma)) gamma = 0.;	
	if (!getparint("weak",&weak)) weak = 1;

	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
        checkpars();



        spar1->a1111 = vp*vp;
	spar1->a3333 = (1+2*eps)*spar1->a1111;
	spar1->a1313 = vs*vs;
	spar1->a2323 = (1+2*gamma)*spar1->a1313;
	spar1->a1212 = spar1->a1313;
	
	

	
        /* need to get a13 from physical constraint */
        a = 1.0;
	b = 2*spar1->a2323;
	c = spar1->a1111*(2.*spar1->a2323 - spar1->a3333);
	/*ddprint(a);
	 ddprint(b);
	 ddprint(c); */
	
	if( moonshine(a,b,c,&x1,&x2)==0 )
	     fprintf(stderr,"\n\n Only COMPLEX solutions \n\n\n");

	/* ddprint(x1);
	
        ddprint(x2); */
	
	if(spar1->a2323 + x1 >0)
	{
	   spar1->a1133 = x1;
	   if( stiff2tv(spar1,&alpha,&beta,&ev,&dv,&gv)!=1)
	      fprintf(stderr,"ERROR in stiff2tv\n\n");

	   if(testconstraint(ev,dv,gamma,alpha,vs) !=1)
	      fprintf(stderr,"CONSTRAINT NOT SATISFIED \n\n");    

	   fprintf(outparfp,"\n Physically reasonable Model (1) \n");
	   
	   fprintf(outparfp," alpha = %g \n",alpha);
           fprintf(outparfp," beta  = %g \n",beta);
	   fprintf(outparfp," e(V)  = %g \n",ev);
	   fprintf(outparfp," d(V)  = %g \n",dv);
	   fprintf(outparfp," gamma = %g \n",gamma);
	   

	}
	else
	   fprintf(outparfp,"x1 not physically reasonable\n\n");
	

	if(spar1->a2323 + x2 >0)
	{
	   spar1->a1133 = x2;
	   if( stiff2tv(spar1,&alpha,&beta,&ev,&dv,&gv)!=1)
	      fprintf(stderr,"ERROR in stiff2tv\n\n");

	   if(testconstraint(ev,dv,gamma,alpha,vs) !=1)
	      fprintf(stderr,"CONSTRAINT NOT SATISFIED \n\n"); 
   
	   fprintf(outparfp,"\n Physically reasonable Model (2) \n");
	   
	   fprintf(outparfp," alpha = %g \n",alpha);
           fprintf(outparfp," beta  = %g \n",beta);
	   fprintf(outparfp," e(V)  = %g \n",ev);
	   fprintf(outparfp," d(V)  = %g \n",dv);
	   fprintf(outparfp," gamma = %g \n",gamma);

	   
	}  else
	   fprintf(outparfp,"\n Second solution not physically reasonable\n\n");
        
	if(weak && testweak(ev,&dv,gamma,alpha,vs) !=1)
	{
	      fprintf(stderr,"ERROR in testweak \n");
	}
        if(weak)
              fprintf(outparfp,"weak approx: d(V)=%g \n",dv);
 
	return (1);
}

int moonshine(double a, double b, double c, double *x1, double *x2)
{
   
   double sqr;
   
   sqr = b*b-4.*a*c;

   if(sqr < 0)
   {
      /* complex roots */
      ddprint(sqr);
      
      return 0;
      
   }
   
   sqr = sqrt(sqr);
   
   
   *x1 = (-b+sqr)/(2*a);
   *x2 = (-b-sqr)/(2*a);

   return 1;
   
}
int testconstraint(double ev,double dv,double gamma,double alpha,double vs)
{
   double f,dummy;
   
   dummy = alpha*alpha /(vs*vs);
   f     = 1 - 1/dummy;
   f     = 1/f;
   
   dummy = 0.5*dummy *(ev*(2-f)-dv)/(1+2*ev*f+sqrt(1+2*dv*f));
   dummy = dummy-gamma;
   
   if(ABS(dummy) > FLT_EPSILON)
   {
      ddprint(dummy);
      
      return -1;
   } else

      return 1;
}

   
int testweak(double ev,double *dv,double gamma,double alpha,double vs)
{
   double f,dummy;
   
   dummy = (vs*vs)/(alpha*alpha);
   f     = 1 - dummy;
   f     = 1/f;
   
   *dv   = ev*(2.-f)-4.*dummy*gamma;
   return 1;
} 
