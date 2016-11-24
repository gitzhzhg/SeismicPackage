/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* HUDSON: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"
#include "anisotropy.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)
#define ddprint(expr) printf(#expr " = %g\n",expr)

int info;


/*********************** self documentation **********************/
char *sdoc[] = {
"									",
"  HUDSON - compute  effective parameters of anisotropic solids	        ",
"	   using Hudson's crack theory.                      		",
"									",
" Required paramters: <none>                                             ",
"                                                                       ",
" Optional parameters                                                   ",
"                                                                       ",
" vp=4.5        p-wave velocity uncracked solid                  	",
" vs=2.53       s-wave velocity uncracked solid                         ",
" rho=2.8       density      						",
" cdens=0	crack density					        ",
" aspect=0      aspect ratio                                            ",
" fill=0        gas filled cracks                                       ",
"               =1 water filled                                         ",
" outpar        =/dev/tty   output file                                 ",
"                                                                       ", 
"Notes:									",
" The cracks are assumed to be vertically aligned, penny-shaped and the	",
" matrix is isotropic. The resulting anisotropic solid is of HTI symmetry.",
"                                                                       ",
" Output:                                                               ",
" Computes(a) stiffness elements                                        ",
"         (b) density normalized stiffness components                   ",
"         (c) generic Thomsen parameters (vp0,vs0,eps,delta,gamma)      ",
"         (d) equivalent VTI parameters (alpha,beta,ev,dv,gv)           ",
"                                                                       ",
"                                                                       ", 
NULL}; 

/*
 * 
 * AUTHOR:: Andreas Rueger, Colorado School of Mines, 10/10/96
 *  
 * Additional notes: 
 *  The routine can be easily modified to allow for any 
 *  filling adding attenuation is not trivial
 *
 * Technical Reference:
 *  Hudson's theory: Hudson, 1981: Wave speed and attenuation of elastic
 *                                 waves in material containing cracks.
 *                                 Geophys. J. R. astr. Soc 64, 133-150
 *                  Crampin, 1984: Effective anisotropic elastic constants
 *                                 for waves propagating through cracked
 *                                 solids: 
 *				  Geophys. J. R. astr. Soc 76, 135-145
 *  Equivalent VTI : Rueger, 1996: Reflection coefficients in transversely
 *                                 isotropic media with vertical and 
 *                                 horizontal axis of symmetry: Geophysics
 */
/**************** end self doc ***********************************/

/* internally defined function */

int hudsonstiff(int fill,double vp,double vs,double rho,double cdens,
		double aspect,Stiff2D *spar1);
void matmatmul(double **T, double **D, double **C1 );
void matmatmov(double **T, double **D);
void matmatadd(double **A, double **B, double **R);
void writemat(double **A);


/* the main program */
int main (int argc, char **argv)
{
	double vp,vs,rho;
	double aspect,cdens;
	double scale,eps,delta,gamma;
	
	int fill;
	
        char *outpar=NULL;      /* name of file holding output parfile  */
        FILE *outparfp=NULL;   /* ... its file pointer                 */

	Stiff2D *spar1, *spar2;
	

	spar1=(Stiff2D*)emalloc(sizeof(Stiff2D));
	spar2=(Stiff2D*)emalloc(sizeof(Stiff2D));

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	if (!getpardouble("vp",&vp)) vp = 4.5;
	if (!getpardouble("vs",&vs)) vs = 2.53;
	if (!getpardouble("rho",&rho)) rho = 2.8;
	if (!getpardouble("aspect",&aspect)) aspect = 0.000001;
	if (!getpardouble("cdens",&cdens)) cdens = 0.0;
	if (!getparint("fill",&fill)) fill = 0;

        /***************  open par-file  ******************/
        if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
        outparfp = efopen(outpar, "w");

        checkpars();


        /*************** check input **********************/
        if (fill !=0 && fill !=1 )
	  err(" \n wrong FILL parameter !! \n");
	
	if(cdens !=0 && aspect==0)
	  err(" \n wrong value for <aspect> ");

        if ( hudsonstiff(fill,vp,vs,rho,cdens,aspect,spar1) !=1 )
          err("  ERROR in <hudsonstiff> \n ");
	
	fprintf(outparfp," \n ----------Hudson's crack model ----------\n \n");
	fprintf(outparfp," vp=%g \t vs=%g \t rho=%g \n",vp,vs,rho);
	fprintf(outparfp," cdens=%g \t aspect=%g \t fill=%i \n\n",
		cdens,aspect,fill);
	
	
        fprintf(outparfp," \n ----------Hudson output ----------\n \n");
	fprintf(outparfp," c11=%g \t c33=%g \n",spar1->a1111,spar1->a3333);
	fprintf(outparfp," c44=%g \t c55=%g \n",spar1->a2323,spar1->a1313);
	fprintf(outparfp," c13=%g \n\n",spar1->a1133);

	/* convert stiffness into density normalized stiffnesses */
	scale=1./rho;
	spar1->a1111=scale*spar1->a1111;
	spar1->a3333=scale*spar1->a3333;
	spar1->a2323=scale*spar1->a2323;
	spar1->a1313=scale*spar1->a1313;
        spar1->a1133=scale*spar1->a1133;

	fprintf(outparfp," a11=%g \t a33=%g \n",
		spar1->a1111,spar1->a3333);
	fprintf(outparfp," a44=%g \t a55=%g \n",
		spar1->a2323,spar1->a1313);
	fprintf(outparfp," a13=%g \n\n",spar1->a1133);
	

        /* convert stiffnesses into generic Thomsen */
        if (stiff2thomVTI (spar1->a3333, spar1->a1111, 
			 spar1->a1133, spar1->a1313, 
			 spar1-> a2323,&vp,&vs,&eps, &delta,&gamma) != 1)
	         err(" ERROR in <stiff2thomVTI> ");

	fprintf(outparfp," vp0=%g \t vs0=%g \t rho=%g \n",vp,vs,rho);
	fprintf(outparfp," eps=%g \t delta=%g \t gamma=%g \n\n",eps,delta,gamma);

	
	/* compute thomsen of equivalent VTI medium */
	spar1->a1212=spar1->a1313;
	
	if(stiff2tv(spar1,&vp,&vs,&eps,&delta,&gamma) != 1)
	   err("\n ERROR in <stiff2tv> \n\n");

	fprintf(outparfp," alpha=%g \t beta=%g \t rho=%g \n",vp,vs,rho);
	fprintf(outparfp," e(V)=%g \t d(V)=%g \t g(V)=%g \n\n",eps,delta,gamma);
	
	fclose(outparfp);

	return 1;

 
}

int hudsonstiff(int fill,double vp,double vs,double rho,double cdens,
		double aspect,Stiff2D *spar1)
{
  /* Note: routine is not written for speed */
           
        double mu,muf=0.,lambda,lambdaf=0.;
	double kf,K,M,U11,U33,scal;
	double q,x;
        int i,j;
	
        double **C0,**C1,**C2,**D,**T;
	
        C0=alloc2double(6,6);
	C1=alloc2double(6,6);
        C2=alloc2double(6,6);
        T=alloc2double(6,6);
	D=alloc2double(6,6);
	
	/* initialize matrices */
	for(i=0;i<6;i++)
	  for(j=0;j<6;j++){
	     C0[j][i]=0.0;
	     C1[j][i]=0.0;
	     C2[j][i]=0.0;
	  }
	

        /* gas or water */
        if(fill==0) {
	  lambdaf=0.0;
	  muf=0.0;
	}
	else if (fill==1){
	  lambdaf=2.25;
	  muf=0.0;
	}
	

	/* compute Lame constants of matrix */
        mu=vs*vs*rho;
        lambda=vp*vp* rho - 2.* mu;

	if(mu <= FLT_EPSILON || lambda <= FLT_EPSILON)
	  err(" wrong matrix parameters \n");
	
	/* compute diagonal matix D */
	kf=lambdaf+2./3.*muf;
        K=( (kf + 4./3.*muf)/(PI*aspect*mu) )* ( 
	     (lambda + 2.*mu)/(lambda + mu) );

        M=( 4.* muf / (PI*aspect*mu) )* ( (lambda + 2.*mu) /
	     (3.*lambda + 4.* mu) );
        U11 = (4./3.)*( lambda + 2.*mu )/ (lambda + mu) / (1.+ K );
        U33 = (16./3.)*(lambda + 2.*mu) / (3.*lambda + 4.*mu) /(1.+ M);

        /* zero order stiffness */
        spar1->a1111 = rho*vp*vp;
	
	spar1->a2323 = rho*vs*vs;
        
	spar1->a1133 = spar1->a1111 - 2.*spar1->a2323;
	
	C0[0][0]=C0[1][1]=C0[2][2]=spar1->a1111;
	C0[3][3]=C0[4][4]=C0[5][5]=spar1->a2323;
	C0[0][1]=C0[0][2]=C0[1][2]=spar1->a1133;
	C0[1][0]=C0[2][0]=C0[2][1]=spar1->a1133;
	/* writemat(C0); */
	
        /* first order stiffness */
        scal= - cdens/mu;
	spar1->a1111 = scal*(lambda + 2.*mu)*(lambda + 2.*mu) ;
        spar1->a3333 = scal* lambda *lambda;
        spar1->a1313 = scal*mu*mu;
        spar1->a1133 = scal*lambda*(lambda + 2.*mu);
	
        T[0][0]=spar1->a1111;
	T[0][1]=T[1][0]=T[0][2]=T[2][0]=spar1->a1133;
	T[1][1]=T[2][2]=T[1][2]=T[2][1]=spar1->a3333;
	T[4][4]=T[5][5]=spar1->a1313;
	
        D[0][0]=D[1][1]=D[2][2]=U11;
	D[4][4]=D[5][5]=U33;
	
        matmatmul(T,D,C1);
	/* writemat(C1); */
	
        matmatmul(D,D,T);

        matmatmov(T,D);
	
	/* second order stiffness */
	scal=cdens*cdens/15.;
	q=15.*(lambda/mu)*(lambda/mu) + 28.*lambda/mu+28.;
	x=2.*mu*(3.*lambda+8.*mu)/(lambda + 2.*mu);

	
	spar1->a1111 = scal*(lambda + 2.*mu)*q;
	spar1->a3333 = scal*lambda*lambda*q/(lambda + 2.*mu);
	spar1->a1313 = scal*x;
	spar1->a1133 = scal*lambda*q;
	T[0][0]=spar1->a1111;
	T[0][1]=T[1][0]=T[0][2]=T[2][0]=spar1->a1133;
	T[1][1]=T[2][2]=T[1][2]=T[2][1]=spar1->a3333;
	T[4][4]=T[5][5]=spar1->a1313;

	matmatmul(T,D,C2);
        /* writemat(C2); */

	matmatadd(C0,C1,T);
	
        matmatadd(T,C2,C0);
	/* writemat(C0); */
        
	spar1->a1111 = C0[0][0];
	spar1->a3333 = C0[2][2];
	spar1->a2323 = C0[3][3];
	spar1->a1313 = C0[5][5];
	spar1->a1133 = C0[0][2];
	 
        /* check for HTI symmetry */
	if( ABS(C0[2][2]-2.*C0[3][3]-C0[1][2])>FLT_EPSILON)
	  return -1;
	else
          return 1;
  
}

void matmatmul(double **A, double **B, double **R )
{
  /* simple R=A*B  operation with double */

  int i,j,k;
  
  for(i=0;i<6;i++)
    for(k=0;k<6;k++){
      R[i][k]=0.0;
  
      for(j=0;j<6;j++)
        R[i][k]=R[i][k]+A[i][j]*B[j][k];
    }
  
}
void matmatmov(double **D, double **T)
{
  int i,k;
  
  /* move matrix D to T */
  for(i=0;i<6;i++)
    for(k=0;k<6;k++)
      T[i][k]=D[i][k];
}

void matmatadd(double **A, double **B, double **R)
{
  /* R=A+B */
  int i,k;
  
  for(i=0;i<6;i++)
    for(k=0;k<6;k++)
      R[i][k]=A[i][k]+B[i][k];
  
}

void writemat(double **A)
{
  /* R=A+B */
  int i,k;
  
  for(i=0;i<6;i++)
    for(k=0;k<6;k++)
      warn("A(%i,%i)=%g",1+i,1+k,A[i][k]);
  
  
}
