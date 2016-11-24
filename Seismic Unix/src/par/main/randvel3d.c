/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* RANDVEL3D: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $		*/

#include <par.h>
#include <time.h>

/*********************** self documentation **********************/
char *sdoc[] ={ 
"									",
" RANDVEL3D - Add a random velocity layer (RVL) to a gridded             ",
"            v(x,y,z) velocity model                                    ",
"									",
"	randvel3d <infile n1= n2= >outfile [parameters]			",
" 									",
" Required Parameters:							",
" n1=		number of samples along 1st dimension			",
" n2=		number of samples along 2nd dimension			",
"									",
" Optional Parameters:							",
"									",
" n3=1          number of samples along 3rd dimension			",
"									",
" mode=1             add single layer populated with random vels	",
"                    =2 add nrvl layers of random thickness and vel     ",
" seed=from_clock    random number seed (integer)			",
"									",
"         ********** mode=1 params **********				",
" ---->New layer geometry info						",
" i1beg=1       1st dimension beginning sample 				",
" i1end=n1/5    1st dimension ending sample 				",
" i2beg=1       2nd dimension beginning sample 				",
" i2end=n2      2nd dimension ending sample 				",
" i3beg=1       3rd dimension beginning sample 				",
" i3end=n3      3rd dimension ending sample 				",
" ---->New layer velocity info						",
" vlsd=v/3     range (std dev) of random velocity in layer, 		",
"               where v=v(0,0,i1) and i1=(i1beg+i1end)/2 	 	",
" add=1         add random vel to original vel (v_orig) at that point 	",
"               =0 replace vel at that point with (v_orig+v_rand) 	",
" how=0         random vels can be higher or lower than v_orig		",
"               =1 random vels are always lower than v_orig		",
"               =2 random vels are always higher than v_orig		",
" cvel=2000     layer filled with constant velocity cvel 		",
"               (overides vlsd,add,how params)			",
" ---->Smoothing parameters (0 = no smoothing)				",
" r1=0.0	1st dimension operator length in samples		",
" r2=0.0	2nd dimension operator length in samples		",
" r3=0.0	3rd dimension operator length in samples		",
" slowness=0	=1 smoothing on slowness; =0 smoothing on velocity	",
"									",
"         ********** mode=2 params **********				",
" nrvl=n1/10    number of const velocity layers to add     		",
" pdv=10.       percentage velocity deviation (max) from input model	",
"									",
" Notes:								",
" *** mode=1 **********							",
" 1. Smoothing radii usually fall in the range of [0,20].		",
" 2. Smoothing radii can be used to set aspect ratio of random velocity ",
"    anomalies in the new layer.  For example (r1=5,r2=0,r3=0) will     ",
"    result in vertical vel streaks that mimick vertical fracturing.    ",
" 3. Smoothing on slowness works better to preserve traveltimes relative",
"    to the unsmoothed case.						",
" 4. Default case is a random velocity (+/-30%) near surface layer whose",
"    thickness is 20% of the total 2D model thickness.			",
" *** mode=2 **********			                          	",
" 5. Each layer vel is a random perturbation on input model at that level.",
" *** both modes ******				",
" 6. The depth dimension is assumed to be along axis 1.			",
"									",
" Example:								",
" 1. 2D RVL with no smoothing						",
"   makevel nz=250 nx=200 | randvel3d n1=250 n2=200 | ximage n1=250      ",
" 2. 3D RVL with no smoothing						",
"   makevel nz=250 nx=200 ny=220 |					",
"   randvel3d n1=250 n2=200 n3=220 | 					",
"   xmovie n1=250 n2=200					    	",
"									",
NULL};

/*
 * Author:  U Houston: Chris Liner c. 2008
 *          Based on smooth3d (CWP: Zhenyue Liu  March 1995)
 *
 */
/**************** end self doc ********************************/

/* Protypes for functions used internally */
void vsm3d(float ***vel,int n3,int n2,int n1,int iter,int depth,
  	float r3,float r2,float r1,float mu,int sl,float vminc,float vmaxc);

int
main(int argc,char **argv)
{
 
	int  n1, n2, n3, depth, iter,  slowness;
	int nn1, nn2, nn3, nrvl,mode;
	float ***v2, pdv;
	float r1, r2, r3, ***vel, mu, vminc, vmaxc;
	int i1beg, i1end, i2beg, i2end, i3beg, i3end;
	int i1, i2, i3;
	int j1, j2, j3, ih, ilay, j;
	float vlsd, tmp=0.0, vorig;
	int add, how;
	unsigned int seed; 	/* random number seed */
	float cvel;
 
	FILE *invp=stdin, *outvp=stdout;


	/* initialization */
	initargs(argc,argv) ;
	requestdoc(0);


	/*-----------get required parameters-----------*/
	if( !getparint("n1",&n1) ) n1 = 0 ;
	if( n1 <= 0 ) err("sample number of 1st dimension invalid" ) ;
	if( !getparint("n2",&n2) ) n2 = 0 ;
	if( n2 <= 0 ) err("sample number of 2nd dimension invalid" ) ;


	/*-----------get optional parameters-----------*/

	if( !getparint("n3",&n3) ) n3 = 1 ;

	/*   get mode from user    */
	if( !getparint("mode",&mode) ) mode = 1 ;
	if( mode < 1  || mode > 2 ) err("mode must be 1 or 2" ) ;

	/* Set seed */
	if (!getparuint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			err("time() failed to set seed");
		}
	}
	sranuni(seed);
	if (mode==2) warn("Random seed for layers = %i",seed);


	/*   single RVL geometry    */
	if( !getparint("i1beg",&i1beg) ) i1beg = 0 ;
	if( !getparint("i1end",&i1end) ) i1end = n1/5 ;
	if( !getparint("i2beg",&i2beg) ) i2beg = 0 ;
	if( !getparint("i2end",&i2end) ) i2end = n2 ;
	if( !getparint("i3beg",&i3beg) ) i3beg = 0 ;
	if( !getparint("i3end",&i3end) ) i3end = n3 ;

	/*   constant velocity layer */
	if( !getparfloat("cvel",&cvel ) ) cvel = 0. ;

	/*   smoothing parameters    */
	if( !getparfloat("r1",&r1) || n1<4) r1 = 0. ;
	if( !getparfloat("r2",&r2) || n2<4) r2 = 0. ;
	if( !getparfloat("r3",&r3) || n3<4) r3 = 0. ;

	/*   many layers     */
	if( !getparint("nrvl",&nrvl) ) nrvl = n1/10 ;
	if( nrvl > n1-1 ) err("nrvl cannot be greater than n1" ) ;
	if( !getparfloat("pdv",&pdv) ) pdv = 10. ;

	/* scale smoothing parameters	*/
	r1 = 0.5*r1*r1 ;
	r2 = 0.5*r2*r2 ;
	r3 = 0.5*r3*r3 ;
 	 
	/*   get iteration number for smoothing operator */
	iter = 2;

	/*   description for vertical dimension    */
	depth = 1;
 
	/*   relative weight at bottom     */
	mu = 1.0;

	/*   smoothing on velocity or slowness     */
	if(!getparint("slowness",&slowness) ) slowness = 0;

	/*   clips of velocity before smoothing     */
	vminc = 0;
	vmaxc = 99999;

     	/*   allocate input file    */
	vel  = alloc3float(n1,n2,n3) ;

	/*   read input velocity file     */
	efseeko(invp,(off_t) 0,SEEK_SET);
	fread((char *)vel[0][0],sizeof(float),n1*n2*n3,invp);

	/*  get layer random velocity std deviation    */
	i1 = (i1beg + i1end)/2;
	vorig = vel[0][0][i1];
	if( !getparfloat("vlsd",&vlsd) ) vlsd = 0.3*vorig;

	/*  get add and how params     */
	if( !getparint("add",&add) ) add = 1 ;
	if( add < 0  || add > 1 ) err("add must be 0 or 1" ) ;
	if( !getparint("how",&how) ) how = 0 ;
	if( how < 0  || how > 2 ) err("how must be 0,1, or 2" ) ;

        checkpars();

	/* single layer with point-to-point random velocities */
	if (mode==1) {
		/* dimensions of RVL */
		nn1 = i1end - i1beg;
		nn2 = i2end - i2beg;
		nn3 = i3end - i3beg;

	     	/*   allocate RVL memory    */
		v2 = alloc3float(nn1,nn2,nn3) ;

		/* make the single RVL ... 
		   loop over axis 3 (y) */
		for (i3=0; i3<nn3; ++i3) {

			/* loop over axis 2 (x) */
			for (i2=0; i2<nn2; ++i2) {

				/* loop over axis 1 (z) */ 
				for (i1=0; i1<nn1; ++i1) {

					if (how==0) {
						tmp = vlsd*(0.5-franuni());
					} else if (how==1) {
						tmp = vlsd*franuni();
					} else if (how==2) {
						tmp = vlsd*franuni();
					}

					if (cvel>0.) {
						vel[i3][i2][i1] = cvel;
					} else { 

						vel[i3][i2][i1] += tmp;
					}

				}
			}
		}
 
		/*   perform smoothing operation    */
		vsm3d(v2,nn3,nn2,nn1,iter,depth,r3,r2,r1,mu,slowness,vminc,vmaxc);

		/* add in, or replace with, the random velocity layer... 
		   loop over axis 3 (y) */
		for (i3=i3beg; i3<i3end; ++i3) {

			j3 = i3 - i3beg;

			/* loop over axis 2 (x) */
			for (i2=i2beg; i2<i2end; ++i2) {

				j2 = i2 - i2beg;

				/* loop over axis 1 (z) */ 
				for (i1=i1beg; i1<i1end; ++i1) {

					j1 = i1 - i1beg;

					if (add==1) {
						if (how==1) {
						  vel[i3][i2][i1] -= v2[j3][j2][j1];
						} else {
						  vel[i3][i2][i1] += v2[j3][j2][j1];
						}
					} else if (add==0) {
						if (how==1) {
						  vel[i3][i2][i1] = vorig - v2[j3][j2][j1];
						} else {
						  vel[i3][i2][i1] = vorig + v2[j3][j2][j1];
						}
					}
					if (cvel>0.) {
						vel[i3][i2][i1] = cvel;
					}

				}
			}
		}	

	}

	/* many constant (random) velocity layers with random thickness */
	if (mode==2) {

		/* axis 1 (z) index */
		i1 = 0;

		/* loop over layers */
		for (ilay=0; ilay<nrvl; ++ilay) {

			/* set addative vel for this layer */
			if (n3>2) {
				i3 = n3/2;
			} else { 
				i3 = 0;
			}
			i2 = n2/2;
			tmp = (0.5-franuni())*pdv*vel[i3][i2][i1]/100;

			/* set thickness of this layer (depth samples) */
			ih = franuni()*n1/(nrvl-ilay) + 1;

			/* loop over depth samples in layer */
			for (j=0; j<ih; ++j) {

				/* loop over axis 3 (y) */
				for (i3=0; i3<n3; ++i3) {

					/* loop over axis 2 (x) */
					for (i2=0; i2<n2; ++i2) {

						vel[i3][i2][i1] += tmp;

					}

				}
				i1 += 1;
			}
		}
 
	}

	/*   write output velocity file     */
	fwrite((char *)vel[0][0],sizeof(float),n1*n2*n3,outvp);  

	/*   close input and output files    */
	fclose(invp) ;
	fclose(outvp) ; 

	return(CWP_Exit());
}
 
/* Prototype for function used internally */
void tripd2(float **d, float **e, float **b, int n, int m);
 
 
void vsm3d(float ***v,int n3,int n2,int n1,int iter,int depth,
	 float r3,float r2,float r1,float mu,int sl,float vmin,float vmax)
/***************************************************************************
Smooth 3d-velocity.  
*************************************************************************/

{
	int  i2, i1, i3, i;		
	float **d=NULL, **e=NULL, **f=NULL, *w, ww=1.0;
 
 /*	compute the weight function */
	w = alloc1float(n1+n2+n3-2);
	if(depth==1){
		mu = (mu*mu-1.0)/(n1*n1);
		for(i1=0; i1<n1; ++i1) w[i1] = 1.0/(1+i1*i1*mu);
	}
	if(depth==2){
 		mu = (mu*mu-1.0)/(n2*n2);
		for(i2=0; i2<n2; ++i2) w[i2] = 1.0/(1+i2*i2*mu);
	}
	if(depth==3){
 		mu = (mu*mu-1.0)/(n3*n3);
		for(i3=0; i3<n3; ++i3) w[i3] = 1.0/(1+i3*i3*mu);
	}

/*	scale  smoothing parameters according to the iteration number	*/
	if(iter==1) {
		r1 /= 3.39*3.39;
		r2 /= 3.39*3.39;
		r3 /= 3.39*3.39;
	} else if(iter==2){
		r1 /= 5.19*5.19;
		r2 /= 5.19*5.19;
		r3 /= 5.19*5.19;
	} else {
		r1 /= 6.60*6.60;
		r2 /= 6.60*6.60;
		r3 /= 6.60*6.60;
	}


	/*  clip velocity  */
	for(i3=0; i3<n3; ++i3) 
	    for(i2=0; i2<n2; ++i2)
		for(i1=0; i1<n1; ++i1){
			if(v[i3][i2][i1] >vmax) v[i3][i2][i1] = vmax;
			if(v[i3][i2][i1] <vmin) v[i3][i2][i1] = vmin;
		}

	if(sl) {
	/*  smoothing on slowness  */
		for(i3=0; i3<n3; ++i3) 
			for(i2=0; i2<n2; ++i2)
				for(i1=0; i1<n1; ++i1)
					v[i3][i2][i1] = 1.0/v[i3][i2][i1];
	}
	

	if(r2>0.) {
 
/*	smoothing velocity in the second direction */

	/* allocate space */
 	d = alloc2float(n1,n2);
	e = alloc2float(n1,n2);
	f = alloc2float(n1,n2);
 
 
	for(i3=0; i3<n3; ++i3){
		if(depth==3) ww = w[i3];
	 	for(i2=0; i2<n2-1; ++i2){
			if(depth==2) ww = w[i2+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i2][i1] = ww+r2*2.0;
				e[i2][i1] = -r2;
 				f[i2][i1] = ww*v[i3][i2+1][i1];
			}
		}
			for(i1=0; i1<n1; ++i1){
	  		d[n2-2][i1] -= r2;
			f[0][i1] += r2*v[i3][0][i1];
  		}
	 	tripd2(d,e,f,n2-1,n1);

	    for(i=1; i<iter; ++i) {
	 	for(i2=0; i2<n2-1; ++i2){
			if(depth==2) ww = w[i2+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i2][i1] = ww+r2*2.0;
				e[i2][i1] = -r2;
 				f[i2][i1] *= ww;
			}
		}
			for(i1=0; i1<n1; ++i1){
	  		d[n2-2][i1] -= r2;
			f[0][i1] += r2*v[i3][0][i1];
  		}
	 	tripd2(d,e,f,n2-1,n1);
	    }

	 	for(i2=0; i2<n2-1; ++i2)
			for(i1=0; i1<n1; ++i1)
				v[i3][i2+1][i1] = f[i2][i1];
	}
	}
 
	if(r3>0.) {
/*	smooth velocity in  the third  direction */

	/* allocate space */
 	d = alloc2float(n1,n3);
	e = alloc2float(n1,n3);
	f = alloc2float(n1,n3); 
 
	for(i2=0; i2<n2; ++i2){
		if(depth==2) ww = w[i2];
	 	for(i3=0; i3<n3-1; ++i3){
			if(depth==3) ww = w[i3+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i3][i1] = ww+2.*r3;
				e[i3][i1] = -r3;
 				f[i3][i1] = ww*v[i3+1][i2][i1];
			}
 		}
			for(i1=0; i1<n1; ++i1){
	  		d[n3-2][i1] -= r3;
			f[0][i1] += r3*v[0][i2][i1];
  		}
	 	tripd2(d,e,f,n3-1,n1);

	    for(i=1; i<iter; ++i){
	 	for(i3=0; i3<n3-1; ++i3){
			if(depth==3) ww = w[i3+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i3][i1] = ww+2.*r3;
				e[i3][i1] = -r3;
 				f[i3][i1] *= ww;
			}
 		}
			for(i1=0; i1<n1; ++i1){
	  		d[n3-2][i1] -= r3;
			f[0][i1] += r3*v[0][i2][i1];
  		}
	 	tripd2(d,e,f,n3-1,n1);
	    }

	 	for(i3=0; i3<n3-1; ++i3)
			for(i1=0; i1<n1; ++i1)
				v[i3+1][i2][i1] = f[i3][i1];
	}
	}
	
	if(r1>0.) {
/*	smooth velocity in  the first direction */

	/* allocate space */
 	d = alloc2float(1,n1);
	e = alloc2float(1,n1);
	f = alloc2float(1,n1);
 
	for(i3=0; i3<n3; ++i3){
		if(depth==3) ww = w[i3];
	 	for(i2=0; i2<n2; ++i2){
			if(depth==2) ww = w[i2];
			for(i1=0; i1<n1-1; ++i1){
				if(depth==1) ww = w[i1+1];
				d[i1][0] = ww+r1*2.0;
				e[i1][0] = -r1;
 				f[i1][0] = ww*v[i3][i2][i1+1];
			}
	  		d[n1-2][0] -= r1;
			f[0][0] += r1*v[i3][i2][0];
	   		tripd2(d,e,f,n1-1,1);

		    for(i=1; i<iter; ++i) {
			for(i1=0; i1<n1-1; ++i1){
				if(depth==1) ww = w[i1+1];
				d[i1][0] = ww+r1*2.0;
				e[i1][0] = -r1;
 				f[i1][0] *= ww;
			}
	  		d[n1-2][0] -= r1;
			f[0][0] += r1*v[i3][i2][0];
	 		tripd2(d,e,f,n1-1,1);
		    }

 			for(i1=0; i1<n1-1; ++i1)
				v[i3][i2][i1+1] = f[i1][0];
		}
	}
	}

	if(sl) {
		for(i3=0; i3<n3; ++i3) 
			for(i2=0; i2<n2; ++i2)
				for(i1=0; i1<n1; ++i1)
					v[i3][i2][i1] = 1.0/v[i3][i2][i1];
	}

	free1float(w);
	if(r1>0. || r2>0. || r3>0.) {
		free2float(d);
		free2float(e);
		free2float(f);
 	}

}
	
void tripd2(float **d, float **e, float **b, int n, int m)
/*****************************************************************************
Given m n-by-n symmetric, tridiagonal, positive definite matri2 A's and m
n-vector b's, the following algorithm overwrites b with the solution to Ax = b.
The first dimension of arrays is independent of the algorithm. 

  d() the diagonal of A 
  e() the superdiagonal of A
*****************************************************************************/
{
	int k, i; 
	float temp;
	
	/* decomposition */
	for(k=1; k<n; ++k) 
		for(i=0; i<m; ++i){ 
	  		 temp = e[k-1][i];
	   		e[k-1][i] = temp/d[k-1][i];
	   d[k][i] -= temp*e[k-1][i];
	}

	/* substitution	*/
	for(k=1; k<n; ++k) 
		 for(i=0; i<m; ++i) 
 			b[k][i] -= e[k-1][i]*b[k-1][i];
	
	for(i=0; i<m; ++i) 
		b[n-1][i] /=d[n-1][i];

	for(k=n-1; k>0; --k) 
		 for(i=0; i<m; ++i) 
			 b[k-1][i] = b[k-1][i]/d[k-1][i]-e[k-1][i]*b[k][i]; 
	
}
