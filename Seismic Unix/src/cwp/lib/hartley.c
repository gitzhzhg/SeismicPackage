/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/

/***************************************************************************
Hartley - routines for fast Hartley transform

srfht -  in-place FHT using the split-radix algorithm
dsrfht - in-place FHT using the split-radix algorithm (double precision)
r4fht -	in-place FHT using the radix-4 algorithm
nextpow2 - find m such that n=2^m via lookup table
nextpow4 - find m such that n=4^m via lookup table
srfht - split-radix in-place FHT

****************************************************************************
Function Prototypes:

void srfht(int *n, int *m, float *f);
void dsrfht(int *n, int *m, double *f);
void r4fht(int n, int m, float *f);
int nextpow2(int n);
int nextpow4(int n);



****************************************************************************
Notes:
srfht -  in-place FHT using the split-radix algorithm
		(single precision)
		does not include division by n
		n and m are not modified
		usage:  srfht(int *n, int *m, float *f)
			n  length of input sequence n=2^m
			m  exponent such that n=2^m
	  		f  input sequence to FHT  
		
dsrfht - in-place FHT using the split-radix algorithm
		(double precision)
		does not include division by n
		n and m are not modified
		usage:  srfht(int *n, int *m, double *f)
			n  length of input sequence n=2^m
			m  exponent such that n=2^m
	  		f  input sequence to FHT  

r4fht -	in-place FHT using the radix-4 algorithm
		does not include division by n
		usage:  r4fht(int n, int m, float *f)
			n  length of input sequence n=4^m
			m  exponent such that n=4^m
	  		f  input sequence to FHT  

nextpow2 - find m such that n=2^m via lookup table
			max(n)=2^24
		
nextpow4 - find m such that n=4^m via lookup table
			max(n)=4^12

srfht - split-radix in-place FHT
	  n	length of input sequence n=2^m
	  m	exponent such that n=2^m
 	  f	input sequence to FHT  
   Reference:
       Sorensen, H. V., Jones, D. L., Burrus, C. S, & Heideman, M. T.,
       1985, On computing the Hartley transform: IEEE Trans. Acoust.,
       Speech, and Signal Proc., v. ASSP-33, no. 4, p. 1231-1238.
			
*****************************************************************************
	possible improvements:
		find optimum length for FHT (nfhto)
		use bit shift operators in r4fht
***************************************************************************
  Credits:	CENPET: Werner M. Heigl, April 2006
**************************************************************************/

/**************** end self doc ********************************/

#include "cwp.h"

void srfht(int *n, int *m, float *f)
{
	int i,j,k;		/* loop variables */
	int is,id;		/* start and step in i-loop */
	int imax,kmax;		/* loop limits */
	int l11,l12,l13,l14;	/* index variables */
	int l21,l22,l23,l24;
	int n1,n2,n4,n8;	/* fractions of signal length n */
	float a,a3,e;		/* trigonometric arguments */
	float cc1,ss1,cc3,ss3;	/* cosines and sines */
	float t1,t2,t3,t4,t5;	/* temps */
	float tpi;		/* 2*PI */
	float r2;		/* sqrt(2) */
	
	/* since this code is a conversion from Fortran 
	   we need to adjust indexing */
	--f;
	
	/********************* first p-1 stages of transform ******************/
	n2   = *n << 1;
	tpi  = 2*PI;
	r2   = sqrtf(2);
	kmax = *m-1;
	imax = *n;
	
	for (k=1; k <= kmax; ++k) {
		is = 1;
		id = n2;
		n2 /= 2;
		n4 = n2 / 4;
		n8 = n2 / 8;
		e  = tpi / n2;
		do {
			for (i=is; (id<0) ? i>=imax : i<=imax; i+=id) {
				l12 = i   + n4;
				l13 = l12 + n4;
				l14 = l13 + n4;
				t1  = f[l12] - f[l14];
				t2  = f[i]   + f[l13];
				t3  = f[l12] + f[l14];
				t4  = f[i]  - f[l13];
				f[l14] = t4 - t1;
				f[l13] = t4 + t1;
				f[l12] = t3;
				f[i]   = t2;
				if (n4 != 1) {
					l21 = i   + n8;
					l22 = l21 + n4;
					l23 = l22 + n4;
					l24 = l23 + n4;
					t1  = f[l22];
					t2  = f[l23];
					t3  = f[l24];
					f[l24] = (t1     - t3)*r2;
					f[l23] = (f[l21] - t2)*r2;
					f[l22] = t1 + t3;
					f[l21] += t2;
					a = e;
					
					for (j=2; j<=n8; ++j) {
						l11 = i + j - 1;
						l12 = l11 + n4;
						l13 = l12 + n4;
						l14 = l13 + n4;
						l21 = i   + n4 - j + 1;
						l22 = l21 + n4;
						l23 = l22 + n4;
						l24 = l23 + n4;
						a3  = a * 3;
						cc1 = cosf(a);
						ss1 = sinf(a);
						cc3 = cosf(a3);
						ss3 = sinf(a3);
						a   = j * e;
						t5  = f[l21] - f[l23];
						t2  = f[l11] - f[l13];
						t1  = t2 + t5;
						t2  -= t5;
						t5  = f[l22] - f[l24];
						t4  = f[l14] - f[l12];
						t3  = t4 + t5;
						t4  -= t5;
						f[l11] += f[l13];
						f[l12] += f[l14];
						f[l21] += f[l23];
						f[l22] += f[l24];
						f[l13] = t1 * cc1 + t3 * ss1;
						f[l14] = t2 * cc3 - t4 * ss3;
						f[l23] = t1 * ss1 - t3 * cc1;
						f[l24] = t2 * ss3 + t4 * cc3;
					} /* end j-loop */
					
				} /* end if */
				
			} /* end i-loop */
						
			is = (id << 1) - n2 +1;
			id <<= 2;
		} while (is < *n) ;
			
	} /* end k-loop */

	/*********************** last stage of transform **********************/
	is = 1;
	id = 4;
	imax = *n;
	do {
		for (i=is; (id<0)? i>=imax : i<=imax; i+=id) {
			j = i + 1;
			t1 = f[i];
			f[i]  = t1 + f[j];
			f[j] = t1 - f[j];
		}
		is = (id << 1) - 1;
		id <<= 2;;
	} while (is < *n);
	
	/*********************** digit reverse counter ***********************/
	j = 1;
	n1 = *n - 1;
	for (i=1; i<=n1; ++i) {
		if (i < j) {
			t1    = f[j];
			f[j]  = f[i];
			f[i]  = t1;
		}
		k = *n / 2;
		while (k < j) {
			j -= k;
			k /= 2;
		}
		j += k;
	}
}

void dsrfht(int *n, int *m, double *f)
{
	int i,j,k;		/* loop variables */
	int is,id;		/* start and step in i-loop */
	int imax,kmax;		/* loop limits */
	int l11,l12,l13,l14;	/* index variables */
	int l21,l22,l23,l24;
	int n1,n2,n4,n8;	/* fractions of signal length n */
	double a,a3,e;		/* trigonometric arguments */
	double cc1,ss1,cc3,ss3;	/* cosines and sines */
	double t1,t2,t3,t4,t5;	/* temps */
	double tpi;		/* 2*PI */
	double r2;		/* sqrt(2) */
	
	/* since this code is a conversion from Fortran 
	   we need to adjust indexing */
	--f;
	
	/********************* first p-1 stages of transform ******************/
	n2   = *n << 1;
	tpi  = 2*PI;
	r2   = sqrt(2);
	kmax = *m-1;
	imax = *n;
	
	for (k=1; k <= kmax; ++k) {
		is = 1;
		id = n2;
		n2 /= 2;
		n4 = n2 / 4;
		n8 = n2 / 8;
		e  = tpi / n2;
		do {
			for (i=is; (id<0) ? i>=imax : i<=imax; i+=id) {
				l12 = i   + n4;
				l13 = l12 + n4;
				l14 = l13 + n4;
				t1  = f[l12] - f[l14];
				t2  = f[i]   + f[l13];
				t3  = f[l12] + f[l14];
				t4  = f[i]  - f[l13];
				f[l14] = t4 - t1;
				f[l13] = t4 + t1;
				f[l12] = t3;
				f[i]   = t2;
				if (n4 != 1) {
					l21 = i   + n8;
					l22 = l21 + n4;
					l23 = l22 + n4;
					l24 = l23 + n4;
					t1  = f[l22];
					t2  = f[l23];
					t3  = f[l24];
					f[l24] = (t1     - t3)*r2;
					f[l23] = (f[l21] - t2)*r2;
					f[l22] = t1 + t3;
					f[l21] += t2;
					a = e;
					
					for (j=2; j<=n8; ++j) {
						l11 = i + j - 1;
						l12 = l11 + n4;
						l13 = l12 + n4;
						l14 = l13 + n4;
						l21 = i   + n4 - j + 1;
						l22 = l21 + n4;
						l23 = l22 + n4;
						l24 = l23 + n4;
						a3  = a * 3;
						cc1 = cos(a);
						ss1 = sin(a);
						cc3 = cos(a3);
						ss3 = sin(a3);
						a   = j * e;
						t5  = f[l21] - f[l23];
						t2  = f[l11] - f[l13];
						t1  = t2 + t5;
						t2  -= t5;
						t5  = f[l22] - f[l24];
						t4  = f[l14] - f[l12];
						t3  = t4 + t5;
						t4  -= t5;
						f[l11] += f[l13];
						f[l12] += f[l14];
						f[l21] += f[l23];
						f[l22] += f[l24];
						f[l13] = t1 * cc1 + t3 * ss1;
						f[l14] = t2 * cc3 - t4 * ss3;
						f[l23] = t1 * ss1 - t3 * cc1;
						f[l24] = t2 * ss3 + t4 * cc3;
					} /* end j-loop */
					
				} /* end if */
				
			} /* end i-loop */
						
			is = (id << 1) - n2 +1;
			id <<= 2;
		} while (is < *n) ;
			
	} /* end k-loop */

	/*********************** last stage of transform **********************/
	is = 1;
	id = 4;
	imax = *n;
	do {
		for (i=is; (id<0)? i>=imax : i<=imax; i+=id) {
			j = i + 1;
			t1 = f[i];
			f[i]  = t1 + f[j];
			f[j] = t1 - f[j];
		}
		is = (id << 1) - 1;
		id <<= 2;;
	} while (is < *n);
	
	/*********************** digit reverse counter ***********************/
	j = 1;
	n1 = *n - 1;
	for (i=1; i<=n1; ++i) {
		if (i < j) {
			t1    = f[j];
			f[j]  = f[i];
			f[i]  = t1;
		}
		k = *n / 2;
		while (k < j) {
			j -= k;
			k /= 2;
		}
		j += k;
	}
}

/* r4fht: radix-4 in-place FHT
	  n	length of input sequence n=4^m
	  m	exponent such that n=4^m
	  f	input sequence to FHT
	  Reference:
	  	R. N. Bracewell, "Computing with the Hartley
		transform", Computers in Physics, v. 9(4), 1995.

*/
void r4fht(int n, int m, float *f)
{
	int i,j,l,k;	/* loop variables */
	int n4;		/* n4=n/4 */
	float tmp;	/* temp storage */	
	int e1,e2,e3,e4,e5,e6,e7,e8;	/* even indeces */
	int l1,l2,l3,l4,l5,l6,l7,l8;	/* odd indeces */
	float a1,a2,a3;			/* argument for cos and sin */
	float c1,c2,c3;			/* cosines */
	float s1,s2,s3;			/* sines */
	float t1,t2,t3,t4,t5,t6,t7,t8,t9,t0;	/* temps */	
	float r2;	/* sqrt(2) */
	
	/* set up parameters */
	n4 = n/4;
	r2 = sqrt(2);
		
	/* permute to radix 4 */	
	j=1; i=0;	
	while (i<n-1) {
		i = i+1;
		if (i<j) {
			tmp    = f[j-1];
			f[j-1] = f[i-1];
			f[i-1] = tmp;
		}
		k = n4;
		while (3*k<j) {
			j = j-3*k;
			k = k/4;
		}
		j = j+k;
	}
	
	/* do discrete Hartley transform */
	/* stage 1 */
	for (i=0;i<n;i+=4) {
		t1 = f[i] + f[i+1];
		t2 = f[i] - f[i+1];
		t3 = f[i+2] + f[i+3];
		t4 = f[i+2] - f[i+3];
		f[i]   = t1 + t3;
		f[i+1] = t1 - t3;
		f[i+2] = t2 + t4;
		f[i+3] = t2 - t4;
	}
			
	/* stages 2 to m */
	for (l=2;l<=m;l++) {
		e1 = (int) powf(2,l+l-3);
		e2 = e1 + e1;
		e3 = e2 + e1;
		e4 = e3 + e1;
		e5 = e4 + e1;
		e6 = e5 + e1;
		e7 = e6 + e1;
		e8 = e7 + e1;
		
		for (j=0;j<n;j+=e8) {
			t1 = f[j] + f[j+e2];
			t2 = f[j] - f[j+e2];
			t3 = f[j+e4] + f[j+e6];
			t4 = f[j+e4] - f[j+e6];
			f[j] = t1 + t3;
			f[j+e2] = t1 - t3;
			f[j+e4] = t2 + t4;
			f[j+e6] = t2 - t4;
			t1 = f[j+e1];
			t2 = f[j+e3]*r2;
			t3 = f[j+e5];
			t4 = f[j+e7]*r2;
			f[j+e1] = t1 + t2 + t3;
			f[j+e3] = t1 - t3 + t4;
			f[j+e5] = t1 - t2 + t3;
			f[j+e7] = t1 - t3 - t4;

			for (k=1;k<e1;k++) {
				l1 = j + k;
				l2 = l1 + e2;
				l3 = l1 + e4;
				l4 = l1 + e6;
				l5 = j + e2 - k;
				l6 = l5 + e2;
				l7 = l5 + e4;
				l8 = l5 + e6;
				a1 = PI*k/e4;
				a2 = a1 + a1;
				a3 = a1 + a2;
				c1 = cos(a1);
				s1 = sin(a1);
				c2 = cos(a2);
				s2 = sin(a2);
				c3 = cos(a3);
				s3 = sin(a3);
				t5 = f[l2]*c1 + f[l6]*s1;
				t6 = f[l3]*c2 + f[l7]*s2;
				t7 = f[l4]*c3 + f[l8]*s3;
				t8 = f[l6]*c1 - f[l2]*s1;
				t9 = f[l7]*c2 - f[l3]*s2;
				t0 = f[l8]*c3 - f[l4]*s3;
				t1 = f[l5] - t9;
				t2 = f[l5] + t9;
				t3 = -t8 - t0;
				t4 = t5 - t7;
				f[l5] = t1 + t4;
				f[l6] = t2 + t3;
				f[l7] = t1 - t4;
				f[l8] = t2 - t3;
				t1 = f[l1] + t6;
				t2 = f[l1] - t6;
				t3 = t8 - t0;
				t4 = t5 + t7;
				f[l1] = t1 + t4;
				f[l2] = t2 + t3;
				f[l3] = t1 - t4;
				f[l4] = t2 - t3;
			}
		}
	}
}

/* nextpow2: find smallest m such that n<=2^m */
int nextpow2(int n)
{
	int pow2[]={      1,      2,     4,     8,    16,    32,     64,
		        128,    256,   512,  1024,  2048,  4096,   8192,
		      16384,  32768, 65536,131072,262144,524288,1048576,
		    2097152,4194304,8388608,16777216};
	int i;
	
	i = 0;
	
	while (pow2[i]<n) 	++i;
	
	return i;
}

/* nextpow4: find smallest m such that n<=4^m */
int nextpow4(int n)
{
	int pow4[]={1,4,16,64,256,1024,4096,16384,65536,
			262144,1048576,4194304,16777216};
	int i=0;
	
	while (pow4[i]<n) 	++i;
	
	return i;
}
