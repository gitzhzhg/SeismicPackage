/* SUGOUPILLAUD: $Revision: 1.7 $ ; $Date: 2011/11/22 18:09:14 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"  									",
" SUGOUPILLAUD - calculate 1D impulse response of	 		",
"     non-absorbing Goupillaud medium					",
"									",
" sugoupillaud < stdin > stdout [optional parameters]			",
"									",
" Required parameters:							",
"	none								",
"									",
" Optional parameters:							",
"l=1source layer number; 1 <= l <= tr.ns				",
"			Source is located at the top of layer l.	",
"	k=1		receiver layer number; 1 <= k			",
"Receiver is located at the top of layer k.				",
"tmax  number of output time-samples; default:				",
"tmax=NINT((2*tr.ns-(l-1)-(k-1))/2)  if k < tr.ns			",
"			tmax=k				if k >=tr.ns	",
"pV=1  flag for vector field seismogram					",
"	(displacement, velocity, acceleration);				",
"=-1 for pressure seismogram.						",
"verbose=0  silent operation, =1 list warnings				",
"									",
" Input: Reflection coefficient series:					",
"									",
"	 impedance[i]-impedance[i+1]					",
" r[i] = ----------------------------- 					",
"	 impedance[i]+impedance[i+1]					",
"									",
"	r[0]= surface refl. coef. (as seen from above)			",
"r[n]= refl. coef. of the deepest interface				",
"									",
" Input file is to be in SU format, i.e., binary floats with a SU header.",
"									",
" Remarks:								",
" 1. For vector fields, a buried source produces a spike of amplitude 1	",
" propagating downwards and a spike of amplitude -1 propagating upwards.",
" A buried pressure source produces spikes of amplitude 1 both in the up",
" and downward directions.						",
"									",
" A surface source induces only a downgoing spike of amplitude 1 at the	",
" top of the first layer (both for vector and pressure fields).		",
" 2. The sampling interval dt in the header of the input reflectivity file",
" is interpreted as a two-way traveltime thicknes of the layers. The sampling",
" interval of the output seismogram is the same as that of the input file.",
NULL};

/* 
 * Credits:
 *	CWP: Albena Mateeva, May 2000, a summer project at Western Geophysical
 *
 *
 * ANOTATION used in the code comments [arises from the use of z-transforms]:
 *		Z-sampled: sampling interval equal to the TWO-way 
 *			traveltime of the layers; 
 *		z-sampled: sampling interval equal to the ONE-way
 *			traveltime of the layers;
 *
 * REFERENCES:
 *
 *	1. Ganley, D. C., 1981, A method for calculating synthetic seismograms 
 *	which include the effects of absorption and dispersion. 
 *	Geophysics, Vol.46, No. 8, p. 1100-1107.
 * 
 *	The burial of the source is based on the Appendix of that article.
 *
 *	2. Robinson, E. A., Multichannel Time Series Analysis with Digital 
 *	Computer Programs: 1983 Goose Pond Press, 2nd edition.
 *
 *	The recursive polynomials Q, P used in this code are described
 *	in Chapter 3 of the book: Wave Propagation in Layered Media.
 *
 *	My polynomial multiplication and division functions "prod" and
 *	"pratio" are based on Robinson's Fortran subroutines in Chapter 1.
 *
 *	4. Clearbout, J. F., Fundamentals of Geophysical Data Processing with
 *	Applications to Petroleum Prospecting: 1985 Blackwell Scientific 
 *	Publications.
 *
 *	Chapter 8, Section 3: Introduces recursive polynomials F, G in a 
 *	more intuitive way than Robinson.
 *	
 *	The connection between the Robinson's P_k, Q_k and Clearbout's 
 *	F_k, G_k is:
 *				P_k(Z) = F_k(Z)
 *				Q_k(Z) = - Z^(k) G_k(1/Z)
 *
 */
/**************** end self doc ***********************************/

/* Prototypes of functions used internally */
int  porder(double p[], int l);
void pcomb(double alfa, double beta, double p1[], double p2[], 
		int l1, int l2, double c[], int l);
void prod(double p1[], double p2[], int l1, int l2, double pp[], int ll);
void pratio(double p1[], double p2[], int l1, int l2, int L, 
	 double q[], int lq);
void pshift(double p[], int l, int k, double psh[], int ll);
void recurs(int k, double q[], double p[], int n, double r[]);
void rev(double p[], int l, double pr[] );
void upsample(double p4[], int l, double p2[], int ll);


segy tr;

int
main(int argc, char **argv)
{
  int n; /* number of subsurface interfaces */
  int l, k; /* source and receiver layers*/
  int tmax; /* number of output samples */
  int pV; /* field-type flag*/
  double *r; /* reflection coefficient series */
  double *x; /* output seismogram*/

  int i; /* loop counter*/
  int verbose; /* verbose flag*/
  int kk, delay; /* help accomodate receiver in the lower 
	 homogeneous half-space (k>n+1)*/
  double *u1, *d1; /* up- and down-going waves in 
	 the 1st layer for surface source	 */
  double *u1b; /* up-going waves in the first layer
	 for buried source*/
  double *uk, *dk; /* up- and down-going waves in layer k;
 output seismogram:  x=uk+dk */ 
  double *Q, *P; /* recursive polynomials; see Robinson */
  double *qr, *pr;/* reverse recursive polynomials*/
  double *cn; /* r[0]*Qn - Pn; for convenience*/
  double uni[1]={1.}; /* unit "polynomial" - for convenience*/
  double *t1, *t2; /* temporary storage for Q,P var-s (Z-sampled) */
  double *td1;/* temp. storage for z-sampled Q,P  */
  double *mix1, *mix2;/* temporary storage for Q*U-type terms*/
  double *mix3, *mix4; /* temporary storage for Q*U-type terms*/
  double *mix;/* temporary storage for z-sampled mix1,2*/
  double ak; /* product of k-1 transmission coef-s*/
  double *d0; /* shaping for a buried source; see Ganley  */
  double *storx; /* temporary storage for x when k=l>1*/


  /* Initialize */
  initargs(argc, argv);
  requestdoc(1);
	
  /* Get subsurface model */
  if (!gettr(&tr)) err("Can't get reflectivity");
  n = tr.ns - 1 ;

  /* Get parameters */
  if (!getparint("k",&k)) k=1;
  if (!getparint("l",&l))l=1;
  if (!getparint("tmax",&tmax)){
 if (k<n+1)  tmax=(2*n+2-(l-1)-(k-1))/2;
	 else tmax=k;
  }
  if (!getparint("pV",&pV))pV=1;
  if (!getparint("verbose",&verbose))verbose=0;

  /* Check parameters */
  if (n<=0) err("The number of subsurface interfaces n must be >=1!");
  if (k<1)  err("Receiver layer k must be >=1 (k=1 corresponds to surface seismogram).");
  if (l<1)  err("Source layer l must be >= 1 (l=1 corresponds to a surface source).");
  if (l>(n+1)) err("The current version of the program requires l<=n+1.");
  if (tmax<0)  err("The number of the output time samples tmax cannot be negative.");
  if(!( pV==1 || pV==-1 )) err("The field-type flag pV should be either 1 or -1.");

  /* Verbose */
  if (verbose) {
	 warn("Number of layers n=%d", n);
	 warn("Source layer l=%d", l);
	 warn("Receiver layer k=%d", k);
	 warn("Output time samples tmax=%d", tmax);
	 warn("Field type flag pV=%d", pV);
  }

  /* Allocate Memory */
  r = ealloc1double(n+1);
  x = ealloc1double(2*tmax);
  u1 = ealloc1double(2*tmax+n);
  d1 = ealloc1double(2*tmax+n);
  u1b = ealloc1double(2*tmax+n);
  uk = ealloc1double(2*tmax);
  dk = ealloc1double(2*tmax);
  storx = ealloc1double(2*tmax);
  Q = ealloc1double(n+1); 
  P = ealloc1double(n+1);
  qr = ealloc1double(n+1); 
  pr = ealloc1double(n+1); 
  cn = ealloc1double(n+1); 
  t1 = ealloc1double(n+1);
  t2 = ealloc1double(n+1); 
  d0 = ealloc1double(n+1);
  td1 = ealloc1double(2*(n+1));
  mix1 = ealloc1double(2*tmax+n);
  mix2 = ealloc1double(2*tmax+n);
  mix3 = ealloc1double(2*(tmax+n));
  mix4 = ealloc1double(2*(tmax+n));
  mix = ealloc1double(2*(tmax+n));
  
  /* Read and check reflectivity values */
  for (i=0; i<=n; ++i) 
	 {
	r[i] = tr.data[i];
	if(r[i]>1. || r[i]<-1.)
	err("Invalid reflection coefficient encountered.");
	 }
  
  /* Account for field type (pressure/displacement) */
  for (i=0; i<=n; ++i)  r[i]*=pV;
  
  /********************* Begin computation **************************/
  if(n==0)
	 /* Trivial case - implies source in l==1, receiver in k>=1 */
	 {
	memset( (void *) x, 0, 2*tmax*DSIZE);
	x[0] = 1.;
	pshift(x,2*tmax,k-1,x,2*tmax);
	 } 
  else 
	 {
	/* Computing u1: upgoing field in layer 1
	 u1 is needed for all source-receiver configurations	*/
	
recurs(n,Q,P,n,r);  /* compute Qn, Pn */
pcomb(r[0],-1.,Q,P,n+1,n+1,cn,n+1); /* cn = r[0]*Qn - Pn */  
pratio(Q,cn,n+1,n+1,2*tmax+n,u1,2*tmax+n);  /* u1 = Qn/cn */

	/* sampling interval of u1: Z (two-way time-thickness of layers) */
	 
	 
	if(l==1)
	/****************** I. SURFACE SOURCE *******************/
	{ 
  
  if(k>n)
	 /* I.1. Receiver in the homogen. half-space below the layers */
	 /* Only downgoing waves come to the receiver => x = dk */
	 /* The wavefield in layer k>=(n+1) is the same as in layer 
 n+1 but delayed by k-(n+1) one-way traveltime units z.	 */
 {  
		/* compute wavefield in layer n+1:
		 -z^(n)(1+r[1])...(1+r[n])/cn */
	
		/* ak = - (1+r[1])*...*(1+r[n]) */
for (i=1, ak=1; i<=n; ++i)  ak *= 1+r[i];
		ak *= -1.;

		/* compute 1/cn; Z-sampled so far	*/
		pratio(uni,cn,1,n+1,2*tmax,dk,2*tmax);
			
		/* resample 1/cn to z */
		upsample(dk,2*tmax,dk,2*tmax);

		/* z^(n)/cn; z-sampled */
		pshift(dk,2*tmax,n,dk,2*tmax);
	
		/* final value of dk at the top 
		 of layer n+1: dk=ak*z^(n)/cn */
for (i=0; i<2*tmax; ++i)  dk[i]*=ak;
		
		/* account for the one-way delay 
 between layer n+1 and k:  */
		pshift(dk,2*tmax,k-n-1,x,2*tmax);

	 } 
  else 
	 /* I.2. Receiver in the layers k:[1;n]; surface source	 */
	 /* To compute the up- and down-going fields in layer k, 
	 first compute the up- and down-going fields in layer 1 
 and propagate them to layer k.  u1 is already found.*/ 
	 {
		/* Computing d1 = -Pn/cn (P=Pn computed before) */
		pratio(P,cn,n+1,n+1,2*tmax+n,d1,2*tmax+n); 
for (i=0; i<2*tmax+n; ++i)  
		d1[i] *= -1.;			/* d1 is Z-sampled */
		
		if(k==1)
		{ 
  /* I.2.1. SURFACE SEISMOGRAM from a surface source
 (no need to propagate u1 and d1)  */
  
  /* total field x = d1 + u1 */
  pcomb(1.,1.,u1,d1,2*tmax+n,2*tmax+n,x,2*tmax);
  /* x is Z-sampled here */
  /* resample x to z */
  upsample(x,2*tmax,x,2*tmax);
		} 
		else 
		{
  /* I.2.2. RECEIVER AT DEPTH k:(1;n]; surface source; 
	 have to propagate u1 and d1 to layer k	*/
  
  /*  d1*Q_(k-1) + u1*P_(k-1)
  uk = -------------------------------
  z^(k-1) (1-r[1])*...*(1-r[k-1]) 

  
  d1*pr_(k-1) + u1*qr_(k-1)
  dk = -------------------------------
  z^(k-1) (1-r[1])*...*(1-r[k-1])  */
  
  
  /* scaling coeficient ak=(1-r[1])...(1-r[k-1]) */
  for (i=1, ak=1; i<k; ++i)  ak *= 1-r[i];
  
  /*______ upgoing field uk: ______*/ 
  recurs(k-1,Q,P,n,r);
  prod(Q,d1,n+1,2*tmax+n,mix1,2*tmax+n);
  prod(P,u1,n+1,2*tmax+n,mix2,2*tmax+n);
  pcomb(1.,1.,mix1,mix2,2*tmax+n,2*tmax+n,mix1,2*tmax+n);
  upsample(mix1,2*tmax+n,mix,2*(tmax+n));
  pshift(mix,2*(tmax+n),-(k-1),mix1,2*tmax+n);
  for (i=0; i<2*tmax; ++i)  
	 uk[i]=mix1[i]/ak;/* uk is z-sampled here */
  
  /*____ downgoing field dk: _______*/
  rev(Q,n+1,qr);
  pshift(qr,n+1,k-1-porder(Q,n+1),qr,n+1);
  rev(P,n+1,pr);
  pshift(pr,n+1,k-1-porder(P,n+1),pr,n+1);
  
  prod(pr,d1,n+1,2*tmax+n,mix1,2*tmax+n); 
  /* pr, d1, mix1 are Z-sampled */
  prod(qr,u1,n+1,2*tmax+n,mix2,2*tmax+n); 
  /* qr, u1, mix2 are Z-sampled */
  pcomb(1.,1.,mix1,mix2,2*tmax+n,2*tmax+n,mix1,2*tmax+n);
  upsample(mix1,2*tmax+n,mix1,2*tmax+n);
  pshift(mix1,2*tmax+n,-(k-1),mix1,2*tmax+n);
  for (i=0; i<2*tmax; ++i)
	 dk[i]=mix1[i]/ak;/* dk is z-sampled here */
  
  /* total wave field in layer k */
  pcomb(1.,1.,uk,dk,2*tmax,2*tmax,x,2*tmax);

		} 
	 }
	} 
	else 
	/********************** II. BURIED SOURCE *************************
	 d0 needed:
  
		P_(l-1) - r[0]*Q_(l-1) - pV*qr_(l-1) + pV*r[0]*pr_(l-1)
		d0 = -------------------------------------------------------
 z^(l-1) (1+r[1])*...*(1+r[l-1])  

	*******************************************************************/
	{ 
  recurs(l-1,Q,P,n,r);
  pcomb(1.,-r[0],P,Q,n+1,n+1,t1,n+1);

  rev(Q,n+1,qr);
  pshift(qr,n+1,l-1-porder(Q,n+1),qr,n+1);
  rev(P,n+1,pr);
  pshift(pr,n+1,l-1-porder(P,n+1),pr,n+1);

  pcomb(-pV*1.,pV*r[0],qr,pr,n+1,n+1,t2,n+1);
  pcomb(1.,1.,t1,t2,n+1,n+1,d0,n+1);
  for (i=1, ak=1; i<l; ++i)  ak *= 1+r[i];
  for (i=0; i<n+1; ++i) d0[i] *= 1/ak;
  /* d0 is ALWAYS Z-sampled;
  shift by (l-1) z postponed to avoid negative powers of z in d0 */
  
  /* for any buried source, the combination d0*u1 is needed */

  prod(u1,d0,2*tmax+n,n+1,mix1,2*tmax+n); /* u1, d0 are Z-sampled*/
  upsample(mix1,2*tmax+n,mix,2*(tmax+n)); /* now d0*u1 is z-sampled */
  
  /* including the postponed (l-1)z shift */
  pshift(mix,2*(tmax+n),-(l-1),mix1,2*tmax+n);
  
  if( k>=l )
	 {
		/* II.1. Receiver Below the Buried Source
		 (no need to compute u1b; u1 and d0 are sufficient) */
		
		/* d1 needed: d1=1-r[0]*u1 ; Z-sampled */
		pcomb(1.,-r[0],uni,u1,1,2*tmax+n,d1,2*tmax+n);
	 
		if(k<=n+1) /* receiver in the layers */
		{
  kk=k;
  delay=0;
		} 
		else	/* receiver below the layers:
			The wavefield is the same as that in layer n+1 but 
			delayed by k-(n+1) one-way traveltime units z. */
		{
  kk=n+1;
  delay=k-n-1;
		} 
		
		/* Compute the field in layer kk and delay it by z=delay: */
		
for (i=1, ak=1; i<kk; ++i)  ak *= 1-r[i];
		
		recurs(kk-1,Q,P,n,r);

		/* computing uk */
		/* uk=0 for a receiver below the layers: k>n; */
		if(k<=n) 
		{
  prod(Q,d1,n+1,2*tmax+n,mix3,2*(tmax+n));
  prod(P,u1,n+1,2*tmax+n,mix4,2*(tmax+n));
  pcomb(1/ak,1/ak,mix3,mix4,2*(tmax+n),2*(tmax+n),mix3,2*(tmax+n));
  prod(d0,mix3,n+1,2*(tmax+n),mix3,2*(tmax+n));
  upsample(mix3,2*(tmax+n),mix3,2*(tmax+n));
  /* for the shift in d0 not accounted before: */
  pshift(mix3,2*(tmax+n),-(l-1+kk-1),uk,2*tmax);
		} 
		else
		memset( (void *) uk, 0, 2*tmax*DSIZE);

		/* computing dk */
		rev(Q,n+1,qr);
		pshift(qr,n+1,kk-1-porder(Q,n+1),qr,n+1);
		rev(P,n+1,pr);
		pshift(pr,n+1,kk-1-porder(P,n+1),pr,n+1);

		prod(pr,d1,n+1,2*tmax+n,mix3,2*(tmax+n));
		prod(qr,u1,n+1,2*tmax+n,mix4,2*(tmax+n));
		pcomb(1/ak,1/ak,mix3,mix4,2*(tmax+n),2*(tmax+n),mix3,2*(tmax+n));
		upsample(mix3,2*(tmax+n),mix3,2*(tmax+n));
		/* include the postponed -(l-1)z shift in d0: */
		pshift(mix3,2*(tmax+n),-(l-1+kk-1),mix3,2*(tmax+n));
		pshift(mix3,2*(tmax+n),delay,mix3,2*(tmax+n));
for (i=0; i<2*tmax; ++i)  dk[i]=mix3[i];

		/* total field in layer k */
		pcomb(1.,1.,uk,dk,2*tmax,2*tmax,x,2*tmax);
	 
		/* If source and receiver are buried in the same layer:
		 Have to average the results from II.1. RECEIVER BELOW and 
		 II.2. RECEIVER ABOVE the buried source to ensure 
		 displacement=0 or pressure=1 at the source at time 0. */
		/* Store the result from II.1: */
if (k==l) for (i=0; i<2*tmax; ++i)  storx[i]=x[i];
	 
	 }
  
  
  if (k<=l) 
	 {
	 /* II.2. Receiver Above the Buried Source:
	 have to compute u1b (u1*d0 is already in mix1):
	 
				 Q_(l-1) - pV*pr_(l-1)
	 u1b = u1*d0 + -------------------------------
  z^(l-1) (1+r[1])*...*(1+r[l-1])*/
	 
	 
		recurs(l-1,Q,P,n,r);
		rev(P,n+1,pr);
		pshift(pr,n+1,l-1-porder(P,n+1),pr,n+1);
		pcomb(1.,-pV*1.,Q,pr,n+1,n+1,t1,n+1);
		upsample(t1,n+1,td1,2*n+2);
		pshift(td1,2*(n+1),-(l-1),td1,2*(n+1));
for (i=1, ak=1; i<l; ++i)  ak *= 1+r[i];
for (i=0; i<2*(n+1); ++i)  td1[i] *= 1/ak;
pcomb(1.,1.,mix1,td1,2*tmax+n,2*(n+1),u1b,2*tmax+n);  
		/* u1b is z-sampled */
	 
		if(k==1)
		/* SURFACE SEISMOGRAM for a buried source
	u1b is enough to compute it: x = (1-r[0])*u1b	 */
for (i=0; i<2*tmax; ++i)  x[i] = u1b[i]*(1-r[0]);
		
		else /* 1< k <= l */
		{		
  /* BURIED RECEIVER ABOVE the buried source:
  have to propagate the field in the first layer 
  (upgoing u1b; downgoing -r[0]*u1b) to layer k. */
  
  recurs(k-1,Q,P,n,r);
		
  /* uk related term of the seismogram: */
  pcomb(1.,-r[0],P,Q,n+1,n+1,t1,n+1);/* t1 is Z-sampled */
		
  /* dk related term of the seismogram: */
  rev(Q,n+1,qr);
  pshift(qr,n+1,k-1-porder(Q,n+1),qr,n+1);
  rev(P,n+1,pr);
  pshift(pr,n+1,k-1-porder(P,n+1),pr,n+1);

  pcomb(1.,-r[0],qr,pr,n+1,n+1,t2,n+1);/* t2 is Z-sampled */

  pcomb(1.,1.,t1,t2,n+1,n+1,t1,n+1);
  upsample(t1,n+1,td1,2*n+2);
  prod(td1,u1b,2*n+2,2*tmax+n,mix,2*(tmax+n));
  pshift(mix,2*(tmax+n),-(k-1),mix2,2*tmax+n);
  for (i=1, ak=1; i<k; ++i)  ak *= 1-r[i];
  
  /* total field in layer k: */
  for (i=0; i<2*tmax; ++i)  x[i]=mix2[i]/ak;

  /* If source and receiver are buried in the same layer:
  Have to average the results from II.1. RECEIVER BELOW 
  (stored in storx) and II.2. RECEIVER ABOVE the buried 
  source (just obtained) to ensure displacement=0 or 
  pressure=1 at the source at time 0.*/
  /* average seismogram: */
  if(k==l)  pcomb(.5,.5,storx,x,2*tmax,2*tmax,x,2*tmax);
  
		}
	 }
	}
	 }
  
  /* output synthetic seismogram (resampled to Z) */
  i=(k-l)/2;
  if ( 2*i == k-l ){  
 for (i=0; i<tmax; ++i)  tr.data[i]=x[2*i];
	 tr.delrt=0;
  }
  else{
 for (i=0; i<tmax; ++i)  tr.data[i]=x[2*i+1];
	 tr.delrt=tr.dt/2000;
  }

  tr.ns=tmax;
  tr.trid=1;

  puttr(&tr);
  

  /* Free space */
  free1double(r);
  free1double(x);
  free1double(u1);
  free1double(d1);
  free1double(u1b);
  free1double(uk); 
  free1double(dk); 
  free1double(storx); 
  free1double(Q);
  free1double(P);
  free1double(qr);
  free1double(pr);
  free1double(cn);
  free1double(t1);
  free1double(t2);
  free1double(d0);
  free1double(td1);
  free1double(mix1);
  free1double(mix2);
  free1double(mix3);
  free1double(mix4);
  free1double(mix);

  return(CWP_Exit());
}


/*--------------------------- functions used ---------------------------*/

/*************************************************************************
imin, imax - return the smaller/larger integer of two integers
--------------------------------------------------------------------------
Notes:  Not used in main(), only in the functions below. 
*************************************************************************/
int imin( int c1, int c2 )
{
  if(c1<=c2)
	 return(c1);
  else
	 return(c2);
}

int imax( int c1, int c2 )
{
  if(c1>=c2)
	 return(c1);
  else
	 return(c2);
}


/*************************************************************************
porder - return the order of a polynomial 
	 (the highest power with a non-zero coefficient)
--------------------------------------------------------------------------
Input:
 p[]  array of polynomial coefficients
 l	 polynomial length (length of p[]) 
--------------------------------------------------------------------------
Notes: If the array p[] contains coefficients of negative powers, 
	 the result would be biased by the maximum negative power.
	 Example: If p[] = a0*z^(-1) + a1*z^0 + a2*z^1 + a3*z^2 and a3=0, 
	 the max. power in p[] is 1 but porder(...) will return 2 instead.
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva2000
-------------------------------------------------------------------------*/
int porder( double p[], int l )
{
  int i=0,k=0,j;
  
  do{
	 j=k;
 if(p[l-1-i]==0)  k=k+1;
	 ++i;
  } while(k>j);
  
  return(l-1-j);
}


/*************************************************************************
pcomb - linear combination of two polynomials
--------------------------------------------------------------------------
Input:
 alfa	 scaling factor
 beta	 scaling factor
 p1[]	 array of polynomial coefficients
 p2[]	 array of polynomial coefficients
 l1	length of p1[] 
 l2	length of p2[] 
 l	 length of output array c[]

Output:
 c[]  array of polynomial coefficients c[i] = alfa*p1[i] + beta*p2[i]
--------------------------------------------------------------------------
Note: calling statements of the kind  pcomb(a,b, P ,Q,l1,l2, P ,l) allowed
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void pcomb( double alfa, double beta, double p1[], double p2[], 
	 int l1, int l2, double c[] , int l)
{
  int i,ii;
  double *tmp;
  
  ii=imax(l1,l2);

  /* Allocate and zero temporary array */ 
  tmp=ealloc1double(ii);
  memset( (void *) tmp, 0, ii*DSIZE);
  
  if (l1>=l2)
 for(i=l1-1; i>l2-1; i--)  tmp[i] = alfa*p1[i];
  else
 for(i=l2-1; i>l1-1; i--)  tmp[i] = beta*p2[i];

  for(i=0; i<imin(l1,l2); ++i)  tmp[i] = alfa*p1[i] + beta*p2[i];
  
  /* Clear output array */
  memset( (void *) c, 0, l*DSIZE);

  /* Output */
  for(i=0; i<=imin(ii,l-1); ++i)  c[i]=tmp[i];
  
  /* Free space */
  free1double(tmp);
}


/*************************************************************************
rev - returns reverse-polinomial coefficients 
--------------------------------------------------------------------------
Input:
 p[]  array of polynomial coefficients
 l	 length of p[]

Output:
 pr[]	 array of polynomial coefficients 
	 pr[0]=p[k], pr[1]=p[k-1], ... , pr[k]=p[0]
	 where k is the order of the input polynomial p[]
--------------------------------------------------------------------------
Note: calling statements of the kind  rev( P ,l, P )  allowed
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void rev(double p[],int l,double pr[])
{
  int i;
  double *tmp;
  int order;
  
  order = porder(p,l);

  /* Allocate and zero temporary array */  
  tmp = ealloc1double(order+1);
  memset( (void *) tmp, 0, (order+1)*DSIZE);
  
  for(i=0; i<=order; ++i)  tmp[i] = p[order-i];
  
  /* Clear output array */ 
  memset( (void *) pr, 0, l*DSIZE);
  
  /* Output */
  for(i=0; i<=order; ++i)  pr[i] = tmp[i];
  
  /* Free space */
  free1double(tmp);
}


/*************************************************************************
pshift - shift a z-polynomial of order n by k units 
--------------------------------------------------------------------------
Input:
 p[]  array of polynomial coefficients
 l	 length of p[] 
 k	 integer (has the meaning of time-shift in this code)
 ll	length of output array psh[]

Output:
 psh[]	the coefficients of a polynomial psh[] = z^(k)*p[]
	 where p[] is the input polynomial in z
--------------------------------------------------------------------------
Notes: k should be such that the resulting polynomial has only (+) powers;
 calling statements of the kindpshift( P ,l,k, P ,ll)  allowed.
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void pshift( double p[], int l, int k, double psh[], int ll )
{
  int ii,i;
  double *tmp;
  
  ii=imax(l+k,l);
  
  /* Allocate and zero temporary array */
  tmp = ealloc1double(ii);
  memset( (void *) tmp, 0, ii*DSIZE);
  
  if(k>=0)
	 for (i=0; i<l; ++i)	tmp[i+k] = p[i];
  else
 for (i=-k; i<l; ++i)  tmp[i+k] = p[i];
  
  /* Clear output array */
  memset( (void *) psh, 0, ll*DSIZE);
  
  /* Output */
  for(i=0; i<imin(l,ll); ++i)  psh[i] = tmp[i];
  
  /* Free space */
  free1double(tmp);
}


/*************************************************************************
prod - polynomial multiplication
--------------------------------------------------------------------------
Input:
 p1[]	 array of polynomial coefficients
 p2[]	 array of polynomial coefficients
 l1	length of p1[]
 l2	length of p2[]
 ll	length of output array pp[]

Output:
 pp[]	 array of polynomial coefficients
	 pp is essentially the convolution of p1 and p2
--------------------------------------------------------------------------
Note: calling statements of the kind  prod( P ,Q,l1,l2, P ,ll)  allowed
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void prod( double p1[], double p2[], int l1, int l2, double pp[], int ll )
{
  int i,j,k;
  double *tmp;

  /* Allocate and zero temporary array */ 
  tmp = ealloc1double(l1+l2-1);
  memset( (void *) tmp, 0, (l1+l2-1)*DSIZE);
  
  for (i=0; i<l1; ++i){
	 for (j=0; j<l2; ++j){
	k=i+j;
	tmp[k]=tmp[k]+p1[i]*p2[j];
	 }
  }
  
  /* Clear output array */ 
  memset( (void *) pp, 0, ll*DSIZE);

  /* Output */
  for(i=0; i<imin(l1+l2-1,ll); ++i)pp[i]=tmp[i];
  
  /* Free space */
  free1double(tmp);
}


/*************************************************************************
pratio - polynomial division
--------------------------------------------------------------------------
Input:
 p1[]	 array of polynomial coefficients
 p2[]	 array of polynomial coefficients
 l1	length of p1[]
 l2	length of p2[]
 L	 desired number of output coefficients
 lq	length of output array q[]

Output:
 q[]  the first L coefficients of the polynomial q=p1/p2
--------------------------------------------------------------------------
Note:	 see E. Robinson, Chap.1 for the algorithm (a Fortran subroutine)
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void pratio( double p1[], double p2[], int l1, int l2, int L, 
  double q[], int lq )
{
  int i=0,k=0,j,ii,jj;
  int lo,o1,o2;
  double *tmp1, *tmp2;
  
  o1=porder(p1,l1);
  o2=porder(p2,l2);
  
  /* Allocate temporary arrays */
  tmp1 = ealloc1double(L+o2+1);
  tmp2 = ealloc1double(o2+1);
  
  /* Zero temporary arrays */
  memset( (void *) tmp1, 0, (L+o2+1)*DSIZE);
  memset( (void *) tmp2, 0, (o2+1)*DSIZE);
  
  do{
	 jj=k;
	 if(p2[i]==0) k=k+1;
	 ++i;
  } while(k>jj);
  
  pshift(p2,l2,-jj,tmp2,o2+1);
  
  lo=imin(o1,L+o2);
  
  for (i=0; i<=lo; ++i)  tmp1[i] = p1[i];
  
  i=0;
  do{
	 tmp1[i]=tmp1[i]/tmp2[0];
	 if(i==L+jj)
	break;
	 
	 k=i;
	 ii=imin(o2,L+jj-i);
	 j=0;
	 for(j=0; j<ii; j++){
	k=k+1;
	tmp1[k]=tmp1[k]-tmp1[i]*tmp2[j+1];
	 }
	 ++i;
  } while(1);
  
  pshift(tmp1,L+o2+1,-jj,tmp1,L+o2+1);
  
  /* Clear output array */
  memset( (void *) q, 0, lq*DSIZE);
  
  /* Output */
  for(i=0; i<imin(L,lq); ++i) q[i]=tmp1[i];
  
  /* Free space */
  free1double(tmp1);
  free1double(tmp2);
}


/*************************************************************************
recurs - compute recursive polynomials Q_k, P_k
--------------------------------------------------------------------------
Input:
 k	 polynomial order; k>=1
 n	 number of subsurface interfaces in a Goupillaud medium 
	 (horizontal interfaces equally spaced in time);
 r[]	 reflection coefficient series of length n+1;

Output:
 q[]	 the coefficients of a polynomial Q of order k
 p[]	 the coefficients of a polynomial P of order k
--------------------------------------------------------------------------
Note:	 see E. Robinson, p.124-125
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void recurs( int k, double q[], double p[], int n, double r[])
{ 
  int i,j;
  double *tq, *qr, *qsh;
  double *tp, *pr, *psh;
  
  /* Allocate temporary arrays */
  tq = ealloc1double(n+1);
  tp = ealloc1double(n+1);
  qr = ealloc1double(n+1);
  pr = ealloc1double(n+1);
  qsh= ealloc1double(n+1);
  psh= ealloc1double(n+1);
  
  /* Zero temporary arrays */
  memset( (void *) tq, 0, (n+1)*DSIZE);
  memset( (void *) tp, 0, (n+1)*DSIZE);
  memset( (void *) qr, 0, (n+1)*DSIZE);
  memset( (void *) pr, 0, (n+1)*DSIZE);
  memset( (void *) qsh, 0, (n+1)*DSIZE);
  memset( (void *) psh, 0, (n+1)*DSIZE);
  
  /* initial values for the recursion: Q1, P1 */
  tp[0]=1;
  tq[1]=-r[1];
  
  /* recursive calculation of Qk, Pk */
  for(j=2; j<=k; j++) 
	 {
	rev(tq,n+1,qr);
	pshift(qr,n+1,(j-1-porder(tq,n+1)),qr,n+1);	
	/* qr - reverse Q polynomial */
	rev(tp,n+1,pr);
	pshift(pr,n+1,(j-1-porder(tp,n+1)),pr,n+1);	
	/* pr - reverse P polynomial */
	
	pshift(qr,n+1,1,qsh,n+1);
	pshift(pr,n+1,1,psh,n+1);
	
	pcomb(1.,-r[j],tq,psh,n+1,n+1,tq,n+1);	/* now tq = Qj */
	pcomb(1.,-r[j],tp,qsh,n+1,n+1,tp,n+1);	/* now tp = Pj */
	
	 }
  
  /* return Qk, Pk */
  for(i=0; i<n+1; ++i)
	 {
	q[i]=tq[i];
	p[i]=tp[i];
	 }
  
  /* Free space */
  free1double(tq);
  free1double(tp);
  free1double(qr);
  free1double(pr);
  free1double(qsh);
  free1double(psh);
}


/*************************************************************************
upsample - resample at half sampling interval
--------------------------------------------------------------------------
Input:
 p4[]	 array of coefficients of a polynomial in Z
 l	 length of p4[]
 ll	length of output array p2[]

Output:
 p2[]	 input array p4[] padded by a zero at every other sample,
	 i.e., the output array p2[] represents a polynomial in z=sqrt(Z)
--------------------------------------------------------------------------
Author:  CWP: Albena Mateeva  2000
------------------------------------------------------------------------*/
void upsample( double p4[], int l, double p2[], int ll )
{
  int i=0, ii;
  double *tmp;
  
  ii=imax(ll,2*l);
  
  tmp = ealloc1double(ii);
  memset( (void *) tmp, 0, ii*DSIZE);
  
  do{
	 tmp[2*i]=p4[i];
	 ++i;
  } while(i<l);
  
  for(i=0;i<ll;++i) p2[i]=tmp[i];
  
  free1double(tmp);
}

