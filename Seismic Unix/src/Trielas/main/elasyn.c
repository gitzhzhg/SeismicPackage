/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* ELASYN: $Test Release: 1.2 $ ; $Date: 2011/11/21 17:00:43 $	*/

#include "par.h"
#include "tri.h"
#include "elastic.h"
#
/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" ELASYN - SYNthetic seismograms for triangulated elastic media		",
"									",
"  elasyn <rayends xg= zg= [optional parameters]			",
"									",
"Required Parameters:							",
"xg=            x coordinates of receiver surface			",
"zg=            z coordinates of receiver surface			",
"									",
"Optional Parameters:							",
"compon=0         horizontal and vertical component seismograms         ",
"		  =3 vertical component (positive downwards)		",
"		  =1 horizontal component				",
"ng=101           number of receivers (uniform distributed along surface)",
"krecord=1        integer index of receiver surface (see notes below)	",
"nt=251           number of time samples				",
"dt=0.004         time sampling interval				",
"ft=0.0           first time sample					",
"inter=0          linear interpolation					",
"inter=1 (default) cross parabolic interpolation 			",
"reftrans=0       =1 complex refl/transm. coefficients considered 	",
"nameref=-1       all rays recorded at interface <krecord> considered 	",     
"                 =0, only direct hits are considered  			",
"                 >0, only rays reflected from interface <nameref>      ",
"lscale=          if defined restricts range of extrapolation		",
"fpeak=0.1/dt     peak frequency of ricker wavelet 			",
"infofile         ASCII-file to store useful information 		",
"xfile=x_compon.bin     bin-file to store x_component traces 		",
"zfile=z_compon.bin     bin-file to store z_component traces 		",
"									",
"NOTES:									",
"Only rays that terminate with index krecord will contribute to the	",
"synthetic seismograms at the receiver (xg,zg) locations.  The		",
"receiver locations are determined by cubic spline interpolation	",
"of the specified (xg,zg) coordinates.					",
"									",
" Warning!!-- This version is not quite complete. There is a bug in the ",
" interpolation routines that causes a segmentation violation on the last",
" couple  of traces.							",
"									",
NULL};

/*
 * AUTHORS:  Andreas Rueger, Colorado School of Mines, 02/02/94
 *            Tariq Alkalifah, Colorado School of Mines, 02/02/94
 *	     (interpolation routines)
 *	     
 * The program is based on :
 *	        gbbeam.c, AUTHOR: Andreas Rueger, 08/12/93
 *	       	sdbeam.c, AUTHOR Dave Hale, CSM, 02/26/91
 */

/**************** end self doc ***********************************/


/* prototypes for functions defined and used internally */
static void makexzs (int nu, float *xu, float *zu, 
	int ns, float *xs, float *zs);
static complex cricker (float w, float wpeak, float delay);
void writeinfo(RayEnd *re, FILE *ifp, int nre);
void interpara(int na, float *a, float *x, int ng, float dg, float fg,
	float tol, int *im, float **ag); 
void interplin(int na, float *a, float *x, int ng, float dg, float fg,
	float tol, int *im, float **ag); 
static void makesyn (int reftrans, int ng, float *xg, int comp,
        float *zg, float fpeak,	int nt, float dt, float ft, 
	FILE *infofp, float **axg, float **azg,	float **phg, 
	float **tg, float dangle, int *im, FILE *xfp, FILE *zfp);
void rayJacob(float dg, float *x, int *num, float *pz, float *ph,
	float *ax, float *az, int nrays);
void polar(float px, float pz, float g11, float g13,
	float g33, float *polx, float *polz, int mode );

/* the main program */
int main (int argc, char **argv)
{
	int nxg,nzg,nxz,ng,krecord,nt,nre,nrealloc,reftrans;
	int compon,nri,nameref,*num,ir,*im,inter,i,ii;
	float *xg,*zg,*zero;
	float *ax,*az,*t,*x,*z,*px,*pz,*ph;
	float dt,ft,lscale,fpeak,dg,dangle;
	float xmax,xmin,polarx,polarz;
	float **axg,**azg,**tg,**phg;
	RayEnd *re;	
	char *ifile, *xfile, *zfile;
	FILE *ifp=NULL, *xfp=NULL, *zfp=NULL;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/********************* get parameters ********************/
	nxg = countparval("xg");
	nzg = countparval("zg");
	if (nxg==0 || nzg==0) err("must specify both xg and zg");
	if (nxg!=nzg) err("number of xg must equal number of zg");
	nxz = nxg;
	if (!getparint("ng",&ng)) ng = 101;
	
	xg = ealloc1float(MAX(nxz,ng));
	zg = ealloc1float(MAX(nxz,ng));
	getparfloat("xg",xg);
	getparfloat("zg",zg);

	if (getparstring("infofile",&ifile)) ifp = efopen(ifile,"w");

	if (!getparfloat("lscale",&lscale)) lscale = FLT_MAX;
	if (!getparint("krecord",&krecord)) krecord = 1;
	if (!getparint("compon",&compon)) compon = 0;
	if (!getparint("nt",&nt)) nt = 251;
	if (!getparfloat("dt",&dt)) dt = 0.004;
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparint("reftrans",&reftrans)) reftrans = 0;
	if (!getparfloat("fpeak",&fpeak)) fpeak = 0.1/dt;
	if (!getparint("nameref",&nameref)) nameref = -1;
	if (!getparint("inter",&inter)) inter = 1;

	if (compon == 0 || compon == 1){
	        if(getparstring("xfile",&xfile))
		 		 xfp = efopen(xfile,"w");
		else xfp = efopen("x_compon.bin","w");
	}

	if (compon == 0 || compon == 3){
	        if(getparstring("zfile",&zfile))
		 		 zfp = efopen(zfile,"w");
		else zfp = efopen("z_compon.bin","w");
	}

        checkpars();


	if ( 0.5/dt < 2.0*fpeak){
		fprintf(stderr," WARNING: ALIASING POSSIBLE \n");
		fprintf(stderr," decrease dt or reduce fpeak \n");
	}

        /* currently no refl/trans coeff supported */
        if(reftrans==1)
	  warn("\n WARNING currently no refl/trans coeff supported \n");
	
        warn(" \n NOTE This is a very simple, go-bye seismogram builder\n ");
	
	nre = 0;
	nrealloc = 200;

	/****** allocate space ********/
	re = ealloc1(nrealloc,sizeof(RayEnd));

	tg  = ealloc2float(3,ng+1);
	axg = ealloc2float(3,ng+1);
	azg = ealloc2float(3,ng+1);
	phg = ealloc2float(3,ng+1);

	im  = ealloc1int(ng+1);

	/* initialize arrays*/
	for(i=0;i<ng;i++){
	  im[i]=0;
	  for(ii=0;ii<3;ii++){
	    tg[i][ii]=0.;
	    axg[i][ii]=0.;
	    azg[i][ii]=0.;
	    phg[i][ii]=0.;
	  }
	}
	    
	/********** read all rayends *********/
	while (fread(&re[nre],sizeof(RayEnd),1,stdin)==1) {
		nre++;
		if (nre==nrealloc) {
			nrealloc += 200;
			re = erealloc1(re,nrealloc,sizeof(RayEnd));
		}
	}

	ax = ealloc1(nre,sizeof(float));
	az = ealloc1(nre,sizeof(float));
	t = ealloc1(nre,sizeof(float));
	x = ealloc1(nre,sizeof(float));
	z = ealloc1(nre,sizeof(float));
	num = ealloc1(nre,sizeof(int));
	px = ealloc1(nre,sizeof(float));
	pz = ealloc1(nre,sizeof(float));
	ph = ealloc1(nre,sizeof(float));

	dangle=re[0].dangle;
	xmax=-FLT_MAX;
	xmin=FLT_MAX;

	/* write out re-end info */
	if(ifp!=NULL) writeinfo(re,ifp,nre);

	/* compute receiver coordinates uniformly sampled in distance s */
	makexzs(nxz,xg,zg,ng,xg,zg);

        /******** how many rayends are of interest ********/
        for(ir = 0, nri=0 ; ir < nre;ir +=1 ){
	    if(re[ir].kend == krecord &&
	    (nameref== -1 || nameref== re[ir].nameref)){

		t[nri]  = re[ir].t;
		x[nri]  = re[ir].x;
		z[nri]  = re[ir].z;
		num[nri]= re[ir].num;
		pz[nri]  = re[ir].pz;
		px[nri]  = re[ir].px;
		ph[nri]  = re[ir].ampliphase;
						
		polar(px[nri],pz[nri],re[ir].g11,re[ir].g13,
			re[ir].g33,&polarx,&polarz,re[ir].mode);

		/* this is the displacement with respect to
		positive x-axis and negative z-axis */

		ax[nri] = polarx*re[ir].ampli;
		az[nri] = -polarz*re[ir].ampli;

		xmax=( x[nri]>xmax ? x[nri]: xmax);
		xmin=( x[nri]<xmin ? x[nri]: xmin);

		nri++;
      	    }
	}
	
	/*********** free workspace **********************/
	free1(re);

	/*********** special case NO ray of interest******/
	if(nri==0){
	        warn("\n NO RAY CONTRIBUTION. Check <krecord> \n");
		zero = ealloc1float(nt);
		for(nri=0;nri<nt;nri++) zero[nri]=0;
		for(nri=0;nri<ng;nri++){
		if (compon == 0 || compon == 1)
	  		fwrite(zero,sizeof(float),nt,xfp);
		if (compon == 0 || compon == 3)
		  fwrite(zero,sizeof(float),nt,zfp);
		}
		return 1;
	}

	/*********** define a tolerance ******************/
	if(lscale == FLT_MAX)
		lscale=(xmax-xmin)*20/nre;


	/*********** receiver spacing ******************/
	dg = (xg[ng-1]-xg[0])/(ng>1?ng-1:1);

	/*********** special case: one receiver *********/
	if(ng==1)
		dg=(xmax-xmin)*5/nre;


	/*********** compte ray jacobian *************/
	rayJacob(dg,x,num,pz,ph,ax,az,nri);

	/*for (ir=0; ir<nri; ++ir)
	 	fprintf(stderr,"x(%i)=%g\t t=%g \n",num[ir],x[ir],t[ir]);
	for (ir=0; ir<nri; ++ir)
	 	fprintf(stderr,"ax(%i)=%g \t az=%g \n",num[ir],ax[ir],az[ir]);
	*/

	if(inter==1){
		/*********** interpolate traveltimes *************/
		interpara(nri,t,x,ng,dg,xg[0],lscale,im,tg);

		/*********** interpolate amplitudes *************/
		interpara(nri,ax,x,ng,dg,xg[0],lscale,im,axg);
		interpara(nri,az,x,ng,dg,xg[0],lscale,im,azg);

		/*********** interpolate phase *************/
		interpara(nri,ph,x,ng,dg,xg[0],lscale,im,phg);
	} else {
		/*********** interpolate traveltimes *************/
		interplin(nri,t,x,ng,dg,xg[0],lscale,im,tg);

		/*********** interpolate amplitudes *************/
		interplin(nri,ax,x,ng,dg,xg[0],lscale,im,axg);
		interplin(nri,az,x,ng,dg,xg[0],lscale,im,azg);

		/*********** interpolate phase *************/
		interplin(nri,ph,x,ng,dg,xg[0],lscale,im,phg);

	}
	/*{
	  int ir,i;
	  
	  for (ir=0; ir<ng; ++ir)
	  for(i=0; i<im[ir]; ++i)
		  fprintf(stderr,"ig=%d im=%i xg=%f tg=%f\n"
			  ,ir,im[ir+1],xg[ir],tg[ir+1][0]);
	for (ir=0; ir<ng; ++ir)
		for(i=0; i<im[ir]; ++i)
	 	fprintf(stderr,"axg(%d)=%f\n",ir,axg[ir][i]);
	for (ir=0; ir<ng; ++ir)
		for(i=0; i<im[ir]; ++i)
	 	fprintf(stderr,"azg(%d)=%f\n",ir,azg[ir][i]);
	for (ir=0; ir<ng; ++ir)
		for(i=0; i<im[ir]; ++i)
	 	fprintf(stderr,"phg(%d)=%f\n",ir,phg[ir][i]);
	
	}*/
	


        /* There seems to be a problem in Tariq'a interpolation
	   routines. I have to shift the offsets by one.
	   Note that the last two traces are zero.*/
	/********** make synthetic seismograms ***********/
	makesyn (reftrans,ng,xg,compon,zg,fpeak,nt,dt,ft,ifp,
		 axg+1,azg+1,phg+1,tg+1,dangle,im+1,xfp,zfp);
	
	return 1;
}

static void makexzs (int nu, float *xu, float *zu, 
	int ns, float *xs, float *zs)
/* interpolate (x,z) coordinates uniformly sampled in distance s */
{
	int iu,nuu,iuu;
	float x,z,xlast,zlast,dx,dz,duu,uu,ds,
		*u,*s,(*xud)[4],(*zud)[4],*us;
	
	xud = (float(*)[4])alloc1float(4*nu);
	zud = (float(*)[4])alloc1float(4*nu);
	u = ealloc1float(nu);
	for (iu=0; iu<nu; ++iu)
		u[iu] = iu;
	csplin(nu,u,xu,xud);
	csplin(nu,u,zu,zud);
	nuu = 20*nu;
	duu = (u[nu-1]-u[0])/(nuu-1);
	s = ealloc1float(nuu);
	s[0] = 0.0;
	xlast = xu[0];
	zlast = zu[0];
	for (iuu=0,uu=0.0,s[0]=0.0; iuu<nuu; ++iuu,uu+=duu) {
		intcub(0,nu,u,xud,1,&uu,&x);
		intcub(0,nu,u,zud,1,&uu,&z);
		dx = x-xlast;
		dz = z-zlast;
		s[iuu] = s[iuu-1]+sqrt(dx*dx+dz*dz);
		xlast = x;
		zlast = z;
	}			
	us = ealloc1float(ns);
	ds = (s[nuu-1]-s[0])/(ns>1?ns-1:1);
	yxtoxy(nuu,duu,0.0,s,ns,ds,0.0,
		0.0,(float)(nu-1),us);
	intcub(0,nu,u,xud,ns,us,xs);
	intcub(0,nu,u,zud,ns,us,zs);
	free1float(us);
	free1float(s);
	free1float(u);
	free1float((float*)xud);
	free1float((float*)zud);
}


static complex cricker (float w, float wpeak, float delay)
/*****************************************************************************
Compute Fourier transform of Ricker wavelet - complex function of frequency
******************************************************************************
Input:
w		frequency at which to evaluate transform
wpeak		peak (dominant) frequency in radians
delay		time shift (used for an approximately causal wavelet)
******************************************************************************
Notes:
The amplitude of the Ricker wavelet at a frequency of 2.5*wpeak is 
approximately 4 percent of that at the dominant frequency wpeak.
The Ricker wavelet effectively begins at time t = -2*PI/wpeak.  Therefore,
for practical purposes, a causal wavelet may be obtained by a time delay
of 2*PI/wpeak.
The Ricker wavelet has the shape of the second derivative of a Gaussian.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 02/28/91
******************************************************************************/
{
	return crmul(cwp_cexp(cmplx(-pow(w/wpeak,2.0),delay*w)),
		4.0*w*w*sqrt(PI)/pow(wpeak,3.0));
}

void writeinfo(RayEnd *re, FILE *ifp, int nre)
{
	int iangle=0;
	fprintf(ifp,"\n The following information is stored at the rayend \n");
	do{
		
		 fprintf(ifp,"\n Ray stops at (%g,%g) at interface %i\n" 
			,re[iangle].x,re[iangle].z,re[iangle].kend);
		 fprintf(ifp," Ray number %i \tTakeoff angle =%g  \n",
			re[iangle].num,re[iangle].ab);
	         fprintf(ifp," Slowness components px=%g pz=%g\n", 
			re[iangle].px,re[iangle].pz);
		 fprintf(ifp," Traveltime =%g  \t Sigma=%g\n", 
			re[iangle].t,re[iangle].sigma);
		 fprintf(ifp," kmah index=%i \t Number of reflections=%i\n", 
			re[iangle].kmah,re[iangle].nref);
		 fprintf(ifp," last reflector=%i \t Angle increment=%g \n",
			re[iangle].nameref, re[iangle].dangle*180./PI);
		 fprintf(ifp," g11=%g \t g33=%g \t g13=%g\n",
			re[iangle].g11, re[iangle].g33,re[iangle].g13);
		 fprintf(ifp," vgx=%g \t vgz=%g \t \n",
			re[iangle].vgx, re[iangle].vgz);
		 fprintf(ifp," Refl/Transm Amplitude =%g Phase=%g\n", 
			re[iangle].ampli,re[iangle].ampliphase); 
		 fprintf(ifp,"\n --------------------------------------------------- \n");
		 iangle++;

               }while(iangle<=nre);
}

void interplin(int na, float *a, float *x, int ng, float dg, float fg,
		float tol, int *im, float **ag)
{
	int iti,itf,it,ix,itmin,itmax,temp,sub;
	float frac,x1,x2,odg,tempf,aa1,aa2;


	itmin = 0;
	itmax = 0;
	odg   = 1/dg;

	for(it=0; it<ng; ++it)
		im[it]=0;

	x1  = x[0]-fg;
	iti = x1*odg;
	aa1 = a[0];

	for(ix=1; ix<na; ++ix){
		x2  = x[ix]-fg;
		itf = x2*odg;
		aa2 = a[ix];
		sub = 1;

		if(itf<iti){
			temp = itf;
			itf  = iti;
			iti  = temp;
			tempf= x1;
			x1   = x2;
			x2   = tempf;
			tempf= aa1;
			aa1  = aa2;
			aa2  = tempf;
			sub  = 0;
		}

		if((x2-x1)>tol){
			for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
				ag[it][im[it]] = 0.0;
				++im[it];
			};

		} else{
			for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
				itmin = MIN(itmin,it);
				itmax = MAX(itmax,it);
				frac  = (it*dg-x1)/(x2-x1);
				ag[it][im[it]]= (1.0-frac)*aa1+frac*aa2;
				++im[it];
			};
		}
		
		if(sub) {
			iti = itf;
			x1  = x2;
			aa1 = aa2;
		}
	}

	for(it=0; it<itmin; ++it){
		ag[it][0] = 0.0;
		++im[it];
	}
	for(it=itmax+1; it<ng; ++it){
		ag[it][0] = 0.0;
		++im[it];
	}
}


static void makesyn (int reftrans, int ng, float *xg, int compon,
        float *zg, float fpeak,	int nt, float dt, float ft, 
	FILE *ifp, float **axg, float **azg, float **phg, 
	float **tg, float dangle, int *im, FILE *xfp, FILE *zfp)
{

	int ntfft,nw,iw,ig,it;
	float dw,w,wpeak,delay,*syn,cosf,sinf;
	double campr,campi,cosw,sinw,cosd,sind,tempd;
	complex *cwave,**csynx,**csynz;

	/* constants */
	ntfft = npfaro(nt,2*nt);
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	wpeak = 2*PI*fpeak;
	delay = 0.0;

	/* allocate workspace */
	syn = ealloc1float(ntfft);
	cwave = ealloc1complex(nw);
	csynx = ealloc2complex(nw,ng);
	csynz = ealloc2complex(nw,ng);

	/* initialize synthetics */
	for (ig=0; ig<ng; ++ig){
		for (iw=0; iw<nw; ++iw){
			csynx[ig][iw] = cmplx(0.0,0.0);
			csynz[ig][iw] = cmplx(0.0,0.0);
		}
	}

	/********** make complex ricker wavelet ************/
	for (iw=0,w=0.0; iw<nw; ++iw,w+=dw) {
		cwave[iw] = cricker(w,wpeak,delay);
		cwave[iw] = crmul(cwave[iw],sqrt(w));
	}

	
	/* loop over receiver locations */
	for (ig=0; ig<ng; ++ig) {
          fprintf(stderr,"ig=%i im=%i tg=%f a=%f \n",
		  ig,im[ig],tg[ig][0],axg[ig][0]);
	  
          /* account for multiple arrivals */
	  for(it=0; it<im[ig]; ++it){
          fprintf(stderr,"ig=%i im=%i tg=%f a=%f phg=%f \n",
		  ig,im[ig],tg[ig][0],axg[ig][0],phg[ig][it]);
	    cosf  = cos(phg[ig][it]);
	    sinf  = sin(phg[ig][it]);

	    /* compute cos, sin, and exp increments */
	    cosd = cos(dw*(tg[ig][it]-ft));
	    sind = sin(dw*(tg[ig][it]-ft));

	    /* initialize cos, sin, and exp */
	    cosw = 1.0;
	    sinw = 0.0;

 	    /* for all frequencies */
	    for(iw=0, w=0.0; iw<nw; ++iw, w+=dw){
              
		campr = cosf*cosw-sinf*sinw;
		campi = cosf*sinw+sinf*cosw;

		csynx[ig][iw].r += campr*axg[ig][it];
		csynx[ig][iw].i += campi*axg[ig][it];

		csynz[ig][iw].r += campr*azg[ig][it];
		csynz[ig][iw].i += campi*azg[ig][it];

		/* update cos, sin */
		tempd = cosw*cosd-sinw*sind;
		sinw = cosw*sind+sinw*cosd;
		cosw = tempd;
	    }

	  }

	  
	  /* x component seismograms */
	  if(compon == 1 || compon == 0){

          	/********** apply wavelet to synthetics *************/
	   	for (iw=0; iw<nw; ++iw)
		  csynx[ig][iw] = cmul(csynx[ig][iw],cwave[iw]);

 	   	/********** inverse Fourier transform ***************/
	   	pfacr(-1,ntfft,csynx[ig],syn);

	  	fwrite(syn,sizeof(float),nt,xfp);
           }

	  /* z component seismograms */
	  if(compon ==3 || compon == 0){

          	/********** apply wavelet to synthetics *************/
	   	for (iw=0; iw<nw; ++iw)
		  csynz[ig][iw] = cmul(csynz[ig][iw],cwave[iw]);

 	   	/********** inverse Fourier transform ***************/
	   	pfacr(-1,ntfft,csynz[ig],syn);

	  	fwrite(syn,sizeof(float),nt,zfp);
          }

     }
        
}	



void rayJacob(float dg, float *x, int *num, float *pz, float *ph,
	float *ax, float *az, int nrays)
{
	int iray,diffnum,caustic;
	float jac1,jac2,jac;

	caustic=0;

	/****** first ray *******/
	jac = (x[0]-x[1])*pz[0];
	diffnum = num[1]-num[0];
	if(diffnum !=1) jac=jac/diffnum;

	ax[0] /= sqrt(ABS(jac));
	az[0] /= sqrt(ABS(jac));
	ph[0]=0;

	/****** all but the last ray **************/
	for(iray=1; iray<nrays-1;iray++){
		jac1 = (x[iray]-x[iray+1])*pz[iray];
		diffnum = num[iray+1]-num[iray];
		if(diffnum != 1)
			jac1 /= diffnum;

		jac2 = (x[iray-1]-x[iray])*pz[iray];
		diffnum = num[iray]-num[iray-1];
		if(diffnum != 1)
			jac2 /= diffnum;

		/* detect caustics 
		if(jac2*jac1 < 0 && caustic==0)
			caustic=1;
		else if(jac2*jac1 <0 && caustic==1)
			caustic=0;*/
		
		/* cusps */
		if(jac1*jac1 < 0.001*dg*dg && jac1*jac1 < 0.001*dg*dg)
			jac=jac;
		else if(jac1*jac1 < 0.001*dg*dg)
			jac=jac2;
		else if(jac2*jac2 < 0.001*dg*dg)
			jac=jac1;
		else
			jac=(jac1+jac2)/2.0;

		ax[iray] /= sqrt(ABS(jac));
		az[iray] /= sqrt(ABS(jac));

		if(caustic==1) ph[iray] -= PI/2.0;

	}

	/****** last ray *******/
	jac = (x[nrays-1]-x[nrays-2])*pz[nrays-1];
	diffnum = num[1]-num[0];
	if(diffnum !=1) jac=jac/diffnum;


	ax[nrays-1] /= sqrt(ABS(jac));
	az[nrays-1] /= sqrt(ABS(jac));
}


void interpara(int na, float *a, float *x, int ng, float dg, float fg,
		float tol, int *im, float **ag)
{
	int iti,itf,it,ix,itmin,itmax,temp,sub,ixl;
	float frac,x1,x2,odg;
	float a1=0.0,a2=0.0,b1=0.0,b2=0.0,c1=0.0,c2=0.0,t1,t2,xl,xl2;
	float x0=0.0,x3,u0=0.0,u1,u2,u3,dxx,dxx3,eps,epsx;
	double den3,num1,den2,den1,num2,num3;


	itmin = 0;
	itmax = 0;
	odg   = 1/dg;
	eps   = 0.001;
	epsx  = 0.01;

	for(it=0; it<ng; ++it)
		im[it]=0;

	x1  = x[0]-fg;
	iti = x1*odg;
	u1 = a[0];
	ixl=-1;

	for(ix=1; ix<na; ++ix){
		x2  = x[ix]-fg;
		itf = x2*odg;
		u2 = a[ix];
		sub = 1;
		x3=x[ix+1]-fg;
		u3=a[ix+1];

		if(itf<iti){
			temp = itf;
			itf  = iti;
			iti  = temp;
			sub  = 0;
		}

		if(ABS(x2-x1)>tol){
			for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
				ag[it][im[it]] = 0.0;
				im[it] += 1;
			};

		} else{
			if(ix==1 || ix==na-1){
				dxx=x2-x1;
				if(ABS(dxx)<eps){
					for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
						itmin = MIN(itmin,it);
						itmax = MAX(itmax,it);
						ag[it][im[it]]= 0.5*(u1+u2);
						im[it] += 1;
					}
				} else{
					
					for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
						itmin = MIN(itmin,it);
						itmax = MAX(itmax,it);
						frac  = (it*dg-x1)/dxx;
						ag[it][im[it]]= (1.0-frac)*u1+frac*u2;
						im[it] += 1;
					}
				}
			} else{
				dxx=x2-x1;
				dxx3=dxx*(x3-x2)*(x1-x0);
				if(ABS(dxx)<eps){
					for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
						itmin = MIN(itmin,it);
						itmax = MAX(itmax,it);
						ag[it][im[it]]= 0.5*(u1+u2);
						im[it] += 1;
					}
				} else if(ABS(dxx3)<epsx){
					
					for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
						itmin = MIN(itmin,it);
						itmax = MAX(itmax,it);
						frac  = (it*dg-x1)/dxx;
						ag[it][im[it]]= (1.0-frac)*u1+frac*u2;
						im[it] += 1;
					}
				} else{
					if(ixl!=ix-1){
						num1=(x1-x0)*(x2-x0);
						den1=1/num1;
						num2=(x1-x0)*(x2-x1);
						den2=1/num2;
						num3=(x2-x1)*(x2-x0);
						den3=1/num3;
						a1=u0*x1*x2*den1-u1*x2*x0*den2+
							u2*x0*x1*den3;
						b1=-u0*(x1+x2)*den1+u1*(x2+x0)*den2-
							u2*(x1+x0)*den3;
						c1=u0*den1-u1*den2+u2*den3;
					}
					num1=(x2-x1)*(x3-x1);
					den1=1/num1;
					num2=(x2-x1)*(x3-x2);
					den2=1/num2;
					num3=(x3-x2)*(x3-x1);
					den3=1/num3;
					a2=u1*x2*x3*den1-u2*x3*x1*den2+
						u3*x1*x2*den3;
					b2=-u1*(x2+x3)*den1+u2*(x3+x1)*den2-
						u3*(x2+x1)*den3;
					c2=u1*den1-u2*den2+u3*den3;
					for(it=MAX(iti+1,0); it<=MIN(itf,ng-1); ++it){
						itmin = MIN(itmin,it);
						itmax = MAX(itmax,it);
						xl=it*dg;
						xl2=xl*xl;
						t1= a1+b1*xl+c1*xl2;
						t2= a2+b2*xl+c2*xl2;
						ag[it][im[it]]=.5*(t1+t2);
						im[it] += 1;
					}
					a1=a2;
					b1=b2;
					c1=c2;
					ixl=ix;
				}
			}
		}
		
		if(sub)
			iti = itf;
		u0=u1;
		x0=x1;
		x1=x2;
		u1=u2;
	}

	for(it=0; it<itmin; ++it){
		ag[it][0] = 0.0;
		im[it] += 1;
	}
	for(it=itmax+1; it<ng; ++it){
		ag[it][0] = 0.0;
		im[it] += 1;
       	}
}
void polar(float px, float pz, float g11, float g13,
	float g33, float *polx, float *polz, int mode )
/*****************************************************************************
compute polarization oriented along slowness. This convention is identical to that used in Aki&Richards, pages 148ff.                     
*******************************************************************************
Input:
px,pz		slowness components
g11,g13,g33 	polarizations squared
mode=0,1,3,4	ray-mode(qP.qSV)

Output:
polx,polz	polarization components

Author:  Andreas Rueger, Colorado School of Mines, 03/14/94
******************************************************************************/
{
	float polarx,polarz;

	if(g11 < FLT_EPSILON){
		polarx=0;
		polarz=sqrt(g33);
	} else if(g33 < FLT_EPSILON) {
		polarz=0;
		polarx=sqrt(g11);
	} else {
		polarx=sqrt(g11)*SGN(g13);
		polarz=sqrt(g33);
        }
	
	if((mode==0 || mode==3) && px*polarx+pz*polarz <0 ){
		polarx=-polarx;
		polarz=-polarz;
	} else if((mode==4 || mode==1) && px*polarx <0){
		polarx=-polarx;
		polarz=-polarz;
	}


	*polx=polarx;
	*polz=polarz;
}
