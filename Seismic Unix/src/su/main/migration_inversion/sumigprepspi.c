 /* Copyright (c) Colorado School of Mines, 2011.*/
 /* All rights reserved.                       */

 /* SUMIGPREPSPI: $Vision: 1.00 $ ; $Date: 2015/08/07 22:19:43 $*/

 #include "su.h"
 #include "segy.h"
 #include "header.h"
 #include <signal.h>


 /*********************** self documentation ******************************/
 char *sdoc[] = {
 "									",
 " SUMIGPREPSPI --- The 2-D PREstack commom-shot Phase-Shift-Plus 	",
 "			interpolation depth MIGration.			",
 "									",
 "   sumigprepspi <indata >outfile [parameters] 			", 
 "									",
 " Required Parameters:						   	",
 "									",
 " nxo=     number of total horizontal output samples			",
 " nxshot=  number of shot gathers to be migrated		   	",
 " nz=      number of depth samples				 	",
 " dx=      horizontal sampling interval			  	",   
 " dz=      depth sampling interval				 	",
 " vfile=   velocity profile, it must be binary format.                 ",
 " 									",
 " Optional Parameters:						   	",
 " fmax=25    the peak frequency of Ricker wavelet used as source wavelet",
 " f1=5,f2=10,f3=40,f4=50     frequencies to build a Hamming window     ",
 " lpad=9999,rpad=9999        number of zero traces padded on both	",
 "                            sides of depth section to determine the	",
 "                            migration aperture, the default values    ",
 "                            are using the full aperture.              ",
 " nflag=0    normalization of cross-correlation:                       ",
 "            0: none, 1: by source wave field                          ",
 " verbose=0  silent, =1 additional runtime information	                ",
 "									",  
 " Notes:								",
 " The input velocity file \'vfile\' consists of C-style binary floats.	",  
 " The structure of this file is vfile[iz][ix]. Note that this means that",
 " the x-direction is the fastest direction instead of z-direction! Such a",
 " structure is more convenient for the downward continuation type	",
 " migration algorithm than using z as fastest dimension as in other SU  ", 
 " programs.						   		",
 "									",
 " Because most of the tools in the SU package (such as  unif2, unisam2, ", 
 " and makevel) produce output with the structure vfile[ix][iz], you will",
 " need to transpose the velocity files created by these programs. You may",
 " use the SU program \'transp\' in SU to transpose such files into the  ",
 " required vfile[iz][ix] structure.					",
 "									",
 " (In C  v[iz][ix] denotes a v(x,z) array, whereas v[ix][iz]  		",
 " denotes a v(z,x) array, the opposite of what Matlab and Fortran	",
 " programmers may expect.)						", 
 "									",
 " Also, sx must be monotonically increasing throughout the dataset, and ",
 " and gx must be monotonically increasing within a shot. You may resort ",
 " your data with \'susort\', accordingly.				",
 "									",
 " The scalco header field is honored so this field must be set correctly.",
 " See selfdocs of \'susort\', \'suchw\'. Also:   sukeyword scalco	",
 "									",
 NULL};

 /*
  * Credits: CWP, Baoniu Han, bhan@dix.mines.edu, April 19th, 1998
  *	  Modified: Chris Stolk, 11 Dec 2005, - changed data input
  *		    to remove erroneous time delay.
  *	  Modified: CWP, John Stockwell 26 Sept 2006 - replaced Han's
  *	  "goto-loop" in two places with "do { }while loops".
  *	  Fixed it so that sx, gx, and scalco are honored.
  *
  *
  * Trace header fields accessed: ns, dt, delrt, d2
  * Trace header fields modified: ns, dt, delrt
  */

 /**************** end self doc *******************************************/

 /* Prototypes for subroutines used internally */
 float *ricker(float Freq,float dt,int *Npoint);
 void get_sx_gx(float *sx, float *gx);

segy tr, tro;

 int
 main (int argc, char **argv)
 {
   int L=10, Bz=0;	/*BIN number for determination of reference velocity.*/
   float c[101];
   float *V,P[100];
   float Sz=0;
   float Y[101];
   double a,a1,a2,theta,theta1,theta2;
   
   
   int nt;		/* number of time samples		*/
   int nz;		/* number of migrated depth samples	*/
   int nx;		/* number of horizontal samples	 */
   
   int nxshot;		/* number of shots to be migrated	*/
   int nxshot_orig;	/* first value of nxshot		*/
   int iz,iw,ix,it,ik;	/* loop counters 			*/
   int igx;		/* integerized gx value			*/
   int ntfft,nxfft;	/* fft size				*/
   int nw,truenw,nk;	/* number of wave number and frequency 	*/	
   
   int dip=45;		/* dip angle				*/
   int nflag;
   
   float sx,gx;		/* x source and geophone location	*/
   float gxmin=0.0,gxmax=0.0;/* x source and geophone location	*/
   float min_sx_gx;     /* min(sx,gx)                           */
   
   float oldgx;		/* old gx position			*/
   float oldgxmin;      /* old gx position			*/
   float oldgxmax;	/* old gx position			*/
   float oldsx=0.0;	/* old sx position			*/
   int oldigx=0;	/* old value of integerized gx value	*/
   int oldisx=0;	/* old value of integerized sx value	*/
   
   int isx=0,nxo;	/* index for source and geophone	*/
   int ix1,ix2,ix3,ixshot,il,ir;/* dummy index			*/
   int lpad,rpad;	/* padding on both sides of the migrated section*/
   
   float *wl=NULL,*wtmp=NULL;
   float fmax;
   float f1,f2,f3,f4;
   int nf1,nf2,nf3,nf4;
   int ntw;
   
   float dt=0.004,dz;	/* time and depth sampling intervals 	*/
   float dw,dk;		/* wavenumber and frequency sampling interval */
   float fw,fk;		/* first wavenumber and frequency	*/
   float w,k;		/* wavenumber and frequency		*/
   float dx;		/* spatial sampling interval		*/
   float **p=NULL;
   float **cresult=NULL;	/* input, output data			*/
   
   float vmin,v1,v2,vmax;
   double kz1;
   double phase1;
   float **v=NULL,**vp=NULL;
   complex cshift1;
   complex *wlsp=NULL;
   complex **cp=NULL;
   complex **cp1=NULL;
   complex **cq=NULL;
   complex **cq1=NULL;
   complex ***cq2=NULL;
   complex ***cq3=NULL;	/* complex input,output			*/
   
   char *vfile="";	/* name of file containing velocities	*/
   FILE *vfp=NULL;	/* ... its file pointer			*/
   
   int verbose;		/* verbose flag				*/

   /* hook up getpar to handle the parameters */
   initargs(argc,argv);
   requestdoc(1);
   
   /* get required parameters */
   MUSTGETPARINT("nz",&nz);
   MUSTGETPARFLOAT("dz",&dz);
   MUSTGETPARSTRING("vfile", &vfile);
   MUSTGETPARINT("nxo",&nxo);
   MUSTGETPARINT("nxshot",&nxshot);
   
   /* get optional parameters */
   if (!getparfloat("fmax",&fmax)) fmax = 25.0;  
   if (!getparfloat("f1",&f1)) f1 = 10.0;
   if (!getparfloat("f2",&f2)) f2 = 20.0;
   if (!getparfloat("f3",&f3)) f3 = 40.0;
   if (!getparfloat("f4",&f4)) f4 = 50.0;
   
   if (!getparint("lpad",&lpad)) lpad=9999;
   if (!getparint("rpad",&rpad)) rpad=9999;
   if (!getparint("dip",&dip)) dip=65;

   if (!getparint("nflag",&nflag)) nflag=0;
   
   if (!getparint("verbose",&verbose))     verbose = 0;
   
   /* allocate space */
   cresult = alloc2float(nz,nxo);
   vp=alloc2float(nxo,nz);
   
   /* load velocity file */
   vfp=efopen(vfile,"r");
   efread(vp[0],FSIZE,nz*nxo,vfp);
   efclose(vfp);
   
   /* zero out cresult array */
   memset((void *) cresult[0], 0, nxo*nz*FSIZE);
   
   /* save value of nxshot */
   nxshot_orig=nxshot;
   
   /* get info from first trace */
   if (!gettr(&tr))  err("can't get first trace");
   nt = tr.ns;
   get_sx_gx(&sx,&gx);
   min_sx_gx = MIN(sx,gx);
   sx = sx - min_sx_gx;
   gx = gx - min_sx_gx;
   
   /* let user give dt and/or dx from command line */
   if (!getparfloat("dt", &dt)) {
     if (tr.dt) { /* is dt field set? */
       dt = ((double) tr.dt)/1000000.0;
     } else { /* dt not set, assume 4 ms */
       dt = 0.004;
       warn("tr.dt not set, assuming dt=0.004");
     }
   }
   if (!getparfloat("dx",&dx)) {
     if (tr.d2) { /* is d2 field set? */
       dx = tr.d2;
     } else {
       dx = 1.0;
       warn("tr.d2 not set, assuming d2=1.0");
     }
   }
   
   checkpars();
   
   do {    /* begin loop over shots */
     
     /* determine frequency sampling interval */
     ntfft = npfar(nt);
     nw = ntfft/2+1;
     dw = 2.0*PI/(ntfft*dt);
     
     /* compute the index of the frequency to be migrated */
     fw=2.0*PI*f1;
     nf1=fw/dw+0.5;
     
     fw=2.0*PI*f2;
     nf2=fw/dw+0.5;
     
     fw=2.0*PI*f3;
     nf3=fw/dw+0.5;
     
     fw=2.0*PI*f4;
     nf4=fw/dw+0.5;  
     
     /* the number of frequency to migrated */
     truenw=nf4-nf1+1;
     fw=0.0+nf1*dw;
     if (verbose)
       warn("nf1=%d nf2=%d nf3=%d nf4=%d nw=%d",nf1,nf2,nf3,nf4,truenw);
     
     /* allocate space */
     wl=alloc1float(ntfft);
     wlsp=alloc1complex(nw);
     
     /* generate the Ricker wavelet */
     wtmp=ricker(fmax,dt,&ntw);
     
     /* zero out wl[] array */
     memset((void *) wl, 0, ntfft*FSIZE);
     
     /* CHANGE BY CHRIS STOLK, Dec. 11, 2005 */
     /* The next two lines are the old code, */
     /* it is erroneous because the peak of  */
     /* the wavelet occurs at positive time  */
     /* instead of time zero. 		*/
     /*
       for(it=0;it<ntw;it++)
       wl[it]=wtmp[it];
     */
     /* New code: we put in the wavelet in a centered fashion */ 
     for(it=0;it<ntw;it++) 
       wl[(it-ntw/2+ntfft) % ntfft]=wtmp[it];
     /* End of new code */
     
     free1float(wtmp);
     
     /* fourier transform in time */
     pfarc(-1,ntfft,wl,wlsp);
     
     /* allocate space */
     p = alloc2float(ntfft,nxo);
     cq = alloc2complex(nw,nxo);
     
     /* zero out p[][] array */
     memset((void *) p[0], 0, ntfft*nxo*FSIZE);
     
     /* initialize a number of items before looping over traces */
     nx = 0;
     igx=0;
     oldsx=sx;
     oldgx=gx;
     oldgxmax=gxmax;
     oldgxmin=gxmin;
     do { /* begin looping over traces within a shot gather */
       
       memcpy( (void *) p[igx], (const void *) tr.data,nt*FSIZE);
       /* get sx and gx */
       get_sx_gx(&sx,&gx);
       sx = (sx - min_sx_gx);
       gx = (gx - min_sx_gx);
       
       igx = gx/dx;
       if (igx==oldigx) 
	 warn("repeated igx!!! check dx or scalco value!!!");
       oldigx = igx;
       
       
       if(gxmin>gx)gxmin=gx;
       if(gxmax<gx)gxmax=gx;
       
       if(verbose)
	 warn(" inside loop:  min_sx_gx %f isx %d igx %d gx %f sx %f",min_sx_gx,isx,igx,gx,sx);
       
       /* sx, gx must increase monotonically */
       if (!(oldsx <= sx) )
	 err("sx field must be monotonically increasing!");
       if (!(oldgx <= gx) )
	 err("gx field must be monotonically increasing!");
       
       ++nx;
     } while(gettr(&tr) && sx==oldsx);
     
     isx=oldsx/dx;
     if (isx==oldisx) 
       warn("repeated isx!!! check dx or scalco value!!!");
     oldisx=isx;
     ixshot=isx;
     if(verbose) {
       warn("sx %f, gx %f , gxmin %f  gxmax %f nx %d",sx,gx,gxmin,gxmax, nx);
       warn("isx %d igx %d ixshot %d" ,isx,igx,ixshot);
     }
     
     /* transform the shot gather from time to frequency domain */
     pfa2rc(1,1,ntfft,nxo,p[0],cq[0]);
     
     /* compute the most left and right index for the migrated */
     /* section*/
     ix1=oldsx/dx;
     ix2=gxmin/dx;
     ix3=gxmax/dx;
     
     if(ix1>=ix3)ix3=ix1;
     if(ix1<=ix2)ix2=ix1;
     il=ix2;
     ir=ix3;
     
     ix2-=lpad;
     ix3+=rpad;
     if(ix2<0)ix2=0;
     if(ix3>nxo-1)ix3=nxo-1;
     
     /* the total traces to be migrated */
     nx=ix3-ix2+1;
     nw=truenw;
     
     /* determine wavenumber sampling (for complex to complex FFT) */
     nxfft = npfa(nx);
     nk = nxfft;
     dk = 2.0*PI/(nxfft*dx);
     fk = -PI/dx;
     
     /* allocate space for velocity profile within the aperature */
     v=alloc2float(nx,nz);   
     
     for(iz=0;iz<nz;iz++)
       for(ix=0;ix<nx;ix++)
	 v[iz][ix]=vp[iz][ix+ix2];
     
     vmax=v[0][0];vmin=v[0][0];
     
     for(iz=0;iz<nz;++iz) {
       for(ix=0;ix<nx;++ix) {
	 if(v[iz][ix]>=vmax) vmax=v[iz][ix];
	 if(v[iz][ix]<=vmin) vmin=v[iz][ix];
       }
     }
     
     /* allocate space */
     cp = alloc2complex(nx,nw);
     cp1 = alloc2complex(nx,nw);
     
     /* transpose the frequency domain data from	*/
     /* data[ix][iw] to data[iw][ix] and apply a	*/
     /* Hamming at the same time			*/
     for (ix=0; ix<nx; ix++) {
       for (iw=0; iw<nw; iw++){
	 float tmpp=0.0,tmppp=0.0;
	 
	 if(iw>=(nf1-nf1)&&iw<=(nf2-nf1)){
	   tmpp=PI/(nf2-nf1);
	   tmppp=tmpp*(iw-nf1)-PI;
	   tmpp=0.54+0.46*cos(tmppp);
	   cp[iw][ix]=crmul(cq[ix+ix2][iw+nf1],tmpp);
	 } else {
	   if(iw>=(nf3-nf1)&&iw<=(nf4-nf1)){ 
	     tmpp=PI/(nf4-nf3);
	     tmppp=tmpp*(iw-nf3);
	     tmpp=0.54+0.46*cos(tmppp);
	     cp[iw][ix]=crmul(cq[ix+ix2][iw+nf1],tmpp);
	   } else {
	     cp[iw][ix]=cq[ix+ix2][iw+nf1];}
	 }
	 cp1[iw][ix]=cmplx(0.0,0.0);
       }
     }
     
     for(iw=0;iw<nw;iw++){
       cp1[iw][ixshot-ix2]=wlsp[iw+nf1];
     }
     
     if(verbose) {
       warn("ixshot %d ix %d ix1 %d ix2 %d ix3 %d",ixshot,ix,ix1,ix2,ix3);
       warn("oldsx %f ",oldsx);
     }
     
     free2float(p);
     free2complex(cq);
     free1float(wl);
     free1complex(wlsp);
     
     cq=alloc2complex(nxfft,nw);
     cq1=alloc2complex(nxfft,nw);
     
     /* loops over depth */
     for(iz=0;iz<nz;++iz){
       float eps, rtmp;

       if (nflag) {
	 /* stabilizing parameter for normalization by source wave field */
         eps = 0.;
	 for(ix=0;ix<nx;ix++){
	   for(iw=0;iw<nw;iw++){
	     rtmp = rcabs(cp1[iw][ix]);
	     eps = eps > rtmp ? eps : rtmp;
	   }
	 }
	 eps *= 10.; /* ad-hoc scaling of stabilizing parameter */
	 eps = eps * eps;
       }

       /* the imaging condition */
       for(ix=0;ix<nx;ix++){
	 for(iw=0,w=fw;iw<nw;w+=dw,iw++){   
	   complex tmp, tmpd;
	   float ratio=10.0;
  
	   if(fabs(ix+ix2-ixshot)*dx<ratio*iz*dz) {
	     tmp=cmul(cp[iw][ix],cp1[iw][ix]);
	     if (nflag) tmpd = cmul(cp1[iw][ix],conjg(cp1[iw][ix]));
	   } else {
	     tmp=cmplx(0.0,0.0);  
	     if (nflag) tmpd = cmplx(1.,0.);
	   }
	   if (nflag) /* normalization by source wave field */
	     cresult[ix+ix2][iz] += tmp.r/(tmpd.r+eps)/ntfft;
	   else /* no normalization  */
	     cresult[ix+ix2][iz] += tmp.r/ntfft;

	 }
       }
       
       for (ik=0; ik<nx; ++ik) {
	 for (iw=0,w=fw; iw<nw;w+=dw, ++iw){
	   cp[iw][ik]=cmul(cp[iw][ik],
			   cwp_cexp(cmplx(0.0,-w*dz/v[iz][ik])));
	   cp1[iw][ik]=cmul(cp1[iw][ik],
			    cwp_cexp(cmplx(0.0,-w*dz/v[iz][ik])));
	   cq[iw][ik] = ik%2 ? cneg(cp[iw][ik]) : cp[iw][ik];
	   cq1[iw][ik] = ik%2 ? cneg(cp1[iw][ik]) : cp1[iw][ik];
	 }
       }
       
       for (ik=nx; ik<nk; ++ik) {
	 for (iw=0; iw<nw; ++iw) {
	   cq[iw][ik] = cmplx(0.0,0.0);
	   cq1[iw][ik] = cmplx(0.0,0.0);
	 }
       }
       
       /* FFT to W-K domain */
       pfa2cc(-1,1,nk,nw,cq[0]);
       pfa2cc(-1,1,nk,nw,cq1[0]);
       
       /* The second time phase shift */
       v1=vmin;
       v2=vmax;

       if((v2-v1)/v1<0.01){

	 for(ik=0,k=fk;ik<nk;++ik,k+=dk) {
	   for(iw=0,w=fw;iw<nw;++iw,w+=dw){
	     
	     if(w==0.0)
	       w=1.0e-10/dt;
	     kz1=1.0-pow(v1*k/w,2.0);
	     
	     if(kz1>=0.0){
	       phase1 = -w*sqrt(kz1)*dz/v1+w*dz/v1;
	       cshift1 = cmplx(cos(phase1), sin(phase1));
	       cq[iw][ik] = cmul(cq[iw][ik],cshift1);
	       cq1[iw][ik] = cmul(cq1[iw][ik],cshift1);
	     } else {
	       phase1 = -w*sqrt(-kz1)*dz/v1;
	       cshift1=cwp_cexp(cmplx(phase1,w*dz/v1));
	       cq[iw][ik] = cmul(cq[iw][ik],cshift1);
	       cq1[iw][ik] = cmul(cq1[iw][ik],cshift1);
	     }
	   }		
	 }
	 pfa2cc(1,1,nk,nw,cq[0]);
	 pfa2cc(1,1,nk,nw,cq1[0]);
	 
	 for(ix=0;ix<nx;++ix){
	   for(iw=0;iw<nw;++iw){
	     cq[iw][ix] = crmul( cq[iw][ix], 1.0/nxfft);
	     cp[iw][ix] =ix%2 ? cneg(cq[iw][ix]) : cq[iw][ix];
	     cq1[iw][ix] = crmul( cq1[iw][ix], 1.0/nxfft);
	     cp1[iw][ix] =ix%2 ? cneg(cq1[iw][ix]) : cq1[iw][ix];
	   }
	 }

       } else {

	 for(ik=0;ik<=L;ik++)
	   c[ik]=vmin+ik*1.0*(vmax-vmin)/((float) L);
	 
	 memset((void *) P, 0, L*FSIZE);

	 for(ix=0;ix<nx;ix++){
	   for(ik=0;ik<L;ik++){
	     if(((v[iz][ix]>=c[ik])
		 &&(v[iz][ix]<c[ik+1]))
		||((ik==L-1)
		   &&(v[iz][ix]==vmax))){
	       P[ik]+=1.0/nx; break;
	     }
	   }
	 }
	 Sz=0.0;
	 for(ik=0;ik<L;ik++) {
	   if(P[ik]!=0.00) Sz=Sz-P[ik]*log(P[ik]);
	 }

	 Bz=exp(Sz)+0.5;
	 Y[0]=0.0;
	 Y[L]=1.0;
	 
	 for(ik=1;ik<L;ik++) {
	   Y[ik]=0.0;
	   for(ix=0;ix<ik;ix++)
	       Y[ik]=Y[ik]+P[ix];
	 }
	 V=alloc1float(Bz+1);
	 V[0]=vmin;

	 for(ix=1;ix<=Bz;ix++){
	   for(ik=0;ik<L;ik++){
	     if((ix*1.0/Bz>Y[ik])&&(ix*1.0/Bz<=Y[ik+1])){	
	       V[ix]=c[ik]+(ix*1.0/Bz-Y[ik])*(c[ik+1]-c[ik])/(Y[ik+1]-Y[ik]);
	       break;
	     }
	   }
	 }
	 
	 V[Bz]=V[Bz]*1.005;  
	 cq2=ealloc3complex(nk,nw,Bz+1);
	 cq3=ealloc3complex(nk,nw,Bz+1);

	 for(ix=0;ix<Bz+1;ix++){
	   for(iw=0,w=fw;iw<nw;++iw,w+=dw)
	     for(ik=0,k=fk;ik<nk;++ik,k+=dk){
	       
	       if(w==0.0)w=1.0e-10/dt;
	       
	       kz1=1.0-pow(V[ix]*k/w,2.0);
	       if(kz1>=0.00){
		 phase1 =-w*sqrt(kz1)*dz/V[ix]+w*dz/V[ix];
		 cshift1 = cwp_cexp(cmplx(0.0,phase1));
		 cq2[ix][iw][ik] = cmul(cq[iw][ik],cshift1);
		 cq3[ix][iw][ik] = cmul(cq1[iw][ik],cshift1);
		 
	       } else {
		 phase1 =-w*sqrt(-kz1)*dz/V[ix];
		 cshift1 =cwp_cexp(cmplx(phase1,w*dz/V[ix]));
		 cq2[ix][iw][ik] = cmul(cq[iw][ik],cshift1);
		 cq3[ix][iw][ik] = cmul(cq1[iw][ik],cshift1);
		 
	       }
	       
	     }
	   
	   pfa2cc(1,1,nk,nw,cq2[ix][0]);
	   pfa2cc(1,1,nk,nw,cq3[ix][0]);

	   for(ik=0;ik<nx;++ik)
	     for(iw=0,w=fw;iw<nw;w+=dw,++iw){
	       float a=0.015,g=1.0;
	       int I=10;
	       
	       if(ik<=I)g=exp(-a*(I-ik)*(I-ik));
	       if(ik>=nx-I)g=exp(-a*(-nx+I+ik)*(-nx+I+ik));
	       g=1.0;
	       
	       cq2[ix][iw][ik] = crmul( cq2[ix][iw][ik], g*1.0/nxfft);
	       cq2[ix][iw][ik] =ik%2 ? cneg(cq2[ix][iw][ik]) : cq2[ix][iw][ik];
	       cq3[ix][iw][ik] = crmul( cq3[ix][iw][ik], g*1.0/nxfft);
	       cq3[ix][iw][ik] =ik%2 ? cneg(cq3[ix][iw][ik]) : cq3[ix][iw][ik];
	     }
	 }
	 
	 for(ix=0;ix<nx;++ix) {
	   for(ik=0;ik<Bz;++ik){
	     
	     if(((v[iz][ix]>=V[ik])&&(v[iz][ix]<V[ik+1]))) {
	       
	       v1=V[ik];v2=V[ik+1];
	       
	       for(iw=0,w=fw;iw<nw;w+=dw,++iw){
		 a1=cq2[ik][iw][ix].r;
		 a2=cq2[ik+1][iw][ix].r;
		 theta1=cq2[ik][iw][ix].i ;theta2=cq2[ik+1][iw][ix].i;  
		 
		 a= a1*(v2-v[iz][ix])/(v2-v1)+a2*(v[iz][ix]-v1)/(v2-v1);
		 theta=theta1*(v2-v[iz][ix])/(v2-v1)+theta2*(v[iz][ix]-v1)/(v2-v1);
		 cp[iw][ix] =cmplx(a,theta);
		 
		 
		 a1=cq3[ik][iw][ix].r;a2=cq3[ik+1][iw][ix].r;
		 theta1=cq3[ik][iw][ix].i ;
		 theta2=cq3[ik+1][iw][ix].i;
		 
		 a= a1*(v2-v[iz][ix])/(v2-v1)+a2*(v[iz][ix]-v1)/(v2-v1);
		 theta=theta1*(v2-v[iz][ix])/(v2-v1)+theta2*(v[iz][ix]-v1)/(v2-v1);
		 cp1[iw][ix] =cmplx(a,theta);
	       }
	       
	       break;
	     }	
	   }
	 }
	 
	 free3complex(cq2);
	 free3complex(cq3);
	 free1float(V);
       }

     }
     
     free2complex(cp);
     free2complex(cp1);
     free2complex(cq);
     free2complex(cq1);
     free2float(v);
     
     --nxshot;
   } while(nxshot);
   
   /* restore header fields and write output */
   for(ix=0; ix<nxo; ix++){
     tro.ns = nz;
     tro.d1 = dz;
     tro.d2 = dx;
     tro.offset = 0;
     tro.trid = 130;
     tro.cdp = tro.tracl = tro.tracr = ix;
     memcpy( (void *) tro.data, (const void *) cresult[ix],nz*FSIZE);
     puttr(&tro);
   }
   
   return(CWP_Exit());	
   
 }

float * ricker(float Freq,float dt,int *Npoint)
{
  int i;	/* they are the dummy counter*/
  float Bpar,t,u,*Amp;
  int Np1,N;
  
  if(Freq==0.0)Freq=30.0;
  if(dt==0.0)dt=0.004;
  Bpar=sqrt(6.0)/(PI*Freq);
  N=ceil(1.35*Bpar/dt);
  Np1=N;
  *Npoint=2*N+1;
  
  Amp=alloc1float(*Npoint);
  
  Amp[Np1]=1.0;
  
  for(i=1;i<=N;i++) {
    t=dt*(float)i;
    u=2.0*sqrt(6.0)*t/Bpar;
    Amp[Np1+i]=Amp[Np1-i]=0.5*(2.0-u*u)*exp(-u*u/4.0);
  }
  
  return Amp;
  
}

void get_sx_gx(float *sx, float *gx)
{ 
  /*****************************************************************************
 get_sx_gx - get sx and gx from headrs
  *****************************************************************************/
  
  float sy;		/* source coordinates */
  float gy;		/* geophone coordinates */

  if (tr.scalco) { /* if tr.scalco is set, apply value */
    if (tr.scalco>0) {
      *sx = (float) tr.sx*tr.scalco;
      *gx = (float) tr.gx*tr.scalco;
      sy = (float) tr.sy*tr.scalco;
      gy = (float) tr.gy*tr.scalco;
    } else { /* if tr.scalco is negative divide */
      *sx = (float) tr.sx/ABS(tr.scalco);
      *gx = (float) tr.gx/ABS(tr.scalco);
      sy = (float) tr.sy/ABS(tr.scalco);
      gy = (float) tr.gy/ABS(tr.scalco);
    }
    
  } else {
    *sx = (float) tr.sx;
    *gx = (float) tr.gx;
    sy = (float) tr.sy;
    gy = (float) tr.gy;
  }
  
  
  /* use pythagorean theorem to remap radial direction */
  /* to x-direction */
  *sx = SGN(*sx-sy)*sqrt((*sx)*(*sx) + sy*sy);
  *gx = SGN(*gx-gy)*sqrt((*gx)*(*gx) + gy*gy);
  
  return;
}
