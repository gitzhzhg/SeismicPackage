/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIGPSPI: $Revision: 1.12 $ ; $Date: 2015/08/07 22:19:43 $       */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>
/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" SUMIGPSPI - Gazdag's phase-shift plus interpolation depth migration   ",
"            for zero-offset data, which can handle the lateral         ",
"            velocity variation.                                        ",
"                                                                       ",
" sumigpspi <infile >outfile vfile= [optional parameters]               ",
"                                                                       ", 
" Required Parameters:							",
" nz=		number of depth sapmles					",
" dz=		depth sampling interval					",
" vfile=	name of file containing velocities			",
"		(Please see Notes below concerning the format of vfile)	",
"									",
" Optional Parameters:                                                  ",
" dt=from header(dt) or .004    time sampling interval                  ",
" dx=from header(d2) or 1.0     midpoint sampling interval              ",
"                                                                       ",
" tmpdir=        if non-empty, use the value as a directory path        ",
"                prefix for storing temporary files; else if the        ",
"                the CWP_TMPDIR environment variable is set use         ",
"                its value for the path; else use tmpfile()             ",
"                                                                       ",
" Notes:								",
" The input velocity file 'vfile' consists of C-style binary floats.	",
" The structure of this file is vfile[iz][ix]. Note that this means that",
" the x-direction is the fastest direction instead of z-direction! Such a",
" structure is more convenient for the downward continuation type	",
" migration algorithm than using z as fastest dimension as in other SU	",
" programs. (In C  v[iz][ix] denotes a v(x,z) array, whereas v[ix][iz]	",
" denotes a v(z,x) array, the opposite of what Matlab and Fortran	",
" programmers may expect.)						",
"									",
" Because most of the tools in the SU package (such as  unif2, unisam2,	",
" and makevel) produce output with the structure vfile[ix][iz], you will",
" need to transpose the velocity files created by these programs. You may",
" use the SU program 'transp' in SU to transpose such files into the	",
" required vfile[iz][ix] structure.					",
"									",
"									",
NULL};

/*
 * Credits: CWP, Baoniu Han, April 20th, 1998
 *
 * Trace header fields accessed: ns, dt, delrt, d2
 * Trace header fields modified: ns, dt, delrt
 */
/**************** end self doc *******************************************/


/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ]; /* filename for the file of traces      */ 
char headerfile[BUFSIZ];/* filename for the file of headers     */
FILE *tracefp;          /* fp for trace storage file            */
FILE *headerfp;         /* fp for header storage file           */
static void closefiles(void);
segy tr;


int main (int argc, char **argv)
{
	int L=10, Bz=0;
	float c[41], *V,P[40],Sz=0,Y[41];
	int nt;			/* number of time samples */
	int nz;			/* number of migrated depth samples */
	int nx;			/* number of midpoints 	*/
	int ik,iz,iw,ix,it;	/* loop counters 	*/
	int nxfft,ntfft;	/* fft size		*/
	int nk,nw;		/* number of wave numbers */	


	float dt,dz,tz=0.0;	/* sampling interval 	*/
	float dk,dw;		/* wave number sampling interval */
	float fk,fw;		/* first wave number 		*/
	float k,w;		/* time,wave number		*/
	float dx;		/* spatial sampling interval	*/
	float **p,**cresult;	/* input, output data		*/
	float vmin=0.0,vmax=0.0,v1,v2,lvmin,lvmax;
	double kz1;
	float **v;
	double phase1;
	complex cshift1;
	complex **cp,**cq,**cq1,***cq2;	/* complex input,output		*/
	double a,a1,a2,theta,theta1,theta2;
	char *vfile="";		/* name of file containing velocities */
	FILE *vfp;
	int verbose=1;		/* flag for echoing info		*/
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* cwp_true for user-given path		*/

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;

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


	/* get optional parameters */
	if (!getparint("nz",&nz)) err("nz must be specified"); 
	if (!getparfloat("dz",&dz)) err("dz must be specified");
	if (!getparstring("vfile", &vfile)) err("vfile must be specified");
	if (!getparint("verbose", &verbose)) verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);
        checkpars();

	/* store traces and headers in tempfiles while getting a count */
	if (STREQ(tmpdir,"")) {
		tracefp = etmpfile();
		headerfp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} 
	else{ 	
		/* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		strcpy(headerfile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+");
		headerfp = efopen(headerfile, "w+");
      		istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}

	nx = 0;
	do {
		++nx;
		efwrite(&tr,HDRBYTES,1,headerfp);
		efwrite(tr.data, FSIZE, nt, tracefp);
	} while (gettr(&tr));

	erewind(tracefp);
	erewind(headerfp);
	
	/* determine frequency sampling (for real to complex FFT) */
	ntfft = npfar(nt);
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	fw = 0;
	
	/* determine wavenumber sampling (for complex to complex FFT) */
	nxfft = npfa(nx);
	nk = nxfft;
	dk = 2.0*PI/(nxfft*dx);
	fk = -PI/dx;


	/* allocate space */
	p = alloc2float(ntfft,nx);
	cp = alloc2complex(nw,nx);
	cq = alloc2complex(nw,nk);
	cq1 = alloc2complex(nw,nk);
	cresult = alloc2float(nz,nx);
	v=alloc2float(nx,nz);

	/* load traces into the zero-offset array and close tmpfile */
	for(ix=0;ix<nx;ix++)
	efread(p[ix], FSIZE, nt, tracefp);
	efclose(tracefp);

	/*load velicoty file*/
	vfp=efopen(vfile,"r");	
	efread(v[0],FSIZE,nz*nx,vfp);
	efclose(vfp);			

	vmax=v[0][0];vmin=v[0][0];
        
        for(iz=0;iz<nz;++iz)
        for(ix=0;ix<nx;ix++) 
        {
         if(v[iz][ix]>=vmax) vmax=v[iz][ix];
         if(v[iz][ix]<=vmin) vmin=v[iz][ix];
        }
	


	/* pad with zeros and Fourier transform t to w */
       	for (ix=0; ix<nx; ix++)
		for (it=nt; it<ntfft; it++)
			p[ix][it] = 0.0;

	pfa2rc(1,1,ntfft,nx,p[0],cp[0]);



	/*loops over depth*/
	for(iz=0;iz<nz;++iz,tz+=dz){	

	for(ix=0;ix<nx;ix++){
	cresult[ix][iz] =0.0;
	for(iw=0;iw<nw;iw++)
	cresult[ix][iz]+=cp[ix][iw].r/ntfft;
	}

	lvmax=v[iz][0];
	lvmin=v[iz][0];

        for(ix=0;ix<nx;ix++){ 
        if(v[iz][ix]>=lvmax) lvmax=v[iz][ix];
        if(v[iz][ix]<=lvmin) lvmin=v[iz][ix];
	}

	for (ik=0; ik<nx; ++ik) 
	for (iw=0,w=fw; iw<nw;w+=dw, ++iw){
	cp[ik][iw]=cmul(cp[ik][iw],cwp_cexp(cmplx(0.0,-w*dz*2.0/v[iz][ik])));
	cq[ik][iw] = ik%2 ? cneg(cp[ik][iw]) : cp[ik][iw];
	}

	for (ik=nx; ik<nk; ++ik)
	for (iw=0; iw<nw; ++iw)
	cq[ik][iw] = cmplx(0.0,0.0);


	/* FFT to W-K domain */
	pfa2cc(-1,2,nw,nk,cq[0]);

	/* The second time phase shift */ 
	v1=lvmin*0.5;
	v2=lvmax*0.5;

	if((v2-v1)/v1<0.01){

        for(ik=0,k=fk;ik<nk;++ik,k+=dk)
		for(iw=0,w=fw;iw<nw;++iw,w+=dw){       

		if(w==0.0)w=1.0e-10/dt;
		kz1=1.0-pow(v1*k/w,2.0);

	        if(kz1>0){       
		phase1 = -w*sqrt(kz1)*dz/v1+w*dz/v1;
		cshift1 = cmplx(cos(phase1), sin(phase1));
		cq1[ik][iw] = cmul(cq[ik][iw],cshift1);
		}       
		else{       
                phase1 = -w*sqrt(-kz1)*dz/v1;
                cshift1=cwp_cexp(cmplx(phase1,w*dz/v1));
                cq1[ik][iw] =cmul(cq[ik][iw],cshift1);
		}
	}
        
	pfa2cc(1,2,nw,nk,cq1[0]);   

        for(ix=0;ix<nx;++ix)
        	for(iw=0;iw<nw;++iw){       
        	cq1[ix][iw] = crmul( cq1[ix][iw], 1.0/nxfft);
        	cp[ix][iw] =ix%2 ? cneg(cq1[ix][iw]) : cq1[ix][iw];
		}
	}
	else{


        for(ik=0;ik<=L;ik++)
        c[ik]=vmin+ik*1.0*(vmax-vmin)/(L*1.0);

        for(ik=0;ik<L;ik++)
        {
        P[ik]=0.0;
        }



        for(ix=0;ix<nx;ix++){
                for(ik=0;ik<L;ik++){
		if(((v[iz][ix]>=c[ik])&&(v[iz][ix]<c[ik+1]))||((ik==L-1)&&(v[iz][ix]==vmax))){
                P[ik]+=1.0/nx; break;
		}
                }
	}
         

        Sz=0.0;
        for(ik=0;ik<L;ik++)
        {if(P[ik]!=0.00) Sz=Sz-P[ik]*log(P[ik]);
        }
         
        Bz=exp(Sz)+0.5;
        Y[0]=0.0; Y[L]=1.0;

        for(ik=1;ik<L;ik++)
        {Y[ik]=0.0;
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

         
        cq2=ealloc3complex(nw,nk,Bz+1);
        
        for(ix=0;ix<Bz+1;ix++){
                for(iw=0,w=fw;iw<nw;++iw,w+=dw)
                for(ik=0,k=fk;ik<nk;++ik,k+=dk){
		/* float kn=fk+nk*dk,tmpk,bk=1.0,kc; */
        
                if(w==0.0)w=1.0e-10/dt;

                kz1=1.0-pow(V[ix]/2.0*k/w,2.0);
                if(kz1>=0.00){
                        phase1 =-w*sqrt(kz1)*dz*2.0/V[ix]+w*dz*2.0/V[ix];
                        cshift1 = cwp_cexp(cmplx(0.0,phase1));
                        cq2[ix][ik][iw] = cmul(cq[ik][iw],cshift1);
         
                        }
                else{
                phase1 =-w*sqrt(-kz1)*dz*2.0/V[ix];
                cshift1 =cwp_cexp(cmplx(phase1,w*dz*2.0/V[ix]));
                cq2[ix][ik][iw] = cmul(cq[ik][iw],cshift1);

                }

        }
                

        pfa2cc(1,2,nw,nk,cq2[ix][0]);

        for(ik=0;ik<nx;++ik)
                for(iw=0,w=fw;iw<nw;w+=dw,++iw){
		float a=0.015,g=1.0;
		int I=20;

		if(ik<=I)g=exp(-a*(I-ik)*(I-ik));
		if(ik>=nx-I)g=exp(-a*(-nx+I+ik)*(-nx+I+ik));

                cq2[ix][ik][iw] = crmul( cq2[ix][ik][iw], g*1.0/nxfft);
                cq2[ix][ik][iw] =ik%2 ? cneg(cq2[ix][ik][iw]) : cq2[ix][ik][iw];
        }

        }  
         
        for(ix=0;ix<nx;++ix)
        for(ik=0;ik<Bz;++ik){
  
        if(((v[iz][ix]>=V[ik])&&(v[iz][ix]<V[ik+1]))) {


                v1=V[ik];v2=V[ik+1];
  
                for(iw=0,w=fw;iw<nw;w+=dw,++iw){

                a1=cq2[ik][ix][iw].r;a2=cq2[ik+1][ix][iw].r;
                theta1=cq2[ik][ix][iw].i ;theta2=cq2[ik+1][ix][iw].i;

                a= a1*(v2-v[iz][ix])/(v2-v1)+a2*(v[iz][ix]-v1)/(v2-v1);
                theta=theta1*(v2-v[iz][ix])/(v2-v1)+theta2*(v[iz][ix]-v1)/(v2-v1);
        
                cp[ix][iw] =cmplx(a,theta);

        }
  
break;          

        }
}
        free3complex(cq2);
        free1float(V);

}
}
	/* restore header fields and write output */
	for(ix=0; ix<nx; ix++){
	efread(&tr,HDRBYTES,1,headerfp);
        tr.ns = nz ;
        tr.d1 = dz ;
	memcpy( (void *) tr.data, (const void *) cresult[ix],nz*FSIZE);
	puttr(&tr);
	}
	
	/* Clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());	

}




	static void closefiles(void)
{
        efclose(headerfp);
        efclose(tracefp);
        eremove(headerfile);
        eremove(tracefile);
        exit(EXIT_FAILURE);
}

