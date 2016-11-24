/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1998.*/
/* All rights reserved.                       */

/* SUMIGPREFDMASTER: $Vision: 1.00 $ ; $Date: 2011/11/21 16:47:10 $       */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>
#include "pvm3.h"


/* dummy tags for the communication purpose */ 
#define NNTASKS 100
#define PARA_MSGTYPE 1
#define VEL_MSGTYPE 2
#define DATA_MSGTYPE 3
#define COM_MSGTYPE 4
#define RESULT_MSGTYPE 5

#define Done 10   
#define UnDone -10
#define FinalDone 0

/* The function to generate Ricker wavelet as the source function */
float * ricker(float Freq,float dt,int *Npoint);

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
"SUPREMIGFDMASTER --- The master code of the 2-D prestack shot-gather   ",
"			45-90 degree finite-difference migration.       ",
"Usage:									",
"sumigprefdmaster <indata >outfile [parameters]				", 
"                                                                       ",
"Required Parameters:                                                   ",
"                                                                       ",
"nxo=		number of total horizontal output samples		",
"nxshot= 	number of shot gathers to be migrated			",
"nz= 		number of depth sapmles					",
"dx= 		horizontal sampling interval				",
"dz= 		depth sampling interval					",
"vfile= 	velocity profile, it must be binary format.		",
"The structure of such a file is vfile[iz][ix], the x-direction is the  ",
"fastest direction instead of z-direction, such a structure is quite    ",
"convenient for the downward continuation type migration algorithm.     ",
"Since most of the velocity file is in vfile[ix][iz] structure, you can ",
"use 'transp' in SU to transpose them into vfile[iz][ix] structure.     ",
"                                                                       ",
"cpufile= or Ntasks= 	slave tasks to be started			",
"cpufile is a file that contains a list computer names, the purpose of  ",
"which is for the multi-processor computer in the network, you can starts",
"several slave processes on that computer by repeat the name of that    ",
"computer several time in cpufile. If all the computers have a single CPU,",
"you can just specify how many slaves to Ntasks instead of cpufile.     ",
"                                                                       ",
"Optional Parameters:							",
"                                                                       ",
"dip=65 	the maximum dip to migrate, it can be 45,65,79,80,87,89,90",
"The computation cost is the same for 45,65,79, and <80<87<89<90	",
"                                                                       ",
"Fmax=25 	the peak frequency of Ricker wavelet used as source wavelet",
"                                                                       ",
"f1=5,f2=10,f3=40,f4=50  	frequencies to build a Hamming window	",
"                                                                       ",
"lpad=9999,rpad=9999  		number of zero traces padded on both    ",
"sides of depth section to determine the migration aperature, the default",
"values are using the full aperature.					",
NULL};

/*
 * Credits: CWP, Baoniu Han, bhan@dix.mines.edu, April 19th, 1998
 *
 *
 * Trace header fields accessed: ns, dt, delrt, d2
 * Trace header fields modified: ns, dt, delrt
 */


/**************** end self doc *******************************************/

/* Globals variables*/
segy tr;
char child[]="sumigprefdslave";

/* dummy counter used for loop */
int count=0;
int total=0;
static int tshot;

/* structure for timing */ 
time_t t1,t2;

float total_t=0.0;

int main (int argc, char **argv)
  
{
	int nt;                 /* number of time samples */
	int nz;                 /* number of migrated depth samples */
	int nx,nxshot;      /* number of midpoints,shotgathers, the folds in a shot
				gather */

	int flag=1;		/*flag to use ft or meter as the unit*/
	int dip=65;		/*maximum dip angle to migrate*/
	int iz,iw,ix,it,oldsx;     /* loop counters*/
	int ntfft;        /* fft size*/
	int nw;              /* number of wave numbers */
	int mytid,tids[NNTASKS],msgtype,rc,i;/*variable for PVM function*/
	int nw1,task; 	
	int lpad=9999,rpad=9999;	/*zero-traces padded on left and right sides*/
	float f1,f2,f3,f4;	/*frequencies to build the Hamming window*/
	int nf1,nf2,nf3,nf4,nfpeak;	/*the index for above frequencies*/
	int NTASKS=0;		/*number of slave tasks to start*/
	char cpu_name[NNTASKS][80];	/*strings to store the computers' name*/
	int flag_cpu=0;			/*flag to control if using NTASKS variable*/

	float sx,gxmin,gxmax;	/*location of  geophone and receivers*/
	int isx,nxo,ifx=0;	/*index for geophone and receivers*/
	int ix1,ix2,ix3;	/*dummy index*/

	float *wl,*wtmp;	/*pointers for the souce function*/
	float Fmax=25;		/*peak frequency to make the Ricker wavelet*/
	int ntw,truenw;		/*number of frequencies to be migrated*/

        float para;
	float dt=0.004,dz;   	/*time, depth sampling interval*/
	float ft;            	/*first time sample*/
	float dw;         	/*frequency sampling interval*/
	float fw;         	/*first frequency*/
	float dx;            	/*spatial sampling interval*/
	float **p,**cresult,**result_tmp;    /* input, output data*/
	float **v;		/*double pointer direct to velocity structure*/ 
	complex *wlsp,**cp,**cq,**cq1; /*pointers for internal usage*/

	char *vfile="";         /* name of file containing velocities */
	char *cpufile="";	/* name of file containing CPU name */

	FILE *vfp,*cpu_fp;

                        
	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* get optional parameters */
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparint("nz",&nz)) err("nz must be specified");
	if (!getparfloat("dz",&dz)) err("dz must be specified");
	if (!getparstring("vfile", &vfile)) err("vfile must be specified");
	if (!getparint("nxo",&nxo)) err("nxo must be specified");
	if (!getparint("nxshot",&nxshot)) err("nshot must be specified");
	if (!getparfloat("Fmax",&Fmax)) err("Fmax must be specified");
	if (!getparfloat("f1",&f1)) f1 = 10.0;
	if (!getparfloat("f2",&f2)) f2 = 20.0;
	if (!getparfloat("f3",&f3)) f3 = 40.0;
	if (!getparfloat("f4",&f4)) f4 = 50.0;
	if (!getparint("lpad",&lpad)) lpad=9999;
	if (!getparint("rpad",&rpad)) rpad=9999;
	if (!getparint("flag",&flag)) flag=1;
	if (!getparint("dip",&dip)) dip=65;
        if (!getparfloat("para",&para)) para = 0.03;

	if (getparstring("cpufile", &cpufile)){
	cpu_fp=fopen(cpufile,"r");
	NTASKS=0;
	while(!feof(cpu_fp)){
	fscanf(cpu_fp,"%s",cpu_name[NTASKS]);
	NTASKS++;
	}
	NTASKS-=1;
	flag_cpu=1;
	}
	else /*if cpufile not specified, the use NTASKS*/
	if (!getparint("NTASKS",&NTASKS)) err("No CPUfile specified, NTASKS must be specified");

	/*allocate space for the velocity profile*/
	tshot=nxshot;
	v=alloc2float(nxo,nz);
        
	/*load velicoty file*/
	vfp=efopen(vfile,"r");
	efread(v[0],FSIZE,nz*nxo,vfp);
	efclose(vfp);

	/*PVM communication starts here*/
	mytid=pvm_mytid();	/*get my pid*/
	task=NTASKS;
	warn("\n %d",task);
	rc=0;
	/*spawn slave processes here*/
	if(!flag_cpu){
	rc=pvm_spawn(child,NULL,PvmTaskDefault,"",task,tids);
	}
	else{
	for(i=0;i<NTASKS;i++){
	rc+=pvm_spawn(child,NULL,PvmTaskHost,cpu_name[i],1,&tids[i]);
	}
	}
        
	/*show the pid of slaves if*/
	for(i=0;i<NTASKS;i++){
	if(tids[i]<0)warn("\n %d",tids[i]);
	else warn("\nt%x\t",tids[i]);
        }

	/*if not all the slaves start, then quit*/
	if(rc<NTASKS){ warn("error");pvm_exit();exit(1);}
        
	/*broadcast the global parameters nxo,nz,dip to all slaves*/
	pvm_initsend(PvmDataDefault);
	rc=pvm_pkint(&nxo,1,1);
	rc=pvm_pkint(&nz,1,1);
	rc=pvm_pkint(&dip,1,1);
        rc=pvm_pkfloat(&para,1,1);
	msgtype=PARA_MSGTYPE;
	task=NTASKS;
	rc=pvm_mcast(tids,task,msgtype);

	/*broadcast the velocity profile to all slaves*/
        pvm_initsend(PvmDataDefault);
        rc=pvm_pkfloat(v[0],nxo*nz,1);
        msgtype=VEL_MSGTYPE; 
        rc=pvm_mcast(tids,task,msgtype);
	
	/*free the space for velocity profile*/
	free2float(v);


/*loop over shot gathers begin here*/
loop:

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


	sx=tr.sx;
	isx=sx/dx;
	gxmin=gxmax=tr.gx;
	oldsx=sx;

        /* determine frequency sampling interval*/
        ntfft = npfar(nt);
        nw = ntfft/2+1;
        dw = 2.0*PI/(ntfft*dt);

	/*compute the index of the frequency to be migrated*/
	fw=2.0*PI*f1;
	nf1=fw/dw+0.5;

	fw=2.0*PI*f2;
	nf2=fw/dw+0.5;
 
	fw=2.0*PI*f3;
	nf3=fw/dw+0.5;

	fw=2.0*PI*f4;
	nf4=fw/dw+0.5;
	nfpeak=2.0*PI*Fmax/dw+0.5;
	
	/*the number of frequency to migrated*/
	truenw=nf4-nf1+1;
	fw=0.0+nf1*dw;
	warn("nf1=%d nf2=%d nf3=%d nf4=%d nw=%d",nf1,nf2,nf3,nf4,truenw);
	fw=0.0;

        /* allocate space */
        wl=alloc1float(ntfft);
        wlsp=alloc1complex(nw);

	/*generate the Ricker wavelet*/
        wtmp=ricker(Fmax,dt,&ntw);

        for(it=0;it<ntfft;it++)
        wl[it]=0.0;

        for(it=0;it<ntw-12;it++)
        wl[it]=wtmp[it+12];
	free1float(wtmp);

	/*Fourier transform the Ricker wavelet to frequency domain*/
        pfarc(-1,ntfft,wl,wlsp);
        
	/* allocate space */
        p = alloc2float(ntfft,nxo);
        cp = alloc2complex(nw,nxo);

        for (ix=0; ix<nxo; ix++)
                for (it=0; it<ntfft; it++)
                        p[ix][it] = 0.0;
       
	
	/*read in a single shot gather*/
	ix=tr.gx/dx;
	memcpy( (void *) p[ix], (const void *) tr.data,nt*FSIZE);

        nx = 0;

	while(gettr(&tr)){
			int igx;

			if(tr.sx!=oldsx){ fseek(stdin,(long)(-240-nt*4),SEEK_CUR); break;}
			igx=tr.gx/dx;
			memcpy( (void *) p[igx], (const void *) tr.data,nt*FSIZE);  
                
			if(gxmin>tr.gx)gxmin=tr.gx;
			if(gxmax<tr.gx)gxmax=tr.gx;
			nx++;
			oldsx=tr.sx;
			}

	warn("\nnx= %d",nx);
	warn("sx %f , gxmin %f  gxmax %f",sx,gxmin,gxmax);

	/*transform the shot gather from time to frequency domain*/
        pfa2rc(1,1,ntfft,nxo,p[0],cp[0]);

	/*compute the most left and right index for the migrated section*/ 
	ix1=sx/dx;
	ix2=gxmin/dx;
	ix3=gxmax/dx;
        
	if(ix1>=ix3)ix3=ix1;
	if(ix1<=ix2)ix2=ix1;

	ix2-=lpad;
	ix3+=rpad;
	if(ix2<0)ix2=0;
	if(ix3>nxo-1)ix3=nxo-1;

	/*the total traces to be migrated*/
	nx=ix3-ix2+1;

	/*allocate space*/
        cq = alloc2complex(nx,nw);
	cq1 = alloc2complex(nx,nw);


	/*transpose the frequency domain data from data[ix][iw] to data[iw][ix] and
	apply a Hamming at the same time*/

	for (ix=0; ix<nx; ix++)
	for (iw=0; iw<nw; iw++){	

	float tmpp=0.0,tmppp=0.0;
	
	if(iw<nf1||iw>nf4)
	cq[iw][ix]=cmplx(0.0,0.0);
	else{
		if(iw>=nf1&&iw<=nf2){tmpp=PI/(nf2-nf1);tmppp=tmpp*(iw-nf1)-PI;tmpp=0.54+0.46*cos(tmppp);
		cq[iw][ix]=crmul(cp[ix+ix2][iw],tmpp);}
		else{
			if(iw>=nf3&&iw<=nf4){tmpp=PI/(nf4-nf3);tmppp=tmpp*(iw-nf3);tmpp=0.54+0.46*cos(tmppp);
			cq[iw][ix]=crmul(cp[ix+ix2][iw],tmpp);}
			else
			{cq[iw][ix]=cp[ix+ix2][iw];}
		}

	}
	cq[iw][ix]=cp[ix+ix2][iw];

/*
if(iw>=nf1&&iw<=nfpeak)tmpp=0.5*(1.0+cos(PI*(nfpeak-iw)/(nfpeak-nf1)));
if(iw>=nfpeak&&iw<=nf4)tmpp=0.5*(1.0+cos(PI*(i-nfpeak)/(nf4-nfpeak)));
cq[iw][ix]=crmul(cp[ix+ix2][iw],tmpp);*/
	cq1[iw][ix]=cmplx(0.0,0.0);
	}


	ix=sx/dx-ifx;
	warn("ix %d",ix);

	for(iw=0;iw<nw;iw++){
	cq1[iw][ix-ix2]=wlsp[iw];
	}


	free2float(p);
	free2complex(cp);
	free1float(wl);
	free1complex(wlsp);

	/*if the horizontal spacing interval is in feet, convert it to meter*/ 
	if(!flag)
	dx*=0.3048;

	/*start of the timing function*/
	time(&t1);

	/* send local parameters to all slaves*/
	pvm_initsend(PvmDataDefault);

	ix=15;
	rc=pvm_pkint(&ix,1,1);

	rc=pvm_pkint(&ntfft,1,1);
        rc=pvm_pkint(&ix2,1,1);
        rc=pvm_pkint(&ix3,1,1);
	rc=pvm_pkint(&isx,1,1);
        rc=pvm_pkfloat(&dx,1,1);
        rc=pvm_pkfloat(&dz,1,1);
        rc=pvm_pkfloat(&dw,1,1);
	rc=pvm_pkfloat(&dt,1,1);
	msgtype=PARA_MSGTYPE;

	task=NTASKS;
	rc=pvm_mcast(tids,task,msgtype);

	
	/* send all the frequency to slaves*/
	count=NTASKS*5; /*count is the number of frequency components in a shot
			gather*/ 
        
	nw=truenw;        
	nw1=nw/(count);
	if(nw1==0)nw1=1;
	total=count=ceil(nw*1.0/nw1);

	/* if it is the first shot gather, send equal data to all the slaves, then for
	the following shot gathers, only send data when slave requests*/

	if(nxshot==tshot){

	for(i=0;i<NTASKS;i++){ 
	float *tmpp;
	float fw1;
	int nww,byte,nwww;
			
        pvm_initsend(PvmDataDefault);
	nww=nf1+i*nw1;fw1=fw+nww*dw;
	nwww=nw1;
        byte=UnDone;

        rc=pvm_pkint(&byte,1,1);
        rc=pvm_pkfloat(&fw1,1,1);
        rc=pvm_pkint(&nwww,1,1);   
	rc=pvm_pkfloat((float *)cq[nww],nx*nwww*2,1);
        rc=pvm_pkfloat((float *)cq1[nww],nx*nwww*2,1);
	msgtype=DATA_MSGTYPE;
	pvm_send(tids[i],msgtype);
	}

	count-=NTASKS;

	}


	while(count){

	int tid0,bufid;
	float *tmpp;
	float fw1;
	int nww,byte,nwww;  
	int i;  
	i=total-count;

        
	msgtype=COM_MSGTYPE;
	bufid=pvm_recv(-1,msgtype);
	rc=pvm_upkint(&tid0,1,1);
	pvm_freebuf(bufid);
        
        pvm_initsend(PvmDataDefault);
        nww=nf1+i*nw1;fw1=fw+nww*dw;
        if(i==total-1)nwww=nw-nw1*i;
        else nwww=nw1;

	byte=UnDone;
        rc=pvm_pkint(&byte,1,1);
        rc=pvm_pkfloat(&fw1,1,1);
        rc=pvm_pkint(&nwww,1,1);
        rc=pvm_pkfloat((float *)cq[nww],nx*nwww*2,1);
        rc=pvm_pkfloat((float *)cq1[nww],nx*nwww*2,1);
        msgtype=DATA_MSGTYPE;
        pvm_send(tid0,msgtype);

        count--;
	}

	ix=Done;
        
        pvm_initsend(PvmDataDefault);
        rc=pvm_pkint(&ix,1,1);

        msgtype=DATA_MSGTYPE;
        pvm_mcast(tids,task,msgtype);


	free2complex(cq);
	free2complex(cq1);

	time(&t2);

	warn("\n %d shot been finished in %f seconds, Ntask=%d",nxshot,difftime(t2,t1),NTASKS);

	nxshot--;                       

	if(nxshot)goto loop;
	

	/*when all the shot gathers done, send signal to all slaves to request the
								partial imaging*/
	ix=FinalDone;
        pvm_initsend(PvmDataDefault);
        rc=pvm_pkint(&ix,1,1);
        msgtype=PARA_MSGTYPE;
        pvm_mcast(tids,task,msgtype);
        
	/*allocate space for the final image*/
        cresult = alloc2float(nz,nxo);
	for(ix=0;ix<nxo;ix++)
        for(iz=0;iz<nz;iz++)
        { cresult[ix][iz]=0.0;
	}

	result_tmp= alloc2float(nz,nxo);
	
	/*receive partial image from all the slaves*/
	msgtype=RESULT_MSGTYPE;
	i=0;

	while(i<NTASKS){
	int bufid;
	bufid=pvm_recv(-1,msgtype);
	rc=pvm_upkfloat(result_tmp[0],nxo*nz,1);
	pvm_freebuf(bufid);
	for(ix=0;ix<nxo;ix++)
	for(iz=0;iz<nz;iz++)
	{
	cresult[ix][iz]+=result_tmp[ix][iz];	
	}
	i=i+1;
	warn("\n i=%d been received",i);
	}

	/*send signal to all slaves to kill themselves*/
	pvm_initsend(PvmDataDefault);
	pvm_mcast(tids,task,COM_MSGTYPE);

	/*output the final image*/
        for(ix=0; ix<nxo; ix++){
                tr.ns = nz ;
                tr.dt = dz*1000000.0 ;
		tr.d2 = dx;
		tr.offset = 0;
		tr.cdp = tr.tracl = ix;
                memcpy( (void *) tr.data, (const void *) cresult[ix],nz*FSIZE);
                puttr(&tr);
        }



        pvm_exit();            
        return EXIT_SUCCESS;
                                
}                               
                                 

float * ricker(float Freq,float dt,int *Npoint)

/*
The function to make a Ricker wavelet.

input:
Freq --> the peak frequency for the Ricker wavelet;
dt --> time sampling interval;

output: 
Npoint --> length of the wavelet in points;
a pointer containing the wavelet itself;
*/

{
	int i,j,k;/* they are the dummy counter*/
	float Alobe,Bpar,Coef,t,u,u2,*Amp;
	int Np1,N;
  
	if(Freq==0.0)Freq=30.0;
	if(dt==0.0)dt=0.004;
	Bpar=sqrt(6.0)/(PI*Freq);
	N=ceil(1.35*Bpar/dt);
	Np1=N;
	*Npoint=2*N+1;
                
	Amp=alloc1float(*Npoint);

	Amp[Np1]=1.0;

	for(i=1;i<=N;i++){
		t=dt*(float)i;
		u=2.0*sqrt(6.0)*t/Bpar;
		Amp[Np1+i]=Amp[Np1-i]=0.5*(2.0-u*u)*exp(-u*u/4.0);
	}

	return Amp;
 
}
