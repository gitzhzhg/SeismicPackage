/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>
#include "pvm3.h"

#define PARA_MSGTYPE 1
#define VEL_MSGTYPE 2
#define DATA_MSGTYPE 3
#define COM_MSGTYPE 4
#define RESULT_MSGTYPE 5

#define Done 10
#define UnDone -10
#define FinalDone 0
 
/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
"                                                                       ",
"                                                                       ",
"The vfile parameter must be specified, the velocity should be stored in",
"binary format in vfile, its structure should be vfile[iz][ix].         ",
NULL};
/**************** end self doc *******************************************/

int main (int argc, char **argv)
 
{
	int Finish,dip=0;
        int nz;                 /* number of migrated depth samples */
        int nx=0,nxo=0;                 /* number of midpoints  */
        int ik,iz,iw,ix,ix2,ix3,il,ir,ixshot;     /* loop counters        */
        int nxfft,ntfft;        /* fft size             */
        int nk,nw;              /* number of wave numbers */
        int mytid,msgtype,rc,parent_tid;


        float dt=0.004,dz;         /* time sampling interval       */
        float dk,dw;            /* wave number sampling interval */
        float fk,fw;            /* first wave number            */
        float k,w;              /* wave number          */
        float dx;               /* spatial sampling interval    */
        float **cresult;    /* input, output data           */
        float vmin,v1;
        double kz1,kz2;
        float **v,**vp;
        double phase1;
        complex cshift1,cshift2;
        complex **cq,**cp,**cp1,**cq1;  /* complex input,output         */

	/*get my and father pids*/
	mytid=pvm_mytid();
	parent_tid=pvm_parent();

	/*receive global parameters*/
	msgtype=PARA_MSGTYPE;	
	rc=pvm_recv(-1,msgtype);
	rc=pvm_upkint(&nxo,1,1);
	rc=pvm_upkint(&nz,1,1);
	rc=pvm_upkint(&dip,1,1);

	/*allocate space for velocity profile and receive velocity from father*/
        vp=alloc2float(nxo,nz);
        msgtype=VEL_MSGTYPE;
        rc=pvm_recv(-1,msgtype);
        rc=pvm_upkfloat(vp[0],nxo*nz,1);

	/*allocate space for the storage of partial image and zero it out now*/
        cresult = alloc2float(nz,nxo);
	for(ix=0;ix<nxo;ix++)
	for(iz=0;iz<nz;iz++)
	cresult[ix][iz]=0.0;

/*loop over shotgather*/
loop:

	/*receive parameters for each shot gather*/
        msgtype=PARA_MSGTYPE;
        rc=pvm_recv(-1,msgtype);
        rc=pvm_upkint(&Finish,1,1);
        if(Finish==FinalDone)goto end;

 	rc=pvm_upkint(&ntfft,1,1);
        rc=pvm_upkint(&ix2,1,1);
        rc=pvm_upkint(&ix3,1,1);
	rc=pvm_upkint(&ixshot,1,1);
	rc=pvm_upkint(&il,1,1);
	rc=pvm_upkint(&ir,1,1);

	nx=ix3-ix2+1;

	rc=pvm_upkfloat(&dx,1,1);
	rc=pvm_upkfloat(&dz,1,1);
 	rc=pvm_upkfloat(&dw,1,1);
        rc=pvm_upkfloat(&dt,1,1);

	/*allocate space for velocity profile within the aperature*/
	v=alloc2float(nx,nz);

	for(iz=0;iz<nz;iz++)    
	for(ix=0;ix<nx;ix++){
	v[iz][ix]=vp[iz][ix+ix2];  
	}


       /* determine wavenumber sampling (for complex to complex FFT) */
        nxfft = npfa(nx);
        nk = nxfft;
        dk = 2.0*PI/(nxfft*dx);
        fk = -PI/dx;


while(1){
	/*receive parameters and data for processing*/
        msgtype=DATA_MSGTYPE;
        rc=pvm_recv(-1,msgtype);
        rc=pvm_upkint(&Finish,1,1);

        if(Finish==Done){free2float(v); goto loop;}

        rc=pvm_upkfloat(&fw,1,1);
        rc=pvm_upkint(&nw,1,1);
        cp = alloc2complex(nx,nw);
	cp1 = alloc2complex(nx,nw);
        rc=pvm_upkfloat((float *)cp[0],nx*nw*2,1);
        rc=pvm_upkfloat((float *)cp1[0],nx*nw*2,1);



        cq = alloc2complex(nk,nw);
	cq1=alloc2complex(nk,nw);

        /* loops over depth */
        for(iz=0;iz<nz;++iz){

        /*the imaging condition*/
/*        for(ix=0;ix<nx;ix++){
        for(iw=0,w=fw;iw<nw;w+=dw,iw++){
                complex tmp;
                float ratio=10.0;
        
                if(fabs(ix+ix2-ixshot)*dx<ratio*iz*dz)
                tmp=cmul(cp[iw][ix],cp1[iw][ix]);
                else tmp=cmplx(0.0,0.0);
                cresult[ix+ix2][iz]+=tmp.r/ntfft;
        }
        }
*/

        
/* anothe imaging condition, slightly different from the above one, but not quite
slow*/  


        for(iw=0,w=fw;iw<nw;w+=dw,iw++){
                float kk=0.0;
                complex tmp; 
                float ratio=1.5;
                if(dip<80)ratio=1.5;
                else ratio=1.5;  
        
                for(ix=0;ix<nx;ix++){
                kk+=(pow(cp1[iw][ix].i,2.0)+pow(cp1[iw][ix].r,2.0))/nx;
                }
                
                for(ix=0;ix<nx;ix++){
                tmp=cmul(cp[iw][ix],cp1[iw][ix]);
                
                if(fabs(ix+ix2-ixshot)*dx<ratio*iz*dz)
        
                tmp=crmul(tmp,1.0/(kk+1.0e-10));

                else tmp=cmplx(0.0,0.0);
  
                cresult[ix+ix2][iz]+=tmp.r/ntfft;
                
                }
                }


                vmin=0;

                for(ix=il-ix2;ix<=ir-ix2;ix++){
                vmin+=1.0/v[iz][ix]/(ir-il+1);
                }
                vmin=1.0/vmin;
                
                for (ik=0;ik<nx;++ik)
                        for (iw=0; iw<nw; ++iw)
                               {
				cq[iw][ik] = ik%2 ? cneg(cp[iw][ik]) : cp[iw][ik];
                        	cq1[iw][ik] = ik%2 ? cneg(cp1[iw][ik]) : cp1[iw][ik];
			}

                for (ik=nx; ik<nk; ++ik)
                        for (iw=0; iw<nw; ++iw)
                        {
			cq[iw][ik] = cmplx(0.0,0.0);
			cq1[iw][ik] = cmplx(0.0,0.0);
			}

                /* FFT to W-K domain */
                
                pfa2cc(-1,1,nk,nw,cq[0]);
		pfa2cc(-1,1,nk,nw,cq1[0]);
                
		v1=vmin;
                                
                for(ik=0,k=fk;ik<nk;++ik,k+=dk)
                        for(iw=0,w=fw;iw<nw;++iw,w+=dw){
                                if(w==0.0)w=1.0e-10/dt;
                                kz1=1.0-pow(v1*k/w,2.0);
                                if(kz1>0.0){
                                phase1 = -w*sqrt(kz1)*dz/v1;
                                cshift1 = cmplx(cos(phase1), sin(phase1));
                                cq[iw][ik] = cmul(cq[iw][ik],cshift1);
				cq1[iw][ik] = cmul(cq1[iw][ik],cshift1);
                                }
                                else{
				cq[iw][ik] = cq1[iw][ik] = cmplx(0.0,0.0);  
                              }
                        }
                

                pfa2cc(1,1,nk,nw,cq[0]);
                pfa2cc(1,1,nk,nw,cq1[0]);

                for(ix=0;ix<nx;++ix)
                        for(iw=0,w=fw;iw<nw;w+=dw,++iw){
/*
	float a=0.015,g=1.0;
	int i,I=20;

	if(ix<=I)g=exp(-a*(I-ix)*(I-ix));
	if(ix>=nx-I)g=exp(-a*(-nx+I+ix)*(-nx+I+ix));
*/
                                cq[iw][ix] = crmul( cq[iw][ix],1.0/nxfft);
                                cq[iw][ix] =ix%2 ? cneg(cq[iw][ix]) : cq[iw][ix];
                                kz2=(1.0/v1-1.0/v[iz][ix])*w*dz;
                                cshift2=cmplx(cos(kz2),sin(kz2));
                                cp[iw][ix]=cmul(cq[iw][ix],cshift2);

				cq1[iw][ix] = crmul( cq1[iw][ix],1.0/nxfft);
                                cq1[iw][ix] =ix%2 ? cneg(cq1[iw][ix]) : cq1[iw][ix];
                                cp1[iw][ix]=cmul(cq1[iw][ix],cshift2);

                        }       
                                
                        }


pvm_initsend(PvmDataDefault);
pvm_pkint(&mytid,1,1);
msgtype=COM_MSGTYPE;
pvm_send(parent_tid,msgtype);
         
free2complex(cp);
free2complex(cp1);
free2complex(cq);
free2complex(cq1);

}

end:


pvm_initsend(PvmDataDefault);
pvm_pkfloat(cresult[0],nxo*nz,1);
msgtype=RESULT_MSGTYPE;
pvm_send(parent_tid,msgtype);
msgtype=COM_MSGTYPE;
pvm_recv(-1,msgtype);
pvm_exit();
exit(0);
}
