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
        int L=6, Bz=0,dip=0;
        float c[101], *V,P[100],Sz=0,Y[101];
	double a,a1,a2,theta,theta1,theta2;

	int Finish;
	int nt;                 /* number of time samples */
        int nz;                 /* number of migrated depth samples */
        int nx,nxo;                 /* number of midpoints  */
        int ik,iz,iw,ix,it,ix2,ix3,ixshot;     /* loop counters        */
        int nxfft,ntfft;        /* fft size             */
        int nk,nw;              /* number of wave numbers */
        int mytid,msgtype,rc,i,parent_tid;


        float dt=0.004,dz,tz;         /* time sampling interval       */
        float ft,fz;            /* first time sample            */
        float dk,dw;            /* wave number sampling interval */
        float fk,fw;            /* first wave number            */
        float k,w;              /* wave number          */
        float dx;               /* spatial sampling interval    */
        float **cresult;    /* input, output data           */
        float vmin,v1,v2,vmax,lvmin,lvmax;
        double kz1,kz2;
        float **v,**vp;
        double phase1;
        complex cshift1,cshift2;
        complex **cq,**cp,**cp1,**cq1,***cq2,***cq3;  /* complex input,output         */

        int verbose=1;          /* flag for echoing info                */
        v=NULL;
	mytid=pvm_mytid();
	parent_tid=pvm_parent();

	msgtype=PARA_MSGTYPE;	

	rc=pvm_recv(-1,msgtype);
	rc=pvm_upkint(&nxo,1,1);
	rc=pvm_upkint(&nz,1,1);
	rc=pvm_upkint(&dip,1,1);

        vp=alloc2float(nxo,nz);
        msgtype=VEL_MSGTYPE;
        rc=pvm_recv(-1,msgtype);
        rc=pvm_upkfloat(vp[0],nxo*nz,1);

        cresult = alloc2float(nz,nxo);
	for(ix=0;ix<nxo;ix++)
	for(iz=0;iz<nz;iz++)
	cresult[ix][iz]=0.0;



loop:

        msgtype=PARA_MSGTYPE;
        rc=pvm_recv(-1,msgtype);

        rc=pvm_upkint(&Finish,1,1);
        if(Finish==FinalDone)goto end;


 	rc=pvm_upkint(&ntfft,1,1);
        rc=pvm_upkint(&ix2,1,1);
        rc=pvm_upkint(&ix3,1,1);
	rc=pvm_upkint(&ixshot,1,1);
	nx=ix3-ix2+1;

	rc=pvm_upkfloat(&dx,1,1);
	rc=pvm_upkfloat(&dz,1,1);
 	rc=pvm_upkfloat(&dw,1,1);
        rc=pvm_upkfloat(&dt,1,1);

        /*allocate space for velocity profile within the aperature*/
        if(v!=NULL){free2float(v);v=NULL;}
        v=alloc2float(nx,nz);

        for(iz=0;iz<nz;iz++)    
        for(ix=0;ix<nx;ix++){
        v[iz][ix]=vp[iz][ix+ix2];  
        }

        vmax=v[0][0];vmin=v[0][0];

        for(iz=0;iz<nz;++iz)
        for(ix=0;ix<nx;ix++)
        {
         if(v[iz][ix]>=vmax) vmax=v[iz][ix];
         if(v[iz][ix]<=vmin) vmin=v[iz][ix];
        }


       /* determine wavenumber sampling (for complex to complex FFT) */
        nxfft = npfa(nx);
        nk = nxfft;
        dk = 2.0*PI/(nxfft*dx);
        fk = -PI/dx;


while(1){

        msgtype=DATA_MSGTYPE;
        rc=pvm_recv(-1,msgtype);
        rc=pvm_upkint(&Finish,1,1);

        if(Finish==Done)goto loop;

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
                
                if(fabs(ix-ixshot+ix2)*dx<ratio*iz*dz)
        
                tmp=crmul(tmp,1.0/(kk+1.0e-10));

                else tmp=cmplx(0.0,0.0);
  
                cresult[ix+ix2][iz]+=tmp.r/ntfft;
                
                }
                }



        for (ik=0; ik<nx; ++ik)
        for (iw=0,w=fw; iw<nw;w+=dw, ++iw){
		cp[iw][ik]=cmul(cp[iw][ik],cexp(cmplx(0.0,-w*dz/v[iz][ik])));
		cp1[iw][ik]=cmul(cp1[iw][ik],cexp(cmplx(0.0,-w*dz/v[iz][ik])));
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

	lvmin=lvmax=v[iz][0];
	for(ix=0;ix<nx;ix++){
		if(v[iz][ix]<=lvmin)lvmin=v[iz][ix];	
		if(v[iz][ix]>=lvmax)lvmax=v[iz][ix];	
	}



        /* The second time phase shift */
        v1=lvmin;  
        v2=lvmax;  
        
        if((v2-v1)/v1<0.01){
         
        for(ik=0,k=fk;ik<nk;++ik,k+=dk)
                for(iw=0,w=fw;iw<nw;++iw,w+=dw){
                
                if(w==0.0)w=1.0e-10/dt;

                kz1=1.0-pow(v1*k/w,2.0);
                 
                if(kz1>=0.0){
                phase1 = -w*sqrt(kz1)*dz/v1+w*dz/v1;
                cshift1 = cmplx(cos(phase1), sin(phase1));
                cq[iw][ik] = cmul(cq[iw][ik],cshift1);
		cq1[iw][ik] = cmul(cq1[iw][ik],cshift1);
                }
                else{   
                phase1 = -w*sqrt(-kz1)*dz/v1;
                cshift1=cexp(cmplx(phase1,w*dz/v1));
                cq[iw][ik] = cmul(cq[iw][ik],cshift1);
		cq1[iw][ik] = cmul(cq1[iw][ik],cshift1);
                }

        }
        pfa2cc(1,1,nk,nw,cq[0]);
	pfa2cc(1,1,nk,nw,cq1[0]);

        for(ix=0;ix<nx;++ix)
                for(iw=0;iw<nw;++iw){  
                cq[iw][ix] = crmul( cq[iw][ix], 1.0/nxfft);
                cp[iw][ix] =ix%2 ? cneg(cq[iw][ix]) : cq[iw][ix];
		cq1[iw][ix] = crmul( cq1[iw][ix], 1.0/nxfft);
                cp1[iw][ix] =ix%2 ? cneg(cq1[iw][ix]) : cq1[iw][ix];

                }
        }
                
        else{    
        
        for(ik=0;ik<=L;ik++)
        c[ik]=lvmin+ik*1.0*(lvmax-lvmin)/(L*1.0);

        for(ik=0;ik<L;ik++)
        {
        P[ik]=0.0;
        }



                for(ix=0;ix<nx;ix++)
{
                for(ik=0;ik<L;ik++)
{
if(((v[iz][ix]>=c[ik])&&(v[iz][ix]<c[ik+1]))||((ik==L-1)&&(v[iz][ix]==lvmax)))
{
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
        for(ix=0;ix<ik;ix++)
        Y[ik]=Y[ik]+P[ix];
        }
 
        V=alloc1float(Bz+1);
 
        V[0]=lvmin;
  
                 
        for(ix=1;ix<=Bz;ix++)
        {
        for(ik=0;ik<L;ik++)
        {if((ix*1.0/Bz>Y[ik])&&(ix*1.0/Bz<=Y[ik+1]))
{        V[ix]=c[ik]+(ix*1.0/Bz-Y[ik])*(c[ik+1]-c[ik])/(Y[ik+1]-Y[ik]);
break;
}
        }
        
        }



        V[Bz]=lvmax*1.005;
  
        cq2=ealloc3complex(nk,nw,Bz+1);
        cq3=ealloc3complex(nk,nw,Bz+1);

        for(ix=0;ix<Bz+1;ix++){
                for(iw=0,w=fw;iw<nw;++iw,w+=dw)
                for(ik=0,k=fk;ik<nk;++ik,k+=dk){
        
                if(w==0.0)w=1.0e-10/dt;
        
                kz1=1.0-pow(V[ix]*k/w,2.0);
                if(kz1>=0.00){
                        phase1 =-w*sqrt(kz1)*dz/V[ix]+w*dz/V[ix];
                        cshift1 = cexp(cmplx(0.0,phase1));
                        cq2[ix][iw][ik] = cmul(cq[iw][ik],cshift1);
			cq3[ix][iw][ik] = cmul(cq1[iw][ik],cshift1);
         
                        }
                else{
                phase1 =-w*sqrt(-kz1)*dz/V[ix];
                cshift1 =cexp(cmplx(phase1,w*dz/V[ix]));
                cq2[ix][iw][ik] = cmul(cq[iw][ik],cshift1);
		cq3[ix][iw][ik] = cmul(cq1[iw][ik],cshift1);

                }

        }
                
        
        pfa2cc(1,1,nk,nw,cq2[ix][0]);
	pfa2cc(1,1,nk,nw,cq3[ix][0]);

        for(ik=0;ik<nx;++ik)
                for(iw=0,w=fw;iw<nw;w+=dw,++iw){
float a=0.015,g=1.0;
int i,I=10;

if(ik<=I)g=exp(-a*(I-ik)*(I-ik));
if(ik>=nx-I)g=exp(-a*(-nx+I+ik)*(-nx+I+ik));

                cq2[ix][iw][ik] = crmul( cq2[ix][iw][ik], g*1.0/nxfft);
                cq2[ix][iw][ik] =ik%2 ? cneg(cq2[ix][iw][ik]) : cq2[ix][iw][ik];
		cq3[ix][iw][ik] = crmul( cq3[ix][iw][ik], g*1.0/nxfft);
		cq3[ix][iw][ik] =ik%2 ? cneg(cq3[ix][iw][ik]) : cq3[ix][iw][ik];

        }
}
        for(ix=0;ix<nx;++ix)
        for(ik=0;ik<Bz;++ik){

        if(((v[iz][ix]>=V[ik])&&(v[iz][ix]<V[ik+1]))) {

  
                v1=V[ik];v2=V[ik+1];
        
                for(iw=0,w=fw;iw<nw;w+=dw,++iw){
  
                a1=cq2[ik][iw][ix].r;a2=cq2[ik+1][iw][ix].r;
                theta1=cq2[ik][iw][ix].i ;theta2=cq2[ik+1][iw][ix].i;

                a= a1*(v2-v[iz][ix])/(v2-v1)+a2*(v[iz][ix]-v1)/(v2-v1);
                theta=theta1*(v2-v[iz][ix])/(v2-v1)+theta2*(v[iz][ix]-v1)/(v2-v1);
                cp[iw][ix] =cmplx(a,theta);


                a1=cq3[ik][iw][ix].r;a2=cq3[ik+1][iw][ix].r;
                theta1=cq3[ik][iw][ix].i ;theta2=cq3[ik+1][iw][ix].i;

                a= a1*(v2-v[iz][ix])/(v2-v1)+a2*(v[iz][ix]-v1)/(v2-v1);
                theta=theta1*(v2-v[iz][ix])/(v2-v1)+theta2*(v[iz][ix]-v1)/(v2-v1);
                cp1[iw][ix] =cmplx(a,theta);
         
  
        }
        
break;          

        }
}
        free3complex(cq2);
	free3complex(cq3);
        free1float(V);

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
