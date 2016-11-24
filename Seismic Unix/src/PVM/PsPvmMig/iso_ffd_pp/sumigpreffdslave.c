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

void retris(complex *data,complex *a,complex *c,complex *b,complex
                endl,complex endr, int nx, complex *d);

void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
     dw,float dz,float dx,float dt, float vc, int dip);


 
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
	int Finish,dip=45;
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
        float vmin,v1;
        double kz1,kz2;
        float **v,**vp;
        double phase1;
        complex cshift1,cshift2;
        complex **cq,**cp,**cp1,**cq1;  /* complex input,output         */

        int verbose=1;          /* flag for echoing info                */

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

        msgtype=DATA_MSGTYPE;
        rc=pvm_recv(-1,msgtype);
        rc=pvm_upkint(&Finish,1,1);

        if(Finish==Done){free2float(v);goto loop;}

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


                                
        
                vmin=v[iz][0];
        
                for(ix=0;ix<nx;ix++){
		if(v[iz][ix]<vmin)vmin=v[iz][ix];
                }
        
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
                                if(kz1>0.15){
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
                                
		float a=0.015,g=1.0;
		int i,I=10;
                                
		if(ix<=I)g=exp(-a*(I-ix)*(I-ix));
		if(ix>=nx-I)g=exp(-a*(-nx+I+ix)*(-nx+I+ix));

                                
                                cq[iw][ix] = crmul( cq[iw][ix],1.0/nxfft);
                                cq[iw][ix] =ix%2 ? cneg(cq[iw][ix]) : cq[iw][ix];
                                kz2=(1.0/v1-1.0/v[iz][ix])*w*dz;
                                cshift2=cmplx(cos(kz2),sin(kz2));
                                cp[iw][ix]=cmul(cq[iw][ix],cshift2);
                
                                cq1[iw][ix] = crmul( cq1[iw][ix],1.0/nxfft);
                                cq1[iw][ix] =ix%2 ? cneg(cq1[iw][ix]) : cq1[iw][ix];
                                cp1[iw][ix]=cmul(cq1[iw][ix],cshift2);
        
                        }

                 
                fdmig( cp, nx, nw,v[iz],fw,dw,dz,dx,dt,v1,dip);
                fdmig( cp1,nx, nw,v[iz],fw,dw,dz,dx,dt,v1,dip);

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

        void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
        dw,float dz,float dx,float dt,float vc,int dip)
{
        int iw,ix;
        float *p,*s1,*s2,w,a0=2.0,v1,vn,trick=0.1, ccx,para=0.08;
        complex endl,endr;
        complex cp2,cp3,cpnm1,cpnm2;
        complex a1,a2,b1,b2;
        complex *data,*d,*a,*b,*c;
        float coefa, coefb;
        float aaa=-8.0*para*dt/PI;
        ccx=-aaa/(2.0*dx*dx);

        
        p=alloc1float(nx);
        s1=alloc1float(nx);  
        s2=alloc1float(nx);
        
        data=alloc1complex(nx);
        d=alloc1complex(nx);
        a=alloc1complex(nx);
        b=alloc1complex(nx);
        c=alloc1complex(nx); 
                
         
        for(ix=0;ix<nx;ix++){
        p[ix]=vc/v[ix];
        p[ix]=(p[ix]*p[ix]+p[ix]+1.0);
        }

/*        if(dip==65){coefa=0.478242060;coefb=0.376369527;}  */

        if(dip!=65){coefa=0.5;coefb=0.25;}
	else{coefa=0.4784689;coefb=0.37607656;}

        v1=v[0];vn=v[nx-1];
        
        for(iw=0,w=fw;iw<nw;iw++,w+=dw){
        if(w==0)w=1.0e-10/dt;
        
        for(ix=0;ix<nx;ix++){
                s1[ix]=coefb*p[ix]*v[ix]*v[ix]/(dx*dx*w*w)+trick;
                s2[ix]=-(1-vc/v[ix])*v[ix]*dz*coefa/(w*dx*dx)*0.5;
        }

        for(ix=0;ix<nx;ix++){
                data[ix]=cp[iw][ix];
        }
        
        cp2=data[1];
        cp3=data[2];
        cpnm1=data[nx-2];
        cpnm2=data[nx-3];
        a1=crmul(cmul(cp2,conjg(cp3)),2.0);
        b1=cadd(cmul(cp2,conjg(cp2)),cmul(cp3,conjg(cp3)));
        
        if(b1.r==0.0&&b1.i==0.0)
        a1=cexp(cmplx(0.0,-w*dx*0.5/v1));
        else a1=cdiv(a1,b1);
        
        if(a1.i>0.0)a1=cexp(cmplx(0.0,-w*dx*0.5/v1));
        
        a2=crmul(cmul(cpnm1,conjg(cpnm2)),2.0);
        b2=cadd(cmul(cpnm1,conjg(cpnm1)),cmul(cpnm2,conjg(cpnm2)));
                
        if(b2.r==0.0&&b2.i==0.0)
        a2=cexp(cmplx(0.0,-w*dx*0.5/vn));
        else a2=cdiv(a2,b2); 
        if(a2.i>0.0)a2=cexp(cmplx(0.0,-w*dx*0.5/vn));
         
        
        for(ix=0;ix<nx;ix++){
                a[ix]=cmplx(s1[ix],s2[ix]+ccx*v[ix]*v[ix]/w);
                
b[ix]=cmplx(1.0-2.0*s1[ix],-2.0*s2[ix]-2.0*ccx*v[ix]*v[ix]/w);
        }

        
        d[0]=cadd(cmul(cadd(b[0],cmul(a[0],a1)),data[0]),cmul(data[1],a[1]));

d[nx-1]=cadd(cmul(cadd(b[nx-1],cmul(a[nx-1],a2)),data[nx-1]),cmul(data[nx-2],a[nx-2]));

        for(ix=1;ix<nx-1;ix++){

d[ix]=cadd(cadd(cmul(data[ix+1],a[ix+1]),cmul(data[ix-1],a[ix-1])),cmul(data[ix],b[ix]));
        }
         
        for(ix=0;ix<nx;ix++){
        data[ix]=cmplx(s1[ix],-s2[ix]+ccx*v[ix]*v[ix]/w);
        b[ix]=cmplx(1.0-2.0*s1[ix],2.0*s2[ix]-2.0*ccx*v[ix]*v[ix]/w);
        }

        endl=cadd(b[0],cmul(data[0],a1));
        endr=cadd(b[nx-1],cmul(data[nx-1],a2));
        
        for(ix=1;ix<nx-1;ix++)
        {
        a[ix]=data[ix+1];
        c[ix]=data[ix-1];
        }
        a[0]=data[1];
        c[nx-1]=data[nx-2];
        

        retris(data,a,c,b,endl,endr,nx,d);
        
        for(ix=0;ix<nx;ix++){
        cp[iw][ix]=data[ix];
        }
        
        }
        
        free1complex(data);
        free1complex(d);
        free1float(p);
        free1complex(b);
        free1complex(a);
	free1complex(c);
        free1float(s1);
        free1float(s2);
        return;
}


void retris(complex *data,complex *a,complex *c, complex *b,
                complex endl,complex endr, int nx, complex *d)
{               
 
        int ix;
        complex *e,den;
        complex *f;
         
        e=alloc1complex(nx);
        f=alloc1complex(nx);
        e[0]=cdiv(cneg(a[0]),endl);
        f[0]=cdiv(d[0],endl);

        for(ix=1;ix<nx-1;++ix){
                den=cadd(b[ix],cmul(c[ix],e[ix-1]));
                e[ix]=cdiv(cneg(a[ix]),den);
                f[ix]=cdiv(csub(d[ix],cmul(f[ix-1],c[ix])),den);
        }               

                 
data[nx-1]=cdiv(csub(d[nx-1],cmul(f[nx-2],c[nx-2])),cadd(endr,cmul(c[nx-2],e[nx-2])));
                
        for(ix=nx-2;ix>-1;--ix)
                data[ix]=cadd(cmul(data[ix+1],e[ix]),f[ix]);
 
        free1complex(e);
        free1complex(f);
        return;
}
        
 
