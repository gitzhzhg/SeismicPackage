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

/*function to solve the complex triangle system*/
void retris(complex *data,complex *a,complex *c,complex *b,complex
                endl,complex endr, int nx, complex *d);

/*function to do the downward propagation using implicit finite-difference method*/
void fdmig( complex **cp, int nx, int nw, float *v,float fw,float
                dw,float dz,float dx,float dt,int dip,float para);

 
/*********************** self documentation ******************************/
char *sdoc[] = {
"The slave code for the prestack finite-difference depth-migration code,", 
"It is called by the master code, so no input or output for this program.", 
NULL};
/**************** end self doc *******************************************/

int main (int argc, char **argv)
 
{
	int Finish,dip=65;
        int nz;                 /* number of migrated depth samples */
        int nxo,nx;             /* number of midpoints  */
        int iz,iw,ix,ix2,ix3,ixshot;     /* loop counters*/
        int ntfft;        	/* fft size*/
        int nw;              /* number of frequency*/
        int mytid,msgtype,rc,parent_tid;


        float dt=0.004,dz;         /*time and depth sampling interval*/
        float dw;            /*frequency sampling interval */
        float fw;            /* first frequency*/
        float w;              /* frequency*/
        float dx;               /* spatial sampling interval*/
        float **cresult;    /*output data*/
        float v1;
        float para;
        double kz2;
        float **vp,**v;
        complex cshift2;
        complex **cp,**cp1;  /* complex input,output         */

	/*get my and father pids*/
	mytid=pvm_mytid();
	parent_tid=pvm_parent();

	/*receive global parameters*/
	msgtype=PARA_MSGTYPE;	
	rc=pvm_recv(-1,msgtype);
	rc=pvm_upkint(&nxo,1,1);
	rc=pvm_upkint(&nz,1,1);
	rc=pvm_upkint(&dip,1,1);
        rc=pvm_upkfloat(&para,1,1);
	
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
        rc=pvm_recv(parent_tid,msgtype);
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




	while(1){
		/*receive parameters and data for processing*/
		msgtype=DATA_MSGTYPE;
		rc=pvm_recv(parent_tid,msgtype);
		rc=pvm_upkint(&Finish,1,1);

		if(Finish==Done) {free2float(v);goto loop; }
		rc=pvm_upkfloat(&fw,1,1);

		rc=pvm_upkint(&nw,1,1);
		cp = alloc2complex(nx,nw);
		cp1 = alloc2complex(nx,nw);
		rc=pvm_upkfloat((float *)cp[0],nx*nw*2,1);
		rc=pvm_upkfloat((float *)cp1[0],nx*nw*2,1);



        /* loops over depth */
        for(iz=0;iz<nz;++iz){

	/*the imaging condition*/
/*	for(ix=0;ix<nx;ix++){
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


		/*get the average velocity*/
		v1=0.0;
                for(ix=0;ix<nx;++ix)
		{v1+=v[iz][ix]/nx;}
		
		/*compute time-invariant wavefield*/
/*                for(ix=0;ix<nx;++ix)
                for(iw=0,w=fw;iw<nw;w+=dw,++iw) {
                        kz2=-(1.0/v1)*w*dz;
                        cshift2=cmplx(cos(kz2),sin(kz2));   
                        cp[iw][ix]=cmul(cp[iw][ix],cshift2);
                        cp1[iw][ix]=cmul(cp1[iw][ix],cshift2);
                }
*/
		/*wave-propagation using finite-difference method*/
                fdmig( cp, nx, nw,v[iz],fw,dw,dz,dx,dt,dip,para);
                fdmig( cp1,nx, nw,v[iz],fw,dw,dz,dx,dt,dip,para);

		/*apply thin lens term here*/                                
                for(ix=0;ix<nx;++ix)
                for(iw=0,w=fw;iw<nw;w+=dw,++iw){
		float Wi=-dw;
		kz2=-(1.0/v[iz][ix])*dz;
/*			kz2=-(1.0/v[iz][ix]-1.0/v1)*w*dz;
			cshift2=cmplx(cos(kz2),sin(kz2));*/
			cshift2=cexp(cmplx(-Wi*kz2,w*kz2));
			cp[iw][ix]=cmul(cp[iw][ix],cshift2);
			cp1[iw][ix]=cmul(cp1[iw][ix],cshift2);
		}
                
}

/*finish a portion of the data, request more*/
pvm_initsend(PvmDataDefault);
pvm_pkint(&mytid,1,1);
msgtype=COM_MSGTYPE;
pvm_send(parent_tid,msgtype);
         
free2complex(cp);
free2complex(cp1);

}


end:

/*everything done,send back partial image and wait for signal to kill itself*/
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
        dw,float dz,float dx,float dt,int dip,float para)
{
        int iw,ix,step=1;
	float *s1,*s2,w,coefa[5],coefb[5],v1,vn,trick=0.1,ccx;
        complex cp2,cp3,cpnm1,cpnm2;
        complex a1,a2,b1,b2;
        complex endl,endr;
        complex *data,*d,*a,*b,*c;
	float aaa=-8.0*para*dt/PI;
	ccx=-aaa/(2.0*dx*dx);
                 
        s1=alloc1float(nx);
        s2=alloc1float(nx);

        data=alloc1complex(nx);
        d=alloc1complex(nx);
        a=alloc1complex(nx);
        b=alloc1complex(nx);
        c=alloc1complex(nx);

        if(dip==45){
        coefa[0]=0.5;coefb[0]=0.25;
	step=1;
	}

        if(dip==65){
        coefa[0]=0.478242060;coefb[0]=0.376369527;
	step=1;
	}

        if(dip==79){
	coefa[0]=coefb[0]=0.4575;
        step=1;
        }


        if(dip==80){
	coefa[1]=0.040315157;coefb[1]=0.873981642;
        coefa[0]=0.457289566;coefb[0]=0.222691983;
	step=2;
	}

        if(dip==87){
        coefa[2]=0.00421042;coefb[2]=0.972926132;
        coefa[1]=0.081312882;coefb[1]=0.744418059;                     
        coefa[0]=0.414236605;coefb[0]=0.150843924;                     
	step=3;
	}

	if(dip==89){
	coefa[3]=0.000523275;coefb[3]=0.994065088;
	coefa[2]=0.014853510;coefb[2]=0.919432661;
	coefa[1]=0.117592008;coefb[1]=0.614520676;
	coefa[0]=0.367013245;coefb[0]=0.105756624;
	step=4;
	}

	if(dip==90){
	coefa[4]=0.000153427;coefb[4]=0.997370236;
	coefa[3]=0.004172967;coefb[3]=0.964827992;
	coefa[2]=0.033860918;coefb[2]=0.824918565;
	coefa[1]=0.143798076;coefb[1]=0.483340757;
	coefa[0]=0.318013812;coefb[0]=0.073588213;
	step=5;   
	}


        v1=v[0];vn=v[nx-1];

loop:

step--;

        for(iw=0,w=fw;iw<nw;iw++,w+=dw){
	float tmp1=0.0,tmp2=0.0;

                if(fabs(w)<=1.0e-10)w=1.0e-10/dt;

                for(ix=0;ix<nx;ix++){
                        s1[ix]=(v[ix]*v[ix])*coefb[step]/(dx*dx*w*w)+trick;
                        s2[ix]=-v[ix]*dz*coefa[step]/(w*dx*dx)*0.5;
                }


                for(ix=0;ix<nx;ix++){
                        data[ix]=cp[iw][ix];
                }

                cp2=data[0];
                cp3=data[1];
                cpnm1=data[nx-1];
                cpnm2=data[nx-2];
                a1=cmul(cp2,conjg(cp3));
/*
                b1=cadd(cmul(cp2,conjg(cp2)),cmul(cp3,conjg(cp3)));
 */
                b1=cmul(cp3,conjg(cp3));
                if(b1.r==0.0 && b1.i==0.0)
                        a1=cexp(cmplx(0.0,-w*dx*0.5/v1));
                else
                        a1=cdiv(a1,b1);

                if(a1.i>0.0)a1=cexp(cmplx(0.0,-w*dx*0.5/v1));
                 
                a2=cmul(cpnm1,conjg(cpnm2));
                b2=cmul(cpnm2,conjg(cpnm2));

                if(b2.r==0.0 && b2.i==0.0)
                        a2=cexp(cmplx(0.0,-w*dx*0.5/vn));
                else
                        a2=cdiv(a2,b2);

                if(a2.i>0.0)a2=cexp(cmplx(0.0,-w*dx*0.5/vn));


                for(ix=0;ix<nx;ix++){
                        a[ix]=cmplx(s1[ix],s2[ix]+ccx*v[ix]*v[ix]/w);
                        
b[ix]=cmplx(1.0-2.0*s1[ix],-2.0*s2[ix]-2.0*ccx*v[ix]*v[ix]/w);
		}

		for(ix=1;ix<nx-1;ix++){
		d[ix]=cadd(cadd(cmul(data[ix+1],a[ix+1]),cmul(data[ix-1],a[ix-1])),cmul(data[ix],b[ix]));
                }
                        
                d[0]=cadd(cmul(cadd(b[0],cmul(a[0],a1)),data[0]),cmul(data[1],a[1]));
		d[nx-1]=cadd(cmul(cadd(b[nx-1],cmul(a[nx-1],a2)),data[nx-1]),cmul(data[nx-2],a[nx-2]));
                        
                for(ix=0;ix<nx;ix++){
                        data[ix]=cmplx(s1[ix],-s2[ix]+ccx*v[ix]*v[ix]/w);
                        
b[ix]=cmplx(1.0-2.0*s1[ix],2.0*s2[ix]-2.0*ccx*v[ix]*v[ix]/w);
		}

		endl=cadd(b[0],cmul(data[0],a1));
                endr=cadd(b[nx-1],cmul(data[nx-1],a2));
                
                
                for(ix=1;ix<nx-1;ix++){
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

if(step) goto loop;

        free1complex(data);
        free1complex(d);
        free1complex(b);
        free1complex(c);
        free1complex(a);
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

                 
	data[nx-1]=cdiv(csub(d[nx-1],cmul(f[nx-2],c[nx-1])),cadd(endr,cmul(c[nx-1],e[nx-2])));
                
        for(ix=nx-2;ix>-1;--ix)
                data[ix]=cadd(cmul(data[ix+1],e[ix]),f[ix]);
 
        free1complex(e);
        free1complex(f);
        return;
}
        
 
