/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUINVCO3D: $Revision: 1.4 $ ; $Date: 2011/11/16 22:14:43 $	*/

#include "su.h"
#include "segy.h"
#define TINY 0.000001 /*avoid devide by zero*/
#define LARGE 1000000

/*********************** self documentation **********************/
char *sdoc[] = {
" SUINVZCO3D - Seismic INVersion of Common Offset data with V(Z) velocity",
"             function in 3D						",
" 									",
"     suinvzco3d <infile >outfile [optional parameters] 		",
"									",
" Required Parameters:							",
" vfile                  file containing velocity array v[nz]		",
" nz=                    number of z samples (1st dimension) in velocity",
" nxm=			number of midpoints of input traces		",
" ny=			number of lines 				",
"									",
" Optional Parameters:							",
" dt= or from header (dt) 	time sampling interval of input data	",
" offs= or from header (offset) 	source-receiver offset	 	",
" dxm= or from header (d2) 	sampling interval of midpoints 		",
" fxm=0                  first midpoint in input trace			",
" nxd=5			skipped number of midpoints (see note)		",
" dx=50.0                x sampling interval of velocity		",
" fx=0.0                 first x sample of velocity			",
" dz=50.0                z sampling interval of velocity		",
" nxb=nx/2		band centered at midpoints (see note)		",
" fxo=0.0                x-coordinate of first output trace 		",
" dxo=15.0		horizontal spacing of output trace 		",
" nxo=101                number of output traces 			",	
" fyo=0.0		y-coordinate of first output trace		",
" dyo=15.0		y-coordinate spacing of output trace		",
" nyo=101		number of output traces in y-direction		",
" fzo=0.0                z-coordinate of first point in output trace 	",
" dzo=15.0               vertical spacing of output trace 		",
" nzo=101                number of points in output trace		",	
" fmax=0.25/dt		Maximum frequency set for operator antialiasing ",
" ang=180		Maximum dip angle allowed in the image		",
" verbose=1              =1 to print some useful information		",
"									",
" Notes:									",
"									",
" This algorithm is based on formula (50) in Geophysics Vol. 51, 	",
" 1552-1558, by Cohen, J., Hagin, F., and Bleistein, N.			",
"									",
" Traveltime and amplitude are calculated by ray tracing.		",
" Interpolation is used to calculate traveltime and amplitude.		", 
" For each midpoint, traveltime and amplitude are calculated in the 	",
" horizontal range of (xm-nxb*dx, xm+nxb*dx). Velocity is changed by 	",
" linear interpolation in two upper trianglar corners whose width is 	",
" nxc*dx and height is nzc*dz.						",	
"									",
" Eikonal equation will fail to solve if there is a polar turned ray.	",
" In this case, the program shows the related geometric information. 	",
"									", 
" Offsets are signed - may be positive or negative. 			", 
"									",
NULL};
/**************** end self doc ***********************************/
/* additional prototypes for ray tracing */
void amplitude(float fx, float fy, int nx, int ny, int nz,
        float dx, float dy, float dz,
        float fxo, float fyo, int nxo, int nyo, int nzo,
        float dxo, float dyo, float dzo,
        int na, float *v, float off, float ***t3d,float ***amp, float ***p3d);


/*segy trace */ 
segy tr, tro;

int main (int argc, char **argv)
{
	int 	ixm,nxm,iym,nt,nxo,nyo,nzo,ixo,iyo,izo,nsamp,
		ny,nz,nxb,i,ix,iy,iz,nz0,nxc,nzc,na,
		verbose;
	float   temp,xo,yo,zo,xi,yi,zi,rs,sx,sy,sz,tsd,ampd,tgd,
		dx,dy,dz,samp,smax,fmax,ang,cosa,
		dxm,dt,fxm,fxo,dxo,fyo,dyo,fzo,dzo,offs,xm,odx,
		ody,odz,odt,sins,sing,
		xs,xg,ys,yg,ey,ez,***outtrace, 
		***amp,***ts,***p3,
		*v,*vnew;
	char *vfile="";

	/* hook up getpar to handle the parameters */
	initargs(argc, argv);
	requestdoc(1);


	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	temp = tr.dt;
	if (!getparfloat("dt",&dt)) dt = temp*0.000001;
	if (dt<0.0000001) err("dt must be positive!\n");
	if (!getparfloat("offs",&offs)) offs = tr.offset;
	if (!getparfloat("dxm",&dxm)) dxm = tr.d2;
	if  (dxm<0.0000001) err("dxm must be positive!\n");
	
	/* get required parameters */
	if (!getparint("ny",&ny)) err("must specify ny!\n");
	if (!getparint("nz",&nz)) err("must specify nz!\n");
	if (!getparint("nxm",&nxm)) err("must specify nxm!\n");
	if (!getparstring("vfile",&vfile)) err("must specify vfile!\n");
	
	/* get optional parameters */
	if (!getparfloat("fxm",&fxm)) fxm = 0.0;
	if (!getparfloat("dxm",&dx)) dx = 50.;
	if (!getparfloat("dy",&dy)) dy=50.;
	if (!getparfloat("dz",&dz)) dz = 50.;
	if (!getparint("nxb",&nxb)) nxb = nxm/2;
	if (!getparint("nxc",&nxc)) nxc = 0;
	if (!getparint("na",&na)) na=100;

	nxc = MIN(nxc,nxb);
	if (!getparint("nzc",&nzc)) nzc = 0;
	nzc = MIN(nzc,nz);
	if (!getparfloat("fxo",&fxo)) fxo = 0.0;
	/* check the first output trace	*/
	if (fxo<fxm )
		err("the first output trace is out of velocity model.\n");
	if (!getparfloat("dxo",&dxo)) dxo = 15.;
	if (!getparint("nxo",&nxo)) nxo = 101;
	if (!getparfloat("fyo",&fyo)) fyo=0.0;
	if (!getparfloat("dyo",&dyo)) dyo = 15.;
        if (!getparint("nyo",&nyo)) nyo = 101;
	if (!getparfloat("fzo",&fzo)) fzo = 0.;
	if (!getparfloat("dzo",&dzo)) dzo = 15.;
	if (!getparint("nzo",&nzo)) nzo = 101;
	if (!getparint("verbose",&verbose)) verbose = 1;
	if (!getparfloat("fmax",&fmax)) fmax = 0.25/dt;
	if (!getparfloat("ang",&ang)) ang = 180;
	cosa = cos(ang*PI/180);
	
        checkpars();

	/* check the ranges of shots and receivers */
	ey = (ny-1)*dy;
	ez = (nz-1)*dz;

	odx = 1.0/dxo;
	ody = 1.0/dyo;
	odz = 1.0/dzo;
	odt = 1.0/dt;
	smax = 0.5/(MAX(dxm,dy)*fmax);
	
	/* allocate space */
	outtrace = ealloc3float(nzo,nxo,nyo);

	
	/* initialize output traces	*/
		for(ixo=0; ixo<nxo; ++ixo)
		for(iyo=0;iyo<nyo;++iyo)
		for(izo=0; izo<nzo; ++izo)
			outtrace[iyo][ixo][izo]=0.0;
	
	/* allocate space for traveltimes and other quantities	*/ 
	ts = ealloc3float(nzo,nxo,nyo);
	amp = ealloc3float(nzo,nxo,nyo);
	p3 = ealloc3float(nzo,nxo,nyo);
	v = ealloc1float(nz);
	vnew = ealloc1float(nzo);

	if(fread(v,sizeof(float),nz,fopen(vfile,"r"))!=nz)
                err("cannot read %d velocities from file %s",nz,vfile);
	for(i=0;i<nzo;++i) {
		temp=i*dzo/dz;
		nsamp=temp;
		temp=temp-nsamp;
		if(nsamp>nz-1) nsamp=nz-1;
		vnew[i]=(1-temp)*v[nsamp]+temp*v[nsamp+1];	
	}

	if(ABS(offs)>2*nxb*dx) err("\t band NXB is too small!");

	if(nxm*dxm<nxo*dxo || ny*dy<nyo*dyo) err("\t not enough range in input  		for the imaging");			    				
	/* compute the first z-sample of velocity model which corresponds
	   to fzo	*/
	nz0 = MAX(0,fzo/dz);
	
	/* set range for traveltimes' calculation */
	amplitude(fxm,0,nxm,ny,nz,dxm,dy,dz,fxo,fyo,nxo,nyo,nzo,dxo,dyo,dzo,
		na,vnew,offs,ts,amp,p3);	
	fprintf(stderr,"\nTables generated...\n");	
/*
	for(iyo=0; iyo<nyo; ++iyo)
		for(ixo=0; ixo<nxo; ++ixo)
			for(izo=0; izo<nzo; ++izo)
		fprintf(stderr, "%f\n",amp[iyo][ixo][izo]);
*/
	/* loop over midpoints */
	for (iym=0; iym<ny; ++iym){
	  for (ixm=0; ixm<nxm; ixm+=1){
	    xm = fxm+ixm*dxm;
   	    xs = xm-0.5*offs;
	    xg = xs+offs;
	    ys=iym*dy;
	    yg=iym*dy;
	   
		/* loop over output points */

		for (iyo=0,yo=fyo; iyo<nyo; ++iyo, yo+=dyo)
		  for (ixo=0,xo=fxo; ixo<nxo; ++ixo,xo+=dxo)
		   {
			i=sqrt((xo-xm)*(xo-xm)+(yo-ys)*(yo-ys))/dx;
			if(i>nxb) continue; 
		    for (izo=1,zo=fzo+dzo; izo<nzo; ++izo,zo+=dzo){ 
			
			/* check range of output */
			if( zo-ez>0||yo-ey>0||ABS(yo-ys)/dyo>nyo-2||ABS(xo-xs)/dxo>nxo-2)
				continue;
			/* determine sample indices */
			xi = ABS((xo-xs)/dxo);
			ix = xi;
			yi = ABS((yo-ys))/dyo;
			iy = yi;
			zi = zo/dzo;
			iz = zi;
			rs = sqrt((xo-xs)*(xo-xs)+(yo-ys)*(yo-ys)+zo*zo);
			/* bilinear interpolation */
			sx = xi-ix;
			sy = yi-iy;
			sz = zi-iz;
			tsd = (1.0-sy)*((1.0-sz)*((1.0-sx)*ts[iy][ix][iz] +
                                                sx*ts[iy][ix+1][iz]) +
                                                sz*((1.0-sx)*ts[iy][ix][iz+1] +
                                                sx*ts[iy][ix+1][iz+1]));

                        tsd+=sy*((1.0-sz)*((1.0-sx)*ts[iy+1][ix][iz] +
                                                sx*ts[iy+1][ix+1][iz]) +
                                                sz*((1.0-sx)*ts[iy+1][ix][iz+1] +
                                                sx*ts[iy+1][ix+1][iz+1]));

			/*aliasing check*/
			sins=p3[iy][ix][iz]*p3[iy][ix][iz]*(v[iz]*v[iz]);
				if (sins>=1.) {
					sins=1./sins;
					sins=sqrt(1.-sins);
				      	}	
		
			xi = ABS((xo-xg))/dxo;
                        ix = xi;
                        yi = ABS((yo-yg))/dyo;
                        iy = yi;
                        zi = zo/dzo;
                        iz = zi;
			/*rg = sqrt((xo-xg)*(xo-xg)+(yo-yg)*(yo-yg)+zo*zo);*/
                        /* bilinear interpolation */
                        sx = xi-ix;
                        sy = yi-iy;
                        sz = zi-iz;

			tgd = (1.0-sy)*((1.0-sz)*((1.0-sx)*ts[iy][ix][iz] +
                                                sx*ts[iy][ix+1][iz]) +
                                        	sz*((1.0-sx)*ts[iy][ix][iz+1] +
                                                sx*ts[iy][ix+1][iz+1]))+
                              sy*((1.0-sz)*((1.0-sx)*ts[iy+1][ix][iz] +
                                                sx*ts[iy+1][ix+1][iz]) +
                                        	sz*((1.0-sx)*ts[iy+1][ix][iz+1]+
                                                sx*ts[iy+1][ix+1][iz+1]));

			sing=p3[iy][ix][iz]*p3[iy][ix][iz]*(v[iz]*v[iz]);
			if(sing>=1.) {
				sing=1./sing;
                        	sing=sqrt(1.-sing);
				   }
	

			/*calculate amplitude and sample index*/
			if(xo<=xs) {
				ampd=amp[iy][ix][iz];
			} else {
				ix=ABS((xo-xs))/dxo;
				ampd=amp[iy][ix][iz];
			}

			samp = (tsd+tgd)*odt;
			nsamp = samp;
			if (nsamp>nt-2) continue;
			samp = samp-nsamp;
                        /* check operator aliasing */
			/*if(ABS(sins+sing)
                                >=fmax) ampd = 0.;
			*/	
                                /* dip filter for imaging */

			/*output summation*/
			outtrace[iyo][ixo][izo] += ampd*(samp*tr.data[nsamp+1]
				+(1.0-samp)*tr.data[nsamp]);

		    }
	          }	
			if(ixm<nxm-1||iym<ny-1) gettr(&tr);

	  }
	if(verbose) fprintf(stderr, "\tfinish line%d\n", iym+1); 
	}

 
	/* write trace */
	temp = 4/sqrt(PI)*dxm;
	for (iyo=0; iyo<nyo; ++iyo)
		for(ixo=0; ixo<nxo; ++ixo){
		/* scale output traces */
 		for(izo=0; izo<nzo; ++izo)	
			outtrace[iyo][ixo][izo] *= temp; 
		/* make headers for output traces */
		tro.offset = offs;
		tro.tracl = tro.tracr = 1+ixo*nyo+iyo;
		tro.ns = nzo;
		tro.d1 = dzo;
		tro.ns = nzo;
		tro.f1 = fzo;
		tro.f2 = fxo;
		tro.d2 = dxo;
		tro.cdp = ixo;
		tro.trid = 200;
		/* copy migrated data to output trace */
		memcpy((void *) tro.data,(const void *) outtrace[iyo][ixo],nzo*sizeof(float));
		puttr(&tro); 
	}

	free3float(ts);
	free1float(v);
	free1float(vnew);
	free3float(p3);
	free3float(amp);
	return(CWP_Exit());
}
 
float determ(float a[3][3])
/*****************************************************************************
Calculation of a 3x3 determinant
******************************************************************************
Input:
a		array of the determinant

Output:
determ		value of the det		
******************************************************************************
Notes: Used internally in function: amplitude()
******************************************************************************
Author:  Meng Xu, Center for Wave Phenomena, 07/8/95
******************************************************************************/
{
        float det;

        det=0.0;
        det=a[0][0]*a[1][1]*a[2][2];
        det+=a[1][0]*a[2][1]*a[0][2];
        det+=a[2][0]*a[1][2]*a[0][1];
        det-=a[0][2]*a[1][1]*a[2][0];
        det-=a[1][2]*a[2][1]*a[0][0];
        det-=a[2][2]*a[1][0]*a[0][1];
        return det;
}

void para3d(
		int nxo,int nyo,int nzo,  
                float dxo, float dyo, float dzo, 
                float ***t3d,float ***sigma3d,float ***p3d,
		float ***dzda23d,float ***a23d,
                float **t2d,float **sigma2d,float **p2d,float **dzda2d,float **a2d)
/*****************************************************************************
Interpolation of ray parameters from 2-D to 3-D
******************************************************************************
Input:
nxo		number of output grids in x
nyo		number of output grids in y
nzo		number of output grids in z
dxo		output sample rate in x
dyo		output sample rate in y
dzo		output sample rate in z
t2d		traveltime array
sigma2d		sigma array
p2d		array for slowness p3
dzda2d		array for dz/da
a2d		array for a2

Output:
t3d		3D array for traveltime
sigma3d		3D array for sigma
p3d		3D array for p3
dzda23d		3D array for dz/da
a23d		3D array for a2

******************************************************************************
Notes: Used internally in function: amplitude()
******************************************************************************
Author:  Meng Xu, Center for Wave Phenomena, 07/8/95
******************************************************************************/

{
        int ix, iy, iz, ix2, nxr;
        float temp1;

        nxr = NINT(sqrt(nxo*dxo*nxo*dxo+nyo*dyo*nyo*dyo)/dxo)+1;
        for(iy=0;iy<nyo;++iy)
        for(ix=0;ix<nxo;++ix)
        for(iz=0;iz<nzo;++iz)
        {
                temp1= sqrt((ix*dxo)*(ix*dxo)+(iy*dyo)*(iy*dyo))/dxo;
                ix2=temp1;
                temp1=temp1-ix2;

                t3d[iy][ix][iz]= (1.0-temp1)*t2d[ix2][iz]+
                        temp1*t2d[ix2+1][iz];

		p3d[iy][ix][iz]= (1.0-temp1)*p2d[ix2][iz]+
                        temp1*p2d[ix2+1][iz];

		sigma3d[iy][ix][iz]= (1.0-temp1)*sigma2d[ix2][iz]+
                        temp1*sigma2d[ix2+1][iz];

		a23d[iy][ix][iz]= (1.0-temp1)*a2d[ix2][iz]+
                        temp1*a2d[ix2+1][iz];

		dzda23d[iy][ix][iz]= (1.0-temp1)*dzda2d[ix2][iz]+
                        temp1*dzda2d[ix2+1][iz];

	}
	
}


void SingleRay(int nx,int nz,float a, float *v,float dx,float dz,float *x,float *p3,float *sigma,float *t,float *dzda2,int ray_end_iz)
/*****************************************************************************
Ray equation solution of one ray in v(z) medium in polar coordinate
******************************************************************************
Input:
nx              number of x samples
nz              number of z smaples
a		polar angle
v		array for v(z)               
dx              x sample interval
dz              z sample interval

Output:
x		array for ray position of x 
p3		array for slowness p3
sigma		array for sigma
t		array for traveltime
dzda2		array for the z derivative to propagation angle a2
******************************************************************************
Notes: Used internally in function: amplitude()
******************************************************************************
Author:  Meng Xu, Center for Wave Phenomena, 07/8/95
******************************************************************************/
{
	int i,ii;
	float fx,zx,tt,sig,temp,step;

	sig=tt=temp=0;
	fx=0;
	zx=dx*nx;

	for(i=0;i<nz;++i)
	{
	sigma[i]=0;
	p3[i]=0;
	x[i]=0;
	t[i]=0;
	dzda2[i]=0;
	}

	
	for(i=0;i<nz;++i)
	{	
		if(1./v[i]/v[i]-sin(a)*sin(a)/v[0]/v[0]>0)  
			{
			p3[i]=sqrt(ABS(1./v[i]/v[i]-sin(a)*sin(a)/v[0]/v[0]));

			sigma[i]=sig;
			sig+=dz/p3[i];

			x[i]=sigma[i]*sin(a)/v[0];
			t[i]=tt;
			tt+=dz/v[i]/v[i]/p3[i];
			dzda2[i]=temp;
			temp-=dz/p3[i]*sin(a)/v[0]*p3[0]/p3[i];
			ray_end_iz=nz-1;
			if(p3[i]<=TINY) 
				{
				fprintf(stderr,"\nTurning point: z=%f\n",i*dz);
				continue;
				}
			if(x[i]<fx||x[i]>zx) {step=i;}
			}
		else
			{
			fprintf(stderr,"\nTurning point: ang=%f,z=%f",a,i*dz);
			ray_end_iz=i;

			for(ii=i; ii<nz; ++ii) 
				{
				sigma[i]=0;
       		                x[i]=-1;
       		                t[i]=0;
                       		dzda2[i]=0;
				}
                        if(x[i]<fx||x[i]>zx) {step=i;}
			i=nz;
			}
	}
}
		


	
void para2d(int na,int nx,int nz,float dx,float dz,float *v,float **t2d,
		float **sigma2d,float **p2d,float **dzda2d,float **a2d)
/*****************************************************************************
Calculate ray parameters on a 2D grid
******************************************************************************
Input:
nx             number of output grids in x
nz             number of output grids in z
dx             output sample rate in x
dz             output sample rate in z
v	       velocity array v(z)
	
Output:
t2d             2D array for traveltime
sigma2d         2D array for sigma
p2d             2D array for p3
dzda2d          2D array for dz/da
a2d             2D array for a2

******************************************************************************
Notes: Used internally in function: amplitude()
******************************************************************************
Author:  Meng Xu, Center for Wave Phenomena, 07/8/95
******************************************************************************/
{
	int i,k,ii;
	int iflag;
	int i00=0;
	int imax;
	int *ray_end_iz;

	float sx;
	float x0;
	float da;	
	float a,**p3,**sigma,**t,**x,**dzda2,**a2;

	da=3.14159265/na/2.;

	p3=ealloc2float(nz,na);
	sigma=ealloc2float(nz,na);
	t=ealloc2float(nz,na);
	x=ealloc2float(nz,na);
	dzda2=ealloc2float(nz,na);
	a2=ealloc2float(nz,na);
	ray_end_iz=ealloc1int(na);
	

	for(i=0,a=0.;i<na;++i,a+=da)
	{
	SingleRay(nx,nz,a,v,dx,dz,x[i],p3[i],
		sigma[i],t[i],dzda2[i],ray_end_iz[i]);
	for(k=0;k<nz;++k) a2[i][k]=a;
	}

	/*interpolation from ray coordinate to (x,z) coordinate*/ 
	for(k=1;k<nz;++k)
	  for(i=0;i<nx;++i)
		{
		x0=dx*i;
		iflag=0;
		for(ii=0;ii<na;++ii)
			{
			if(ii==0) {i00=0;}
			if(x[ii][k]>x0&&x[i00][k]<=x0)
				{
				iflag=ii; 
				ii=na;
				}
			}
		if(iflag==0) 
			{
			for(ii=0;ii<na;++ii)
                        	{
                        	if(ray_end_iz[ii]==k) {imax=ii;}
				}
			iflag=na-1;
			sx=0;
			}
		else	{
			sx=(x[iflag][k]-x0)/(x[iflag][k]-x[iflag-1][k]);
			}	
			
		t2d[i][k]=(1.0-sx)*t[iflag][k] + 
					sx*t[iflag-1][k];
		sigma2d[i][k]=(1.0-sx)*sigma[iflag][k] +     
                                        sx*sigma[iflag-1][k];
		p2d[i][k]=(1.0-sx)*p3[iflag][k] +     
                                        sx*p3[iflag-1][k];
		a2d[i][k]=(1.0-sx)*a2[iflag][k] +
                                        sx*a2[iflag-1][k];
		dzda2d[i][k]=(1.0-sx)*dzda2[iflag][k] +
                                        sx*dzda2[iflag-1][k];
if(t2d[i][k]==0) fprintf(stderr,"z=%f\n",k*dz);
		}	
	free2float(p3);
	free2float(sigma);
	free2float(t);
	free2float(x);
	free2float(dzda2);
	free2float(a2);	

}

void amplitude(float fx, float fy, int nx, int ny, int nz, 
	float dx, float dy, float dz,
    	float fxo, float fyo, int nxo, int nyo, int nzo, 
	float dxo, float dyo, float dzo,
    	int na, float *v, float off, float ***t3d,float ***amp, float ***p3d)
/*****************************************************************************
Calculate Jacobian and Beylkin determinant to give the amplitude weight
******************************************************************************
Input:
fx		first x position in input data
fy		first y position in input data
nx=nxo		number of velocity grids in x
ny=nyo		number of velocity grids in y
nz		number of velocity grids in z
fxo		first x position in output
fyo		first y position in output		
nxo             number of output grids in x
nyo             number of output grids in y
nzo             number of output grids in z
dxo             output sample rate in x
dyo             output sample rate in y
dzo             output sample rate in z
off		offset
na		number of ray angles
v		1D array of velocity v(z)

Output:
t3d             3D array for traveltime
amp         3D array for amplitude weight
p3d		slowness  in z direction p_3
******************************************************************************
Author:  Meng Xu, Center for Wave Phenomena, 07/8/95
******************************************************************************/
{
        int i,j,k;
	int nxr;
	int ix,jl,jr,il,ir;

	float xi,sx;
	float r,a1,a2,a1s,a1g,a2s,a2g,temp,temp1,temp2;
	float det[3][3],ps[3],pg[3];
        float **t2d,**p2d,**sigma2d,**dzda2d,**a2d;
	float ***jacob,***asag,***h;
	float ***sigma3d,***dzda23d,***a23d;

	nxr = NINT(sqrt(nxo*dxo*nxo*dxo+nyo*dyo*nyo*dyo)/dxo)+1;

        p2d=ealloc2float(nzo,nxr);
        t2d=ealloc2float(nzo,nxr);
        sigma2d=ealloc2float(nzo,nxr);
        dzda2d=ealloc2float(nzo,nxr);
        a2d=ealloc2float(nzo,nxr);

	jacob=ealloc3float(nzo,nxo,nyo);
	asag=ealloc3float(nzo,nxo,nyo);
	h=ealloc3float(nzo,nxo,nyo);
        sigma3d=ealloc3float(nzo,nxo,nyo);
        dzda23d=ealloc3float(nzo,nxo,nyo);
        a23d=ealloc3float(nzo,nxo,nyo);


	para2d(na,nxr,nzo,dxo,dzo,v,t2d,
                sigma2d,p2d,dzda2d,a2d);

/*	for(i=0;i<nx;++i)
        for(k=0;k<nz;++k)
	{
        fprintf(stderr,"%f\n",t2d[i][k]);	
	r=sqrt(i*dx*i*dx+k*dz*k*dz);
	fprintf(stderr,"%f\n",r/v[0]);
	}
*/
	para3d(
                nxo,nyo,nzo,
                dxo,dyo,dzo,
                t3d,sigma3d,p3d,
                dzda23d,a23d,
                t2d,sigma2d,p2d,dzda2d,a2d);

	/*compute J and thus A*/
	xi=off/dxo;
	ix=xi;
	sx=xi-ix;
	
	for(i=0;i<nyo;++i)
	 for(j=0;j<nxo;++j)
	  for(k=1;k<nzo;++k) {
		if(j==0)
			a1=3.14159265/2.;
		else 
		    a1=atan(i*dyo/j*dxo);

		a2=a23d[i][j][k];
		det[0][0]=sin(a2)*cos(a1)/v[0];
		det[0][1]=sin(a1)*sin(a2)/v[0];
		det[0][2]=p3d[i][j][k];
		det[1][0]=-sigma3d[i][j][k]*sin(a1)*sin(a2)/v[0];
		det[1][1]=sigma3d[i][j][k]*cos(a1)*sin(a2)/v[0];
		det[1][2]=0.0;
		det[2][0]=sigma3d[i][j][k]*cos(a1)*cos(a2)/v[0];
		det[2][1]=sigma3d[i][j][k]*sin(a1)*cos(a2)/v[0];
		det[2][2]=dzda23d[i][j][k];
		temp=4.*PI*sqrt(v[0]*ABS(determ(det)));
		if(temp>0.0){
			jacob[i][j][k]=100.*sqrt(sin(a2))/temp;
		}	
	}
	for(i=0;i<nyo;++i)
         for(j=0;j<nxo;++j)
          for(k=0;k<nzo;++k)
		asag[i][j][k]=jacob[i][j][k]*jacob[i][ABS(j-ix)][k];

	/*compute h*/
	for(i=1;i<nyo;++i)
         for(j=0;j<nxo;++j)
          for(k=1;k<nzo;++k)
		{
		if(t3d[i][j][k]==0)
                        {
                        amp[i][j][k]=0;
                        }
                else{
		if(j==0) a1s=PI/2.;
		else a1s=atan(i*dyo/j/dxo);
		if(ABS(j*dxo-off)==0) a1g=PI/2;
		else a1g=atan(i*dyo/ABS(j*dxo-off));
		a2s=a23d[i][j][k];
		a2g=a23d[i][ABS(j-ix)][k];
		if(j==nxo-1){jl=j-1; jr=j;}
		else {jl=j-1; jr=j+1;}
		temp1=(sigma3d[i][jl][k]-sigma3d[i][jr][k])/dxo/2.;	
		if(ABS(j-ix)==0){jl=ABS(j-ix); jr=jl+1;} 
                else {jl=ABS(j-ix)-1; jr=jl+1;}    
                temp2=(sigma3d[i][jl][k]-sigma3d[i][jr][k])/dxo;
		
		ps[0]=1./v[0]*cos(a1s)*sin(a2s);
		ps[1]=1./v[0]*sin(a1s)*sin(a2s);
		ps[2]=p3d[i][j][k];
		pg[0]=1./v[0]*cos(a1g)*sin(a2g);
		pg[1]=1./v[0]*sin(a1g)*sin(a2g);
                pg[2]=p3d[i][ABS(j-ix)][k];

		det[0][0]=ps[0]+pg[0];
		det[0][1]=ps[1]+pg[1];
		det[0][2]=ps[2]+pg[2];

		det[1][0]=-(1.+ps[0]*temp1)/sigma3d[i][j][k]
			-(1.+pg[0]*temp2)/sigma3d[i][ABS(j-ix)][k];
		det[1][1]=-(ps[1]*temp1)/sigma3d[i][j][k]
			-(pg[1]*temp2)/sigma3d[i][ABS(j-ix)][k];
		det[1][2]=(ps[0]*(1.+ps[0]*temp1)/sigma3d[i][j][k]
			+ps[1]*(ps[1]*temp1)/sigma3d[i][j][k])/ps[2]
			+(pg[0]*(1.+pg[0]*temp2)/sigma3d[i][ABS(j-ix)][k]
                        +pg[1]*(pg[1]*temp2)/sigma3d[i][ABS(j-ix)][k])/pg[2];
		if(i==nyo-1){il=i-1; ir=i;}
                else {il=i-1; ir=i+1;}
                temp1=(sigma3d[il][j][k]-sigma3d[ir][j][k])/dyo/2.;
                temp2=(sigma3d[il][ABS(j-ix)][k]-sigma3d[ir][ABS(j-ix)][k])
			/2./dyo;

		det[2][0]=-(ps[0]*temp1)/sigma3d[i][j][k]
                        -(pg[0]*temp2)/sigma3d[i][ABS(j-ix)][k];
                det[2][1]=-(1.+ps[1]*temp1)/sigma3d[i][j][k]
                        -(1.+pg[1]*temp2)/sigma3d[i][ABS(j-ix)][k];
                det[2][2]=(ps[0]*(ps[0]*temp1)/sigma3d[i][j][k]
                        +ps[1]*(1.+ps[1]*temp1)/sigma3d[i][j][k])/ps[2]
                        +(pg[0]*(pg[0]*temp2)/sigma3d[i][ABS(j-ix)][k]
                        +pg[1]*(1.+pg[1]*temp2)/sigma3d[i][ABS(j-ix)][k])/pg[2];
		h[i][j][k]=determ(det);

		r=sqrt((ps[0]+pg[0])*(ps[0]+pg[0])+(ps[1]+pg[1])*(ps[1]+pg[1])
		   +(ps[2]+pg[2])*(ps[2]+pg[2]));
		amp[i][j][k]=h[i][j][k]/asag[i][j][k]/r;
		}

	}
	 for(i=1;i<nxo;++i)
          for(k=1;k<nzo;++k)
		amp[0][i][k]=amp[1][i][k]; 

	free2float(p2d);
        free2float(t2d);
        free2float(sigma2d);
        free2float(dzda2d);
        free2float(a2d);

}	



