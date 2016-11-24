/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[]={
"									",
" SUDATUMFD - 2D zero-offset Finite Difference acoustic wave-equation	",
"		 DATUMing    						",
"									",
" sudatumfd <stdin > stdout [optional parameters]			",
"									",
" Required parameters:						   	",
"									",
" nt=	   number of time samples on each trace	       			",
" nx=	   number of receivers per shot gather				",
" nsx=	  number of shot gathers				    	",
" nz=	   number of downward continuation depth steps			",
" dz=	   depth sampling interval (in meters)				",
" mx=	   number of horizontal samples in the velocity model		",
" mz=	   number of vertical samples in the velocity model		",
" vfile1=       velocity file used for thin-lens term	    		",
" vfile2=       velocity file used for diffraction term			",
" dx=           horizontal sampling interval (in meters)                ",
"									",
" Optional parameters:						   	",
"									",
" dt=.004       time sampling interval (in seconds)			",
" buff=5	number of zero traces added to each side of each   	",
"	     shot gather as a pad			       		",
" tap_len=5     taper length (in number of traces)			",
" x_0=0.0       x coordinate of leftmost position in velocity model     ",
"									",
" Notes:								",
" The algorithm is a 45-degree implicit-finite-difference scheme based  ",
" on the one-way wave equation.  It works on poststack (zero-offset)    ",
" data only.  The two velocity files, vfile1 and vfile2, are binary     ",
" files containing floats with the format v[ix][iz].  There are two     ",
" potentially different velocity files for the thin-lens and            ",
" diffraction terms to allow for the use of a zero-velocity layer       ",
" which allows for datuming from an irregular surface.                  ",
"                                                                       ",
" Source and receiver locations must be set in the header values in     ",
" order for the datuming to work properly.  The leftmost position of    ",
" of the velocity models given in vfile1 and vfile2 must also be given. ",
" 									",
" 									",
NULL};

/* 
 * Author:  Chris Robinson, 10/16/00, CWP, Colorado School of Mines
 *
 *
 * References:
 *  Beasley, C., and Lynn, W., 1992, The zero-velocity layer: migration
 *    from irregular surfaces: Geophysics, 57, 1435-1443.
 *
 *  Claerbout, J. F., 1985, Imaging the earth's interior:  Blackwell
 *    Scientific Publications.
 *
 */

/**************** end self doc *******************************************/

void tlens(complex ***ifr,complex ***ifr1,int nsx,int nx_pad,int nw,
	   float dw,float dz,float v,float dx,float **vel,
	   int zvel,int **gx,int buff,float x_0);

void diff(complex ***ifr1,complex ***ifr2,int nsx,int nx_pad,int nw,
	   float dw,float dz,float v,float dx,float **vel,
	   int zvel,int **gx,int buff,float x_0);

void padsub(complex ***ifr,complex ***ifr_pad,int nsx,int nx,
	    int nw,int nx_pad,int buff); 

void unpadsub(complex ***ifr,complex ***ifr_pad,int nsx,int nx,
	      int nw,int nx_pad,int buff);

void taper(complex ***ifr_pad,complex ***ifr_pt,int nsx,int nx_pad,
	   int nw,float dw,int tap_len,int buff,int nx);

segy tr;

int main(int argc, char **argv)
{
   FILE *vfp1,*vfp2;
   int nt,nz,nx,nx_pad,nsx;
   int mx,mz;
   int it,ix,isx,j;
   int **gx,**sx,**cdp,**offset;
   int buff,tap_len;

   float dt,dx,dz,x_0;
   float ***intime,***outtime;
   complex ***ifr,***ofr,***ofr1,***ifr3;
   complex ***ifr1,***ifr2;
   float v,**vel1,**vel2;

   int ntfft,nw,iw;
   float dw,*storage1,*p1,*p2,pi=PI;
   complex *cp1,*cp2;

   char *vfile1="";
   char *vfile2="";

   initargs(argc,argv);
   requestdoc(1);

   if (!getparint("nt",&nt)) err("must specify nt");
   if (!getparint("nx",&nx)) err("must specify nx");
   if (!getparint("nz",&nz)) err("must specify nz");
   if (!getparint("nsx",&nsx)) err("must specify nsx");
   if (!getparfloat("v",&v)) v=1000.0;
   if (!getparfloat("dz",&dz)) err("must specify dz");
   if (!getparfloat("x_0",&x_0)) err("must specify x_0");
   if (!getparint("buff",&buff)) err("must specify buff");
   if (!getparint("tap_len",&tap_len)) err("must specify tap_len");
   if (!getparint("mx",&mx)) err("must specify mx");
   if (!getparint("mz",&mz)) err("must specify mz");
   if (!getparstring("vfile1",&vfile1)) err("must specify vfile1");
   if (!getparstring("vfile2",&vfile2)) err("must specify vfile2");

   vfp1 = fopen(vfile1,"r");   
   vel1 = alloc2float(mz,mx);
   fread(vel1[0],sizeof(float),mz*mx,vfp1);
   fclose(vfp1);
   vfp2 = fopen(vfile2,"r");
   vel2 = alloc2float(mz,mx);
   fread(vel2[0],sizeof(float),mz*mx,vfp2);
   fclose(vfp2);

   nx_pad = nx+2*buff;
  
   intime = alloc3float(nt,nsx,nx);

   /*********************Initialize Array*******************/
   for(ix=0;ix<nx;ix++)
   for(it=0;it<nt;it++)
   for(isx=0;isx<nsx;isx++)
   {
      intime[ix][isx][it] = 0.0;
   }
   /*********************************************************/

   gx = alloc2int(nsx,nx);
   sx = alloc2int(nsx,nx); 
   cdp = alloc2int(nsx,nx);
   offset = alloc2int(nsx,nx);

   /*********************Read Traces into Array******************/
   ix=0; isx=0;
   while(gettr(&tr)&&ix<nx)
   {
      nt = (int) tr.ns;
      if(!getparfloat("dt",&dt))
      {
	 if(tr.dt)
	 {
	    dt = ((double) tr.dt)/1000000.0;
	 }
	 else
	 {
	    dt = 0.004;
	    warn("tr.dt not set, assuming dt=0.004");
	 }
      }/*end if*/
      if(!getparfloat("dx",&dx))
      {
	 if(tr.d2)
	 {
	    dx = tr.d2;
	 }
	 else
	 {
	    dx=1.0;
	    warn("tr.d2 not set, assuming d2=1.0");   
	 }
      }/*end if*/
      checkpars();
      gx[ix][isx] = tr.gx;
      sx[ix][isx] = tr.sx;
      cdp[ix][isx] = tr.cdp;
      offset[ix][isx] = tr.offset;
      memcpy( (void *) intime[ix][isx], (void *) tr.data, nt*4);
      ix++;
      if(ix==nx) {ix=0; isx++;}
   }/*end while*/
   /**********************End Read Traces*****************************/


   /*******************Data Processing*******************************/
   /*****To set outtime=intime without looping use the following*****/
   /*memcpy((void *) &outtime[0][0][0],(void *) &intime[0][0][0],nx*nsx*nt*4);*/

   /**************Temporal Fourier Transform Data*******************/ 
   ntfft = npfar(2*nt);
   nw=ntfft/2+1;
   dw=2.0*pi/((ntfft-1)*dt);

   ifr = alloc3complex(nw,nsx,nx);
   ofr = alloc3complex(nw,nsx,nx_pad);

   for(isx=0;isx<nsx;isx++)
   {
      for(ix=0;ix<nx;ix++)
      {
	 storage1 = alloc1float(nt);
	 for(it=0;it<nt;it++) storage1[it] = intime[ix][isx][it];
	 p1 = alloc1float(ntfft);
	 cp1 = alloc1complex(nw);
	 memcpy(p1,storage1,nt*4);
	 for(it=nt;it<ntfft;it++) p1[it]=0.0;
	 pfarc(1,ntfft,p1,cp1);
	 for(iw=0;iw<nw;iw++)
	 {
	    ifr[ix][isx][iw] = cp1[iw];
	 }
	 free1float(storage1);
	 free1float(p1);
	 free1complex(cp1);
      }
   }
   free3float(intime);
   /*********End Temporal Fourier Transform**************************/

   ifr1 = alloc3complex(nw,nsx,nx_pad);
   ifr2 = alloc3complex(nw,nsx,nx_pad);

   padsub(ifr,ifr1,nsx,nx,nw,nx_pad,buff);
   free3complex(ifr);

   for(j=0;j<nz;j++)
   {
      /*warn("j=%d\n",j);*/
      taper(ifr1,ifr2,nsx,nx_pad,nw,dw,tap_len,buff,nx);
      tlens(ifr2,ifr1,nsx,nx_pad,nw,dw,dz,v,dx,vel1,
	    j,gx,buff,x_0);
      diff(ifr1,ifr2,nsx,nx_pad,nw,dw,dz,v,dx,vel2,
	   j,gx,buff,x_0);
      memcpy((void *) &ifr1[0][0][0],(void *) &ifr2[0][0][0],nx_pad*nsx*nw*8);
   }
   free3complex(ifr1);

   ifr3 = alloc3complex(nw,nsx,nx);
   unpadsub(ifr2,ifr3,nsx,nx,nw,nx_pad,buff);
   free3complex(ifr2);

   ofr1 = alloc3complex(nw,nsx,nx);
   memcpy((void *) &ofr1[0][0][0],(void *) &ifr3[0][0][0],nx*nsx*nw*8);
   free3complex(ifr3);

   /*********Temporal Inverse Fourier Transform Data*****************/
   outtime = alloc3float(nt,nsx,nx);
   for(isx=0;isx<nsx;isx++)
   {
      for(ix=0;ix<nx;ix++)
      {
	 p2 = alloc1float(ntfft);
	 cp2 = alloc1complex(nw);
	 for(iw=0;iw<nw;iw++) 
	 {
	    cp2[iw] = ofr1[ix][isx][iw];
	 }
	 pfacr(-1,ntfft,cp2,p2);
	 for(it=0;it<nt;it++) outtime[ix][isx][it] = p2[it]/(1.0*ntfft); 
	 free1float(p2);
	 free1complex(cp2);
      }
   }
   free3complex(ofr);
   /*********End Temporal Inverse Fourier Transform********************/



   /**********************End Data Processing*************************/


   /******************Output Data into Trace Format******************/
   for(isx=0;isx<nsx;isx++)
   {
      for(ix=0;ix<nx;ix++)
      {
	 tr.ns = nt;
	 tr.dt = dt*1000000;
	 tr.gx = gx[ix][isx];
	 tr.sx = sx[ix][isx];
	 tr.cdp = cdp[ix][isx];
	 tr.offset = offset[ix][isx];
	 memcpy( (void *) tr.data, (void *) outtime[ix][isx], nt*4);
	 puttr(&tr);
      }
   }
   free3float(outtime);
   /*****************************************************************/

   free2int(gx);
   free2int(sx);
   free2int(cdp);
   free2int(offset);

   return(CWP_Exit());
}




/**************************thin lens subroutine*****************************/
void tlens(complex ***val_ifr_pad,complex ***val_ifr1,int val_nsx,int val_nx_pad,
	   int val_nw,float val_dw,float val_dz,float val_v,float val_dx,
	   float **val_vel,int val_zlev,
	   int **val_gx,int val_buff,float val_x_0)
{
  int i,j,k,a;
  complex mult;
  float ig;
  a = 0;
  for(i=0;i<val_nsx;i++)
  {
    for(j=0;j<val_nx_pad;j++)
    {
      if(j<val_buff)
      {
	a = val_gx[0][i];
      }
      if(j>val_nx_pad-1-val_buff)
      {
	a = val_gx[val_nx_pad-1-2*val_buff][i];
      }
      if(j>=val_buff && j<=val_nx_pad-1-val_buff)
      {
	a = val_gx[j-val_buff][i];
      }
      val_v = 0.5*val_vel[(int)((a-val_x_0)/val_dx)][val_zlev];
      ig = (-1.0/val_v)*val_dz;
      for(k=0;k<val_nw;k++)
      {
	 mult = cmplx(cos(k*val_dw*ig),sin(k*val_dw*ig));
	 val_ifr1[j][i][k] = cmul(val_ifr_pad[j][i][k],mult);
      }
    }
  }
}
/*************************************************************************/



/*************************diffraction term subroutine********************/
void diff(complex ***val_ifr1,complex ***val_ifr2,int val_nsx,int val_nx_pad,
	  int val_nw,float val_dw,float val_dz,float val_v,float val_dx,
	  float **val_vel,int val_zlev,
	  int **val_gx,int val_buff,float val_x_0)
{
  int i,j,k,m,n,a;
  float w;
  complex *e,*f,*d,*ohm,*alpha,*beta,*gamma;
  complex num1,num2,num3,num4,num5;
  complex num6,num7,num8,num9,num10,num11;
  float sigma,tau;

  sigma = 0.5;
  tau = 0.25;

  e = alloc1complex(val_nx_pad);
  f = alloc1complex(val_nx_pad);
  d = alloc1complex(val_nx_pad);
  ohm = alloc1complex(val_nx_pad);
  alpha = alloc1complex(val_nx_pad);
  beta = alloc1complex(val_nx_pad);
  gamma = alloc1complex(val_nx_pad);

  a = 0;
  for(i=0;i<val_nsx;i++)
  {
    for(k=0;k<val_nw;k++)
    {
      w = k*val_dw;
      if(w<5.0) w=5.0;
      for(n=0;n<val_nx_pad;n++)
      {
	if(n<val_buff)
	{
	  a = val_gx[0][i];
	}
	if(n>val_nx_pad-1-val_buff)
	{
	  a = val_gx[val_nx_pad-1-2*val_buff][i];
	}
	if(n>=val_buff && n<=val_nx_pad-1-val_buff)
	{
	  a = val_gx[n-val_buff][i];
	}
	val_v = 0.5*val_vel[(int)((a-val_x_0)/val_dx)][val_zlev];
	alpha[n] = cmplx(0.0,-1.0*val_v*sigma*val_dz/(val_dx*val_dx*2.0*w));
	beta[n] = cmul(alpha[n],cadd(cmplx(1.0,0.0),
		  cmplx(0.0,-2.0*val_v*tau/(w*sigma*val_dz))));
	gamma[n] = cmul(alpha[n],cadd(cmplx(1.0,0.0),
		  cmplx(0.0,2.0*val_v*tau/(w*sigma*val_dz))));
      }
      /**********************Calculate d[n]'s**********************/
      num6 = cmul(cadd(cmplx(1.0,0.0),cmul(cmplx(-2.0,0.0),gamma[0]))
		  ,val_ifr1[0][i][k]);
      num7 = cmul(gamma[0],val_ifr1[1][i][k]);
      d[0] = cadd(num6,num7);
      ohm[0] = cmul(cmplx(-1.0,0.0),cdiv(d[0],beta[0])); 
      for(n=1;n<(val_nx_pad-1);n++)
      {
	num3 = cmul(gamma[n],val_ifr1[n-1][i][k]);
	num4 = cmul(cadd(cmplx(1.0,0.0),cmul(cmplx(-2.0,0.0),gamma[n]))
		    ,val_ifr1[n][i][k]);
	num5 = cmul(gamma[n],val_ifr1[n+1][i][k]);

	d[n] = cadd(num3,cadd(num4,num5));
	ohm[n] = cmul(cmplx(-1.0,0.0),cdiv(d[n],beta[n]));
      }
      num8 = cmul(gamma[val_nx_pad-1],val_ifr1[val_nx_pad-2][i][k]);
      num9 = cmul(cadd(cmplx(1.0,0.0),cmul(cmplx(-2.0,0.0),gamma[val_nx_pad-1]))
		  ,val_ifr1[val_nx_pad-1][i][k]);
      d[val_nx_pad-1] = cadd(num8,num9);
      ohm[val_nx_pad-1] = cmul(cmplx(-1.0,0.0),cdiv(d[0],beta[0]));
      /***********************************************************/ 
      /*******************Calculate e's and f's*******************/
      e[1] = cadd(cdiv(cmplx(1.0,0.0),beta[1]),cmplx(2.0,0.0));
      f[1] = ohm[1];
      for(m=2;m<val_nx_pad-1;m++)
      {
	 num1 = cadd(cdiv(cmplx(1.0,0.0),beta[m]),cmplx(2.0,0.0));
	 num2 = cdiv(cmplx(-1.0,0.0),e[m-1]);
	 e[m] = cadd(num1,num2);
	 f[m] = cadd(ohm[m],cdiv(f[m-1],e[m-1]));
      }
      e[0] = cmplx(0.0,0.0);
      e[val_nx_pad-1] = cmplx(0.0,0.0);
      /**********************************************************/
      /*******Downward continue one freq. on one shot gather*****/
      val_ifr2[val_nx_pad-1][i][k] = cmplx(0.0,0.0);
      num10 = cdiv(f[val_nx_pad-2],e[val_nx_pad-2]);
      val_ifr2[val_nx_pad-2][i][k] = cmul(cmplx(-1.0,0.0),num10);
      for(j=val_nx_pad-3;j>0;j--)
      {
	 num11 = cadd(val_ifr2[j+1][i][k],cmul(cmplx(-1.0,0.0),f[j]));
	 val_ifr2[j][i][k] = cdiv(num11,e[j]);
      }
      val_ifr2[0][i][k] = cmplx(0.0,0.0);
      /**********************************************************/
    }/*end freq loop*/
  }/*end source loop*/

  free1complex(e);
  free1complex(f);
  free1complex(d);
  free1complex(ohm);
  free1complex(alpha);
  free1complex(beta);
  free1complex(gamma);
}
/***********************************************************************/





/*******************Padding subroutine********************************/
void padsub(complex ***val_ifr, complex ***val_ifr_pad,int val_nsx,
	    int val_nx,int val_nw,int val_nx_pad,int val_buff)
{
  int i,j,k;

  for(i=0;i<val_nsx;i++)
  {
    for(j=0;j<val_nx_pad;j++)
    {
      for(k=0;k<val_nw;k++)
      {
	if(j<val_nx+val_buff && j>-1+val_buff )
	  val_ifr_pad[j][i][k] = val_ifr[j-val_buff][i][k];
	else 
	{
	  val_ifr_pad[j][i][k] = cmplx(0.0,0.0); 
	}
      }
    }
  }
}
/*********************************************************************/



/*********************Unpadding routine*******************************/
void unpadsub(complex ***val_ifr, complex ***val_ifr_upad,int val_nsx,
	      int val_nx,int val_nw,int val_nx_pad,int val_buff)
{
  int i,j,k;

  for(i=0;i<val_nsx;i++)
  {
    for(j=0;j<val_nx;j++)
    {
      for(k=0;k<val_nw;k++)
      {
	val_ifr_upad[j][i][k] = val_ifr[j+val_buff][i][k];
      }
    }
  }
}
/********************************************************************/



/****************Tapering subroutine**********************************/
void taper(complex ***val_ifr_pad,complex ***val_ifr_pt,int val_nsx,
	   int val_nx_pad,int val_nw,float val_dw,int val_tap_len,
	   int val_buff,int val_nx)
{
  int i,j,k,m;
  complex *tap_func;
  float pi=3.1415927;

  tap_func = alloc1complex(val_nx_pad);

  if(val_tap_len==0)
  {
    for(m=0;m<val_nx_pad;m++) tap_func[m]=cmplx(1.0,0.0);
  }
  if(val_tap_len>0)
  {
    for(m=0;m<val_nx_pad;m++)
    {
      if(m<val_buff) tap_func[m]=cmplx(0.0,0.0);
      if(m>=val_buff && m<(val_buff+val_tap_len-1))
      {
	tap_func[m]=cmplx(sin(2.0*pi*(m+1-val_buff)/(4.0*val_tap_len)),0.0);
      }
      if(m>=(val_buff+val_tap_len-1) && m<=(val_buff+val_nx-1-(val_tap_len-1)))
      {
	tap_func[m]=cmplx(1.0,0.0);
      }
      if(m>(val_buff+val_nx-1-(val_tap_len-1)) && m<=(val_buff+val_nx-1))
      {
	tap_func[m]=cmplx(cos(2.0*pi*(m-(val_buff+val_nx-1-(val_tap_len-1)))/
			  (4.0*val_tap_len)),0.0);
      }
      if(m>(val_buff+val_nx-1)) tap_func[m]=cmplx(0.0,0.0);
    }
  }

  for(i=0;i<val_nsx;i++)
  {
    for(j=0;j<val_nx_pad;j++)
    {
      for(k=0;k<val_nw;k++)
      {
	 val_ifr_pt[j][i][k] = cmul(tap_func[j],val_ifr_pad[j][i][k]);
      }
    }
  }

  free1complex(tap_func);
}
/********************************************************************/
