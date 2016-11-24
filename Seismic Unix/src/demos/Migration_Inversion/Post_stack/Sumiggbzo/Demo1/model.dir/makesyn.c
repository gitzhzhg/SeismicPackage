#include "par.h"

char *sdoc[] = {
"MAKESYN - MAKE SYNthetic data p(x,y,t)					",
"									",
"makesyn >outfile nx= nt= [optional parameters]				",
"									",
"Required Parameters:							",
"nx=                    number of x samples (3rd dimension)		",
"nt=                    number of t samples (1st dimension)		",
"									",
"Optional Parameters:							",
"dx=1.0                 x sampling interval				",
"fx=0.0                 first x sample					",
"dt=1.0                 t sampling interval				",
"ft=0.0                 first t sample					",
"fmax=0.25/dt           maximum frequency (default is half Nyquist)	",
NULL};

main (int argc, char **argv)
{
	int nx,ny,nt,ix,iy,it;
	float dx,dy,dt,fx,fy,ft,fmax,x,y,t,*p;
	FILE *outfp=stdout;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!\n");
	if (!getparint("nt",&nt)) err("must specify nt!\n");
	
	/* get optional parameters */
	if (!getparfloat("dx",&dx)) dx = 1.0;
	if (!getparfloat("dt",&dt)) dt = 1.0;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparfloat("fmax",&fmax)) fmax = 0.25/dt;
	
	/* allocate space */
	p = alloc1float(nt);
	
	/* compute and write data */
	for (ix=0,x=fx; ix<nx; ++ix,x+=dx) {
		for (it=0; it<nt; ++it)
			p[it] = 0.0;
		if (ix==2*nx/3) {
			for (it=0,t=ft; it<nt; ++it,t+=dt) {
				p[it] = 1.0-2.0*franuni();
			}
		}
		bflowpass(6,fmax*dt,nt,p,p);
		fwrite(p,sizeof(float),nt,outfp);
	}
	
	/* free space before returning */
	free1float(p);
	return 0;
}
