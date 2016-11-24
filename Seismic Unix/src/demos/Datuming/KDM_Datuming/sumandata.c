/* MANDATA: $Revision: 0.00 $ ; $Date: 1997/12/22 21:49:05 $	*/

#include "su.h" 
#include "segy.h" 

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" MANDATA.C  -  Synthetic seismograms from horizontal reflectors in     ",
"               constant velocity media.                                ",
" 									",
" MANDATA >outfile  [optional parameters]	                 	",
" 									",
" Required Parameters:							",
" >outfile      file containing seismograms                      	",
" 									",
" Optional Parameters:							",
" nt=501        	number of time samples				",
" dt=0.004      	time sampling interval (sec)			",
" ft=0.0        	first time (sec)				",
" fpeak=0.2/dt		peak frequency of symmetric Ricker wavelet (Hz)	",
" nxg=			number of receivers of input traces		",
" dxg=15		receiver sampling interval (m)			",
" fxg=0.0		first receiver (m)				",
" dxs=50		shot sampling interval (m)			",
" fxs=0.0		first shot (m)				        ",
" dx=50         	x sampling interval (m)				",
" fx=0.         	first x sample (m)				",
" dz=50         	z sampling interval (m)				",
" zdat=0                level to put receivers on                       ",
" zref                  depth of horizontal reflector                   ",
" v=2000                medium velocity                                 ",
" ls=0			=1 for line source; =0 for point source		",
" verbose=0		=1 to print some useful information		",
" 									",
" Notes:								",
"									",
NULL};

/*
 * Trace header fields set: trid, counit, ns, dt, delrt,
 *				tracl. tracr, fldr, tracf,
 *				sx, gx
 */
/**************** end self doc ***********************************/



/* segy trace */
segy tr;

int
main (int argc, char **argv)
{
	int 	it,ls,ixs,ixg,nxs,nxg,nt,
		verbose,tracl;
	float   zref,zdat,v,h,hs,hb,*temp,rs,rg,time,amp,
		fpeak,dx,dz,fx,
		dxg,dxs,dt,fxg,fxs,ft,xs,xg,emang;
	Wavelet *w;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters */
	
	if (!getparint("nt",&nt)) nt = 501; CHECK_NT("nt",nt);
	if (!getparfloat("dt",&dt)) dt = 0.004;
	if (!getparfloat("ft",&ft)) ft = 0.0;
	if (!getparfloat("fpeak",&fpeak)) fpeak = 0.2/dt;
	if (!getparint("nxg",&nxg)) nxg = 101;
	if (!getparfloat("dxg",&dxg)) dxg = 15;
	if (!getparfloat("fxg",&fxg)) fxg = 0.0;
	if (!getparint("nxs",&nxs)) nxs = 1;
	if (!getparfloat("dxs",&dxs)) dxs = 50;
	if (!getparfloat("fxs",&fxs)) fxs = 0.0;
	if (!getparfloat("dx",&dx)) dx = 50;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("dz",&dz)) dz = 50;
	if (!getparint("ls",&ls)) ls = 0;
	if (!getparint("verbose",&verbose)) verbose = 0;

        if (!getparfloat("v",&v)) v=2000;
        if (!getparfloat("zdat",&zdat)) zdat=0.0;
        if (!getparfloat("zref",&zref)) err("Must specify zref!\n");
	

	/* make wavelet */
	makericker(fpeak,dt,&w);
	
	
	/* set constant segy trace header parameters */
	memset((void *) &tr, (int) '\0', sizeof(tr));
	tr.trid = 1;
	tr.counit = 1;
	tr.ns = nt;
	tr.dt = 1.0e6*dt;
	tr.delrt = 1.0e3*ft;
	tr.d2 = dxg;

        
	
	/* loop over shots */

	for (ixs=0, tracl=0; ixs<nxs; ++ixs){
	    xs = fxs+ixs*dxs;

	    
	    /* loop over receivers */
	    for (ixg=0; ixg<nxg; ++ixg){
		xg = xs + fxg+ixg*dxg;


                /* create arrays and zero */

                temp=ealloc1float(nt);
                for (it=0; it<nt; ++it)
                      temp[it] = 0.0;

        	for (it=0; it<nt; ++it)
                      tr.data[it] = 0.0;
        

        	/* two-way time and amplitude */


        	h = xg-xs;
		
                hs = h*(zref-zdat)/((2*zref)-zdat);
                hb = h*(zref)/((2*zref)-zdat);

                rs = sqrt((hb*hb)+(zref*zref));
                rg = sqrt((hs*hs)+((zref-zdat)*(zref-zdat)));

                time = (rs+rg)/v;

                emang = (zref-zdat)/rg;

        	if (ls)
        	     amp = (1/(4*PI))*(1/(rs+rg));
        	else
        	     amp = (1/(4*PI))*(1/(rs+rg));
               
 
        	/* add sinc wavelet to trace */
        	addsinc(time,amp,nt,dt,ft,temp);     
        
        	/* convolve wavelet with trace */
        	conv(w->lw,w->iw,w->wv,nt,0,temp,nt,0,tr.data);               
        
        	/* free workspace */
        	free1float(temp);


		/* set segy trace header parameters */
		tr.tracl = tr.tracr = ++tracl;
		tr.sx = NINT(xs);
		tr.gx = NINT(xg);
		/* write trace */
		puttr(&tr);

	    }
	    warn("\t finish shot %f",xs);
	}

	return EXIT_SUCCESS;
}
