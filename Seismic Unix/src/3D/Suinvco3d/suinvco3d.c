/* SUINVCO3D: $Revision: 1.1 $ ; $Date: 2001/03/14 18:14:10 $	*/

#include "su.h"
#include "segy.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" SUINVCO3D - Seismic INVersion of Common Offset data with V(X,Y,Z) velocity	",
"	     function in 3D							",
" 										",
"     suinvco3d <infile >outfile [optional parameters] 				",
"										",
" Required Parameters:								",
" vfile=		  file containing velocity array v[nvy][nvx][nvz]	",
" nzv=		   number of z samples (1st dimension) in velocity		",
" nxm=			number of midpoints of input traces			",
" nym=			number of lines 					",
" geo_type=		geometry type						",
"			1 ---- general velocity distribution v(x,y,z)		",
"			2 ---- v(x,z) medium					",
"			3 ---- v(z) medium					",
" com_type=		computation type, determines what tables are needed	",
"			1 ---- only needs traveltime,	   weight=1.0		",
"			2 ---- traveltime, propagation angles,  weight=ctheta	",
"			3 ---- traveltime, angle and amplitude,			",
"						  weight=det/as/ag/(1+ctheta)	",
" nzt=		   number of z samples (1st dimension) in traveltime		",
" nxt=		   number of x samples (2nd dimension) in traveltime		",
" nyt=		   number of y samples (3rd dimension) in traveltime		",
" tfile		  file containing traveltime array t[nyt][nxt][nzt]		",
" ampfile		file containing amplitude array amp[nyt][nxt][nzt]	",
" d21file		file containing Beylkin determinant component array	",
" d22file		file containing Beylkin determinant component array	",
" d23file		file containing Beylkin determinant component array	",
" d31file		file containing Beylkin determinant component array	",
" d32file		file containing Beylkin determinant component array	",
" d33file		file containing Beylkin determinant component array	",
" a1file		 file containing ray propagation angle (polar) array	",
" b1file		 file containing ray propagation angle (azimuth) array	",
"										",
" Optional Parameters:								",
" dt= or from header (dt) 	time sampling interval of input data		",
" offs= or from header (offset) 	source-receiver offset 			",
" dxm= or from header (d2) 	x sampling interval of midpoints 		",
" fxm=0		  first midpoint in input trace					",
" dym=50.0		y sampling interval of midpoints 			",
" fym=0		  y-coordinate of first midpoint in input trace			",
" nxv=		   number of x samples (2nd dimension) in velocity		",
" nyv=		   number of y samples (3rd dimension) in velocity		",
" dxv=50.0		x sampling interval of velocity				",
" fxv=0.0		first x sample of velocity				",
" dyv=50.0		y sampling interval of velocity				",
" fyv=0.0		first y sample of velocity				",
" dzv=50.0		z sampling interval of velocity				",
" fzv=0.0		first z sample of velocity				",
" nxb=nx/2		band centered at midpoints (see note)			",
" fxo=0.0		x-coordinate of first output trace 			",
" dxo=15.0		horizontal spacing of output trace 			",
" nxo=101		number of output traces 				",	
" fyo=0.0		y-coordinate of first output trace			",
" dyo=15.0		y-coordinate spacing of output trace			",
" nyo=101		number of output traces in y-direction			",
" fzo=0.0		z-coordinate of first point in output trace 		",
" dzo=15.0		vertical spacing of output trace 			",
" nzo=101		number of points in output trace			",	
" dxt=100.0		x-coordinate spacing of input tables(traveltime, etc)	",
" dyt=100.0		y-coordinate spacing of input tables(traveltime, etc)	",
" dzt=100.0		z-coordinate spacing of input tables(traveltime, etc)	",
" xt0=0.0		x-coordinate of first input tables			",
" xt1=0.0		x-coordinate of last input tables			",
" yt0=0.0		y-coordinate of first input tables		 	",
" yt1=0.0		y-coordinate of last input tables			",
" fmax=0.25/dt		Maximum frequency set for operator antialiasing		",
" ang=180		Maximum dip angle allowed in the image			",
" apet=45		aperture open angle for summation			",
" alias=0		=1 to set the anti-aliasing filter			",
" verbose=1		=1 to print some useful information			",
"										",
" Notes:									",
"										",
" The information needed in the computation of the weighting factor		",
" in Kirchhoff inversion formula includes traveltime, amplitude, 		",
" and Beylkin determinant at each grid point for each source/receiver		",
" point. For a 3-D nonzero common-offset inversion, the Beylkin			",
" determinant is computed from a 3x3 matrix with each element 			",
" containing a sum of quantities from the source and the receiver.		",
" The nine elements in the Beylkin matrix for each source/receiver		",
" can be determined by eight quantities. These quantities can be		",
" computed by the dynamic ray tracer. Tables of traveltime, amplitude,		",
" and Beylkin matrix elements from each source and receiver are			",
" pre-computed and stored in files.						",
"										",
" For each trace, tables of traveltime, amplitude and Beylkin matrix		",
" at the source and receiver location are input or interpolated from		",
" neighboring tables. For the computation of weighting factor, linear		",
" interpolation is used to determine the weighting factor at each		",
" output grid point, and weighted diffraction summation is then 		",
" applied. For each midpoint, the traveltimes and weight factors are		",
" calculated in the horizontal range of (xm-nxb*dx-z*tan(apet),			",
" xm+nxb*dx+z*tan(apet)).							",
"										",
" Offsets are signed - may be positive or negative. 				", 
"										",
NULL};

/*
 * This algorithm is based on the inversion formulas in chaper 5 of
 * _Mathematics of Multimensional Seismic Migration, Imaging and Inversion_ 
 * (Springer-Verlag, 2000), by Bleistein, N., Cohen, J.K.
 * and Stockwell, Jr., J. W.
 */
/**************** end self doc ***********************************/

#define TINY 0.0000001 /*avoid devide by zero*/
#define LARGE 1000000

void GetFileNameType1(char *infile,float xs,float ys,float xs0,float xs1,float ys0,float ys1,char *xsfile); 
void GetFileNameType2(char *infille,float xs,float xs0,float xs1,char *xsfile);
short ReadTable(char* infile,int nxt,int nyt,int nzt,float*** outArray); 
short ReadVelocity(char *vfile,int nxv,int nyv,int nzv,float fxv,float dxv,
float fyv,float dyv,float fzv,float dzv,int nxo,int nyo,int nzo,float fxo,
float dxo, float fyo,float dyo,float fzo,float dzo,float ***vout); 

/*segy trace */ 
segy tr, tro;

int
main (int argc, char **argv)
{
	int 	ixm,nxm,iym,nt,nxo,nyo,nzo,ixo,iyo,izo,nsamp,
		nym,nxb,i,ix,ixg,iy,iyg,iz,
		verbose,alias,geo_type,com_type,
		nxt,nyt,nzt,*aperture,nxv,nyv,nzv;
	float   temp,xo,yo,zo,xi,xig,zi,sx,sz,sxg,tsd,tgd,yi,sy,yig,syg,
		xs_beg,xg_beg,ys_beg,fym,
		amps,ampg,weight,
		dym,samp,smax,fmax,ang,cosa,
		dxm,dt,fxm,fxo,dxo,fyo,dyo,fzo,dzo,offs,xm,
		odt, smaxx,smaxy,
		dxt,dyt,dzt,apet,dxv,dyv,dzv,fxv,fyv,fzv,
		pxs,pys,pzs,pxg,pyg,pzg, 
		a1s,a1g,b1s,b1g,
		d11,d12,d13,d21,d22,d23,d31,d32,d33,
		d21s,d22s,d23s,d31s,d32s,d33s,
		d21g,d22g,d23g,d31g,d32g,d33g,
		ctheta,det,aliasx,aliasy,
		xs,xg,ys,vs,xt0,xt1,yt0,yt1,***outtrace=NULL, 
		***txs=NULL,***txg=NULL,***ampxs=NULL,***ampxg=NULL,
		***a1xs=NULL,***a1xg=NULL,***b1xs=NULL,***b1xg=NULL,
		***d21xs=NULL,***d21xg=NULL,***d22xs=NULL,
		***d22xg=NULL,***d23xs=NULL,***d23xg=NULL,
		***d31xs=NULL,***d31xg=NULL,***d32xs=NULL,
		***d32xg=NULL,***d33xs=NULL,***d33xg=NULL,***vout=NULL;
	char *vfile="";
	char *tfile="";
	char *ampfile="";
	char *d21file="";
	char *d22file="";
	char *d23file="";
	char *d31file="";
	char *d32file="";
	char *d33file="";
	char *a1file="";
	char *b1file="";

	char xsfile[80];
	char xgfile[80];

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
	
	/* get parameters for input data*/
	if (!getparint("geo_type",&geo_type)) 
			err("must specify geometry type!\n");
	/* 1---general velocity distribution v(x,y,z)
	   2---v(x,z) medium
	   3---v(z) medium  */
	if (!getparint("com_type",&com_type)) 
			err("must specify computation type!\n");
	/* 1---only needs traveltime table, weight=1.0
	   2---traveltime, a1, b1 tables,   weight=ctheta
	   3---everything		   weight=det/as/ag/(1+ctheta) */  
	if (!getparint("nym",&nym)) err("must specify nym!\n");
	if (!getparint("nxm",&nxm)) err("must specify nxm!\n");
	if (!getparfloat("fxm",&fxm)) fxm = 0.0;
	if (!getparfloat("dxm",&dxm)) dxm = 50.;
	if (!getparfloat("fym",&fym)) fym = 0.0;
	if (!getparfloat("dym",&dym)) dym=50.;

	/* get parameters for velocity model*/
	if (!getparint("nyv",&nyv)) {
	    if (geo_type ==1 ) err("must specify nyv!\n");
	    else nyv =1;
	}
	if (!getparint("nzv",&nzv)) err("must specify nzv!\n");
	if (!getparint("nxv",&nxv)) {
	    if (geo_type == 1 || geo_type ==2) err("must specify nxv!\n");
	    else nxv =1;
	}
	if (!getparfloat("fxv",&fxv)) fxv = 0.0;
	if (!getparfloat("dxv",&dxv)) dxv = 50.;
	if (!getparfloat("fyv",&fyv)) fyv = 0.0;
	if (!getparfloat("dyv",&dyv)) dyv=50.;
	if (!getparfloat("fzv",&fzv)) fzv = 0.0;
	if (!getparfloat("dzv",&dzv)) dzv = 50.;
	if (!getparstring("vfile",&vfile)) err("must specify vfile!\n");
	
	/* get required parameters about wavefied tables*/
	if (!getparstring("tfile",&tfile)) err("must specify timefile!\n");
	if (!getparstring("ampfile",&ampfile) && com_type ==3) 
				err("must specify ampfile!\n");
	if (!getparstring("d21file",&d21file) && com_type ==3) 
				err("must specify d21file!\n");
	if (!getparstring("d22file",&d22file) && com_type ==3) 
				err("must specify d22file!\n");
	if (!getparstring("d23file",&d23file) && com_type ==3) 
				err("must specify d23file!\n");
	if (!getparstring("d31file",&d31file) && com_type ==3) 
				err("must specify d31file!\n");
	if (!getparstring("d32file",&d32file) && com_type ==3) 
				err("must specify d32file!\n");
	if (!getparstring("d33file",&d33file) && com_type ==3) 
				err("must specify d33file!\n");
	if (!getparstring("a1file",&a1file))  
	    if (com_type ==2 || com_type ==3  ) 
				err("must specify a1file!\n"); 
	if (!getparstring("b1file",&b1file)) 
	    if (com_type ==2 || com_type ==3  ) 
				err("must specify b1file!\n"); 

	if (!getparint("nyt",&nyt)) err("must specify nyt!\n");
	if (!getparint("nzt",&nzt)) err("must specify nzt!\n");
	if (!getparint("nxt",&nxt)) err("must specify nxt!\n");
	if (!getparfloat("dyt",&dyt)) dyt = 100.0;
	if (!getparfloat("dzt",&dzt)) dzt = 100.0;
	if (!getparfloat("dxt",&dxt)) dxt = 100.0;
	if (!getparfloat("xt0",&xt0)) xt0 = 0.0;
	if (!getparfloat("xt1",&xt1)) xt1 = 0.0;
	if (!getparfloat("yt0",&yt0)) yt0 = 0.0;
	if (!getparfloat("yt1",&yt1)) yt1 = 0.0;

	/* get optional parameters */
	if (!getparint("nxb",&nxb)) nxb = nxm/2;
	if (!getparfloat("apet",&apet)) apet=45.;
	if (!getparint("alias",&alias)) alias = 0;
	if (!getparint("verbose",&verbose)) verbose = 1;
	if (!getparfloat("fmax",&fmax)) fmax = 0.25/dt;
	if (!getparfloat("ang",&ang)) ang = 180;
	cosa = cos(ang*PI/180);

	/* get optional parameters for output image*/
	if (!getparfloat("fxo",&fxo)) fxo = 0.0;
	/* check the first output trace	*/
	if (fxo<fxm )
		err("Does it make sense that fxo<fxm? \n");
	if (!getparfloat("dxo",&dxo)) dxo = 15.;
	if (!getparint("nxo",&nxo)) nxo = 101;
	if (!getparfloat("fyo",&fyo)) fyo=0.0;
	if (!getparfloat("dyo",&dyo)) dyo = 15.;
	if (!getparint("nyo",&nyo)) nyo = 101;
	if (!getparfloat("fzo",&fzo)) fzo = 0.;
	if (!getparfloat("dzo",&dzo)) dzo = 15.;
	if (!getparint("nzo",&nzo)) nzo = 101;

	/* print out the read-in parameters */
	fprintf(stderr,"\n parameters have been read in...");
	fprintf(stderr,"\n geo_type=%d,com_type=%d",geo_type,com_type);
	fprintf(stderr,"\n dt=%f,nt=%d,offs=%f",dt,nt,offs);
	fprintf(stderr,"\n nxm=%d,nym=%d",nxm,nym);
	fprintf(stderr,"\n dxm=%f,dym=%f",dxm,dym);
	fprintf(stderr,"\n fxm=%f,fym=%f",fxm,fym);
	fprintf(stderr,"\n vfile=%s",vfile);
	fprintf(stderr,"\n nxv=%d,nyv=%d,nzv=%d",nxv,nyv,nzv);
	fprintf(stderr,"\n dxv=%f,dyv=%f,dzv=%f",dxv,dyv,dzv);
	fprintf(stderr,"\n fxv=%f,fyv=%f,fzv=%f",fxv,fyv,fzv);
	fprintf(stderr,"\n tfile=%s",tfile);
	fprintf(stderr,"\n ampfile=%s",ampfile);
	fprintf(stderr,"\n d21file=%s",d21file);
	fprintf(stderr,"\n d22file=%s",d22file);
	fprintf(stderr,"\n d23file=%s",d23file);
	fprintf(stderr,"\n d31file=%s",d31file);
	fprintf(stderr,"\n d32file=%s",d32file);
	fprintf(stderr,"\n d33file=%s",d33file);
	fprintf(stderr,"\n a1file=%s",a1file);
	fprintf(stderr,"\n b1file=%s",b1file);
	fprintf(stderr,"\n dxt=%f,nxt=%d",dxt,nxt);
	fprintf(stderr,"\n dyt=%f,nyt=%d",dyt,nyt);
	fprintf(stderr,"\n dzt=%f,nzt=%d",dzt,nzt);
	fprintf(stderr,"\n xt0=%f,xt1=%f",xt0,xt1);
	fprintf(stderr,"\n yt0=%f,yt1=%f",yt0,yt1);
	fprintf(stderr,"\n nxb=%d,ang=%f,apet=%f",nxb,ang,apet);
	fprintf(stderr,"\n dxo=%f,nxo=%d,fxo=%f",dxo,nxo,fxo);
	fprintf(stderr,"\n dyo=%f,nyo=%d,fyo=%f",dyo,nyo,fyo);
	fprintf(stderr,"\n dzo=%f,nzo=%d,fzo=%f",dzo,nzo,fzo);
	fprintf(stderr,"\n fmax=%f,alias=%d,verbose=%d\n",fmax,alias,verbose);
	
	odt = 1.0/dt;
	smax = 0.5/(MAX(dxm,dym)*fmax);
	aliasx = 0.5/dxm/fmax;
	aliasy = 0.5/dym/fmax;

	/* allocate space */
	outtrace = ealloc3float(nzo,nxo,nyo);

	
	/* initialize output traces	*/
	for(ixo=0; ixo<nxo; ++ixo)
	    for(iyo=0;iyo<nyo;++iyo)
		for(izo=0; izo<nzo; ++izo)
			outtrace[iyo][ixo][izo]=0.0;
	
	/* allocate space for traveltimes and other quantities	*/ 
	txs = ealloc3float(nzt,nxt,nyt);
	txg = ealloc3float(nzt,nxt,nyt);
	if (com_type == 3 ) {
	   ampxs = ealloc3float(nzt,nxt,nyt);
	   ampxg = ealloc3float(nzt,nxt,nyt);
	   d21xs = ealloc3float(nzt,nxt,nyt);
	   d21xg = ealloc3float(nzt,nxt,nyt);
	   d22xs = ealloc3float(nzt,nxt,nyt);
	   d22xg = ealloc3float(nzt,nxt,nyt);
	   d23xs = ealloc3float(nzt,nxt,nyt);
	   d23xg = ealloc3float(nzt,nxt,nyt);
	   d31xs = ealloc3float(nzt,nxt,nyt);
	   d31xg = ealloc3float(nzt,nxt,nyt);
	   d32xs = ealloc3float(nzt,nxt,nyt);
	   d32xg = ealloc3float(nzt,nxt,nyt);
	   d33xs = ealloc3float(nzt,nxt,nyt);
	   d33xg = ealloc3float(nzt,nxt,nyt);
	}
	if ( com_type==2 || com_type == 3 ) {
	   a1xs = ealloc3float(nzt,nxt,nyt);
	   a1xg = ealloc3float(nzt,nxt,nyt);
	   b1xs = ealloc3float(nzt,nxt,nyt);
	   b1xg = ealloc3float(nzt,nxt,nyt);
	}

	vout = ealloc3float(nzo,nxo,nyo);
	aperture = ealloc1int(nzo);

	/* initialize output traces	*/
	if (com_type == 3 ) {
		for(ix=0; ix<nxt; ++ix)
		for(iy=0;iy<nyt;++iy)
		for(iz=0; iz<nzt; ++iz) {
			ampxs[iy][ix][iz]=1.0;
			ampxg[iy][ix][iz]=1.0;
			}
	}

	/* Read in velocity file */
	ReadVelocity(vfile, nxv, nyv, nzv, fxv, dxv, fyv, dyv, fzv, dzv, 
		nxo, nyo, nzo, fxo, dxo, fyo, dyo, fzo, dzo, vout); 

	/* determine the aperture array */
	for(iz=0; iz<nzo; ++iz) {
		if (apet < 89.0 )
		aperture[iz] = nxb+(dzo*iz*tan(apet*PI/180)+0.5)/dxm;
		else
		aperture[iz] = nxb;
	}
	fprintf(stderr,"\nvfile have been read in...\n");
	
	if(ABS(offs)>2*nxb*dxm) err("\t band NXB is too small!");

	if((nxm-1)*dxm<(nxo-1)*dxo || (nym-1)*dym<(nyo-1)*dyo) err("\t not enough range in input  		for the imaging");			    				
	vs = vout[0][0][0];
	smaxx = 0.5*vs/dxm/fmax;
	smaxy = 0.5*vs/dym/fmax;

	/* initialize varibles */
	amps = ampg =1.0;
	weight = 1.0;
	pxs = pys = pzs =0.0;
	pxg = pyg = pzg =0.0;
	d21s = d22s = d23s = d31s = d32s = d33s =0.0;
	d21g = d22g = d23g = d31g = d32g = d33g =0.0;
	ctheta = 0.0;
	det = 1.0;

	/* read in tables for special case. */
	if ( geo_type==3 ) {
	  /* traveltime table */
	  ReadTable(tfile,nxt,nyt,nzt,txs); 
	  ReadTable(tfile,nxt,nyt,nzt,txg); 

	  if ( com_type == 3 ) {

		/* amplitude table */
		ReadTable(ampfile,nxt,nyt,nzt,ampxs); 
		ReadTable(ampfile,nxt,nyt,nzt,ampxg); 

		/* d21 table */
		ReadTable(d21file,nxt,nyt,nzt,d21xs); 
		ReadTable(d21file,nxt,nyt,nzt,d21xg); 

		/* d22 table */
		ReadTable(d22file,nxt,nyt,nzt,d22xs); 
		ReadTable(d22file,nxt,nyt,nzt,d22xg); 

		/* d23 table */
		ReadTable(d23file,nxt,nyt,nzt,d23xs); 
		ReadTable(d23file,nxt,nyt,nzt,d23xg); 

		/* d31 table */
		ReadTable(d31file,nxt,nyt,nzt,d31xs); 
		ReadTable(d31file,nxt,nyt,nzt,d31xg); 

		/* d32 table */
		ReadTable(d32file,nxt,nyt,nzt,d32xs); 
		ReadTable(d32file,nxt,nyt,nzt,d32xg); 

		/* d33 table */
		ReadTable(d33file,nxt,nyt,nzt,d33xs); 
		ReadTable(d33file,nxt,nyt,nzt,d33xg); 

	   }

	   if (com_type==2 || com_type==3 ) {
		/* a1 table */
		ReadTable(a1file,nxt,nyt,nzt,a1xs); 
		ReadTable(a1file,nxt,nyt,nzt,a1xg); 

		/* b1 table */
		ReadTable(b1file,nxt,nyt,nzt,b1xs); 
		ReadTable(b1file,nxt,nyt,nzt,b1xg); 
	  }
	}

	/* loop over midpoints */
	for (ixm=0; ixm<nxm; ixm+=1){
	  xm = fxm+ixm*dxm;
   	  xs = xm-0.5*offs;
	  xg = xs+offs;
	  xs_beg = xs - dxt*(nxt-1)/2;
	  xg_beg = xg - dxt*(nxt-1)/2;

	  if (geo_type == 2 ) {
	    /* traveltime table */
	    GetFileNameType2(tfile, xs, xt0, xt1, xsfile);
	    GetFileNameType2(tfile, xg, xt0, xt1, xgfile);
	    ReadTable(xsfile,nxt,nyt,nzt,txs); 
	    ReadTable(xgfile,nxt,nyt,nzt,txg); 

	    if ( com_type == 3 ) {

		/* amplitude table */
		GetFileNameType2(ampfile, xs, xt0, xt1, xsfile);
		GetFileNameType2(ampfile, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,ampxs); 
		ReadTable(xgfile,nxt,nyt,nzt,ampxg); 

		/* d21 table */
		GetFileNameType2(d21file, xs, xt0, xt1, xsfile);
		GetFileNameType2(d21file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,d21xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d21xg); 

		/* d22 table */
		GetFileNameType2(d22file, xs, xt0, xt1, xsfile);
		GetFileNameType2(d22file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,d22xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d22xg); 

		/* d23 table */
		GetFileNameType2(d23file, xs, xt0, xt1, xsfile);
		GetFileNameType2(d23file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,d23xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d23xg); 

		/* d31 table */
		GetFileNameType2(d31file, xs, xt0, xt1, xsfile);
		GetFileNameType2(d31file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,d31xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d31xg); 

		/* d32 table */
		GetFileNameType2(d32file, xs, xt0, xt1, xsfile);
		GetFileNameType2(d32file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,d32xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d32xg); 

		/* d33 table */
		GetFileNameType2(d33file, xs, xt0, xt1, xsfile);
		GetFileNameType2(d33file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,d33xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d33xg); 

	    }

	    if (com_type==2 || com_type==3 ) {
		/* a1 table */
		GetFileNameType2(a1file, xs, xt0, xt1, xsfile);
		GetFileNameType2(a1file, xg, xt0, xt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,a1xs); 
		ReadTable(xgfile,nxt,nyt,nzt,a1xg); 

		/* b1 table */
		GetFileNameType2(b1file, xs, xt0, xt1, xsfile);
		GetFileNameType2(b1file, xg, xt0, xt1, xgfile); 
		ReadTable(xsfile,nxt,nyt,nzt,b1xs); 
		ReadTable(xgfile,nxt,nyt,nzt,b1xg); 

	    }
	  }

	  for (iym=0; iym<nym; ++iym){
	    ys=fym+ iym*dym;
	    ys_beg=ys - dyt*(nyt-1)/2;

	    if (geo_type == 1 ) {
	    /* traveltime table */
	    GetFileNameType1(tfile, xs, ys,xt0, xt1,yt0,yt1, xsfile);
	    GetFileNameType1(tfile, xg, ys,xt0, xt1,yt0,yt1, xgfile);
	    ReadTable(xsfile,nxt,nyt,nzt,txs); 
	    ReadTable(xgfile,nxt,nyt,nzt,txg); 

	    if ( com_type == 3 ) {

		/* amplitude table */
		GetFileNameType1(ampfile, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(ampfile, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,ampxs); 
		ReadTable(xgfile,nxt,nyt,nzt,ampxg); 

		/* d21 table */
		GetFileNameType1(d21file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(d21file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,d21xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d21xg); 

		/* d22 table */
		GetFileNameType1(d22file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(d22file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,d22xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d22xg); 

		/* d23 table */
		GetFileNameType1(d23file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(d23file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,d23xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d23xg); 

		/* d31 table */
		GetFileNameType1(d31file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(d31file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,d31xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d31xg); 

		/* d32 table */
		GetFileNameType1(d32file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(d32file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,d32xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d32xg); 

		/* d33 table */
		GetFileNameType1(d33file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(d33file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,d33xs); 
		ReadTable(xgfile,nxt,nyt,nzt,d33xg); 

	    }

	    if (com_type==2 || com_type==3 ) {
		/* a1 table */
		GetFileNameType1(a1file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(a1file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,a1xs); 
		ReadTable(xgfile,nxt,nyt,nzt,a1xg); 

		/* b1 table */
		GetFileNameType1(b1file, xs,ys, xt0, xt1,yt0,yt1, xsfile);
		GetFileNameType1(b1file, xg,ys, xt0, xt1,yt0,yt1, xgfile);
		ReadTable(xsfile,nxt,nyt,nzt,b1xs); 
		ReadTable(xgfile,nxt,nyt,nzt,b1xg); 

	    }
	    }

		/* loop over output points */
		for (iyo=0,yo=fyo; iyo<nyo; ++iyo, yo+=dyo){
		  yi = (yo-ys_beg)/dyt;
		  iy = yi;
		  sy = yi-iy;

		  yig = (yo-ys_beg)/dyt;
		  iyg = yig;
		  syg = yig-iyg;

		  if (iy<0 ||iy > nyt-2 ) continue;
		  if (iy==0 ||sy < 0 ) continue;
		  if (iyg<0 ||iyg > nyt-2 ) continue;
		  if (iyg==0 ||syg < 0 ) continue;

		  for (ixo=0,xo=fxo; ixo<nxo; ++ixo,xo+=dxo){

		    i=sqrt((xo-xm)*(xo-xm)+(yo-ys)*(yo-ys))/dxm;
		    if(i>nxb) continue; 

		    xi = (xo-xs_beg)/dxt;
		    ix = xi;
		    sx = xi-ix;

		    xig = (xo-xg_beg)/dxt;
		    ixg = xig;
		    sxg = xig-ixg;

		    if (ix<0 ||ix > nxt-2 ) continue;
		    if (ix==0 ||sx < 0 ) continue;
		    if (ixg<0 ||ixg > nxt-2 ) continue;
		    if (ixg==0 ||sxg < 0 ) continue;

		    for (izo=1,zo=fzo+dzo; izo<nzo; ++izo,zo+=dzo){ 
			
			/* check range of output */

			if(i>aperture[izo]) continue; 

			/* determine sample indices */
			zi = zo/dzt;
			iz = zi;
			if(iz<1||iz >nzt-2) continue;

			/* bilinear interpolation */
			sz = zi-iz;

			tsd = (1.0-sy)*((1.0-sz)*((1.0-sx)*txs[iy][ix][iz] +
						     sx*txs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*txs[iy][ix][iz+1] 
						  + sx*txs[iy][ix+1][iz+1]));

  			tsd+=sy*((1.0-sz)*((1.0-sx)*txs[iy+1][ix][iz] +
						sx*txs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*txs[iy+1][ix][iz+1]
					     + sx*txs[iy+1][ix+1][iz+1]));     

			tgd = (1.0-syg)*((1.0-sz)*((1.0-sxg)*txg[iyg][ixg][iz] +
						     sxg*txg[iyg][ixg+1][iz]) + 
					    sz*((1.0-sxg)*txg[iyg][ixg][iz+1] 
						  + sxg*txg[iyg][ixg+1][iz+1]));

  			tgd+=syg*((1.0-sz)*((1.0-sxg)*txg[iyg+1][ixg][iz] +
						sxg*txg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*txg[iyg+1][ixg][iz+1]
					     + sxg*txg[iyg+1][ixg+1][iz+1]));

		if (com_type ==3 ) {
		
			amps = (1.0-sy)*((1.0-sz)*((1.0-sx)*ampxs[iy][ix][iz] +
						     sx*ampxs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*ampxs[iy][ix][iz+1] 
						  + sx*ampxs[iy][ix+1][iz+1]));

  			amps+=sy*((1.0-sz)*((1.0-sx)*ampxs[iy+1][ix][iz] +
						sx*ampxs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*ampxs[iy+1][ix][iz+1]
					     + sx*ampxs[iy+1][ix+1][iz+1]));

			if (amps <TINY ) continue;

			ampg=(1.0-syg)*((1.0-sz)*((1.0-sxg)*ampxg[iyg][ixg][iz]
						 + sxg*ampxg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*ampxg[iyg][ixg][iz+1] 
						+ sxg*ampxg[iyg][ixg+1][iz+1]));

  			ampg+=syg*((1.0-sz)*((1.0-sxg)*ampxg[iyg+1][ixg][iz] +
						sxg*ampxg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*ampxg[iyg+1][ixg][iz+1]
					     + sxg*ampxg[iyg+1][ixg+1][iz+1]));

			if (ampg <TINY ) continue;
		}

		if (com_type ==3 ) {
			d21s = (1.0-sy)*((1.0-sz)*((1.0-sx)*d21xs[iy][ix][iz] +
						     sx*d21xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*d21xs[iy][ix][iz+1] 
						  + sx*d21xs[iy][ix+1][iz+1]));

  			d21s+=sy*((1.0-sz)*((1.0-sx)*d21xs[iy+1][ix][iz] +
						sx*d21xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*d21xs[iy+1][ix][iz+1]
					     + sx*d21xs[iy+1][ix+1][iz+1]));
			
			d21g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*d21xg[iyg][ixg][iz]
						 + sxg*d21xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*d21xg[iyg][ixg][iz+1] 
						+ sxg*d21xg[iyg][ixg+1][iz+1]));

  			d21g+=syg*((1.0-sz)*((1.0-sxg)*d21xg[iyg+1][ixg][iz] +
						sxg*d21xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*d21xg[iyg+1][ixg][iz+1]
					     + sxg*d21xg[iyg+1][ixg+1][iz+1]));

			d22s = (1.0-sy)*((1.0-sz)*((1.0-sx)*d22xs[iy][ix][iz] +
						     sx*d22xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*d22xs[iy][ix][iz+1] 
						  + sx*d22xs[iy][ix+1][iz+1]));

  			d22s+=sy*((1.0-sz)*((1.0-sx)*d22xs[iy+1][ix][iz] +
						sx*d22xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*d22xs[iy+1][ix][iz+1]
					     + sx*d22xs[iy+1][ix+1][iz+1]));
			
			d22g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*d22xg[iyg][ixg][iz]
						 + sxg*d22xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*d22xg[iyg][ixg][iz+1] 
						+ sxg*d22xg[iyg][ixg+1][iz+1]));

  			d22g+=syg*((1.0-sz)*((1.0-sxg)*d22xg[iyg+1][ixg][iz] +
						sxg*d22xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*d22xg[iyg+1][ixg][iz+1]
					     + sxg*d22xg[iyg+1][ixg+1][iz+1]));

			d23s = (1.0-sy)*((1.0-sz)*((1.0-sx)*d23xs[iy][ix][iz] +
						     sx*d23xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*d23xs[iy][ix][iz+1] 
						  + sx*d23xs[iy][ix+1][iz+1]));

  			d23s+=sy*((1.0-sz)*((1.0-sx)*d23xs[iy+1][ix][iz] +
						sx*d23xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*d23xs[iy+1][ix][iz+1]
					     + sx*d23xs[iy+1][ix+1][iz+1]));
			
			d23g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*d23xg[iyg][ixg][iz]
						 + sxg*d23xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*d23xg[iyg][ixg][iz+1] 
						+ sxg*d23xg[iyg][ixg+1][iz+1]));

  			d23g+=syg*((1.0-sz)*((1.0-sxg)*d23xg[iyg+1][ixg][iz] +
						sxg*d23xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*d23xg[iyg+1][ixg][iz+1]
					     + sxg*d23xg[iyg+1][ixg+1][iz+1]));

			d31s = (1.0-sy)*((1.0-sz)*((1.0-sx)*d31xs[iy][ix][iz] +
						     sx*d31xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*d31xs[iy][ix][iz+1] 
						  + sx*d31xs[iy][ix+1][iz+1]));

  			d31s+=sy*((1.0-sz)*((1.0-sx)*d31xs[iy+1][ix][iz] +
						sx*d31xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*d31xs[iy+1][ix][iz+1]
					     + sx*d31xs[iy+1][ix+1][iz+1]));
			
			d31g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*d31xg[iyg][ixg][iz]
						 + sxg*d31xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*d31xg[iyg][ixg][iz+1] 
						+ sxg*d31xg[iyg][ixg+1][iz+1]));

  			d31g+=syg*((1.0-sz)*((1.0-sxg)*d31xg[iyg+1][ixg][iz] +
						sxg*d31xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*d31xg[iyg+1][ixg][iz+1]
					     + sxg*d31xg[iyg+1][ixg+1][iz+1]));

			d32s = (1.0-sy)*((1.0-sz)*((1.0-sx)*d32xs[iy][ix][iz] +
						     sx*d32xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*d32xs[iy][ix][iz+1] 
						  + sx*d32xs[iy][ix+1][iz+1]));

  			d32s+=sy*((1.0-sz)*((1.0-sx)*d32xs[iy+1][ix][iz] +
						sx*d32xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*d32xs[iy+1][ix][iz+1]
					     + sx*d32xs[iy+1][ix+1][iz+1]));
			
			d32g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*d32xg[iyg][ixg][iz]
						 + sxg*d32xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*d32xg[iyg][ixg][iz+1] 
						+ sxg*d32xg[iyg][ixg+1][iz+1]));

  			d32g+=syg*((1.0-sz)*((1.0-sxg)*d32xg[iyg+1][ixg][iz] +
						sxg*d32xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*d32xg[iyg+1][ixg][iz+1]
					     + sxg*d32xg[iyg+1][ixg+1][iz+1]));

			d33s = (1.0-sy)*((1.0-sz)*((1.0-sx)*d33xs[iy][ix][iz] +
						     sx*d33xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*d33xs[iy][ix][iz+1] 
						  + sx*d33xs[iy][ix+1][iz+1]));

  			d33s+=sy*((1.0-sz)*((1.0-sx)*d33xs[iy+1][ix][iz] +
						sx*d33xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*d33xs[iy+1][ix][iz+1]
					     + sx*d33xs[iy+1][ix+1][iz+1]));
			
			d33g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*d33xg[iyg][ixg][iz]
						 + sxg*d33xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*d33xg[iyg][ixg][iz+1] 
						+ sxg*d33xg[iyg][ixg+1][iz+1]));

  			d33g+=syg*((1.0-sz)*((1.0-sxg)*d33xg[iyg+1][ixg][iz] +
						sxg*d33xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*d33xg[iyg+1][ixg][iz+1]
					     + sxg*d33xg[iyg+1][ixg+1][iz+1]));

		}

		if (com_type==2 || com_type ==3 ) {

			a1s = (1.0-sy)*((1.0-sz)*((1.0-sx)*a1xs[iy][ix][iz] +
						     sx*a1xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*a1xs[iy][ix][iz+1] 
						  + sx*a1xs[iy][ix+1][iz+1]));

  			a1s+=sy*((1.0-sz)*((1.0-sx)*a1xs[iy+1][ix][iz] +
						sx*a1xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*a1xs[iy+1][ix][iz+1]
					     + sx*a1xs[iy+1][ix+1][iz+1]));
			
			a1g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*a1xg[iyg][ixg][iz]
						 + sxg*a1xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*a1xg[iyg][ixg][iz+1] 
						+ sxg*a1xg[iyg][ixg+1][iz+1]));

  			a1g+=syg*((1.0-sz)*((1.0-sxg)*a1xg[iyg+1][ixg][iz] +
						sxg*a1xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*a1xg[iyg+1][ixg][iz+1]
					     + sxg*a1xg[iyg+1][ixg+1][iz+1]));

			b1s = (1.0-sy)*((1.0-sz)*((1.0-sx)*b1xs[iy][ix][iz] +
						     sx*b1xs[iy][ix+1][iz]) + 
					    sz*((1.0-sx)*b1xs[iy][ix][iz+1] 
						  + sx*b1xs[iy][ix+1][iz+1]));

  			b1s+=sy*((1.0-sz)*((1.0-sx)*b1xs[iy+1][ix][iz] +
						sx*b1xs[iy+1][ix+1][iz]) + 
					sz*((1.0-sx)*b1xs[iy+1][ix][iz+1]
					     + sx*b1xs[iy+1][ix+1][iz+1]));
			
			b1g=(1.0-syg)*((1.0-sz)*((1.0-sxg)*b1xg[iyg][ixg][iz]
						 + sxg*b1xg[iyg][ixg+1][iz])
					+ sz*((1.0-sxg)*b1xg[iyg][ixg][iz+1] 
						+ sxg*b1xg[iyg][ixg+1][iz+1]));

  			b1g+=syg*((1.0-sz)*((1.0-sxg)*b1xg[iyg+1][ixg][iz] +
						sxg*b1xg[iyg+1][ixg+1][iz]) + 
					sz*((1.0-sxg)*b1xg[iyg+1][ixg][iz+1]
					     + sxg*b1xg[iyg+1][ixg+1][iz+1]));

			pxs = sin(a1s)*cos(b1s);
			pys = sin(a1s)*sin(b1s);
			pzs = cos(a1s);

			pxg = sin(a1g)*cos(b1g);
			pyg = sin(a1g)*sin(b1g);
			pzg = cos(a1g);

			ctheta = pxs*pxg+pys*pyg+pzs*pzg;
		}
		
		if ( com_type ==3 ) {
			d11 = (pxs+pxg);
			d12 = (pys+pyg);
			d13 = (pzs+pzg);
			d21 = d21s+d21g;
			d22 = d22s+d22g;
			d23 = d23s+d23g;
			d31 = d31s+d31g;
			d32 = d32s+d32g;
			d33 = d33s+d33g;

			det = d11*d22*d33 + d21*d32*d13 + d31*d12*d23
			    - d31*d22*d13 - d21*d12*d33 - d11*d32*d23;

		}

		if (com_type==1 ) weight=1.0; 
		if (com_type==2 ) weight = ctheta;
		if (com_type==3 ) 
			weight = det/amps/ampg/(1.0+ctheta);

			samp = (tsd+tgd)*odt;
			nsamp = samp;
			if (nsamp>nt-2) continue;
			samp = samp-nsamp;
			/* check operator aliasing */
			if (alias==1) {
			  if (pxs+pxg > vout[iyo][ixo][izo]*aliasx) weight =0.0;
			  if (pys+pyg > vout[iyo][ixo][izo]*aliasy) weight =0.0;
			} 
			
			/*output summation*/
			outtrace[iyo][ixo][izo] += weight*(samp*tr.data[nsamp+1]
				+(1.0-samp)*tr.data[nsamp]);

		    }
		  }
		}
		if (geo_type ==1 ) gettr(&tr); 
	  }
	  if( geo_type !=1 ) gettr(&tr); 
 	  if(verbose) fprintf(stderr, "\tfinish line%d\n", ixm+1); 
	}

 
	/* write trace */
	temp = dxm*dym/16/PI/PI/PI;  /* beta_1 */
/*	temp = dxm*dym/8/PI/PI/PI; */ /* beta_2 */
	for (iyo=0; iyo<nyo; ++iyo)
		for(ixo=0; ixo<nxo; ++ixo){
		/* scale output traces */
 		for(izo=0; izo<nzo; ++izo) {	
			if (com_type == 3) 
			   outtrace[iyo][ixo][izo] *= vout[iyo][ixo][izo];
			outtrace[iyo][ixo][izo] *= temp;
		}
		/* make headers for output traces */
		tro.offset = offs;
		tro.tracl = tro.tracr = 1+iyo*nxo+ixo;
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

	free3float(txs);
	free3float(txg);
	free3float(vout);
	if (com_type == 2 || com_type == 3) {
		free3float(a1xs);
		free3float(a1xg);
		free3float(b1xs);
		free3float(b1xg);
	}
	if (com_type == 3) {
		free3float(ampxs);
		free3float(ampxg);
		free3float(d21xs);
		free3float(d21xg);
		free3float(d22xs);
		free3float(d22xg);
		free3float(d23xs);
		free3float(d23xg);
		free3float(d31xs);
		free3float(d31xg);
		free3float(d32xs);
		free3float(d32xg);
		free3float(d33xs);
		free3float(d33xg);
	}

	return EXIT_SUCCESS;
}

void GetFileNameType1(char *infile,float xs,float ys,float xs0,float xs1,float ys0,float ys1,char *xsfile) 
{
	char 	xstmp[80], append[20];
	int	iappxs,iappys;
	float	x,y;

	strcpy(xstmp,infile);

	x = xs;
	if (x < xs0) x = xs0;
	if (x > xs1) x = xs1;
	y = ys;
	if (y < ys0) y = ys0;
	if (y > ys1) y = ys1;

/*	if (x >= 0.0 ) iappxs=(x+0.0001)*1000;
	else iappxs=(x-0.0001)*1000;
	if (y >= 0.0) iappys=(y+0.0001)*1000;
	else iappys=(y-0.0001)*1000;*/
	if (x >= 0.0 ) iappxs=x+0.0001;
	else iappxs=x-0.0001;
	if (y >= 0.0) iappys=y+0.0001;
	else iappys=y-0.0001;

	sprintf(append,"%i",iappxs);
	strcat(xstmp, append);
	sprintf(append,"_%i",iappys);
	strcat(xstmp, append);
	strcpy(xsfile, xstmp);

}

void GetFileNameType2(char *infile,float xs,float xs0,float xs1,char *xsfile)
{
	char 	xstmp[80],append[20];
	int	iappxs;
	float   x;

	strcpy(xstmp,infile);

	x = xs;
	if (x < xs0) x = xs0;
	if (x > xs1) x = xs1;

/*	if (x >= 0.0 ) iappxs=(x+0.0001)*1000;
	else iappxs=(x-0.0001)*1000; */
	if (x >= 0.0 ) iappxs=x+0.0001;
	else iappxs=x-0.0001;

	sprintf(append,"%i",iappxs);
	strcat(xstmp, append);
	strcpy(xsfile, xstmp);
}

short ReadTable(char *infile,int nxt,int nyt,int nzt,float ***outArray) 
{
	FILE	*ifp;
	int	iread, ix,iy,iz;
	float	*t3d;

	t3d = ealloc1float(nzt*nxt*nyt);

	if ( (ifp=fopen(infile,"r")) ==NULL ) { 
		fprintf(stderr,"cannot open file %s\n", infile);
		return -1;
	}

	iread=fread(t3d,sizeof(float),nzt*nxt*nyt,ifp);
	if (iread != nxt*nyt*nzt) {
		fprintf(stderr,"Only read %d values of %s\n",iread,infile);
		free1float(t3d);
		return -2;
	}
	fclose(ifp);

   	for(iy=0; iy<nyt; ++iy)
	   for(ix=0; ix<nxt; ++ix)
		for(iz=0; iz<nzt; ++iz)
			outArray[iy][ix][iz]=t3d[iz+ix*nzt+iy*nzt*nxt]; 

	free1float(t3d);
	return 0;
}

short ReadVelocity(char *vfile,int nxv,int nyv,int nzv,float fxv,float dxv,
float fyv,float dyv,float fzv,float dzv,int nxo,int nyo,int nzo,float fxo,
float dxo, float fyo,float dyo,float fzo,float dzo,float ***vout) 
{
	FILE	*ifp;
	int	iread, ixo,iyo,izo,ix,iy,iz;
	int	i1,i2,i3,i4,i5,i6,i7,i8;
	float	xi,sx,yi,sy,zi,sz;
	float   *v;

	if ( (ifp=fopen(vfile,"r")) ==NULL ) { 
		fprintf(stderr,"cannot open velocity file %s", vfile);
		return -1;
	}

	v = ealloc1float(nzv*nxv*nyv);
	iread=fread(v,sizeof(float),nzv*nxv*nyv,ifp);
	if (iread != nxv*nyv*nzv) {
		fprintf(stderr,"Only read %d values of %s\n",iread,vfile);
		free1float(v);
		return -2;
	}
	fclose(ifp);
/*	fprintf(stderr,"read %d values of %s\n",iread,vfile);*/

	if (nxv == 1 && nyv ==1) {
	   for(iyo=0; iyo<nyo; ++iyo) 
	   for(ixo=0; ixo<nxo; ++ixo) 
		for(izo=0; izo<nzo; ++izo) {
		   zi = (fzo + izo*dzo - fzv)/dzv;
		   iz = zi;
		   sz = zi - iz;
		   if (iz <0 ) { iz =0; sz = 0.0; }
		   if (nzv>1 && iz >nzv-2 ) { iz =nzv-2; sz = 1.0; }

		   vout[iyo][ixo][izo] = (1.0-sz)*v[iz] + sz*v[iz+1];
		}
	}
	else if (nxv > 1 && nyv ==1) {
	   for(iyo=0; iyo<nyo; ++iyo) 
	   for(ixo=0; ixo<nxo; ++ixo) {
		xi = (fxo + ixo*dxo - fxv)/dxv;
		ix = xi;
		sx = xi - ix;
		if (ix <0 ) { ix =0; sx = 0.0; }
		if (ix >nxv-2 ) { ix =nxv-2; sx = 1.0; }

		for(izo=0; izo<nzo; ++izo) {
		   zi = (fzo + izo*dzo - fzv)/dzv;
		   iz = zi;
		   sz = zi - iz;
		   if (iz <0 ) { iz =0; sz = 0.0; }
		   if (nzv>1 && iz >nzv-2 ) { iz =nzv-2; sz = 1.0; }

		   i1 = ix*nzv + iz;
		   i2 = (ix+1)*nzv + iz;
		   i3 = ix*nzv + iz +1;
		   i4 = (ix+1)*nzv + iz +1;
		   vout[iyo][ixo][izo] = 
			   (1.0-sz)*((1.0-sx)*v[i1] + sx*v[i2]) 
			   + sz*((1.0-sx)*v[i3] + sx*v[i4]);
		}
	   }
	}
	else if (nxv > 1 && nyv >1) {
   	for(iyo=0; iyo<nyo; ++iyo) {
	   yi = (fyo + iyo*dyo - fyv)/dyv;
	   iy = yi;
	   sy = yi - iy;
	   if (iy <0 ) { iy =0; sy = 0.0; }
	   if (iy >nyv-2 ) { iy =nyv-2; sy = 1.0; }

	   for(ixo=0; ixo<nxo; ++ixo) {
		xi = (fxo + ixo*dxo - fxv)/dxv;
		ix = xi;
		sx = xi - ix;
		if (ix <0 ) { ix =0; sx = 0.0; }
		if (ix >nxv-2 ) { ix =nxv-2; sx = 1.0; }

		for(izo=0; izo<nzo; ++izo) {
		   zi = (fzo + izo*dzo - fzv)/dzv;
		   iz = zi;
		   sz = zi - iz;
		   if (iz <0 ) { iz =0; sz = 0.0; }
		   if (nzv>1 && iz >nzv-2 ) { iz =nzv-2; sz = 1.0; }

		   i1 = iy*nxv*nzv + ix*nzv + iz;
		   i2 = iy*nxv*nzv + (ix+1)*nzv + iz;
		   i3 = iy*nxv*nzv + ix*nzv + iz +1;
		   i4 = iy*nxv*nzv + (ix+1)*nzv + iz +1;
		   i5 = (iy+1)*nxv*nzv + ix*nzv + iz;
		   i6 = (iy+1)*nxv*nzv + (ix+1)*nzv + iz;
		   i7 = (iy+1)*nxv*nzv + ix*nzv + iz +1;
		   i8 = (iy+1)*nxv*nzv + (ix+1)*nzv + iz +1;
		   vout[iyo][ixo][izo] = 
			   (1.0-sy)*((1.0-sz)*((1.0-sx)*v[i1] + sx*v[i2]) 
			   + sz*((1.0-sx)*v[i3] + sx*v[i4]))
  			   + sy*((1.0-sz)*((1.0-sx)*v[i5] + sx*v[i6]) 
			   + sz*((1.0-sx)*v[i7] + sx*v[i8]));     

		}
	   }
	}
	}

	free1float(v);
	return 0;   
}
